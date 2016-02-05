# parse the toggling events in this file

# 1. generate a signal LUT
# 2. count the events

#' Parses a VCD file and generated toggle count statistics.
#'
#' @param vcd The VCDFile to parse
#' @param top The top signal, if none is given the top signal of the vcdfile is taken
#' @param depth depth in the module tree before signals are accumulated, default=0 (top only), -1 is infinite depth
#'
#' For more elaborate filtering of nodes, pre-edit the signal-tree in the vcd-object.
#' Black/Whitelisting of signals is an optional further enhancement.
#'
#' @return a module tree and a list of toggle counts and a list of dump events
#'
#' parseToggles(vcd,"top",3)
#' parseToggles(vcd,"SBOX1",3)


# examples, not run
# parseToggles(vcd,"top",3)
# parseToggles(vcd,"SBOX1",3)

parseToggles <- function(vcd,top=NA,depth=0){
  # assume we have a sane VCDFile object

  # 1. find the desired root signal
  vartree <- vcd$hierarchy

  if (!is.na(top)) topNode <- Find(vartree$root,top,"level")
  if (is.null(topNode)) topNode <- vartree$root

  # 2. make a copy of the tree for working on
  vartree <- Clone(topNode,attributes = T)

  # 3. prepare the list of signals
  rootLevel <- vartree$level # in data.tree this is 1 (but was documented as 0 in v0.2.4)
  detailLevel <- min(vartree$height,rootLevel + depth)
  detailSignals <- data.tree::Traverse(vartree, traversal = "level", pruneFun = function(x) {x$level <= detailLevel}) # keep all with a lower level

  # 4. select all multibit signals and add the artificially generated subsignals for counting
  # the individual bits of the last level nultibit-values need to be included now
  # as the have level == detailLevel + 1 and are not captured by the above traversal
  multiBitIdxs <- which(unlist(sapply(detailSignals,function(x) {return((x$bits > 1)&&(x$level==detailLevel))})))
  vBitSignals <- unlist(sapply(detailSignals[multiBitIdxs],function(node) node$children))
  relevantSignals <- c(detailSignals,vBitSignals)

  # 5. collect the signal names and construct mapping (hashtable) for signals
  #    that are mapped to the same group for counting
  #    also create a mapping from nodenames to nodes

  #    signal names are not unique, see IEEE Std 1364-2001 version C §18.2.3.8, Comment b (p.336)
  #    in that case the toggling event is accounted for only once
  #    we do not distinguish by modules

  nameBucketLUT <- hash::hash()
  for (node in relevantSignals) {
    bucket <- node$name
    for (sig in node$Get("name")) {
      nameBucketLUT[[sig]] <- bucket
    }
  }
  on.exit(hash::clear(nameBucketLUT),add=T)

  names(relevantSignals) <- sapply(relevantSignals,function(x) x$name)
  nodeByNameLUT <- hash::hash(relevantSignals)
  on.exit(hash::clear(nodeByNameLUT),add=T)

  # 6. prepare toggle count structure
  # growing vectors/lists at last level are a huge performance problem
  # linking lists is much better, performance is limited by the memory demand of these deeply nested lists
  # collapsing them into a vector (unlist) when they get too big might help
  # the final unlist step also works, when the $val entry holds a vector instead of a single value

  vallist<-unique(hash::values(nameBucketLUT))

  # make lists of lists of lists
  # about twice as fast as hashtable of lists of lists
  counts <- vector("list",length(vallist))
  names(counts)<-vallist
  for (i in vallist) {
    counts[[i]] <- vector("list",4)
    names(counts[[i]]) <- c("0","1","z","x")
    # a dummy timestamp "X" spares checking for NULL/NAN in the inner loop
    for (j in 1:4) counts[[i]][[j]] <- list(time=list(val="X"),count=list(val=0L))
  }

  timestamps <- vector("list",0L)

  # making counts a hashtable makes things surprisingly slower,
  # but we can make a sig to bucketIdx LUT for faster list element accesses later
  # hashtable of lists of lists .. suprisingly slower ..

  nameIdxList <- 1L:as.integer(length(counts))
  names(nameIdxList) <- names(counts)

  mapping<-sapply(hash::names.hash(nameBucketLUT),function(x) nameIdxList[[nameBucketLUT[[x]]]])
  names(mapping)<-hash::names.hash(nameBucketLUT)
  nameBucketIdxLUT <- hash::hash(mapping)
  on.exit(hash::clear(nameBucketIdxLUT),add=T)


  # 7. let the parsing fun begin (using readr::read_lines)
  # TODO: here we assume one entry per line.
  # for future releases use a more sophisticated reader that can deliver tokens
  if (!file.exists(vcd$filename)) {
    warning("File does not exist: ", vcd$filename)
  }

  con <- file(
    vcd$filename, "r", blocking = TRUE,
    encoding = getOption("encoding"), raw = FALSE
  )
  on.exit(close(con))

  # we assume dumpstart was set sensible and scan to the next timestamp
  readLines(con, n = (vcd$dumpstart-2)) #skip
  event <- readLines(con, n = 1)
  while (strHead(event) != "#") {
    event <- readLines(con, n = 1) #readr::read_Lines does not work for subsequent reads on a connection
    if (length(event) == 0) {
      warning("premature end of file")
      break
    }
  }

  timestamp <- "0"
  multibitvals <- hash::hash()
  on.exit(hash::clear(multibitvals),add=T)

  #readLine return empty vector when EOF is reached, "" for an empty line
  while(length(event)) {
    indicator <- strHeadLower(event)

        # TIMESTAMP
    if (indicator == "#") {
      timestamp <- strTail(event)
      timestamps<-list(prev=timestamps,val=timestamp)
    }
    # DUMP-EVENT
    if (indicator == "$") {
      #dump events are currently ignored
      #just cache of multibit-Signals is updated

      while (event != "$end") {
        indicator <- strHeadLower(event)

        if (isMultiBit(indicator)) {
          valname <- strsplit(strTail(event)," ")[[1]]
          sig <- valname[2]
          mbNode <- nodeByNameLUT[[sig]]
          # mbNode will be NULL for signals we do not want to count
          if (!is.null(mbNode)) {
            val <-
              leftExtend(valname[1],as.integer(mbNode$bits))
            multibitvals[[sig]] <- val
          }
        }

        event <- readLines(con, n = 1)
        if (length(event) == 0) break
      }
      #fix the indicator overwritten by inner loop
      indicator <- "$"
    }

    # SCALAR
    indicatorIdx <- scalarIndicatorToInt(indicator)
    # if (any(indicator == c("0","1","x","z"))) {
    if (indicatorIdx) {

      sig <- strTail(event)
      bucket <- nameBucketIdxLUT[[sig]]

      # bucket will be NULL for signals we do not want to count
      # we only create a new chain link, when we have a new timestamp
      # the lookups are taking some time, but assigning the results to intermediate values
      # make R copy the whole linked list of list which is a real performance killer
      if (!(is.null(bucket))) {
        tval <- counts[[bucket]][[indicatorIdx]]$time$val
        if ((!is.null(tval)) && (tval != timestamp)) {
          counts[[bucket]][[indicatorIdx]]$time <- list(prev=counts[[bucket]][[indicatorIdx]]$time,val=timestamp)
          counts[[bucket]][[indicatorIdx]]$count <- list(prev=counts[[bucket]][[indicatorIdx]]$count,val=1L)
        } else {
          counts[[bucket]][[indicatorIdx]]$count$val <- counts[[bucket]][[indicatorIdx]]$count$val + 1L
        }
      }
    }

    # MULTIBIT-VARIABLE
    if (isMultiBit(indicator)) {
      valname <- strsplit(strTail(event)," ")[[1]]
      sig <- valname[2]
      mbNode <- nodeByNameLUT[[sig]]
      # mbNode will be NULL for signals we do not want to count
      if (!is.null(mbNode)) {
        val <-
          leftExtend(valname[1],as.integer(mbNode$bits))

        lastval <- multibitvals[[sig]]
        if (is.null(lastval))
          lastval <- val

        # compare bitwise and set toggle counts
        bits.lastval <- strRevAndSplit(lastval)
        bits.val     <- strRevAndSplit(val)

        whichToggled <- which(!bits.lastval == bits.val)
        bitsig <- sapply(whichToggled, function(i) paste0(c(sig,".",as.character(i - 1)),collapse = ""))
        buckets <- sapply(bitsig, function(x) nameBucketIdxLUT[[x]])
        for (j in 1:length(whichToggled)) {
          i <- whichToggled[j]
          bucket <- buckets[j]
          indicatorIdx <- scalarIndicatorToInt(bits.val[i])
          tval <- counts[[bucket]][[indicatorIdx]]$time$val
          if ((!is.null(tval)) && (tval != timestamp)) {
            counts[[bucket]][[indicatorIdx]]$time <- list(prev=counts[[bucket]][[indicatorIdx]]$time,val=timestamp)
            counts[[bucket]][[indicatorIdx]]$count <- list(prev=counts[[bucket]][[indicatorIdx]]$count,val=1L)
          } else {
            counts[[bucket]][[indicatorIdx]]$count$val <- counts[[bucket]][[indicatorIdx]]$count$val + 1L
          }
        }

        multibitvals[[sig]] <- val
      }
    }

    event <- readLines(con, n = 1)
  }

  #finally we can prune vartree
  #vartree$Prune(pruneFun = function(x) {x$level <= detailLevel}) # broken in data.tree 0.2.4
  vartree <- Clone(topNode,attributes = T,pruneFun = function(x) {x$level <= detailLevel})

  # and unlist the counts
  for (i in names(counts)) {
    for (j in names(counts[[i]])) {
      # [-1] removes the dummy timestamp
      times <- unlist(counts[[i]][[j]]$time,use.names = F)[-1]
      counts[[i]][[j]]<-unlist(counts[[i]][[j]]$count,use.names = F)[-1]
      names(counts[[i]][[j]])<-times
    }
  }

  timestamps <- unlist(timestamps, use.names = F)

  return(list(hierarchy = vartree,counts = counts,timestamps = timestamps))
}

