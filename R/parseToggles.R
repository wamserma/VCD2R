# parse the toggling events in this file

# 1. generate a signal LUT
# 2. count the events

#' Parses a VCD file and generated toggle count statistics.
#'
#' @param vcd The VCDFile to parse
#' @param top The top signal, if none is given the top signal of the vcdfile is taken
#' @param depth depth in the module tree before signals are accumulated, default=0 (top only), -1 is infinite depth
#'
#' @return a module tree and a list of toggle counts and a list of dump events
#'
#' parseToggles(vcd,"top",3)
#' parseToggles(vcd,"SBOX1",3)
#'
#' @importFrom Kmisc str_rev

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
  accumSignals <- data.tree::Traverse(vartree, traversal = "level", filterFun = function(x) {x$level == detailLevel})

  # TODO: allow filtering on var type: argument varfilter is a character vector of types, the filter function is expandend by any(x$type==varfilter)

  # 4. select all multibit signals and add the artificially generated subsignals for counting
  multiBitIdxs <- which(unlist(sapply(accumSignals,function(x) x$bits > 1)))
  vBitSignals <- unlist(sapply(accumSignals[multiBitIdxs],function(node) node$children))
  relevantSignals <- c(detailSignals,accumSignals[-multiBitIdxs],vBitSignals)
  detailSignals <- c(detailSignals,vBitSignals)
  accumSignals <- c(accumSignals[-multiBitIdxs],vBitSignals)

  # 5. collect the signal names and construct mapping (hashtable) for signals
  #    that are mapped to the same group for counting
  #    also create a mapping from nodenames to nodes

  nameBucketLUT <- hash::hash()
  for (node in relevantSignals) {
    bucket <- node$name
    for (sig in node$Get("name")) {
      nameBucketLUT[sig] <- bucket
    }
  }
  on.exit(hash::clear(nameBucketLUT),add=T)


  # 6. prepare toggle count structure
  # we use nested lists
  # TODO: use hash instead of list on top level and check performance
  # growing vectors/lists at last level are a huge performance problem
  # not huge but FUCKING HORRIBLE
  # linking lists is much better, performance is limited by the memory demand of these deeply nested lists
  # collapsing them into a vector (unlist) when they get too big might help
  # the final unlist step also works, when the $val entry holds a vector instead of a single value

  vallist<-unique(hash::values(nameBucketLUT))
  counts <- vector("list",length(vallist))
  names(counts)<-vallist

  for (i in vallist) {
    counts[[i]] <- vector("list",4)
    names(counts[[i]]) <- c("0","1","z","x")
    for (j in 1:4) counts[[i]][[j]] <- list(time=vector("list"),count=vector("list"))
  }


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
  while (substr(event,1,1) != "#") {
    event <- readLines(con, n = 1) #readr::read_Lines does not work for subsequent reads on a connection
    if (length(event) == 0) {
      warning("premature end of file")
      break
    }
  }

  timestamp <- "0"
  multibitvals <- hash::hash()
  on.exit(hash::clear(multibitvals),add=T)

  lcount <- 0
  itime <- proc.time()[3]
  ltime <- proc.time()[3]

  #readLine return empty vector when EOF is reached, "" for an empty line
  while(length(event != 0)) {
    indicator <- tolower(substr(event,1,1))
    # TIMESTAMP
    if (indicator == "#") {
      timestamp <- substring(event,2)
    }
    # DUMP-EVENT
    if (indicator == "$") {
      #substring(event,2)
      #TODO: handle dump events here
      #until a better solution is here: fast forward
      #TODO: for correct counts we need to parse multibit-values here
      while (event != "$end") {
        indicator <- tolower(substr(event,1,1))

        if (any(indicator == c("b","r"))) {
          valname <- strsplit(substring(event,2)," ")[[1]]
          sig <- valname[2]
          mbNode <- FindFirstFast(detailSignals,sig)
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
    if (any(indicator == c("0","1","x","z"))) {
      sig <- substring(event,2)
      bucket <- nameBucketLUT[[sig]]
      # bucket will be NULL for signals we do not want to count
      # we only create a new chain link, when we have a new timestamp
      if (!(is.null(bucket))) {
        if ((!is.null(counts[[bucket]][[indicator]]$time$val)) && (counts[[bucket]][[indicator]]$time$val != timestamp)) {
          counts[[bucket]][[indicator]]$time <- list(prev=counts[[bucket]][[indicator]]$time)
          counts[[bucket]][[indicator]]$count <- list(prev=counts[[bucket]][[indicator]]$count)
        }
        counts[[bucket]][[indicator]]$time$val <- timestamp
        counts[[bucket]][[indicator]]$count$val <- incwithNULL(counts[[bucket]][[indicator]]$count$val)
     }
    }

    # MULTIBIT-VARIABLE
    if (any(indicator == c("b","r"))) {
      valname <- strsplit(substring(event,2)," ")[[1]]
      sig <- valname[2]
      mbNode <- FindFirstFast(detailSignals,sig)
      # mbNode will be NULL for signals we do not want to count
      if (!is.null(mbNode)) {
        val <-
          leftExtend(valname[1],as.integer(mbNode$bits))

        lastval <- multibitvals[[sig]]
        if (is.null(lastval))
          lastval <- val

        # compare bitwise and set toggle counts
        # reverse because in VCD MSB is left
        # reversing before splitting is twice as fast
        bits.lastval <- strsplit(str_rev(lastval),"")[[1]]
        bits.val     <- strsplit(str_rev(val),"")[[1]]

        for (i in which(!bits.lastval == bits.val)) {
          as.character(i - 1)
          bitsig <- paste0(c(sig,".",as.character(i - 1)),collapse = "")
          bucket <- nameBucketLUT[[bitsig]]
          indicator <- bits.val[i]
          if ((!is.null(counts[[bucket]][[indicator]]$time$val)) && (counts[[bucket]][[indicator]]$time$val != timestamp)) {
            counts[[bucket]][[indicator]]$time <- list(prev=counts[[bucket]][[indicator]]$time)
            counts[[bucket]][[indicator]]$count <- list(prev=counts[[bucket]][[indicator]]$count)
          }
          counts[[bucket]][[indicator]]$time$val <- timestamp
          counts[[bucket]][[indicator]]$count$val <- incwithNULL(counts[[bucket]][[indicator]]$count$val)
        }

        multibitvals[[sig]] <- val
      }
    }

    event <- readLines(con, n = 1)
    #lcount <- lcount + 1
    #if ((lcount %% 10000) == 1) {
    #  ptime<-((proc.time()[3]-itime)/lcount)*(6035158-lcount-vcd$dumpstart)
    #  names(ptime)<-c("remaining (seconds)")
    #  print(ptime)
    #  ltime <- proc.time()[3]
    #}
  }

  #finally we can prune vartree
  #vartree$Prune(pruneFun = function(x) {x$level <= detailLevel}) # broken in data.tree 0.2.4
  vartree <- Clone(topNode,attributes = T,pruneFun = function(x) {x$level <= detailLevel})

  # and unlist the counts
  for (i in names(counts)) {
    for (j in names(counts[[i]])) {
      times <- unlist(counts[[i]][[j]]$time,use.names = F)
      counts[[i]][[j]]<-unlist(counts[[i]][[j]]$count,use.names = F)
      names(counts[[i]][[j]])<-times
    }
  }

  #elapsed time when debugging
  #print(proc.time()[3]-itime)

  return(list(hierarchy = vartree,counts = counts))
}

