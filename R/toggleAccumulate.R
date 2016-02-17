# returns new counts for a parseResult

accumulate <- function(sig,parseResult) {
  # find sig in tree - if not there warn and return
  top<-Find(parseResult$hierarchy,sig)
  if (is.null(top)) {
    warning(gettextf(sig,"%s is not available in this dump"))
    return(parseResult$counts)
  }

  # Be careful when aggregating: signals may show up in multiple modules, that
  # is multiple places in the hierarchy, but they must be counted only once!
  # or would that mean that the wire toggles in each module simultaneously?
  # the activity would then add up

  # traverse the levels
  # for duplicate mentions only keep highest level signal
  trav <- data.tree::Traverse(top,traversal = "level")
  trav <- trav[!duplicated(sapply(trav, function(x) x$name))]

  travLevels <- sapply(top$level:top$height,function(x) {
    return(trav[sapply(trav, function(n) n$level==x)])
  })

  #doParallel::registerDoParallel(3)
  for (lev in length(travLevels):1) {
    for (var in travLevels[[lev]]) {

      #  lvlcounts<-foreach::`%dopar%`(foreach::foreach(var=travLevels[[lev]]),{
      #    childcounts<-sapply(var$children,function(x) parseResult$counts[[x$name]])
      #    # do only if lookup in counts is NULL, lookup signals from counts
      #    sum0<-addToggleVecsL(sapply(childcounts,function(x) x$`0`))
      #    sum1<-addToggleVecsL(sapply(childcounts,function(x) x$`1`))
      #    sumZ<-addToggleVecsL(sapply(childcounts,function(x) x$`z`))
      #    sumX<-addToggleVecsL(sapply(childcounts,function(x) x$`x`))
      #    return(list(name=var$name,counts=list(`0`=sum0,`1`=sum1,`z`=sumZ,`x`=sumX)))
      #  })
      #  # assign the results to signals
      #  for (v in lvlcounts) {
      #    parseResult$counts[[v$name]] <- v$counts
      #  }

      # only sum if we do not have any values
      if (!all(sapply(parseResult$counts[[var$name]],length)==0)) {
        #print(gettextf("length > 0 skipping %s",var$name))
        next
        }

      # only sum if there are children to sum from
      # may occur if the hierarchy and/or the counts have been edited
      if (is.null(var$children)) {
        #print(gettextf("no children skipping %s",var$name))
        next
      }

      childnames <- sapply(var$children,function(x)
        x$name)

      if (length(childnames) == 1) {
        parseResult$counts[[var$name]] <-
          parseResult$counts[[childnames[[1]]]]
      } else {
        parseResult$counts[[var$name]][["0"]] <-
          addToggleVecsByName(childnames,"0",parseResult$counts)
        parseResult$counts[[var$name]][["1"]] <-
          addToggleVecsByName(childnames,"1",parseResult$counts)
        parseResult$counts[[var$name]][["x"]] <-
          addToggleVecsByName(childnames,"x",parseResult$counts)
        parseResult$counts[[var$name]][["z"]] <-
          addToggleVecsByName(childnames,"z",parseResult$counts)
      }
    }
  }
  return(parseResult$counts)
}
