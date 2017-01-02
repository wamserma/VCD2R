
#' accumulate toggle counts
#' returns accumulated counts (and only counts) for a (sub-hierarchy) of a parseResult (as returned by \link{parseToggles})
#'
#' @param sig the top signal of the sub-hierarchy for which counts shall be accumulated
#' @param parseResult what has been returned by \link{parseToggles}
#'
#' @export
accumulate <- function(sig,parseResult) {
  # find sig in tree - if not there warn and return
  top<-FindNodeGeneric(parseResult$hierarchy,sig,traversal = "level")
  if (is.null(top)) {
    warning(gettextf(sig,fmt="%s is not available in this dump"))
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

  for (lev in length(travLevels):1) {
    for (var in travLevels[[lev]]) {

      # only sum if we do not have any values
      if (!all(sapply(parseResult$counts[[var$name]],length)==0)) {
        #print(gettextf("length > 0 skipping %s",var$name)) #nolint
        next
        }

      # only sum if there are children to sum from
      # may occur if the hierarchy and/or the counts have been edited
      if (is.null(var$children)) {
        #print(gettextf("no children skipping %s",var$name)) #nolint
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
