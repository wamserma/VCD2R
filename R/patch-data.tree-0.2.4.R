# =======
# patching corner
# =======

# -------
# data.tree < 0.3.0 CRAN
# -------

# we need to overwrite Clone
# this fixes the children list issue from < 0.3.0
# this fixes parent issue for root node from < 0.2.4

Clone <- function(node, pruneFun = NULL, attributes = FALSE, initialCall = T) {

  myclone <- node$clone()

  if (initialCall) {
    myclone$parent <- NULL
  }
  if (attributes) attributes(myclone) <- attributes(node)
  if (!is.null(pruneFun) && length(node$children) > 0) {
    keep <- sapply(node$children, pruneFun)
    children <- node$children[keep]
    rm(list = names(node$children)[!keep], envir = myclone)
  } else children <- node$children
  myclone$children <- lapply(children, function(x) Clone(x, pruneFun, attributes, F))
  for (child in myclone$children) {
    myclone[[child$name]] <- child
    child$parent <- myclone
  }
  if (length(myclone$children) == 0) myclone$children <- NULL
  return (myclone)
}

# data.tree does not provide a find function

Find <- function(node, name, traversal = c("pre-order", "post-order", "in-order", "level",
                                     "ancestor"), all = F){

  trav <- data.tree::Traverse(node, traversal, pruneFun = NULL, filterFun = NULL)
  ret <- trav[sapply(trav,function(x) x$name == name)]

  if (length(ret) == 0) return(NULL)
  if (!all) return(ret[[1]])
  return(ret)

}

#trav <- Traverse(node, traversal, pruneFun = NULL, filterFun = NULL)
FindFirstFast <- function(trav, name){
  ret <- NULL
  for (x in trav) {
    if (x$name == name) {
      ret <- x
      break
    }
  }
  return(ret)
}
