# =======
# patching corner
# =======

# data.tree does not provide a find function as flexible as this one

Find <- function(node, value, field="name", traversal = c("pre-order", "post-order", "in-order", "level",
                                     "ancestor"), all = F){

  trav <- data.tree::Traverse(node, traversal, pruneFun = NULL, filterFun = NULL)
  ret <- trav[sapply(trav,function(x) any(x[[field]] == value))]

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
