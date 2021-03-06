## string helpers

#' @keywords internal
strHead <- function(x) {
  substr(x,1,1)
  }
strHeadLower <- function(x) {
  tolower(substr(x,1,1))
  }
strTail <- function(x) {
  substring(x,2)
  }
strRevAndSplit<-function(x) {
  rev(strsplit(x,"")[[1]])
  }

# map ("0","1","z","x",other) -> (1,2,3,4,0)
scalarIndicatorToInt <- function(x) {
  x<-strsplit(x,"")[[1]][1]
  ret <- 0
  if (x == "0") {
    ret <- 1;
  } else if (x == "1") {
    ret <- 2;
  } else if (x == "z") {
    ret <- 3;
  } else if (x == "x") {
    ret <- 4;
  } else {
    ret <- 0;
  }
  return(ret)
  }

# map ("b","r",other) -> (1,2,0)
isMultiBit <- function(x) {
    x<-strsplit(x,"")[[1]][1]
  ret <- 0
  if (x == "b") {
    ret <- 1;
    } else if (x == "r") {
        ret <- 2;
        } else {
        ret <- 0;
      }
  return(ret)
  }

# Note: we are assuming less that 2147483647 toggles per (accumulated) signal per timestamp,
# which is safe for all design sizes we might want to handle here

addwithNA<-function(a,b) {
  if (is.na(a)) return(b)
  if (is.na(b)) return(a)
  return(a+b)
}

incwithNA<-function(a) {
  if (is.na(a)) a<-0L
  return(a+1L)
}

incwithNULL <- function(a) {
  if (is.null(a)) {
    return(1L)
  } else {
    return(a + 1L)
  }
}

leftExtend <- function(val,bits){
  n <- nchar(val)
  ext <- substr(val,1,1)
  if (ext == "1") ext <- "0"
  ext<-rep(ext,bits-n)
  return(paste0(c(ext,val),collapse=""))
}


# add two named integer vectors
addToggleVecs <- function(a,b){
   c<-vector("integer",0L)
   nc<-sort(unique(c(names(a),names(b))))
   c<-sapply(nc,function(x) {
     addwithNA(a[x],b[x])
     })
   names(c)<-nc
   return(c)
}

stringnumLT <- function(x,y){
  nx <- nchar(x)
  ny <- nchar(y)

  if (nx != ny) return (nx < ny)
  return (x < y)
}

# iterative scanning  -- fastest version
# assumes all required signals are available, will fail otherwise when trying to access a element of NULL-vector
# assumes all timestamps are in correct order
# performance is acceptable, if parallelisation is desired, this is best done over varcounts to avoid unnecessary copying of input data, see also Rcpp parallel
addToggleVecsByName <- function(vnames,vtype=c("0","1","z","x"),counts=vector("list",0L)){
  if (length(counts)==0) {
    return(counts)
  }
  if (length(vtype)>1) {
    warning("argument vtype has length > 1, remaining values ignored")
    vtype<-vtype[1]
  }
  timestamps<-unique(unlist(sapply(vnames,function(x) names(counts[[x]][[vtype]]))))
  if (length(timestamps) == 0) return(vector("integer",0L))
  timestamps<-sorttimestamps(timestamps)
  varcounts<-sapply(counts[vnames],function(var) var[[vtype]])
  varcounts<-varcounts[!sapply(varcounts,function(x) {
    is.null(x) || (length(x) < 1)
    })]
  countIdxs<-rep(1L,length(varcounts))
  matchIdx<-vector("integer",0L)
  countHeads<-sapply(1L:length(varcounts),function(x) varcounts[[x]][countIdxs[[x]]])
  ret <- vector("integer",length(timestamps))
  names(ret)<-timestamps

  # the following assumes timestamps are sorted!
  for (ts in timestamps) {
    if (length(matchIdx)>0) {
      partial<-sapply(matchIdx,function(x) varcounts[[x]][countIdxs[[x]]])
      countHeads[matchIdx]<-partial
      names(countHeads)[matchIdx]<-names(partial)
    }
    matchIdx<-which(names(countHeads)==ts)
    ret[[ts]]<-sum(countHeads[matchIdx])
    countIdxs[matchIdx]<-countIdxs[matchIdx]+1L
  }
  return(ret)
}

# sorts timestamps/numbers stored as strings, dropping empty strings
# throws error for emtpy vector input
sorttimestamps<-function(x){
  xchar<-sapply(x,nchar)
  maxlen<-max(xchar)
  ret<-unlist(lapply(1:maxlen,function(i) sort(x[xchar==i])))
  return(ret)
}

# test whether a is smaller b, assuming both are big integers written as strings without leading zeroes
numstring.lower<-function(a,b){
  sorttimestamps(c(a,b))
  return(sorttimestamps(c(a,b))[1]==a)
}

#' map NA entries to 0
#' @param x a (numerical) vector possibly containing NAs
#'
#' @return the input vector with NAs replaces by 0
#' @keywords internal
noNA <- function(x) {
  sapply(x, function(y)
    if (is.na(y)) {
      0
    } else {
      y
    })
}

#' A more flexible variant of data.tree::FindNode(). This implementation allows to set a traversal and search by arbitrary fields.
#'
#' `data.tree::Traverse` is used with the `traversal` argument to produce an ordering of the nodes which is subsequently searched. The `traversal` option therefore allows to exert some control over the order of results if the results are not unique.
#'
#' @param node The root node of the tree to be searched.
#' @param value The value for which to seach.
#' @param field The field to be searched. The fieldname is given as a string.
#' @param traversal The order in which the tree should be traversed.
#' @param all whether only the first match or all matches shall be returned
#'
#' @return Depending on the value of `all` either a single node or a list of nodes. In the case that no match is found, `NULL` is returned.
#' @export
FindNodeGeneric <- function(node, value, field="name", traversal = c("pre-order", "post-order", "in-order", "level",
                                                                     "ancestor"), all = F){

  trav <- data.tree::Traverse(node, traversal, pruneFun = NULL, filterFun = NULL)
  ret <- trav[sapply(trav,function(x) any(x[[field]] == value))]

  if (length(ret) == 0) return(NULL)
  if (!all) return(ret[[1]])
  return(ret)
}
