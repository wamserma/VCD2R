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
  if (length(vtype)!=0) {
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
