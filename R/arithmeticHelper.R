## string helpers, using Rcpp and inline

#' @importFrom Rcpp sourceCpp
#' @importFrom inline cxxfunction

strHead <- cxxfunction(signature(x="character"), plugin="Rcpp", body='
    std::string s = as<std::string>(x);
    return wrap(s[0]);
    ')

strHeadLower <- cxxfunction(signature(x="character"), plugin="Rcpp", body='
    std::string s = as<std::string>(x);
    register char c = tolower(s[0]);
    return wrap(c);
    ')

strTail <- cxxfunction(signature(x="character"), plugin="Rcpp", body='
    std::string s = as<std::string>(x);
    s = &(s[1]);
    return wrap(s);
    ')

# map ("0","1","z","x",other) -> (1,2,3,4,0)
scalarIndicatorToInt <- cxxfunction(signature(x="character"), plugin="Rcpp", body='
    char c = (as<std::string>(x))[0];
    unsigned int ret;
      if (c == \'0\') {ret = 1;}
      else if (c == \'1\') {ret = 2;}
      else if (c == \'z\') {ret = 3;}
      else if (c == \'x\') {ret = 4;}
      else {ret = 0;}
    return wrap(ret);
    ')

# map ("b","r"other) -> (1,2,0)
isMultiBit <- cxxfunction(signature(x="character"), plugin="Rcpp", body='
                                    char c = (as<std::string>(x))[0];
                                    unsigned int ret;
                                    if (c == \'b\') {ret = 1;}
                                    else if (c == \'r\') {ret = 2;}
                                    else {ret = 0;}
                                    return wrap(ret);
                                    ')
# Note: we are assuming less that 2147483647 toggles per (accumulated) signal per timestamp,
# which is safe for all design sizes we might want to handle here

addwithNA<-function(a,b) {
  if (is.na(a)) a<-0L
  if (is.na(b)) b<-0L
  return(a+b)
}

incwithNA<-function(a) {
  if (is.na(a)) a<-0L
  return(a+1L)
}

incwithNULL <- function(a) {
  if (is.null(a)) {
    return(1L)
  }
  else
  {
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

# tests
#leftExtend("10",5)=="00010"
#leftExtend("01",6)=="000001"
#leftExtend("ZX",7)=="ZZZZZZX"
#leftExtend("X1",8)=="XXXXXXX1"

# add two named integer vectors
addToggleVecs <- function(a,b){
   c<-vector("integer",0L)
   nc<-sort(unique(c(names(a),names(b))))
   c<-sapply(nc,function(x) {addwithNA(a[x],b[x])})
   names(c)<-nc
   return(c)
}

#tests
# t<-1:4
# names(t)<-c("13","24","36","48")
# u<-1:6
# names(u)<-c("17","24","36","42","48","51")
# ref <- c(1,1,4,6,4,9,6)
# names(ref)<-c("13","17","24","36","42","48","51")
