
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

incwithNULL<-function(a) {
  if (is.null(a)) a<-0L
  return(a+1L)
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

