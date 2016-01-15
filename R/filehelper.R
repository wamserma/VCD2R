#' Read a bunch of lines from a file.
#'
#' @param con the connection (file) to read from
#' @param skipLines Skip this number of lines before reading data
#' @return a list telling whether EOF has been reached and the data

nextLines <- function(con,skipLines=0) {
  nLines <- 1000 # stepping

  # skip over skipLines lines
  # seek is from the devil https://stat.ethz.ch/R-manual/R-devel/library/base/html/seek.html
  while (skipLines > nLines) {
    readLines(con = con,n = nLines,ok = T, warn = F, skipNul = T)
  }
  while (skipLines > 0) {
    readLines(con = con,n = 1,ok = T, warn = F, skipNul = T)
  }


  buf <- readLines(con = con,n = nLines,ok = T, warn = T, skipNul = F)
  if (length(buf) < nLines) {
    eof <-T
  } else {
    eof <- F
  }

  return(list(eof = eof,data = buf))
}
