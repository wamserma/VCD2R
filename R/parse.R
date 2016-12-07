#' an interface to return parse results, setting sensible defaults
#'
#' @return An VCDFileObject containing information on \code{file}.

#' @importFrom Wmisc Tokenizer

buildParseReturn <- function(vcdfile = NA,
                             timescale = NA,
                             dumpstart = NA,
                             hierarchy = NA,
                             date = NA,
                             version = NA,
                             linesRead = 0) {
  if (!is.na(vcdfile)) {
    if (is.na(timescale))
      timescale <- vcdfile$timescale
    if (is.na(dumpstart))
      dumpstart <- vcdfile$dumpstart
    if (is.na(hierarchy))
      hierarchy <- vcdfile$hierarchy
    if (is.na(date))
      date <- vcdfile$date
    if (is.na(version))
      version <- vcdfile$version
  }

  ret <- list(
    vcd = vcdfile,
    timescale = timescale,
    version = version,
    dumpstart = dumpstart,
    hierarchy = hierarchy,
    date = date,
    linesRead = linesRead
  )
}

# ===================
# parsing functions
# ===================

#' Parse a VCDFile for its header fields
#'
#' @param vcdfile The file to open.
#'
#' @return An list containing the parse results for \code{file}.

parseVCDHeader <- function(vcdfile) {
  keywords <- c(
    "$comment",
    "$date",
    "$enddefinitions",
    "$scope",
    "$timescale",
    "$upscope",
    "$var",
    "$version",
    "$dumpall",
    "$dumpon",
    "$dumpoff",
    "$dumpvars")

  parseResult <- parseVCDForKeys(vcdfile,keywords,header = T)
  return (parseResult)
}


#' Parse a VCDFile for a given list of sections
#' This function is the main workhorse, delegating to speciaclised
#' functions for parsing the individual fields.
#' Altogether this implements a top-down parser for VCD files.
#'
#' @param vcdfile The file to open.
#'
#' @return An list containing the parse results for \code{file}.
parseVCDForKeys <- function(vcdfile,keys,header) {
  if (!(length(keys) > 0)) {
    return (buildParseReturn(vcdfile))
  }

  if (!file.exists(vcdfile$filename)) {
    warning("File does not exist: ", filename)
    return(buildParseReturn(vcdfile))
  }

  tok<-Tokenizer$new(vcdfile$filename)

  vcd <- buildParseReturn()

  done <- F

  buf <- tok$nextToken()

  while (!is.na(buf))
    {
      # fail fast
      if (substr(buf,1,1) != '$') {
        isEmptyLine <- !grepl("[^[:space:]]+",buf$data[i])
        if (!isEmptyLine) {
          warning("Ignored data outside block/scope at line ",lines.read + i," in input file.")
        }
        buf <- tok$nextToken()
      } else {
        key <- buf

        if (!any(keys == key)) {
          warning("Invalid keyword \"",key," \" at line ",lines.read + i," in input file.")
          buf <- tok$nextToken()
          tokens.read <- tokens.read + 1
        } else {
          ret <- parseBlock(tok,key)
          if (key == "$comment") {
            # comments are currently ignored
            # alternate options would be collecting them into a separate variable
            # or generating warnings
          }

          if (key == "$date") {
            vcd$date <- ret
          }
          if (key == "$timescale") {
            vcd$timescale <- ret
          }
          if (key == "$version") {
            vcd$version <- ret
          }
          if (key == "$enddefinitions") {
            vcd$dumpstart <- ret$bufPos + ret$chunksParsed + lines.read
            if (header == T) {
              done <- T
              break
            }
          }

          if (key == "$scope") {
            if (!is.na(vcd$hierarchy)) {
              warning("multiple top modules, only lastest is kept")
            }
            vcd$hierarchy = ret$data
          }

          if ((key == "$upscope") | (key == "$var")) {
            warning("Malformed VCD file: ",key," outside scope.")
          }

          if (any(key == c("$dumpall","$dumpon","$dumpoff","$dumpvars")) & header) {
            warning("Malformed VCD file: ",key," in header.")
          }

          # record all dump-events, so they will not be plotted as toggles
          # recreating the waveforms can be done by reading all four to-vectors in a mergesort-fashion

        } # endif parsed data
      } #endif validity check "fail fast"


  }
  return(vcd)
}

#' scope parsing dispatcher function to keep \code{parseVCDForKeys} readable
#' also eases handling of nested scopes (e.g. vars definitions)
#'
#' @param vcdfile The file to open.
#'
#' @return An list containing the parse results for \code{file}.
parseBlock <- function(tok,key) {
  f <- match.fun(paste0("parse_",sub("\\$","",key)))
  parsedData <- f(tok)
}
