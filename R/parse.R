#' @importFrom Wmisc Tokenizer
NULL

# ===================
# parsing functions
# ===================

#' Parse a VCDFile for its header fields
#'
#' @param vcdfile The file to open.
#'
#' @return An list containing the parse results for \code{file}.

#' @keywords internal
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
#' @param keys The keys to parse for. Given by the Verilog VCD spec.
#' @param header If true only the header is parsed, otherwise the whole signal hierarchy is parsed
#'
#' @keywords internal
#' @return An list containing the parse results for \code{file}.
parseVCDForKeys <- function(vcdfile,keys,header=F) {
  if (!(length(keys) > 0)) {
    return(vcdfile)
  }

  if (!file.exists(vcdfile$filename)) {
    warning("File does not exist: ", vcdfile$filename)
    return(vcdfile)
  }

  tok<-Tokenizer$new(vcdfile$filename)

  vcd <- list(
    timescale = NA,
    version = NA,
    dumpstart = NA,
    hierarchy = NA,
    date = NA
  )

  offset <- tok$getOffset()
  buf <- tok$nextToken()

  while (!is.na(buf)){
      # fail fast
      if (substr(buf,1,1) != "$") {
          isEmptyLine <- !grepl("[^[:space:]]+",buf)
        if (!isEmptyLine) {
          warning("Ignored data outside block/scope at offset ",offset," bytes in input file.")
        }
      } else {
        key <- buf

        if (!any(keys == key)) {
          warning("Invalid keyword \"",key," \" at offset ",offset," bytes in input file.")
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
            vcd$dumpstart <- tok$getOffset()
            if (header == T) {
              buf<-NA_character_
              break
            }
          }

          if (key == "$scope") {
            if (any(class(vcd$hierarchy)=="Node")) {
              warning("multiple top modules, only lastest is kept. At offset ",offset," bytes in input file.")
            }
            vcd$hierarchy <- ret
          }

          if ( (key == "$upscope") | (key == "$var") ) {
            warning("Malformed VCD file: ",key," outside scope at offset ",offset," bytes in input file.")
          }

          if (any(key == c("$dumpall","$dumpon","$dumpoff","$dumpvars")) & header) {
            warning("Malformed VCD file: ",key," in header at offset ",offset," bytes in input file.")
          }

          # record all dump-events, so they will not be plotted as toggles
          # recreating the waveforms can be done by reading all four to-vectors in a mergesort-fashion

        } # endif parsed data
      } #endif validity check ("fail fast")
    offset <- tok$getOffset()
    buf <- tok$nextToken()
  }
  tok$close()
  return(vcd)
}
