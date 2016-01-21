



# ====================
# The VCDFile Object and Related Objects (the interface stuff)
# ====================

#' VCD2R -- Loading Value Change Dumps int R.
#'
#' TODO: Package doc.
NULL

#' Create an VCDFileObject
#'
#' @param file The file to open.
#' @param parseHeader A flag whether the header should be parsed on opening the file. Default is TRUE.
#' @return An VCDFileObject containing information on \code{file}.
#' @examples
#' VCDFile()
#' VCDFile("inter.vcd")
#' VCDFile("inter.vcd",F)

VCDFile <- function(filename = NA_character_,parseHeader = T)
{
  vcd <- list(
    filename  = filename,
    date      = NA,
    timescale = NA_integer_,
    dumpstart = 0, # keep a rought track of where this part starts, so we can fast-forward to it at later parsing
    variables = NA
  )

  if (!is.na(filename)) {
    if (!file.exists(filename)) {
      warning("File does not exist: ", filename)
      vcd$filename = NA_character_
    } else {
      if (parseHeader) {
        parseResult <- parseVCDHeader(vcd)
        vcd$date <- parseResult$date
        vcd$version <- parseResult$version
        vcd$timescale <- parseResult$timescale
        vcd$dumpstart <- parseResult$dumpstart
        vcd$variables <- parseResult$variables
      }
    }
  }

  ## Set the name for the class
  class(vcd) <- append(class(vcd),"VCDFile")
  return(vcd)
}

#' an interface to return parse results, setting sensible defaults
#'
#' @param
#' @param
#' @return An VCDFileObject containing information on \code{file}.

buildParseReturn <- function(vcdfile = NA,
                             timescale = NA,
                             dumpstart = NA,
                             variables = NA,
                             date = NA,
                             version = NA,
                             linesRead = 0) {
  if (!is.na(vcdfile)) {
    if (is.na(timescale))
      timescale <- vcdfile$timescale
    if (is.na(dumpstart))
      dumpstart <- vcdfile$dumpstart
    if (is.na(variables))
      variables <- vcdfile$variables
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
    variables = variables,
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
    "$version"
  )
  #  "$dumpall",
  #  "$dumpon",
  #  "$dumpoff",
  #  "$dumpvars")

  parseResult <- parseVCDForKeys(vcdfile,keywords,header = T)
  return (parseResult)

}

#' Parse a VCDFile for its body fields, namely the toggle events
#'
#' @param vcdfile The file to open.
#'
#' @return An list containing the parse results for \code{file}.

parseVCDBody <- function(vcdfile) {
  keywords <- c("$dumpall",
                "$dumpon",
                "$dumpoff",
                "$dumpvars")

  parseResult <- parseVCDForKeys(vcdfile,keywords,header = F)
  return (parseResult)

}

#' Parse a VCDFile for a given list of sections
#' This function is the main workhorse, delegating to specuiaclises
#' functions for parsing the individual fields
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

  con <- file(
    vcdfile$filename, "r", blocking = TRUE,
    encoding = getOption("encoding"), raw = FALSE
  )
  on.exit(close(con))

  lines.read <- 0
  buf <- list(eof = F, data = vector(mode = "character"))

  vcd <- buildParseReturn()

  done <- F

  while ((!buf$eof) & (!done)) {
    lines.read <- lines.read + length(buf$data)
    buf <- nextLines(con)

    i <- 1
    while (i <= length(buf$data))
    {
      # fail fast
      if (substr(buf$data[i],1,1) != '$') {
        isEmptyLine <- !grepl("[^[:space:]]+",buf$data[i])
        if (!isEmptyLine) {

        warning("Ignored data outside block/scope at line ",lines.read + i," in input file.")
        }
        i <- i+1
      } else {
        key <- strsplit(buf$data[i],' ')[[1]][1]

        if (!any(keys == key)) {
          warning("Invalid keyword \"",key," \" at line ",lines.read + i," in input file.")
          i <- i + 1
        } else {
          ret <- parseBlock(buf,i,key,con,vcdfile)
          i <- ret$bufPos
          if (key == "$comment") {
            # comments are currently ignored
            # alternate options would be collecting them into a separate variable
            # or generating warnings
          }

          if (key == "$date") {
            vcd$date <- ret$data
          }
          if (key == "$timescale") {
            vcd$timescale <- ret$data
          }
          if (key == "$version") {
            vcd$version <- ret$data
          }
          if (key == "$enddefinitions") {
            vcd$dumpstart <- ret$bufPos + ret$chunksParsed + lines.read
            if (header == T) {
              done <- T
              break
            }
          }
          # TODO handle the signal parsing
          # build a module/variable/scopetree
          # each entry is either a var or a submodule, vars have a type ->
          # type is: scope | reg | trireg | task | integer | float
          # a bit more complicated
          # later we need to map signals to buckets, which we might be able to do with named vectors (one for each target switching value)
          # if we parse with infinite level, we can recreate the waveforms
          # for each of the lowest selected modules we need to build 4 switch target vectors, each named by timestamps
          # we then need a LUT to map signal to its accumulator group
          # for multi-bit values we create a own module/scope and make single-bit slices

          # recreating the waveforms can be done by reading all four to-vectors in a mergesort-fashion

        } # endif parsed data
      } #endif validity check

    } #endif loop through buffer
  }
  return(vcd)
}

#' scope parsing dispatcher function to keep \code{parseVCDForKeys} readable
#' also eases handling of nested scopes (e.g. vars definitions)
#'
#' @param vcdfile The file to open.
#'
#' @return An list containing the parse results for \code{file}.


parseBlock <- function(buf,i,key,con,vcdfile) {
  f <- match.fun(paste0("parse_",sub("\\$","",key)))
  parsedData <- f(buf,i,con,vcdfile)

  # some functions return elaborate structures and need special handling
  # all other just return a linecount and a string

  # TODO call parsing hook based on key
  # hand over buffer with current i, handle the return of the subparser
  # adjust linecount: every subparser must return also the number of lines it parsed

  # can scopes be nested? - yes for signal defs, but not alaway
  # return a copy of the updated vcdfile if headefile was parsed, if body was parsed also return parsed toggles
}

# =========================
# pretty printing function for VCDFile
# =========================

print.VCDFile <- function(x, printVars = F) {
  writeLines(paste0("file: ",x$filename))
  writeLines(paste0("generated by: ",x$version))
  writeLines(paste0("dump recorded on ",x$date))
  writeLines(paste0("timescale: ",paste0(x$timescale,collapse=" ")))
  writeLines(paste0("number of signals: ",NA))
  # TODO print Metadata
  if (printVars)
    NULL # TODO print variable tree
  invisible(x)
}
