# Main package file
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Create an VCDFileObject
#'
#' @param file The file to open.
#' @param parseHeader A flag whether the header should be parsed on opening the file. Default is TRUE.
#' @return An VCDFileObject containing information on \code{file}.
#' @examples
#' VCDFile()
#' VCDFile("inter.vcd")
#' VCDFile("inter.vcd",F)

VCDFile <- function(filename=NA_character_,parseHeader=T)
{

  vcd <- list(
    filename  = filename,
    timescale = NA_integer_,
    varstart  = 0, # keep a rought track of where these segments start,
    dumpstart = 0, # so we can seek in at later parsing
    variables = NA
  )

  if(!is.na(filename)) {
    if (!file.exists(filename)){
      warning("File does not exist: ", filename)
      vcd$filename = NA_character_
    } else {
      if (parseHeader) {
        parseResult <- parseVCDHeader(vcd)
        vcd$timescale <- parseResult$timescale
        vcd$varstart <- parseResult$varstart
        vcd$dumpstart <- parseResult$dumpstart
      }
    }

  }

  ## Set the name for the class
  class(vcd) <- append(class(vcd),"VCDFile")
  return(vcd)
}

#' Parse a VCDFile for its header fields
#'
#' @param vcdfile The file to open.
#'
#' @return An list containing various information on \code{file}.

parseVCDHeader <- function(vcdfile){
  ret <- list(timescale = vcdfile$timescale, varstart = vcdfile$varstart,dumpstart = vcdfile$dumpstart)

  if (!file.exists(vcdfile$filename)){
    warning("File does not exist: ", filename)
    return(ret)
  }

  con <- file(vcdfile$filename, "r", blocking = TRUE,
              encoding = getOption("encoding"), raw = FALSE)

  keywords <- c("$comment",
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

  found.timescale <- F
  lines.read <- 0

  buf <- list(eof = F, data = NA)

  while (!buf$eof) {
    buf <- nextLines(con)
    lines.read <- lines.read + length(buf)


    for (i in 1:length(buf$data)) {
      print(buf$data[i])
      key <- strsplit(buf$data[i],' ')[[1]][1]
      if (!any(keywords == key)) {
        warning("Invalid keyword \"",key," \" at line ",lines.read+i," in input file.")
        } else {
        # TODO call parsing hook based on key
        # hand over buffer with current i, handle the return of the subparser
        # adjust linecount: every subparser must return also the number of lines it parsed
      }

    }
  }

  close(con)
}
