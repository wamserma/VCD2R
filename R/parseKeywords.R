# =========================
# The dedicated parser functions
#
# Each function parses/reads everything that follows until it hits an $end tag.
# =========================

# -------------------------
# dispatcher
# this two-level dispatching is intended to add specific functionalty (as in date parsing)
# or a possible upcoming hook functionality
# -------------------------

#' scope parsing dispatcher function to keep \code{parseVCDForKeys} readable
#' also eases handling of nested scopes (e.g. vars definitions)

#' @param key The keyword for dispatching
#' @param tok A tokenizer wrapping the VCD file to read from
#'
#' @return An list containing the parse results for \code{file}.
#' @keywords internal
parseBlock <- function(tok,key) {
  f <- match.fun(paste0("parse_",sub("\\$","",key)))
  parsedData <- f(tok)
  return(parsedData)
}

# -------------------------
# simple string based fields
# -------------------------

parse_comment <- function(tok) {
  ret <- paste0(parseStringFields(tok),collapse = " ")
  return(ret)
}

parse_date <- function(tok) {
  ret <- paste0(parseStringFields(tok),collapse = " ")
  return(ret)
}

parse_enddefinitions <- function(tok) {
  offset <- tok$getOffset()
  ret <- parseStringFields(tok)
  if (!(length(ret)==0)){
    warning("$enddefinitions missing $end")
    tok$setOffset(offset)
  }
  return(ret)
}

parse_timescale <- function(tok) {
  ret <- parseStringFields(tok)
  r<-regexec("([[:digit:]]+)[[:space:]]*([m|u|n|p|f]?s)",ret)
  ret<-regmatches(ret,r)[[1]][2:3]
  names(ret)<-c("scale","unit")
  return(ret)
}

parse_version <- function(tok) {
  ret <- paste0(parseStringFields(tok),collapse = " ")
  return(ret)
}

# -------------------------
# module hierarchy and
# variable/signal definitions
# -------------------------

# parse scope is a bit more complicated:
# it does recursive parsing of the scope
# but not here, this is just a dispatch on the outermost occurence

parse_scope <- function(tok) {
  ret <- parseHierarchy(tok)
  return(ret)
}

parse_upscope <- function(tok) {
  offset <- tok$getOffset()
  ret <- parseStringFields(tok)
  if (!(length(ret)==0)){
    warning("$upscope missing $end")
    tok$setOffset(offset)
  }
  return(ret)
}

parse_var <- function(tok) {
  ret <- parseStringFields(tok)
  return(ret)
}

# collect dumps
parse_dumpall <- function(tok) {
  ret <- parseStringFields(tok)
  return(ret)
}
parse_dumpon <- function(tok) {
  ret <- parseStringFields(tok)
  return(ret)
}
parse_dumpoff <- function(tok) {
  ret <- parseStringFields(tok)
  return(ret)
}
parse_dumpvars <- function(tok) {
  ret <- parseStringFields(tok)
  return(ret)
}

# -------------------------
# worker functions
# -------------------------

# reads all tokens until and $end token or the end of the file is encountered
parseStringFields <- function(tok) {
  ret <- vector(mode = "character")
  buf <- tok$nextToken()

  done <- F

  while (!done) {
    done <- is.na(buf) ||
      grepl("\\$end$|\\$end ",buf,fixed = F,ignore.case = F)
    if (!done) {
      isEmptyLine <- !grepl("[^[:space:]]+",buf)
      if (!isEmptyLine) {
        ret<- c(ret,buf)
      }
      buf <- tok$nextToken()
    }
  }
  return (ret)
}
