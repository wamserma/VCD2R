# =========================
# The dedicated parser functions
#
# Each function parses/reads everything that follows until it hits an $end tag.
# =========================

# -------------------------
# simple string based fields
# -------------------------

parse_comment <- function(tok) {
  ret <- parseStringFields(tok)
  return(ret)
}

parse_date <- function(tok) {
  ret <- parseStringFields(tok)
  return(ret)
}

parse_enddefinitions <- function(tok) {
  ret <-
    parseStringFields(tok)
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
  ret <- parseStringFields(tok)
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
  ret <- parseStringFields(tok)
  return(ret)
}

parse_var <- function(tok) {
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
    }
    buf <- tok$nextToken()
  }
  return (ret)
}
