# =========================
# The dedicated parser functions
#
# Each function parses reads everything that follows until it hits an $end tag.
# =========================

# -------------------------
# simple string based fields
# -------------------------

parse_comment <- function(buf,i,key,con,vcdfile) {
  ret <- parseStringFields(buf,i,key,con,vcdfile,field = "comment")
  return(ret)
}

parse_date <- function(buf,i,key,con,vcdfile) {
  ret <- parseStringFields(buf,i,key,con,vcdfile,field = "date")
  return(ret)
}

parse_enddefinitions <- function(buf,i,key,con,vcdfile) {
  ret <-
    parseStringFields(buf,i,key,con,vcdfile,field = "enddefinitions")
  return(ret)
}

parse_timescale <- function(buf,i,key,con,vcdfile) {
  ret <- parseStringFields(buf,i,key,con,vcdfile,field = "timescale")
  r<-regexec("([[:digit:]]+)[[:space:]]*([m|u|n|p|f]?s)",ret$data)
  ret$data<-regmatches(ret$data,r)[[1]][2:3]
  names(ret$data)<-c("scale","unit")
  return(ret)
}

parse_version <- function(buf,i,key,con,vcdfile) {
  ret <- parseStringFields(buf,i,key,con,vcdfile,field = "version")
  return(ret)
}

# -------------------------
# module hierarchy and
# variable/signal definitions
# -------------------------


parse_scope <- function(buf,i,key,con,vcdfile) {
  # this creates the signal-tree in a dept-first-manner
  ret <- parseStringFields(buf,i,key,con,vcdfile,field = "scope")
  return(ret)
}

parse_upscope <- function(buf,i,key,con,vcdfile) {
  ret <- parseStringFields(buf,i,key,con,vcdfile,field = "upscope")
  return(ret)
}

parse_var <- function(buf,i,key,con,vcdfile) {
  ret <- parseStringFields(buf,i,key,con,vcdfile,field = "var")
  return(ret)
}

# -------------------------
# worker functions
# -------------------------


parseStringFields <- function(buf,i,key,con,vcdfile,field) {
  ret <-
    list(
      chunksParsed = 0,bufPos = i, data = vector(mode = "character")
    )
  buf$data[i] <-
    substr(buf$data[i],nchar(field) + 3,nchar(buf$data[i]))

  done <- F

  while ((!done) && (i <= length(buf$data))) {
    done <-
      grepl("\\$end$|\\$end ", buf$data[i],fixed = F,ignore.case = F)
    if (!done) {
      isEmptyLine <- !grepl("[^[:space:]]+",buf$data[i])
      if (!isEmptyLine) {
        ret$data <- c(ret$data,buf$data[i])
      }
      i <- i + 1

      # refresh the buffer if needed
      if (i > length(buf$data)) {
        if (buf$eof) {
          warning("Premature EOF while scanning ",field,".")
        } else {
          buf <- nextLines(con)
          i <- 0
          ret$chunksParsed <- ret$chunksParsed + 1
        }
      }
    } else {
      # we have a line containing an $end
      tokens <- strsplit(buf$data[i]," ")[[1]]
      endpos <- match("$end",tokens)

      # push the remainder of the line back for further parsing
      if (endpos < length(tokens)) {
        buf$data[i] <-
          paste0(tokens[endpos + 1:length(tokens)],collapse = " ")
        i <- i - 1
      }

      #only return something if $end is not the first statement in line
      if (endpos > 1) {
        ret$data <-
          c(ret$data,paste0(tokens[1:endpos - 1],collapse = " "))
      }
      i <- i + 1
    }
  }
  ret$bufPos <- i
  return (ret)
}
