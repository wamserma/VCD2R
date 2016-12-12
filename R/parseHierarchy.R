# read the hierarchy into a data.tree
# call functions with data.tree::fun()
# functions that are called often should be imported:
# @importFrom data.tree fun
# but this might only matter if we have gazillions of signals


#' Parse a hierarchy from a VCD File
#'
#' This function parses the module/variable hierarchy from the VCDFile into a
#' \code{data.tree} structure.
#'
#' @param tok a \link{Tokenizer} set up to read from the VCDFile
#'
#' @return returns the parsed hierarchy

parseHierarchy <- function(tok) {
  # create an empty tree and hand off to the module parser
  node <- data.tree::Node$new()
  parseScope(node,tok)
  return(node)
}


#' Parse a scope statement from a VCD File
#'
#' This function parses a single scope from the VCDFile into a
#' \code{data.tree} (subtree-)structure.
#'
#' @param node the hierarchy where nodes leaves shall be appended
#' @param tok a \link{Tokenizer} set up to read from the VCDFile
#'
#' @return returns the parsed hierarchy

parseScope <- function(node,tok) {
  # this function is entered only when a new scope is hit
  data <- parseStringFields(tok)
  # we create a non-leaf node, if data is of proper type
  if (!any(data[1] == c("begin","fork","function","module","task"))) {
    warning("Invalid scope type found: ",data[1])
  }
    node$type <- data[1]
    node$name <- data[2]

    # ----
    # now we are inside a scope
    # we gather all keywords until we hit an upscope
    # this is the recursive part of the parsing
    # ----

    buf <- tok$nextToken()

    while (!is.na(buf)){
      # fail fast
      if (substr(buf,1,1) != "$") {
        isEmptyLine <- !grepl("[^[:space:]]+",buf)
        if (!isEmptyLine) {
          warning("Invalid statement in scope definition: ",buf)
        }
       } else {
        key <- buf
        if (!any(c("$var","$scope","$upscope","$comment") == key)) {
          warning("Invalid keyword in scope definition: ",key)
        } else {
          if (key == "$comment") {
            # comments are currently ignored
            # alternate options would be collecting them into a separate variable
            # or generating warnings
            parseStringFields(tok)
           }
          if (key == "$var") {
            data <- parseStringFields(tok)
            if (!any(
              data[1] == c(
                "event","integer","parameter","real","reg","supply0","supply1",
                "time","tri","triand","trior","trireg","tri0","tri1","wand",
                "wire","wor")
            )) {
              warning("Invalid var type: ",data[1])
            } else {
              if (data[2] == 1) { # single bit # nolint
              child <- node$AddChild(data[3])
              child$type <- data[1]
              child$bits <- data[2]
              child$humanReadableName <- data[4]
              } else { # multi bit, gets appropriate number of children #nolint
                child <- node$AddChild(data[3])
                child$type <- data[1]
                child$bits <- data[2]
                child$humanReadableName <- data[4]

                for (idx in 0:(as.numeric(data[2])-1)) {
                cchild <- child$AddChild(paste0(data[3],".",idx,collapse=""))
                cchild$type <- data[1]
                cchild$bits <- 1
                cchild$humanReadableName <- paste0(data[4],", bit ",idx,collapse="")
                }
              }
            }
          }

          if (key == "$scope") { # recurse #nolint
            child <- node$AddChild("new scope")
            parseScope(child,tok)
           }

          if (key == "$upscope") {
            parseStringFields(tok)
            break
          }

        } # end valid keyword
        buf <- tok$nextToken()
      } # end while
  } # end inside scope
  return(node)
}
