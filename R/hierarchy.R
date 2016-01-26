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
#' @param vcd the VCDFile to parse from
#' @param buf a buffer to use when reading from that file
#' @param bufPos current reading position in the buffer
#' @param con a ext connection to the VCDFile opened for reading, used to refill the buffer
#'
#' @return returns the parsed hierarchy

parseHierarchy <- function(buf,bufPos,con,vcdfile) {
  # create an empty tree and hand off to the module parser
  node <- data.tree::Node$new()
  ret <- parseModule(node,buf,bufPos,con,vcdfile)

  ret$data <- node
  return(ret)
}


#' Parse a scope statement from a VCD File
#'
#' This function parses a single scopefrom the VCDFile into a
#' \code{data.tree} (subtree-)structure.
#'
#' @param vcd the VCDFile to parse from
#' @param buf a buffer to use when reading from that file
#' @param bufPos current reading position in the buffer
#' @param con a ext connection to the VCDFile opened for reading, used to refill the buffer
#' @param the hierarchy where nodes leaves shall be appenden (really needed?)
#'
#' @return returns the parsed hierarchy

parseModule <- function(node,buf,i,con,vcdfile) {
  # this function is entered only when a new scope is hit
  ret <- parseStringFields(buf,i,key,con,vcdfile,field = "scope")
  data <- strsplit(ret$data," ")[[1]]
  i <- ret$bufPos
  buf <- ret$buf

  # we create a non-leaf node, if data is of proper type
  if (!any(data[1] == c("begin","fork","function","module","task"))) {
    warning("Invalid scope type found: ",data[1]) # TODO: if we could obtain the absolute line number in the input file here, the warning wold be more helpful
  } else {
    node$type <- data[1]
    node$name <- data[2]

    # ----
    # now we are inside a scope
    # we gather all keywords until we hit an upscope
    # ----

    while (i <= length(buf$data))
    {
      # fail fast
      if (substr(buf$data[i],1,1) != '$') {
        isEmptyLine <- !grepl("[^[:space:]]+",buf$data[i])
        if (!isEmptyLine) {
          warning("Invalid statement in scope definition: ",buf$data[i])
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
        key <- strsplit(buf$data[i],' ')[[1]][1]

        if (!any(c("$var","$scope","$upscope","$comment") == key)) {
          warning("Invalid keyword in scope definition: ",key)
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
          if (key == "$comment") {
            # comments are currently ignored
            # alternate options would be collecting them into a separate variable
            # or generating warnings
            ret <-
              parseStringFields(buf,i,key,con,vcdfile,"comment")
            i <- ret$bufPos
            buf <- ret$buf

          }

          if (key == "$var") {
            ret <- parseStringFields(buf,i,key,con,vcdfile,"var")
            i <- ret$bufPos
            buf <- ret$buf
            data <- strsplit(ret$data,"[[:blank:]]+")[[1]]

            if (!any(
              data[1] == c(
                "event","integer","parameter","real","reg","supply0","supply1","time","tri","triand","trior","trireg","tri0","tri1","wand","wire","wor"
              )
            )) {
              warning("Invalid var type: ",data[1])
            } else {
              if (data[2] == 1) {
              child <- node$AddChild(data[3])
              child$type <- data[1]
              child$bits <- data[2]
              child$humanReadableName <- data[4]
              } else {
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

          if (key == "$scope") {
            child <- node$AddChild("new scope")
            ret <- parseModule(child,buf,i,con,vcdfile)
            i <- ret$bufPos
            buf <- ret$buf
          }


          if (key == "$upscope") {
            ret <- parseStringFields(buf,i,key,con,vcdfile,"upscope")
            i <- ret$bufPos
            buf <- ret$buf
            break
          }

        } # end valid keyword
      } # end
    } # end while
    ret$data <- node
    ret$buf <- buf
  } # end inside scope
  return(ret)
}
