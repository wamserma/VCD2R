# code for diplaying/plotting here

#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%

plotToggles <-
  function(vcd,
           parse,
           top,
           weights = list(
             "0" = -1,
             "1" = 1,
             "z" = 0,
             "x" = 1
           ),
           type = c("dygraph", "plotly"),
           ...) {

    #check whether top is part of the hierarchy
    if (is.null(Find(parse$hierarchy,top))) {
      stop(top, " is not in the parsed hierarchy")
    }

    if (is.null(parse$counts[[top]]) ||
        all(sapply(parse$counts[[top]], function(x)
          length(x) < 1))) {
      parse$counts <- accumulate(top, parse)
    }

    ys <- parse$counts[[top]]

    for (val in names(ys)) {
      if ( (length(ys[[val]]) == 0) || (weights[[val]] == 0) ){
        ys[[val]] <- NULL # drop signal that have no count or weight zero
      } else{
        ys[[val]] <- sapply(noNA(ys[[val]]), function(x)
          weights[[val]] * x)
      }
    }

    ys[["sum"]] <-
      rowSums(sapply(1:length(ys), function(x)
        ys[[x]][parse$timestamps]), na.rm = T)


    p <- NULL

    dotargs<-list(...)

    if (type == "dygraph"){
      events <- vector("list",0L)
      if (!is.null(dotargs$events)) {
        events<-dotargs$events
      }
      p <- plotToggles.dygraph(parse$timestamps, ys, vcd$timescale,events)
    }

    if (type == "plotly"){
      p <- plotToggles.plotly(parse$timestamps, ys, vcd$timescale,...)
    }
    invisible(list(plot=p,counts=parse$counts))
  }

plotToggles.dygraph <-
  function(timestamps, ys, timescale,events=vector("list",0L)) {
    df<-cbind(as.numeric(timestamps),as.data.frame(sapply(ys, function(y) noNA(y[timestamps])),row.names=timestamps))
    p<-dygraphs::dygraph(df, main = "Toggle Counts vs. Runtime",
                         ylab = "toggle events",
                         xlab = gettextf("time in steps of %s %s",timescale["scale"],timescale["unit"])) %>%
      # set dySeries Labels here
      dygraphs::dyOptions(stackedGraph = FALSE, stepPlot=T) %>%
      dygraphs::dyRangeSelector()
      #dygraphs::dyRoller(rollPeriod = 2) # average over one clock cycle, TODO: make optional # nolint

    if (length(events) > 0) {
      for (e in 1:length(events)) {
        e.name <- names(events)[[e]]
        e.times <- events[[e]]
        for (ts in e.times) {
          p %<>% dygraphs::dyEvent(ts, label = e.name, labelLoc = "top")
        }
      }
    }

    #TODO make annotations for certain values like in presAnnotation example
    invisible(p)
  }

plotToggles.plotly <-
  function(timestamps, ys, timescale,...) {
    p <- plotly::plot_ly(...) %>%
      plotly::layout(xaxis = list(title = timescale["unit"]),
                     yaxis = list(title = "toggles"))

    for (val in names(ys)) {
      if (val != "sum")
        p <-
          plotly::add_trace(
            p,
            x = timestamps,
            y = ys[[val]],
            fill = "tozeroy",
            name = paste0("toggles to", val, collapse = " "),
            line = list(shape = "hv")
          )
    }
    p <-
      plotly::add_trace(
        p,
        x = timestamps,
        y = ys[["sum"]],
        name = "weighted sum",
        line = list(shape = "hv")
      )
    invisible(p)
  }
