# code for diplaying/plotting here

# TODO: select which type of toggles -> remove with weights 0
# TODO: move summing into top function
# TODO: select colors from RColorBrewer

plotToggles <-
  function(vcd,counts,
           top,
           weights = list(
             "0" = -1,
             "1" = 1,
             "z" = 0,
             "x" = 1
           ),
           type = c("dygraph", "plotly")) {
    if (is.null(counts[[top]]) ||
        all(sapply(counts[[top]], function(x)
          length(x) < 1))) {
      counts <- accumulate(top, counts)
    }

    p <- NULL

    if (type == "dygraph") {
      p <- plotToggles.dygraph(counts, top, weights)
    }

    if (type == "plotly")
    {
      p <- plotToggles.plotly(counts, top, weights)
    }
    return(list(p, counts))
  }

plotToggles.dygraph <-
  function(vcd,counts,
           top,
           weights = list(
             "0" = -1,
             "1" = 1,
             "z" = 0,
             "x" = 1
           )) {

  }

plotToggles.plotly <-
  function(vcd,counts,
           top,
           weights = list(
             "0" = -1,
             "1" = 1,
             "z" = 0,
             "x" = 1
           )) {

    ys<-counts[[top]]

    for (val in names(ys)) {
      if (length(ys[[val]])==0) {ys[[val]]<-NULL} else
      {ys[[val]]<-sapply(noNA(ys[[val]]),function(x) weights[[val]]*x)}
    }

    ys[["sum"]]<-rowSums(sapply(1:length(ys),function(x) ys[[x]][parse$timestamps]),na.rm=T)


    p<-plotly::plot_ly() %>%
      plotly::layout(xaxis = list(title=vcd$timescale["unit"]), yaxis = list(title="toggles"))

    for (val in names(ys)) {
      if (val!="sum")
        p<-plotly::add_trace(p,x = parse$timestamps, y = ys[[val]], fill = "tozeroy", name=paste0("toggles to",val,collapse = " "), line=list(shape="hv"))

    }
    p<-plotly::add_trace(p,x = parse$timestamps, y = ys[["sum"]], name="weighted sum", line=list(shape="hv"))



    return(p)
  }


# helper

noNA <- function(x) {
  sapply(x, function(y) if(is.na(y)) {0} else {y})
}
