context("VCD plotting")

test_that("invalid plot types are caught",{
  vcd<-VCDFile("wikipedia.vcd")
  parse<-parseToggles(vcd,depth=-1L)
  expect_error(plotToggles(vcd,parse,type="foo"),"Plot type not supported.")
})

test_that("invalid signal selection is caught",{
  vcd<-VCDFile("wikipedia.vcd")
  parse<-parseToggles(vcd,depth=-1L)
  expect_error(plotToggles(vcd,parse,top="foo"),"foo is not in the parsed hierarchy.")
})

test_that("plotting with ggplot2 works",{
  vcd<-VCDFile("wikipedia.vcd")
  parse<-parseToggles(vcd,depth=-1L)
  pt<-plotToggles(vcd,parse,type="ggplot2",toggle_hold_time = 5)
  pt$plot$plot_env<-NULL
  expect_equal_to_reference(pt$plot,file="ggplot.rds")
})

test_that("plotting with dygraph works",{
  vcd<-VCDFile("wikipedia.vcd")
  parse<-parseToggles(vcd,depth=-1L)
  pt<-plotToggles(vcd,parse,type="dygraphs",toggle_hold_time = 50)
  expect_equal_to_reference(pt$plot,file="dygraph.rds")
})

test_that("plotting with dygraph with events works",{
  vcd<-VCDFile("wikipedia.vcd")
  parse<-parseToggles(vcd,depth=-1L)
  pt<-plotToggles(vcd,parse,type="dygraphs",toggle_hold_time = 50,
                  events = c("time 1000"="1000","another event"="2000"))
  expect_equal_to_reference(pt$plot,file="dygraph-event.rds")
})

test_that("plotting with plotly works",{
  vcd<-VCDFile("wikipedia.vcd")
  parse<-parseToggles(vcd,depth=-1L)
  pt<-plotToggles(vcd,parse,type="plotly",toggle_hold_time = 5)
  # plotly generates a UID for each plot, set a non-UID here
  nuid<-"123456789acf"
  pt$plot$x$cur_data<-nuid
  names(pt$plot$x$visdat)<-nuid
  names(pt$plot$x$attrs)<-nuid
  names(pt$plot$x$layoutAttrs)<-nuid
  expect_equal_to_reference(pt$plot,file="plotly.rds")
})
