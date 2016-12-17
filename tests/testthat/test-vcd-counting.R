context("VCD toggle parsing and counting")

test_that("parsing toggle counts works (one level)",{
  source("countref.R")
  vcd<-VCDFile("wikipedia.vcd")
  counts.top<-parseToggles(vcd)
  counts.top$hierarchy<-data.tree::ToListSimple(counts.top$hierarchy)
  expect_equal(counts.top,counts.topref)
})

test_that("parsing toggle counts works (full design)",{
  source("countref.R")
  vcd<-VCDFile("wikipedia.vcd")
  counts.all<-parseToggles(vcd,depth=-1)
  counts.all$hierarchy<-data.tree::ToListSimple(counts.all$hierarchy)
  expect_equal(counts.all$hierarchy,counts.allref$hierarchy)
  expect_equal(counts.all$timestamps,counts.allref$timestamps)
  # R CMD check and devtools::test() produce different orders
  expect_named(counts.all$counts,names(counts.allref$counts),ignore.order = T)
  expect_equal(counts.all$counts[names(counts.allref$counts)],counts.allref$counts)
})

test_that("parsing toggle counts works (subhierarchy))",{
  source("countref.R")
  vcd<-VCDFile("wikipedia.vcd")
  counts.sub<-parseToggles(vcd,"#",depth=2)
  expect_equal(counts.sub$hierarchy,counts.subref$hierarchy)
  expect_equal(counts.sub$timestamps,counts.subref$timestamps)
  # R CMD check and devtools::test() produce different orders
  expect_named(counts.sub$counts,names(counts.subref$counts),ignore.order = T)
  expect_equal(counts.sub$counts[names(counts.subref$counts)],counts.subref$counts)
})

test_that("accumulating for a non-existent signal gives an error",{
  vcd<-VCDFile("wikipedia.vcd")
  parsedVCD<-parseToggles(vcd,top=NA,depth=-1L)
  expect_warning(counts<-accumulate("gnat",parsedVCD),"gnat is not available in this dump")
  expect_identical(counts,parsedVCD$counts)
})

test_that("simple accumulating works",{
  vcd<-VCDFile("wikipedia.vcd")
  parsedVCD<-parseToggles(vcd,top=NA,depth=-1L)
  expect_equal(sapply(FUN=length,parsedVCD$counts$`#`),c("0"=0,"1"=0,"z"=0,"x"=0))
  counts<-accumulate("logic",parsedVCD)
  expect_equal(counts$`#`,list("0"=c("2296"=2),"1"=integer(0),"z"=integer(0),"x"=integer(0)))
})
