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


test_that("parsing toggles of nonexistent file fails gracefully",{
  vcd<-VCDFile("wikipedia.vcd")
  vcd$filename<-"gnat.vcd"
  expect_warning(counts<-parseToggles(vcd),"File does not exist: gnat.vcd")
  expect_equal(counts,list())
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

test_that("parsing and accumulating works for aliased signals",{
  vcd<-VCDFile("wikipedia-mod2.vcd")
  parsedVCD<-parseToggles(vcd,top="top",depth=0L)
  expect_equal(sapply(FUN=length,parsedVCD$counts$top),c("0"=4,"1"=2,"z"=0,"x"=0))
  expect_equal(parsedVCD$counts$top$"0",c("0"=4,"2211"=1,"2296"=2,"2302"=1))
  expect_equal(parsedVCD$counts$top$"1",c("0"=2,"2296"=1))
})

test_that("accumulating after works for aliased signals",{
  vcd<-VCDFile("wikipedia-mod2.vcd")
  parsedVCD<-parseToggles(vcd,top="top",depth=-1L)
  expect_equal(sapply(FUN=length,parsedVCD$counts$"top"),c("0"=0,"1"=0,"z"=0,"x"=0))
  counts<-accumulate("top",parsedVCD)
  expect_equal(sapply(FUN=length,counts$top),c("0"=4,"1"=2,"z"=0,"x"=0))
  expect_equal(counts$top$"0",c("0"=4,"2211"=1,"2296"=2,"2302"=1))
  expect_equal(counts$top$"1",c("0"=2,"2296"=1))
})

test_that("warning when no timestamps are present",{
  vcd<-VCDFile("wikipedia-mod3.vcd")
  expect_warning(parseToggles(vcd,top="top",depth=-1L),"premature end of file")
})

test_that("variable dump is parsed",{
  vcd<-VCDFile("wikipedia-mod4.vcd")
  parsedVCD<-parseToggles(vcd,top="top",depth=-1L)
  expect_equal(sapply(FUN=length,parsedVCD$counts$"top"),c("0"=0,"1"=0,"z"=0,"x"=0))
  counts<-accumulate("top",parsedVCD)
  expect_equal(sapply(FUN=length,counts$top),c("0"=4,"1"=2,"z"=0,"x"=0))
  expect_equal(counts$top$"0",c("0"=4,"2211"=1,"2296"=2,"2302"=3))
  expect_equal(counts$"#.4"$"0",c("2302"=1))
  expect_equal(counts$"#.6"$"0",c("2302"=1))
})
