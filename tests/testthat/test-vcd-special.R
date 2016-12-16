context("VCD parsing special cases")

test_that("sanity checks in parseVCDForKeys work",{
  vcd<-VCDFile("wikipedia-mod.vcd",F)
  vcd2<-parseVCDForKeys(vcd,c())
  expect_identical(vcd,vcd2)

  vcd3<-VCDFile("wikipedia-mod.vcd",F)
  vcd3$filename<-"gnat.vcd"
  expect_warning(vcd4<-parseVCDForKeys(vcd3,c("$comment")),"File does not exist: gnat.vcd")
  expect_identical(vcd3,vcd4)
})
