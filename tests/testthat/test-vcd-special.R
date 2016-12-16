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

test_that("detection of malformed entries in VCD works",{
  longwarning<-"$upscope missing $end
Malformed VCD file: $upscope outside scope at offset 0364 bytes in input file.
Invalid statement at offset 0599 bytes in scope definition: invalidtoken
Invalid keyword at offset 0642 bytes in scope definition: $invalidkeyword
Invalid keyword at offset 0659 bytes in scope definition: $end
Invalid keyword at offset 0754 bytes in scope definition: $dumpvars
Invalid statement at offset 0964 bytes in scope definition: invalidtoken
multiple top modules, only lastest is kept. At offset 0723 bytes in input file."
  expect_warning(vcd<-VCDFile("wikipedia-mod.vcd"),longwarning)
})
