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
  # see http://stackoverflow.com/questions/35712544/how-to-test-for-multiple-warnings-in-an-unknown-order-using-testthat/35730070#35730070 #nolint

  gotWarnings<-character(0)
  withCallingHandlers({
    vcd<-VCDFile("wikipedia-mod.vcd")
  },
    warning=function(e) {
    # Push warning onto vector in parent frame.
    gotWarnings <<- c(gotWarnings, conditionMessage(e))
    invokeRestart("muffleWarning")
  })

  longwarning<-c("$upscope missing $end",
                 "Malformed VCD file: $upscope outside scope at offset 0364 bytes in input file.",
                 "Invalid statement at offset 0599 bytes in scope definition: invalidtoken",
                 "Invalid keyword at offset 0642 bytes in scope definition: $invalidkeyword",
                 "Invalid keyword at offset 0659 bytes in scope definition: $end",
                 "Invalid keyword at offset 0754 bytes in scope definition: $dumpvars",
                 "Invalid statement at offset 0964 bytes in scope definition: invalidtoken",
                 "multiple top modules, only lastest is kept. At offset 0723 bytes in input file.",
                 "Ignored data outside block/scope at offset 01022 bytes in input file.",
                 "Malformed VCD file: '$dumpall' in header at offset 01035 bytes in input file.",
                 "Malformed VCD file: '$dumpon' in header at offset 01050 bytes in input file.",
                 "Malformed VCD file: '$dumpoff' in header at offset 01064 bytes in input file.",
                 "Malformed VCD file: '$dumpvars' in header at offset 01079 bytes in input file.",
                 "Invalid keyword '$invalidkey' at offset 01095 bytes in input file.",
                 "Invalid scope type found: magic",
                 "Invalid var type: 'potter' at offset 01234 bytes in input file.",
                 "multiple top modules, only lastest is kept. At offset 01108 bytes in input file.",
                 "$enddefinitions missing $end")
  # Ensure no unexpected warnings,
  expect_equal(length(gotWarnings), length(longwarning))

  # Test that each warning I want is there
  # all.equal qould be shorter, but this gives better reporting
  for (i in 1:length(gotWarnings)){
    expect_equal(longwarning[i], gotWarnings[i])
  }
})
