context("helper functions")

test_that("mapping of logic values to indices works",{
          expect_equal(scalarIndicatorToInt("0"),1)
          expect_equal(scalarIndicatorToInt("1"),2)
          expect_equal(scalarIndicatorToInt("zoo"),3)
          expect_equal(scalarIndicatorToInt("x"),4)
          expect_equal(scalarIndicatorToInt("q"),0)
})

test_that("mapping of multi-bit-types to indices works",{
  expect_equal(isMultiBit("b"),1)
  expect_equal(isMultiBit("r"),2)
  expect_equal(isMultiBit("q"),0)
})


test_that("adding of toggle-vecs works",{
  t<-1:4
  names(t)<-c("13","24","36","48")
  u<-1:6
  names(u)<-c("17","24","36","42","48","51")
  ref <- c(1,1,4,6,4,9,6)
  names(ref)<-c("13","17","24","36","42","48","51")
  expect_equal(addToggleVecs(t,u),ref)
})

test_that("left-extension of dumped values works",{
  expect_equal(leftExtend("10",5),"00010")
  expect_equal(leftExtend("01",6),"000001")
  expect_equal(leftExtend("ZX",7),"ZZZZZZX")
  expect_equal(leftExtend("X1",8),"XXXXXXX1"
  )
})
