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

test_that("head works",{
  expect_identical(strHead("gnu"),"g")
  expect_identical(strHead(""),"")
  expect_identical(strHead(c("foo","bar")),c("f","b"))
})

test_that("head to lower works",{
  expect_identical(strHeadLower("Gnu"),"g")
  expect_identical(strHeadLower(""),"")
  expect_identical(strHeadLower(c("foo","Bar")),c("f","b"))
})

test_that("tail works",{
  expect_identical(strTail("Gnat"),"nat")
  expect_identical(strTail(""),"")
  expect_identical(strTail(c("foo","bar")),c("oo","ar"))
})


test_that("rev and split works",{
  expect_identical(strRevAndSplit("Gnat"),c("t","a","n","G"))
  expect_identical(strRevAndSplit(""),character(0))
  expect_identical(strRevAndSplit(c("foo","bar")),c("o","o","f"))
})

test_that("incrementing works",{
  expect_identical(incwithNA(NA_integer_),1L)
  expect_identical(incwithNA(NA_character_),1L)
  expect_identical(incwithNA(0),1)
  expect_identical(incwithNA(3),4)
  expect_identical(incwithNULL(NA_integer_),NA_integer_)
  expect_error(incwithNULL(NA_character_))
  expect_identical(incwithNULL(0),1)
  expect_identical(incwithNULL(NULL),1L)
  expect_identical(incwithNULL(3),4)
})

test_that("comparing string-encoded numbers works",{
  expect_true(stringnumLT("42","43"))
  expect_false(stringnumLT("43","42"))
  expect_false(stringnumLT("42","42"))
  expect_true(stringnumLT("42","143"))
  expect_false(stringnumLT("142","42"))
  expect_true(stringnumLT("","143"))
  expect_false(stringnumLT("142",""))
  expect_false(stringnumLT("",""))
})




test_that("sorting timestamps works",{
  t<-c("13","24","8","36","48","17","24","36","3","42","48","51","147")
  expect_equal(sorttimestamps(t),c("3","8","13","17","24","24","36","36","42","48","48","51","147"))
  expect_equal(sorttimestamps(c("")),c(""))
  expect_error(sorttimestamps(c()))
})

test_that("adding toggle-count-vectors works",{
  t<-1:4
  names(t)<-c("13","24","36","48")
  u<-1:6
  names(u)<-c("17","24","36","42","48","51")
  ref <- c(1,1,4,6,4,9,6)
  names(ref)<-c("13","17","24","36","42","48","51")
  counts<-list("a"=list("1"=t,"0"=u,"x"=c("0"=0L),"z"=c("0"=0L)),
               "b"=list("1"=u,"0"=t,"x"=c("0"=0L),"z"=c("0"=0L)))
  expect_identical(addToggleVecsByName("foo","bar"),list())
  expect_warning(ret<-addToggleVecsByName(c("a","b"),counts = counts),
                 "argument vtype has length > 1, remaining values ignored")
  expect_equal(ret,ref)
})
