context("getMETA")

test_that("getMETA", {
  tt <- getMETA("fugus")  

  expect_is(tt, "numeric")
  expect_equal(length(tt), 4)  
})
