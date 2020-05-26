context("getSYNS")

test_that("getSYNS", {
  tt <- getSYNS("Fagus sylvatica", process=TRUE)

  expect_is(tt, "character") 
})
