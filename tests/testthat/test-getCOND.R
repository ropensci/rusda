context("getCOND")

test_that("getCOND", {
  tt <- getCOND("fungus")
 
  expect_is(tt, "list")
  expect_is(tt$sp, "integer")
  expect_is(tt$hfu, "integer")
  expect_is(tt$hf.st, "integer")
  expect_is(tt$spe.st, "integer")  
})
