context("associations")

test_that("associations", {
  tt <- associations("Fagus sylvatica", database = "both", spec_type = "fungus")

  expect_is(tt, "list")
  expect_is(tt$synonyms, "list")
  expect_is(tt$associations, "list") 
})
