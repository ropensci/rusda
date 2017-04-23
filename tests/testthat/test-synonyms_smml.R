context("synonyms_smml")

test_that("synonyms_smml", {
  tt <- synonyms_smml("Fagus sylvatica", spec_type = "fungus")                  

  expect_is(tt, "list")
})
