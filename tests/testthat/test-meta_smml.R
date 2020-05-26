context("meta_smml")

test_that("meta_smml", {
  tt <- meta_smml("Polyporus badius", spec_type = "fungus")
  
  expect_is(tt, "matrix")
})
