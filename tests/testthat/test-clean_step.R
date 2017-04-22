context("clean_step")

test_that("clean_step", {
  tt <- clean_step("Fagus sylvatica", spec_type = "fungus", synonyms_incl = TRUE, syns = TRUE, taxa = TRUE)

  expect_is(tt, "character")  
})
