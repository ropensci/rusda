context("literature")

test_that("literature", {
  tt <- literature("Polyporus badius", process = TRUE, spec_type = "fungus")

  expect_is(tt, "list")
})
