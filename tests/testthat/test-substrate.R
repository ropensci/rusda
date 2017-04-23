context("substrate")

test_that("substrate", {
  tt <- substrate("Fagus sylvatica")

  expect_is(tt, "list")
})
