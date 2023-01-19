
objet <- varlist(iris, to_df = T)
test_that("varlist() returns un dataframe", {
  expect_is(objet, "data.frame")
})
