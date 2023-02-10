
objet <- varlist(iris, tbl = T)
test_that("varlist() returns un dataframe", {
  expect_is(objet, "data.frame")
})
