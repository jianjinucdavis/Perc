context("Importing Data Tests")

# test classes are right

# test errors
test_that("edgelists of more than 3 columns are not allowed", {
  testEdgelist1 <- data.frame(col1 = letters[1:10], col2 = letters[1:10],
                              col3 = sample(1:10, 10, replace = TRUE),
                              col4 = rnorm(1:10))
  expect_error(as.conflictmat(testEdgelist1))
})
# test warnings

