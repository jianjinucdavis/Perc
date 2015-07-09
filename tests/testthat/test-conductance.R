# to do 
# testthat if conf is not in the right format, raise an error. -- checked
# testthat output is a list of two elements. -- checked

context("Testing Conductance Function")

test_that("input 'conf' is of 'conf.mat'", {
  set.seed(1)
  edgelist1 <- data.frame(col1 = sample(letters[1:26], 100, replace = TRUE), 
                          col2 = sample(letters[1:26], 100, replace = TRUE), 
                          stringsAsFactors = FALSE)
  edgelist1 <- edgelist1[-which(edgelist1$col1 == edgelist1$col2), ]
  testMatrix1 <- edgelisttomatrix(edgelist1)
  expect_error(conductance(testMatrix1, maxLength = 2),
               "Turn conf into a 'conf.mat' using 'as.conflictmat'.")
})

test_that("input 'conf' is of 'conf.mat'", {
  set.seed(1)
  edgelist1 <- data.frame(col1 = sample(letters[1:15], 200, replace = TRUE), 
                          col2 = sample(letters[1:15], 200, replace = TRUE), 
                          stringsAsFactors = FALSE)
  edgelist1 <- edgelist1[-which(edgelist1$col1 == edgelist1$col2), ]
  testMatrix1 <- as.conflictmat(edgelist1)
  expect_is(conductance(testMatrix1, maxLength = 2), "list")
  expect_equal(length(conductance(testMatrix1, maxLength = 2)), 2)
})

