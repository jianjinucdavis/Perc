# to do 
# testthat if conf is not in the right format, raise an error. -- checked
# testthat if maxlength is of right format  -- checked
# testthat output is of right format
# testthat output is correct.

context("Testing findAllPaths Function")

# test data generation
set.seed(1)
edgelist1 <- data.frame(col1 = sample(letters[1:15], 200, replace = TRUE), 
                        col2 = sample(letters[1:15], 200, replace = TRUE), 
                        stringsAsFactors = FALSE)
edgelist1 <- edgelist1[-which(edgelist1$col1 == edgelist1$col2), ]
testMatrix2 <- as.conflictmat(edgelist1)

test_that("input 'conf' is of 'conf.mat'", {
  
  testMatrix1 <- edgelisttomatrix(edgelist1)
  expect_error(findAllPaths(testMatrix1, maxLength = 2),
               "Turn conf into a 'conf.mat' using 'as.conflictmat'.")
})

test_that("return error for incorrect maxLength", {
  
  expect_error(findAllPaths(testMatrix2, maxLength = 1), "'maxLength' should be no smaller than 2.")
  expect_error(findAllPaths(testMatrix2, maxLength = 7), "'maxLength' should be no greater than 6.")
  expect_error(findAllPaths(testMatrix2, maxLength = 2.3), "'maxLength' needs to be an integer.")
})

test_that("output is a list of the right structure", {
  outputlist_maxLength2 <- findAllPaths(testMatrix2, maxLength = 2)
  outputlist_maxLength4 <- findAllPaths(testMatrix2, maxLength = 4)
  
  expect_is(outputlist_maxLength2, "list")
  expect_is(outputlist_maxLength4, "list")
  
  expect_equal(length(outputlist_maxLength2), 2)
  expect_equal(length(outputlist_maxLength4), 2)
  expect_equal(length(outputlist_maxLength2[[2]]), 1)
  expect_equal(length(outputlist_maxLength4[[2]]), 3)
})


test_that("outputs are correct", {
  
  expect_equal_to_reference(findAllPaths(testMatrix2, maxLength = 2), file = "findAllPathsOutput1.rds")
  expect_equal_to_reference(findAllPaths(testMatrix2, maxLength = 3), file = "findAllPathsOutput2.rds")
  expect_equal_to_reference(findAllPaths(testMatrix2, maxLength = 4), file = "findAllPathsOutput3.rds")
})

