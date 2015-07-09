# to do 
# testthat if conf is not in the right format, raise an error. -- checked
# testthat if len is of right format -- checked
# testthat if ID is found in matrix  -- checked
# testthat output is of right format -- checked 
# testthat output is correct. 


context("Testing findIDpaths Function")

# test data generation
set.seed(1)
edgelist1 <- data.frame(col1 = sample(letters[1:15], 200, replace = TRUE), 
                        col2 = sample(letters[1:15], 200, replace = TRUE), 
                        stringsAsFactors = FALSE)
edgelist1 <- edgelist1[-which(edgelist1$col1 == edgelist1$col2), ]
testMatrix2 <- as.conflictmat(edgelist1)

test_that("input 'conf' is of 'conf.mat'", {
  
  testMatrix1 <- edgelisttomatrix(edgelist1)
  expect_error(findIDpaths(testMatrix1, "A", len = 2),
               "Turn conf into a 'conf.mat' using 'as.conflictmat'.")
})

test_that("return error for incorrect len", {
  
  expect_error(findIDpaths(testMatrix2, "A", len = 1), "len should be no smaller than 2.")
  expect_error(findIDpaths(testMatrix2, "A", len = 7), "len should be no greater than 6.")
  expect_error(findIDpaths(testMatrix2, "A", len = 2.3), "'len' needs to be an integer.")
})


test_that("return error for incorrect ID", {
  
  expect_error(findIDpaths(testMatrix2, "AB", len = 3), "ID not found in the conflict matrix. Making sure the ID and the conflict matrix are correct.")
})

test_that("output is a matrix of len + 1 column, or a list of two if no paths found starting at ID", {
  
  expect_is(findIDpaths(testMatrix2, "a", len = 2), "matrix")
  expect_equal(dim(findIDpaths(testMatrix2, "a", len = 2))[2], 3)
  expect_equal(dim(findIDpaths(testMatrix2, "a", len = 4))[2], 5)
  testMatrix3 <- testMatrix2
  testMatrix3[1,] <- 0
  expect_is(findIDpaths(testMatrix3, "a", len = 2), "list")
  expect_equal(findIDpaths(testMatrix3, "a", len = 2)[[2]], "no pathways found starting at a")
})



test_that("outputs are correct", {
  
  expect_equal_to_reference(findIDpaths(testMatrix2, "a", len = 2), file = "findIDpathsOutput1.rds")
})
