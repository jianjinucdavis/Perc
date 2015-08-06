context("Testing bt.test function")


# test data generation
set.seed(1)
edgelist1 <- data.frame(col1 = sample(letters[1:15], 200, replace = TRUE), 
                        col2 = sample(letters[1:15], 200, replace = TRUE), 
                        stringsAsFactors = FALSE)
edgelist1 <- edgelist1[-which(edgelist1$col1 == edgelist1$col2), ]
testMatrix3 <- as.conflictmat(edgelist1)


test_that("outputs are correct", {
  testthat::skip_on_cran()

  expect_equal_to_reference(bt.test(testMatrix3), file = "bt.testoutput.rds")
  
})