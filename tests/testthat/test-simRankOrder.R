# to do 
# testthat if data is not in the right format, raise an error.
# testthat if values in data are 0 - 1.
# testthat output is a list of three elements.
# testthat output is correct.

context("Testing simRankOrder Function")

# test data generation
set.seed(1)
edgelist1 <- data.frame(col1 = sample(letters[1:15], 200, replace = TRUE), 
                        col2 = sample(letters[1:15], 200, replace = TRUE), 
                        stringsAsFactors = FALSE)
edgelist1 <- edgelist1[-which(edgelist1$col1 == edgelist1$col2), ]
testMatrix2 <- as.conflictmat(edgelist1)

# tests
test_that("input 'data' is of 'matrix'", {
  
  expect_error(simRankOrder(edgelist1, num = 2, kmax = 5),
               "The second element 'p.hat' from the output of 'conductance' should be used.")
})

test_that("values in input 'data' is between 0 - 1.", {
  conductanceOutput <- conductance(testMatrix2, 2)
  conductanceOutput[[2]][1,5] <- -1
  expect_error(simRankOrder(conductanceOutput[[2]], num = 2, kmax = 5),
               "The second element 'p.hat' from the output of 'conductance' should be used.")
  
  conductanceOutput[[2]][1,5] <- 1.5
  expect_error(simRankOrder(conductanceOutput[[2]], num = 2, kmax = 5),
               "The second element 'p.hat' from the output of 'conductance' should be used.")
  
})
