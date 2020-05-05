# Tests for .norm.

context(".norm")

expect_equal(round(strand:::.norm(c(10, 1, 100), out_sd = 2), 2), c(0, -2, 2))
expect_equal(round(strand:::.norm(c(10, NA, 100), out_sd = 2), 2),  c(-1.41, NA,  1.41))

w <- data.frame(x = c(10, 1, 100, 15, NA, 100), 
                y = c(rep("A", 3), rep("B", 3)), 
                z = rep(c("C", "D"), 3))


test_that("x must be a vector", {
  m <- matrix(1:6, ncol = 2)
  
  expect_error(strand:::.norm(m))
  expect_error(strand:::.norm(as.data.frame(m)))
})

test_that("x must be numeric", {
  expect_error(strand:::.norm(letters[1:5]))
  expect_error(strand:::.norm(c(TRUE, TRUE, FALSE)))
})


test_that("close input values", {
  
  # We've had issues where tiny input differences in inputs produce big
  # differences in outputs.  We have (hopefully) corrected these.  See the code
  # for more comments on the issue.
  
  
  x <- c(-1, -1 + 1e-12, 0, 1, 1)
  z <- strand:::.norm(x)
  
  # Output should be c(-1, -1, 0, 1, 1)
  
  expect_true(abs(z[1] - z[2]) < 1e-20)
})

context("normalize")

out_list1 <- list(y = array(c(2, 3), dimnames = list(c("A", "B"))))
out_list2 <- list(y = c(2, 3))
out_list3 <- list(y = array(c(2, 3), dimnames = list(c("A", "C"))))

test_that("out_sd not list", {
  expect_error(normalize(w, in_var = "x", 
                         within_var = "y",
                         out_sd = c("x", "y")
  ))
})

test_that("out_sd elements not arrays", {
  expect_error(normalize(w, in_var = "x", 
                         within_var = "y",
                         out_sd = out_list2
  ))
})

test_that("out_sd elements should exist in data frame", {
  expect_error(normalize(w, in_var = "x", 
                         within_var = "y",
                         out_sd = out_list3
  ))
})

test_that("vector case should have null in_var / within_var", {
  expect_error(normalize(w$x, in_var = "x"))
  expect_error(normalize(w$x, within_var = "y"))
})

test_that("x must be a vector or data frame", {
  expect_error(normalize(matrix(1:6, nc = 2)))
})

test_that("in_var and within_var must be in x", {
  expect_error(normalize(w, in_var="x", within_var="a", out_sd=out_list1))
  expect_error(normalize(w, in_var="a", within_var="y", out_sd=out_list1))
})

test_that("the in_var column must be numeric", {
  expect_error(normalize(w, in_var="y", within_var="x", out_sd=out_list1))
})

# Tricky cases involving adj_var

df <- data.frame(x = c(10, 1, 30, 15, NA, 30), 
                 y = c(rep("A", 3), rep("B", 3)), 
                 z = rep(c("C", "D"), 3),
                 w = c(-1, 2, -1, 3, -2, -3))


test_that("adj_var with one variable", {
  
  result <- normalize(df, in_var = "x", adj_var = "w")
  
  expect_equal(round(result, 2), c(-0.58, -1.29,  1.29,  0.58,    NA,  0.00))  
})


# normalize should fail if adj_var is all 0

df$w <- 0

test_that("adj_var is all 0", {
  
  expect_error(normalize(df, in_var = "x", adj_var = "w"))
  
})

# TODO Investigate tests where standard deviations do not equal 1. Perhaps there
# is a problem with small samples? For big samples, sd looks in line.

expect_equal(normalize(as.numeric(NA)), as.numeric(NA))
expect_equal(round(normalize(c(10, 1, 100)), 2), c(0, -1, 1))
expect_equal(round(normalize(c(10, NA, 100)), 2),  c(-0.71, NA,  0.71))

# Same in a data frame

expect_equal(round(normalize(data.frame(x = c(10, 1, 100)), in_var = "x"), 2),
             c(0, -1, 1))
expect_equal(round(normalize(data.frame(x = c(10, NA, 100)), in_var = "x"), 2), 
             c(-0.71, NA, 0.71))

# simple out_sd. Only works for .norm. Not allowed as option (directly) in
# normalize.

# Complex data.frame, including listed out_var

w <- data.frame(x = c(10, 1, 100, 10, NA, 100), y = c(rep("A", 3), rep("B", 3)))

expect_equal(round(normalize(w, in_var = "x"), 2), 
             c(-.29, -1.38, 0.98, -.29, NA, 0.98))
expect_equal(round(normalize(w, in_var = "x", within_var = "y"), 2), 
             c(0.00, -1.29, 1.29, -0.58, NA, 0.58))

# Full complexity

w <- data.frame(x = c(10, 1, 100, 15, NA, 100), 
                y = c(rep("A", 3), rep("B", 3)), 
                z = rep(c("C", "D"), 3))

out_list1 <- list(y = array(c(2, 3), dimnames = list(c("A", "B"))))

expect_equal(round(normalize(w, in_var = "x", 
                             within_var = "y", out_sd = out_list1), 2), 
             c(0.00, -0.58, 0.58, -1.29, NA, 1.29))

# Richer data.frame and more complex outlist.

set.seed(9)
numb <- 100
w <- data.frame(x = rnorm(numb), y = c(rep("A", numb/2), rep("B", numb/2)), z = rep(c("C", "D"), numb/2))

# Simple case to warm up.

test <- normalize(w, in_var = "x", within_var = c("y", "z"))

expect_equal(as.numeric(round(tapply(test, w$y, sd))), c(1,1))

# More interesting target.

out_list2 <- list(y = array(c(2, 3), dimnames = list(c("A", "B"))),
                  z = array(c(0.5), dimnames = list(c("C"))))

test <- normalize(w, in_var = "x", within_var = c("y", "z"), out_sd = out_list2)

# Note that, as the code discusses, you do not get exacly, or even close, to 
# the output standard deviations that you want. At best, the relative ratios of
# standard deviations, at least within a given factor, look OK.

expect_equal(as.numeric(round(tapply(test, w$y, sd), 2)), c(0.80, 1.17))
expect_equal(as.numeric(round(tapply(test, w$z, sd), 2)), c(0.70, 1.24))

# Note how the standard deviations for A and B are nowhere near their targets in
# the next test. In fact, we get the (exact?) same answers as above because the
# ratio of the target standard deviations within variable y is the same, 2 to 3,
# in both cases.

# TODO Investigate: shouldn't the fact that the target for C (within Z) is 1/4
# of A (within y) in the first case versus 1/24 in the second change things?

out_list3 <- list(y = array(c(12, 18), dimnames = list(c("A", "B"))),
                  z = array(c(0.5), dimnames = list(c("C"))))

test2 <- normalize(w, in_var = "x", within_var = c("y", "z"), out_sd = out_list3)

expect_equal(as.numeric(round(tapply(test2, w$y, sd), 2)), c(0.80, 1.17))
expect_equal(as.numeric(round(tapply(test2, w$z, sd), 2)), c(0.70, 1.24))


# Tricky cases involving out_sd

w <- data.frame(x = c(10, 1, 100, 15, NA, 100), 
                y = c(rep("A", 3), rep("B", 3)), 
                z = rep(c("C", "D"), 3))


test_that("warm up", {
  expect_equal(round(normalize(w$x), 2), c(-0.59, -1.34,  0.96,  0.01, NA,  0.96))
})

test_that("out_sd alone with single level", {
  
  result <- normalize(w, in_var = "x",
                      out_sd = list(y = array(c(20), dimnames = list(c("A")))))
  
  expect_equal(round(result, 2), c(-0.58, -1.29,  1.29,  0.00,    NA,  0.58))  
})

test_that("out_sd alone with two levels", {
  
  result <- normalize(w, in_var = "x",
                      out_sd = list(y = array(c(2, 20), dimnames = list(c("A", "B")))))
  
  # Note that this test does not give us the results that we, allegedly, want.
  # We want the standard deviation of the B level for y to be much bigger than
  # the standard deviation of the A level. Instead, we get:
  
  #   > round(tapply(result, w$y, sd, na.rm = TRUE), 2)
  #   A    B 
  #   0.94 0.91 
  
  # The reason is because of the interaction with the initial data (in which 
  # all the input values are very positive) and the standard requirement that 
  # the final result be N(0, 1).
  
  expect_equal(round(result, 2), c(-0.58, -1.29, 0.58, 0.00, NA, 1.29))  
})


# Tricky cases involving adj_var

df <- data.frame(x = c(10, 1, 30, 15, NA, 30), 
                 y = c(rep("A", 3), rep("B", 3)), 
                 z = rep(c("C", "D"), 3),
                 w = c(-1, 2, -1, 3, -2, -3))


test_that("adj_var with one variable", {
  
  result <- normalize(df, in_var = "x", adj_var = "w")
  
  expect_equal(round(result, 2), c(-0.58, -1.29,  1.29,  0.58,    NA,  0.00))  
})


