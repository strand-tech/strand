context("adjust")

z <- c(-1, -1, 0, 1, 1)

test_that("in_var not character", {
  in_df <- data.frame(y = z,
                      x1 = c(7, 7, 9, 7, 7)
  )
  expect_error(adjust(in_df, 3.14, "x1"))
  expect_error(adjust(in_df, TRUE, "x1"))
})

test_that("adj_var not character", {
  in_df <- data.frame(y = z,
                      x1 = c(7, 7, 9, 7, 7)
  )
  expect_error(adjust(in_df, "y", 3.14))
  expect_error(adjust(in_df, "y", TRUE))
})

test_that("in_var not length 1", {
  in_df <- data.frame(y = z,
                      x1 = c(7, 7, 9, 7, 7)
  )
  expect_error(adjust(in_df, c("foo", "bar"), "x1"))
  expect_error(adjust(in_df, character(0), "x1"))
})

test_that("adj_var length 0", {
  in_df <- data.frame(y = z,
                      x1 = c(7, 7, 9, 7, 7)
  )
  expect_error(adjust(in_df, "y", character(0)))
})

test_that("var names not in input df", {
  in_df <- data.frame(y = z,
                      x1 = c(7, 7, 9, 7, 7)
  )
  expect_error(adjust(in_df, "foo",   "x1"))
  expect_error(adjust(in_df, "y"   , "foo"))
})

test_that("in_var must be numeric", {
  in_df <- data.frame(y = letters[1:5],
                      x1 = c(7, 7, 9, 7, 7)
  )
  expect_error(adjust(in_df, "y", "x1"))
})

z <- c(-1, -1, 0, 1, 1)

test_that("1 orthogonal adj_var", {
  in_df <- data.frame(y = z,
                      x1 = c(7, 7, 9, 7, 7)
  )
  
  expect_equal(adjust(in_df, "y", "x1"), z)
})

test_that("1 orthogonal adj_var with looping", {
  in_df <- data.frame(y = z,
                      x1 = c(7, 7, 9, 7, 7)
  )
  
  expect_equal(adjust(in_df, "y", "x1", loops = 3), z)
})

test_that("2 orthogonal adj_vars", {
  in_df <- data.frame(y = z,
                      x1 = c(7, 7,  9, 7, 7),
                      x2 = c(5, 5, -3, 5, 5)
  )
  
  expect_equal(adjust(in_df, "y", c("x1", "x2")), z)
})

test_that("3 orthogonal adj_vars", {
  in_df <- data.frame(y = z,
                      x1 = c(7, 7,  9, 7, 7),
                      x2 = c(5, 5, -3, 5, 5),
                      x3 = c(6, 0,  3, 0, 6)
  )
  
  expect_equal(adjust(in_df, "y", c("x1", "x2", "x3")), z)
})

test_that("3 orthogonal adj_vars with looping", {
  in_df <- data.frame(y = z,
                      x1 = c(7, 7,  9, 7, 7),
                      x2 = c(5, 5, -3, 5, 5),
                      x3 = c(6, 0,  3, 0, 6)
  )
  
  expect_equal(adjust(in_df, "y", c("x1", "x2", "x3"), loops = 5), z)
})

test_that("3 orthogonal adj_vars plus rescaling", {
  in_df <- data.frame(y = 5 * z + 23,
                      x1 = c(7, 7,  9, 7, 7),
                      x2 = c(5, 5, -3, 5, 5),
                      x3 = c(6, 0,  3, 0, 6)
  )
  
  expect_equal(adjust(in_df, "y", c("x1", "x2", "x3")), z)
  
  
  
  in_df <- data.frame(y = c(17, 17, 22, 99, 99),
                      x1 = c(7, 7,  9, 7, 7),
                      x2 = c(5, 5, -3, 5, 5),
                      x3 = c(6, 0,  3, 0, 6)
  )
  
  expect_equal(adjust(in_df, "y", c("x1", "x2", "x3")), z)
})

test_that("repeated orthogonal adj_vars", {
  in_df <- data.frame(y = z,
                      x1 = c(7, 7, 9, 7, 7),
                      x2 = c(7, 7, 9, 7, 7),
                      x3 = c(7, 7, 9, 7, 7)
  )
  
  expect_equal(adjust(in_df, "y", c("x1", "x2", "x3")), z)
})

test_that("random numeric inputs", {
  set.seed(123)
  
  in_df <- data.frame(y  = runif(10),
                      x1 = runif(10),
                      x2 = runif(10)
  )
  
  out <- c(0.13739244, 0.41963718, -0.72746192, 0.72746192, 1.09309382,
           -1.60654069, -1.09309382, -0.41963718, -0.13739244, 1.60654069)
  
  expect_equal(adjust(in_df, "y", c("x1", "x2")), out, tolerance = 1e-7)
})

test_that("random numeric inputs with missings", {
  set.seed(123)
  
  in_df <- data.frame(y = runif(10),
                      x1 = runif(10),
                      x2 = runif(10)
  )
  
  is.na(in_df$y [3]) <- TRUE
  is.na(in_df$x1[6]) <- TRUE
  is.na(in_df$x2[7]) <- TRUE
  
  out <- c(-0.85208612, 0, NA, 0.40253863, 1.45324186,
           NA, NA, -1.45324186, -0.40253863, 0.85208612)
  
  expect_equal(adjust(in_df, "y", c("x1", "x2")), out, tolerance = 1e-7)
})

test_that("random non-numeric inputs", {
  
  set.seed(123)
  
  in_df <- data.frame(y  = runif(10),
                      x1 = sample(letters[1:3]  , 10, replace = TRUE),
                      x2 = sample(c(TRUE, FALSE), 10, replace = TRUE),
                      stringsAsFactors = FALSE
  )
  
  out <- c(-1.09309382248304, 1.09309382248304, -0.419637178044617,
           -0.1373924363076, 0.727461922453155, -0.727461922453155,
           -1.60654069474707, 1.60654069474707, 0.1373924363076,
           0.419637178044617)
  
  expect_equal(adjust(in_df, "y", c("x1", "x2")), out, tolerance = 1e-14)
})
