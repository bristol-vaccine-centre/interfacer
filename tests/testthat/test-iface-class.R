
test_that("list output sensible", {
  
  tmp = as.list(i_diamonds, flatten = TRUE)
  testthat::expect_equal(tmp$columns[[1]]$name, "carat")
  testthat::expect_equal(tmp$columns[[1]]$type, "numeric")
  testthat::expect_equal(tmp$has_default, FALSE)
  
})



