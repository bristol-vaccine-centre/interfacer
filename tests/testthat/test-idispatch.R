
testfn = function(x,...) {
  interfacer::idispatch(x,
    testfn.diamonds = i_diamonds,
    testfn.iris = i_iris
  )
}

testfn.diamonds = function(x, ...) {
  sprintf("%d rows of diamonds",nrow(x))
}

testfn.iris = function(x, ...) {
  sprintf("%d rows of iris",nrow(x))
}


test_that("dispatch works", {
  
  expect_equal(
    testfn(ggplot2::diamonds),
    "53940 rows of diamonds"
  )
  
  expect_equal(
    testfn(iris),
    "150 rows of iris"
  )
  
  expect_error(
    testfn(mtcars)
  )
  
})
