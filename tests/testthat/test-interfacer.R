# defined in helper-data.R
# print(i_diamonds)

test_that("conversion works", {
  
  xfn = function(df = i_diamonds) {
    return(ivalidate(df))
  }
  
  # Type checking passes
  ggplot2::diamonds %>% expect_equal(., xfn(.))
  
  # Type coercion works (at least for numerics)
  tmp = ggplot2::diamonds %>% 
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.character)) %>%
    xfn() 
  
  expect_equal(tmp, ggplot2::diamonds)
  
  # Type checking fails
  expect_error(
    ggplot2::diamonds %>% 
      dplyr::select(-carat,-price) %>%
      xfn(),
    regexp = "missing:"
  )
  
  expect_error(
    ggplot2::diamonds %>% 
      dplyr::group_by(carat, price) %>%
      xfn(),
    regexp = "additional:"
  )
  
})


test_that("multiple errors reported", {
  
  xfn = function(df = iface(
    cut = enum(A,B,C,D) ~ "this is not true",
    color = enum(X,Y,Z) ~ "neither is this",
  )) {
    return(ivalidate(df))
  }
  
  expect_error(xfn(ggplot2::diamonds),regexp = "cannot be coerced to a enum\\(A, B, C, D\\)")
  expect_error(xfn(ggplot2::diamonds),regexp = "cannot be coerced to a enum\\(X, Y, Z\\)")
  
})


# TODO: this test works in all situations apart from devtools::check.
# For some reason in that specific context the environment does not pick up the 
# definitions in testthat/helper-data.R
# test_that("Nested DF works", {
#   
#   
#   xfn = function(df = i_diamonds_cat) {
#     return(ivalidate(df))
#   }
#   
#   expect_no_error(
#     nested_diamonds %>% xfn()
#   )
#   
#   expect_error(
#     nested_diamonds %>% 
#       dplyr::mutate(data = purrr::map(data, ~ .x %>% dplyr::select(-price))) %>%
#       xfn(),
#     regexp = "missing"
#   )
#   
# })