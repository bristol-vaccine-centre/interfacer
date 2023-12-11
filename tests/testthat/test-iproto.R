test_that("iface prototyping", {
  
  expect_equal(
    iproto(i_diamonds),
    ggplot2::diamonds %>% dplyr::filter(FALSE)
  )
  
  expect_equal(
    iproto(i_iris), 
    iris %>% tibble::as_tibble() %>% dplyr::filter(FALSE)
  )
  
  
})
