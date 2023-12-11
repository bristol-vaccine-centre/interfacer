



test_that("imappper works", {
  
  x = function(df = iface(col1 = integer ~ "an integer column" ), ...) {
    df = ivalidate(df,...)
    return(df)
  }
  
  input=tibble::tibble(col2 = c(1,2,3))
  
  # This fails because col1 is missing
  expect_error(
    x(input)
  )
  
  # This fixes it for this input
  tmp = x(input, .imap=imapper(col1 = col2))
  expect_equal(tmp$col1, input$col2)
  
  
})
