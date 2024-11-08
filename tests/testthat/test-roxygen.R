test_that("@iparam generates an entry", {
  out <- roxygen2::roc_proc_text(roxygen2::rd_roclet(), "
    #' Bar
    #'
    #' @iparam df A test dataframe
    bar <- function(df = interfacer::iface(
      x = integer ~ \"The X column\",
      y = double ~ \"The Y column\",
    )) {}

  ")
  
  out = format(out$bar.Rd)
  
  # I expect to see here the foo dot params documented
  
  lapply(c(
      "\\item{df}{A test dataframe", 
      "\\item x (integer) - The X column", 
      "\\item y (double) - The Y column"), 
  function(.x) {
    expect(
      stringr::str_detect(out,stringr::fixed(.x)),
      paste0("could not find documentation string: ",.x)
    )
  })
  
  
})