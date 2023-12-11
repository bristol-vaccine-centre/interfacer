## Check validity checks ----

do_test = function(case, pass_data, fail_data, null_value = NA) {
  test_that(sprintf("values %s",case), {
    expect_error(
      .get_conv(case)(fail_data)
    )
    pass_data %>% expect_equal(
      ., .get_conv(case)(.)
    )
    if (!isTRUE(is.na(null_value))) {
      expect_equal( .get_conv(case)(NULL), null_value )
    
      typed_na = pass_data[1:5]
      typed_na[1:5] = NA
      typed_na %>% expect_equal(
        ., .get_conv(case)(.)
      )
    }
    
  })
}

do_test("proportion", runif(100,0,1), runif(100,-1,1), numeric())
do_test("positive_double", runif(100,0,1), runif(100,-1,1), numeric())
do_test("double", runif(100,0,1), c("not","a","number"), numeric())

do_test("integer", sample.int(20,100,replace = TRUE), runif(100,0,10), integer())
do_test("positive_integer", sample.int(20,100,replace = TRUE), sample.int(20,100,replace = TRUE)-10, integer())

do_test("in_range(10,20)", sample.int(10,100,replace = TRUE)+10, sample.int(20,100,replace = TRUE)-10, integer())

fctr = factor(c("one","two","three")[rbinom(100,3,0.2)+1], levels = c("one","two","three"))
bad_fctr = factor(c("one","two","four")[rbinom(100,3,0.2)+1], levels = c("one","two","three","four"))

do_test("enum(one,two,three)", fctr, bad_fctr, factor(levels = c("one","two","three")))
do_test("factor", fctr, list(1,"a",NA), factor())
do_test("anything", list(1,"a",NA), stop(), character())


do_test("logical", as.logical(rbinom(100,1,0.1)) , c(1,2,3), logical())

do_test("group_unique", 1:100, rep(1:10,10), character())

do_test("complete", c(1:100), c(1:10,20:100))
do_test("complete", fctr, bad_fctr)

do_test("list(character)", list(c("a","b","c"),c("d","e","f")), c(1,2,3))

# .get_conv("list(character)")(list(c("a","b","c"),c("d","e","f")))
# .get_conv("list(character)")(list(c("a","b","c"),c(1,2,3)))
# .get_conv("list(character)")(c(1,2,3))

## Check type coercion ----

do_coerce = function(case, pass_data, type) {
  test_that(sprintf("coerce %s",case), {
    tmp =  .get_conv(case)(pass_data)
    expect_equal(class(tmp)[[1]],type)
  })
}

do_coerce("factor", letters, "factor")
do_coerce("factor", c(1:100), "factor")
do_coerce("factor", iris$Species, "factor")

do_coerce("integer", as.character(1:10), "integer")
do_coerce("double", as.character(runif(100)), "numeric")

## Check list of dataframes  ----



testthat::test_that("Nested dataframe recursion tests",{

  i_df = interfacer::iface(temp = integer ~ "int col")
  df = tibble::tibble(temp = 1L)
  bad_df = tibble::tibble(fail = 1L)

  tmp = .get_conv("list(i_df)")(list(df,df,df))
  expect_identical(tmp, list(df,df,df))
  
  expect_error(
    .get_conv("list(i_df)", "test_fn", "test_param")(list(df,bad_df,df))
  )

  expect_identical(
    .get_conv("list(i_df)", "test_fn", "test_param")(NULL),
    list()
  )

})



