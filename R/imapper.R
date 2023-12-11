#' Specify mappings that can make dataframes compatible with an interface
#' 
#' This function is expected to be used only in a `.imap = imappper(...)` context
#' to overcome some mapping issues 
#'
#' @param ... a set of `dplyr::mutate()` specifications that when applied to
#' a dataframe will rename or otherwise fix missing columns
#'
#' @return a set of mappings
#' @export
#'
#' @concept interface 
#'
#' @examples
#' x = function(df = iface(col1 = integer ~ "an integer column" ), ...) {
#'   df = ivalidate(df,...)
#' }
#' input=tibble::tibble(col2 = c(1,2,3)) 
#' # This fails because col1 is missing
#' try(x(input))
#' # This fixes it for this input
#' x(input, .imap=imapper(col1 = col2))
imapper = function(...) {
  tmp = rlang::enexprs(...)
  return(structure(
    tmp,
    class = c("imapper",class(tmp))
  ))
}