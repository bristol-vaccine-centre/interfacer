#' Generate a zero length dataframe conforming to an `iface` specification
#' 
#' This function is used internally for default values for a dataframe
#' parameter. It generates a zero length dataframe that conforms to a `iface`
#' specification, in terms of column names, data types and groupings. Such a
#' dataframe is not guaranteed to be fully conformant to the `iface`
#' specification if, for example, completeness constraints are applied.
#'
#' @param iface the specification
#'
#' @return a dataframe conforming to `iface`
#' @export
#'
#' @concept interface 
#'
#' @examples
#' i = interfacer::iface(
#'   col1 = integer ~ "A number",
#'   col2 = character ~ "A string"
#' )
#' 
#' iproto(i)
iproto = function(iface) {
  out = tibble::tibble()
  for (name in .spec_cols(iface)) {
    type = .spec_type_of(iface,name)
    out = out %>% dplyr::mutate(!!name := .get_conv(type)(NULL))
  }
  out = out %>% dplyr::group_by(!!!.spec_grps(iface,sym=TRUE))
  return(out)
}