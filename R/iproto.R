#' Generate a zero length tibble conforming to a spec
#'
#' @param iface the specification
#'
#' @return a tibble conforming to `iface`
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