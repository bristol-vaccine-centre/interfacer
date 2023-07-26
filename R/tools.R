
# TODO: convert a dataframe to an interface spec dynamically
# rationale is that there is something you have to conform the output to or 
# input as
# as.iface.dataframe

# TODO: nested list columns and recursion.

#' Create an `iface` interface from an example dataframe
#'
#' Copies to the clipboard
#'
#' @param df a prototype dataframe
#'
#' @return nothing
#' @export
#'
#' @examples
#' if(FALSE) iclip(iris)
iclip = function(df) {
  names = colnames(df)
  types = unname(sapply(df,class))
  types = ifelse(types %in% names(.conv), types, paste0("as.",types))
  docs = sprintf("a %s column",names)
  groups = .none(dplyr::group_vars(df),collapse = " + ",fmt = ".groups = ~ %s",none = ".groups = FALSE")
  tmp = sprintf("interfacer::iface(\n\t%s\n)",
    paste0(
      c(
        sprintf("%s = %s ~ \"%s\"", names, types, docs),
        groups
      ),
      collapse = ",\n\t"
    )
  )
  clipr::write_clip(tmp)
}
