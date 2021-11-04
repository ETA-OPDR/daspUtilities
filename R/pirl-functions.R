#' Replace PIRL element DF column names with friendly Names
#'
#' @description \code{pirl_name_replace} takes a df with column names that are listed
#' as pirl elements and replaces them with friendly names
#' @export
pirl_name_replace <- function(df) {
  pirl_names <- str_replace(names(df), 'PIRL', 'p')
  pirl_names <- str_replace_all(pirl_names, ' ', "")
  ordered_colnames <-pirl_key$Variable_Name[match(pirl_names,pirl_key$R_Variable)]
  colnames(df) <- ordered_colnames
}
