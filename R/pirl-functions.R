#'
#' @export
pirl_replace <- function(df) {
  pirl_names <- str_replace(names(df), 'PIRL', 'p')
  pirl_names <- str_replace_all(pirl_names, ' ', "")
  ordered_colnames <-pirl_key$Variable_Name[match(pirl_names,pirl_key$R_Variable)]
  colnames(df) <- ordered_colnames
}
