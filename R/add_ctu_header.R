#' Insert CTU standard header to the active document
#' This function is not intended to be used in any scripts.
#' @return adds CTU header to the active document
#' @export
add_ctu_header <- function(){
rstudioapi::insertText(c(1,1),
glue::glue(
"
# /* ************************************************** */
# /* Study: projnum projname                            */
# /* Purpose: purpose                                   */
# /* Author: author                                     */
# /* Date created: {Sys.Date()}                           */
# /* Last update:                                       */
# /* Date of database export: date                      */
# /* Underlying SAP version:                            */
# /* Underlying Study Protocol version:                 */
# /* ****************************************************/"))
}
