#' Colour specific text in a html document
#' @param colour colour to use for the content
#' @param ... content to be coloured
#' @details
#' In quarto/Rmarkdown documents, use the function in an inline R chunk
#' (\code{`r coloured_text(...)`}) or in an R chunk with the result set to \code{asis}
#'
#' @importFrom htmltools tags
#' @export
#' @examples
#' coloured_text("red", "This text should be red")
#' # also works with multiple contents
#' coloured_text("red", "This text should be red", "this one too")
#' # For multiple colours, use multiple calls to the function:
#' coloured_text("red", "this is red,")
#' coloured_text("blue", "and this is blue!")
coloured_text <- function(colour, ...){
  tags$span(style = paste0("color:", colour, "; "), ...)
}



