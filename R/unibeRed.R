#' UNIBE red
#' Get the official UNIBE red colour. Note that the \code{unibeCols} package has all of the UNIBE colours.
#' @param alpha alpha for transparency (0 to 1)
#'
#' @return hexidecimal colour code
#' @export
#' @importFrom grDevices rgb
#' @seealso [unibeCols::unibeRed()]
#' @examples
#' plot(1:5, col = unibeRed())
unibeRed <- function(alpha = 1){
  rgb(230, 0, 46, 255*alpha, maxColorValue = 255)
}

