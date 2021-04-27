#' UNIBE red
#' Get the official UNIBE red colour
#' @param alpha alpha for transparency (0 to 1)
#'
#' @return colour code
#' @export
#'
#' @examples
#' plot(1:5, col = unibeRed())
unibeRed <- function(alpha = 1){
  rgb(230, 0, 46, 255*alpha, maxColorValue = 255)
}

