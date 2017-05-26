#' Substring of a Character Vector From the Right
#'
#' Extract substrings starting from the right.
#' @param x Character; Vector
#' @param n Integer; Number of characters to keep from the right. Defaults to 1.
#' @export
#' @return List of filenames for zoo objects.
#' @examples
#' substrRight()

substrRight <- function(x, n = 1){
  substr(x, nchar(x)-n+1, nchar(x))
}