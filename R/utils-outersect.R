#' @export
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}