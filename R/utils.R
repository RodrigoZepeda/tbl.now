#' Function to get an attribute from an object or default value
#'
#' Checks if attribute exists in object and returns `default` if not
#'
#' @param x An object with attribute `name`
#' @param name The name of the attribute in `x`
#' @param default (optional) The default value of attribute `name` in `x`
#'
#' @examples
#' \dontrun{
#' #Create an object
#' x <- 42
#' attr(x, "meaning") <- "meaning of life"
#'
#' #See the attributes
#' attributes(x)
#'
#' #Return the meaning attribute
#' attr_default(x, "meaning")
#'
#' #Return null for attribute that doesn't exist
#' attr_default(x, "DOES_NOT_EXIST")
#'
#' #Or return default when it doesn't exist
#' attr_default(x, "DOES_NOT_EXIST", default = 15)
#'
#' }
#'
#' @keywords internal
attr_default <- function(x, name, default = NULL) {
  val <- attr(x, name, exact = TRUE)
  if (is.null(val)) default else val
}


