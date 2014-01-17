#' Global Set og ggplot2 Colors
#' 
#' Globally set the color and fill of ggplot2.
#' 
#' @param col A single color character string.
#' @param fill logical.  If \code{TRUE} fill is globally altered.
#' @param colour logical.  If \code{TRUE} colour is globally altered.
#' @param reset logical.  If colour and fill are reset to black before applyin new color.
#' @references http://stackoverflow.com/a/21175042/1000343
#' @export
#' @seealso \code{\link[ggplot2]{update_geom_defaults}}
#' @examples
#' ggplot_col("purple")
#' 
#' ggplot(mtcars, aes(factor(cyl))) + geom_bar()
#' ggplot(mtcars, aes(hp, mpg, group = factor(cyl))) + geom_point()
ggplot_col <- function(col = "black", fill = TRUE, colour = TRUE, reset = TRUE) {
    params <- ls(pattern = '^geom_', envir = as.environment('package:ggplot2'))
    geoms <- gsub("geom_", "", params)

    if (reset) {
        invisible(lapply(geoms, update_geom_defaults, list(fill = "black", 
        	colour = "black")))
    }
    if (colour && !fill) {
        invisible(lapply(geoms, update_geom_defaults, list(colour = col)))
    } else {
        if (colour && fill) {
            invisible(lapply(geoms, update_geom_defaults, list(fill = col, 
            	colour = col)))
        } else {
            if (!colour && fill) {
                invisible(lapply(geoms, update_geom_defaults, list(fill = col)))
            } else {
                stop("better rethink why you're using this function")
            }
        }
    }
}
