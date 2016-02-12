#' Globally Set ggplot2 Colors/Fills
#' 
#' Globally set the color and fill of ggplot2.
#' 
#' @param color A single color character string.
#' @param alter.fill logical.  If \code{TRUE} fill is globally altered.
#' @param alter.color logical.  If \code{TRUE} colour is globally altered.
#' @param reset logical.  If colour and fill are reset to black before applying 
#' new color.
#' @references \url{http://stackoverflow.com/a/21175042/1000343}
#' @export
#' @seealso \code{\link[ggplot2]{update_geom_defaults}}
#' @examples
#' ggplot_global_set_color("purple")
#' 
#' ggplot(mtcars, aes(factor(cyl))) + geom_bar()
#' ggplot(mtcars, aes(hp, mpg, group = factor(cyl))) + geom_point()
ggplot_global_set_color <- function(color = "black", alter.fill = TRUE, alter.color = TRUE, reset = TRUE) {
    
    params <- ls(pattern = 'Geom[A-Z]', envir = as.environment('package:ggplot2'))
    geoms <- gsub("Geom", "", params)
# 
#     geoms
    
    if (reset) {
        invisible(lapply(geoms, ggplot2::update_geom_defaults, list(fill = "black", 
        	colour = "black")))
    }
    if (alter.color && !alter.fill) {
        invisible(lapply(geoms, ggplot2::update_geom_defaults, list(colour = color)))
    } else {
        if (alter.color && alter.fill) {
            invisible(lapply(geoms, ggplot2::update_geom_defaults, list(fill = color, 
            	colour = color)))
        } else {
            if (!alter.color && alter.fill) {
                invisible(lapply(geoms, ggplot2::update_geom_defaults, list(fill = color)))
            } else {
                stop("better rethink why you're using this function")
            }
        }
    }
}
