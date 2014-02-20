#' Rename the Margins of ggplot2 facet_grid
#' 
#' Convert the default (all) when seeting ggplot2's 
#' \code{\link[ggplot2]{facet_grid}} to \code{margins = TRUE}. 
#' 
#' @param newname The new strip.text name to overwrite (all).
#' @references \url{http://permalink.gmane.org/gmane.comp.lang.r.ggplot2/5279}
#' @keywords margin
#' @export
#' @examples
#' ggplot(mtcars, aes(cyl)) +  
#'     geom_point(stat="bin", size = 2,  
#'         aes(shape = gear, position = "stack")) +  
#'     facet_grid(carb ~ gear, margins = TRUE,  
#'         labeller=label_rename_margin("Total")) 
#' 
#' ggplot(mtcars, aes(cyl)) +  
#'     geom_point(stat="bin", size = 2,  
#'         aes(shape = gear, position = "stack")) +  
#'     facet_grid(carb ~ gear, margins = "gear",  
#'         labeller=label_rename_margin("Total")) 
label_rename_margin <- function(newname="Total") { 
    function(variable, value) { 
        value <- as.character(value) 
        value[value == "(all)"] <- newname 
        value 
    } 
} 
 