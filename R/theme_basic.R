#' ggplot2 Theme with No Background or Gridlines. 
#' 
#' A ggplot2 theme with no background and no gridlines.
#' 
#' @param x logical.  If \code{TRUE} vertical gridlines are added.
#' @param y logical.  If \code{TRUE} horizontal gridlines are added.
#' @param plot.box logical.  If \code{TRUE} a full box surrounds the plot area.  If \code{FALSE} only the x and y axis are shown.
#' @note Both x and y can not be \code{TRUE}.  Use 
#' \code{\link[ggplot2]{theme_bw}} instead.
#' @export
#' @seealso \code{\link[ggplot2]{theme}}
#' @importFrom ggplot2 theme_bw theme element_blank element_line element_rect
#' @examples
#' ggplot(mtcars, aes(factor(cyl))) + geom_bar() + theme_basic()
#' ggplot(mtcars, aes(factor(cyl))) + geom_bar() + theme_basic(x = TRUE)
#' ggplot(mtcars, aes(factor(cyl))) + geom_bar() + theme_basic(y = TRUE)
#' ggplot(mtcars, aes(factor(cyl))) + geom_bar() + theme_basic(x = TRUE, y = TRUE)
theme_basic <- function(x = FALSE, y = FALSE, plot.box = FALSE) {
    a <- theme_bw() 
    if (!x && !y) {
        b <- a + theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    } else {
        if (!x && y) {
            b <- a + theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank())
        } else {
            if (x && !y) {
                b <- a + theme(panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank())
            } else {
                return(message("`x` and `y` set to TRUE; use `theme_bw()` instead"))
            }    
        }
    }
    if (!plot.box) {
        b <- b + theme(panel.background = element_rect(fill = "white", 
                colour = "black"), panel.border = element_rect(fill = NA, 
                colour = "white"), axis.line = element_line()) 
    } else {
        b <- b + theme(panel.background = element_rect(fill = "white", 
                colour = "white"), panel.border = element_rect(fill = NA, 
                colour = "grey50")) 
    }
    b
}
