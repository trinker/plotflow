#' Quickly Set ggplot2 panel.grid 
#' 
#' Set ggplot2's panel.grid quickly.
#' 
#' @param xmaj A colour choice for panel.grid.major.x.  Use \code{NULL} to 
#' remove.
#' @param xmin A colour choice for panel.minor.x.  Use \code{NULL} to remove.
#' @param ymaj A colour choice for panel.grid.major.y.  Use \code{NULL} to 
#' remove.
#' @param ymin A colour choice for panel.minor.y.  Use \code{NULL} to remove.
#' @param x A colour choice.  Sets both panel.grid.major.x and panel.minor.x.  
#' Overides \code{xmin} and \code{xmax}.  Use \code{NULL} to remove.
#' @param y A colour choice.  Sets both panel.grid.major.y and panel.minor.y.  
#' Overides \code{ymin} and \code{ymax}.  Use \code{NULL} to remove.
#' @importFrom ggplot2 theme element_blank element_line
#' @keywords gridelines
#' @export
#' @seealso \code{\link[ggplot2]{theme}}
#' @examples
#' \dontrun{
#' ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) +
#'     geom_bar()  +
#'     theme_apa() +
#'     y0(cushion(as.factor(mtcars$cyl))) +
#'     xlab("Cylinders") +
#'     ylab("Total") + qgrid()
#' 
#' ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) +
#'     geom_bar()  +
#'     theme_apa() +
#'     y0(cushion(as.factor(mtcars$cyl))) +
#'     xlab("Cylinders") +
#'     ylab("Total") + qgrid(x=NULL)
#' 
#' ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) +
#'     geom_bar()  +
#'     theme_apa() +
#'     y0(cushion(as.factor(mtcars$cyl))) +
#'     xlab("Cylinders") +
#'     ylab("Total") + qgrid(x=NULL, y="red")
#' }
qgrid <- function(xmaj="grey94", xmin="grey94", ymaj="grey94", ymin="grey94", x, y) {

    if (!missing(x)) {
        xmaj <- xmin <- x
    }

    if (!missing(y)) {
        ymaj <- ymin <- y
    }

    funs <- lapply(list(xmaj, xmin, ymaj, ymin), function(x) {
        if (is.null(x)) {
            element_blank()
        } else {
            element_line(colour = x)
        }
    })

    theme(
        panel.grid.major.x = funs[[1]],
        panel.grid.minor.x = funs[[2]],
        panel.grid.major.y = funs[[3]],
        panel.grid.minor.y = funs[[4]]
    )

}