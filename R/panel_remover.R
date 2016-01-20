#' Remove Specific strip.background
#' 
#' Remove a specific strip.background from a ggplot2 
#' \code{\link[ggplot2]{facet_grid}} object.
#' 
#' @param ggplot_obj A ggplot2 object with strip.background (produced from 
#' \code{\link[ggplot2]{facet_grid}}).
#' @param y logical.  If \code{TRUE} the y axis strip.background is removed.  
#' If \code{FALSE} the x axis strip.background is removed.
#' @return Returns a ggplot2 object with specific 
#' \href{http://docs.ggplot2.org/current/theme.html}{strip.background}.
#' removed.
#' @references \url{http://stackoverflow.com/a/19067970/1000343}
#' @keywords strip.background
#' @author Baptiste Auguie and Tyler Rinker<tyler.rinker@@gmail.com>
#' @export
#' @import gridExtra
#' @importFrom grid grob
#' @importFrom ggplot2 ggplotGrob
#' @seealso \code{\link[ggplot2]{theme}}
#' @examples
#' a <- ggplot(mtcars, aes(mpg, hp)) +
#'   geom_point() +
#'   facet_grid(cyl~gear) 
#' 
#' panel_remover(a)
#' panel_remover(a, FALSE)
panel_remover <- function(ggplot_obj, y = TRUE) {

    g <- ggplot2::ggplotGrob(ggplot_obj)
    keep <- !grepl(sprintf("strip-%s", ifelse(y, 'right', 'top')), g$layout$name)
    g$grobs <- g$grobs[keep]
    g$layout <- g$layout[keep, ]
    out <- gridExtra::arrangeGrob(g)

    class(out) <- c("panel_remover", class(out))
    out
}


#' Plots a panel_remover Object
#'
#' Plots a panel_remover object.
#'
#' @param x The \code{panel_remover} object.
#' @param \ldots Other arguments passed to \code{\link[gridExtra]{grid.arrange}}.
#' @method plot panel_remover
#' @export
plot.panel_remover <- function(x, ...){
    gridExtra::grid.arrange(x, ...)
}

#' Prints a panel_remover Object
#'
#' Prints a panel_remover object.
#'
#' @param x The \code{panel_remover} object.
#' @param \ldots Other arguments passed to \code{\link[gridExtra]{grid.arrange}}.
#' @method print panel_remover
#' @export
print.panel_remover <- function(x, ...){
    graphics::plot(x, ...)
}

