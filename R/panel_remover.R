#' Remove Specific strip.background
#' 
#' Remove a specific strip.background from a ggplot2 
#' \code{\link[ggplot2]{facet_grid}} object.
#' 
#' @param ggplot_obj A ggplot2 object with strip.background (produced from 
#' \code{\link[ggplot2]{facet_grid}}).
#' @param y logical.  If \code{TRUE} the y axis strip.background is removed.  
#' If \code{FALSE} the x axis strip.background  is removed.
#' @return Returns a ggplot2 object with specific strip.background 
#' removed.
#' @references \url{http://stackoverflow.com/a/19064621/1000343}
#' @keywords strip.background
#' @author Roland (stackoverflow.com) and Tyler Rinker<tyler.rinker@@gmail.com>
#' @export
#' @import gridExtra
#' @importFrom grid grob
#' @importFrom ggplot2 ggplotGrob
#' @examples
#' a <- ggplot(mtcars, aes(mpg, hp)) +
#'   geom_point() +
#'   facet_grid(cyl~gear) 
#' 
#' panel_remover(a)
#' panel_remover(a, FALSE)
panel_remover <- function(ggplot_obj, y = TRUE) {

  g <- ggplotGrob(ggplot_obj)
  what <- ifelse(y, "y", "x")

    g$grobs <- lapply(g$grob, function(gr) {
        if (any(grepl(paste0("strip.text.", what), names(gr$children)))) {
            gr$children[[grep("strip.background", names(gr$children))]] <- zeroGrob()
            gr$children[[grep("strip.text", names(gr$children))]] <- zeroGrob()
        }
        return(gr)
    })
    class(g) <- unique( c("arrange", "ggplot", class(g), class(ggplot_obj)) )
    g
}

zeroGrob <- function() {
    g0 <- grob(name="NULL")
    class(g0) <- c("zeroGrob",class(g0))
    g0
}