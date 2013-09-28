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
#' @references \url{http://stackoverflow.com/a/19064688/1000343}
#' @keywords strip.background
#' @author Amine Gassem and Tyler Rinker<tyler.rinker@@gmail.com>
#' @export
#' @importFrom grid editGrob getGrob gpar grid.draw
#' @importFrom ggplot2 ggplotGrob 
#' @examples
#' a <- ggplot(mtcars, aes(mpg, hp)) +
#'   geom_point() +
#'   facet_grid(cyl~gear) 
#' 
#' panel_remover(a, FALSE)
panel_remover <- function(ggplot_obj, y = TRUE) {

    g <- ggplotGrob(ggplot_obj)
    gg <- g$grobs
    loc <- ifelse(y, "right", "top")
    strip_index <- which(grepl(sprintf("strip-%s", loc), g$layout$name))
    for(i in strip_index)
      gg[[i]] <- editGrob(getGrob(gg[[i]], "strip.back",
          grep=TRUE, global=TRUE), gp = gpar(fill=NA)
    )
    
    g$grobs <- gg
    grid.draw(g)

}