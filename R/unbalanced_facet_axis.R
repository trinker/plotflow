#' Add Tick Marks to an Unbalanced facet_wrap
#' 
#' Adds the tick marks to an unblanced grouping of 
#' \code{\link[ggplot2]{facet_wrap}} plots.
#' 
#' @param ggplot_obj An unbalanced ggplot2 \code{\link[ggplot2]{facet_wrap}} 
#' object. 
#' @param position Either \code{"up"} (match unbalanced facet's postition) or 
#' \code{"down"} (along bottom most axis).
#' @author \href{http://stackoverflow.com/users/1320535/julius}{Julius} 
#' (stackoverflow.com) 
#' @references \url{http://stackoverflow.com/a/13316126/1000343}
#' @keywords facet_wrap axis
#' @export
#' @examples
#' set.seed(10)
#' mtcars[["new"]] <- sample(LETTERS[1:7], nrow(mtcars), TRUE)
#' 
#' library(ggplot2)
#' 
#' unbalanced_facet_axis(ggplot(mtcars, aes(x=mpg, y=hp)) +
#'     geom_line() +
#'     facet_wrap(~new, ncol=2))
#' 
#' unbalanced_facet_axis(ggplot(mtcars, aes(x=mpg, y=hp)) +
#'     geom_line() +
#'     facet_wrap(~new, ncol=3), "down")
unbalanced_facet_axis <- function(ggplot_obj, position = c("up", "down")) {

    position <- match.arg(position)
    p <- ggplot2::ggplot_build(ggplot_obj)
    gtable <- ggplot2::ggplot_gtable(p)
    dev.off()
    dims <- apply(p$panel$layout[2:3], 2, max)
    nrow <- dims[1]
    ncol <- dims[2]
    panels <- sum(grepl("panel", names(gtable$grobs)))
    space <- ncol * nrow
    n <- space - panels
    if(panels != space){
        idx <- (space - ncol - n + 1):(space - ncol)
        gtable$grobs[paste0("axis_b",idx)] <- list(gtable$grobs[[paste0("axis_b",panels)]])
        if(position == "down"){
            rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], "]"), 
                gtable$layout$name)
            lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
            gtable$layout[rows, c("t","b")] <- gtable$layout[lastAxis, c("t")]
        }
    }
    class(gtable) <- c("unbalanced_facet_axis", "gtable", "ggplot")
    gtable

}


#' Prints a unbalanced_facet_axis object.
#' 
#' Prints a unbalanced_facet_axis object.
#' 
#' @param x The unbalanced_facet_axis object
#' @param newpage logical.  If \code{TRUE} \code{\link[grid]{grid.newpage}} is called.  
#' @param viewport logical.  If character \code{\link[grid]{seekViewport}} is 
#' used.  If an object \code{\link[grid]{pushViewport}} is used.  If \code{NULL} 
#' neither viewport is used.
#' @param \ldots ignored
#' @export
#' @method print unbalanced_facet_axis
print.unbalanced_facet_axis <- function(x, newpage = is.null(viewport), viewport = NULL, ...) {
    if(newpage) grid::grid.newpage()
     if(is.null(viewport)){
        grid::grid.draw(x)
    } else {
        if (is.character(viewport)) {
            grid::seekViewport(viewport)
        } else {
            grid::pushViewport(viewport)
		}
        grid::grid.draw(x)
        grid::upViewport()
    }
    invisible(x)

}

