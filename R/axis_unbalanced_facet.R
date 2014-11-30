#' Add Tick Marks to an Unbalanced facet_wrap
#' 
#' Adds the tick marks to an unblanced grouping of 
#' \code{\link[ggplot2]{facet_wrap}} plots.
#' 
#' @param ggplot_obj An unbalanced ggplot2 \code{\link[ggplot2]{facet_wrap}} 
#' object. 
#' @param position Either \code{"up"} (match unbalanced facet's postition) or 
#' \code{"down"} (along bottom most axis).
#' @param viewpage logical.  If \code{TRUE} \code{\link[grid]{grid.newpage}} is
#' used.
#' @param vp logical.  If \code{TRUE} a viewport is used.
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
unbalanced_facet_axis <- function(ggplot_obj, position = c("up", "down"),
    newpage = is.null(vp), vp = NULL) {

    ggplot2:::set_last_plot(ggplot_obj)
    if(newpage) grid:::grid.newpage()
    position <- match.arg(position)
    p <- ggplot2::ggplot_build(ggplot_obj)
    gtable <- ggplot2::ggplot_gtable(p)
 
    dims <- apply(p$panel$layout[2:3], 2, max)
    nrow <- dims[1]
    ncol <- dims[2]
    
    # number of panels in the plot
    panels <- sum(grepl("panel", names(gtable$grobs)))
    space <- ncol * nrow
    
    # missing panels
    n <- space - panels
    
    # checking whether modifications are needed
    if(panels != space){
         # indices of panels to fix
        idx <- (space - ncol - n + 1):(space - ncol)
        # copying x-axis of the last existing panel to the chosen panels 
        # in the row above
        gtable$grobs[paste0("axis_b",idx)] <- list(gtable$grobs[[paste0("axis_b",
            panels)]])
        if(position == "down"){
            # if position == down then shifting labels down to the same level as 
            # the x-axis of last panel
            rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], "]"), 
                gtable$layout$name)
            lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
            gtable$layout[rows, c("t","b")] <- gtable$layout[lastAxis, c("t")]
        }
    }
    # again part of print.ggplot, plotting adjusted version
    if(is.null(vp)){
        grid::grid.draw(gtable)
    } else {
        if (is.character(vp)) {
            grid::gseekViewport(vp)
        } else {
            grid::gpushViewport(vp)
        }
        grid::grid.draw(gtable)
        grid::gupViewport()
    }
    invisible(p)
}
