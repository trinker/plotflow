#' Add Tick Marks to an Unbalanced facet_wrap
#'
#' Adds the tick marks to an unbalanced grouping of
#' \code{\link[ggplot2]{facet_wrap}} plots.
#'
#' @param ggplot_obj An unbalanced ggplot2 \code{\link[ggplot2]{facet_wrap}}
#' object.
#' @param position Either \code{"up"} (match unbalanced facet's position,
#' as is the default of ggplot2) or \code{"down"} (along bottom most axis).
#' @author Original by \href{http://stackoverflow.com/users/1320535/julius}{Julius}
#' (stackoverflow.com). Updated by Mikko Korpela for ggplot2 >= 2.2.0.
#' @references \url{http://stackoverflow.com/a/13316126/1000343}
#' @keywords facet_wrap axis
#' @importFrom ggplot2 ggplot_build ggplot_gtable
#' @export
#' @examples
#' set.seed(2)
#' mtcars2 <- mtcars
#' mtcars2[["new"]] <- sample(LETTERS[1:7], nrow(mtcars), TRUE)
#'
#' library(ggplot2)
#'
#' unbalanced_facet_axis(ggplot(mtcars2, aes(x=mpg, y=hp)) +
#'     geom_line() +
#'     facet_wrap(~new, ncol=2))
#'
#' unbalanced_facet_axis(ggplot(mtcars2, aes(x=mpg, y=hp)) +
#'     geom_line() +
#'     facet_wrap(~new, ncol=3), "down")
unbalanced_facet_axis <- function(ggplot_obj, position = c("up", "down")) {
    pos <- match.arg(position)
    gb <- ggplot_build(ggplot_obj)
    gt <- ggplot_gtable(gb)
    ## Nothing done for position == "up" (nowadays default in ggplot2)
    if (pos == "down") {
        layout <- gt$layout
        layout_names <- layout$name
        grob_names <- vapply(gt$grobs, `[[`, "", "name")
        idx_axb <- which(grepl("^axis-b", layout_names) & grob_names != "NULL")
        t_axb <- layout$t[idx_axb]
        b_axb <- layout$b[idx_axb]
        max_t <- max(t_axb)
        max_b <- max(b_axb)
        layout$t[idx_axb] <- max_t
        layout$b[idx_axb] <- max_b
        gt$layout <- layout
    }
    class(gt) <- c("unbalanced_facet_axis", class(gt))
    gt
}

#' Plots an unbalanced_facet_axis object.
#'
#' Plots an unbalanced_facet_axis object.
#'
#' @param x The unbalanced_facet_axis object
#' @param newpage logical.  If \code{TRUE} \code{\link[grid]{grid.newpage}} is called.
#' @param viewport logical.  If character \code{\link[grid]{seekViewport}} is
#' used.  If an object \code{\link[grid]{pushViewport}} is used.  If \code{NULL}
#' neither viewport is used.
#' @param \ldots ignored
#' @export
#' @method plot unbalanced_facet_axis
plot.unbalanced_facet_axis <- function(x, newpage = is.null(viewport), viewport = NULL, ...) {
    NextMethod("plot", NULL, newpage = newpage, vp = viewport, ...)
}

#' Prints an unbalanced_facet_axis object.
#'
#' Calls \code{\link{plot}} with the same arguments.
#'
#' @param x The unbalanced_facet_axis object
#' @param \ldots arguments passed to the plot method
#' @export
#' @method print unbalanced_facet_axis
print.unbalanced_facet_axis <- function(x, ...) {
    plot(x, ...)
}
