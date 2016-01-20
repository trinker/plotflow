#' Dual Y-axis for ggplot2
#'
#' lot dual y-axis for ggplot2 objects.
#'
#' @param lhs A plot whose y axis shall be on the left hand side.
#' @param rhs A plot whose y axis shall be on the right hand side.
#' @param angle Angle to rotate y-axis on right hand side.
#' @return Returns an \code{\link[gridExtra]{arrangeGrob}} with extra class
#' \code{ggdual_axis} that plots by default.  This allows it to be further
#' combined with other grobs via \code{\link[gridExtra]{grid.arrange}}.
#' @references \url{http://stackoverflow.com/a/27608585/1000343}
#' \url{http://stackoverflow.com/a/25699817/1000343}
#' @export
#' @examples
#' p1 <- ggplot(mtcars, aes(mpg, disp)) +
#'     geom_line(colour = "blue") +
#'     theme_bw() +
#'     theme(plot.margin = grid::unit(c(.5, 1, .5, 0), "cm"))
#'
#' p2 <- ggplot(mtcars, aes(mpg, drat)) +
#'     geom_line(colour = "red") +
#'     theme_bw() +
#'     theme(plot.margin = grid::unit(c(.5, 1, .5, 0), "cm"))
#'
#' ggdual_axis(lhs = p1, rhs = p2)
ggdual_axis <- function(lhs, rhs, angle = 270) {
    # 1. Fix the right y-axis label justification
    
    r <- name <- NULL
    rhs <- rhs + ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = 0))
    # 2. Rotate the right y-axis label by 270 degrees by default
    rhs <- rhs + ggplot2::theme(axis.title.y = ggplot2::element_text(angle = angle))

    # 3a. Use only major grid lines for the left axis
    lhs <- lhs + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    # 3b. Use only major grid lines for the right axis
    #     force transparency of the backgrounds to allow grid lines to show
    rhs <- rhs + ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        plot.background = ggplot2::element_rect(fill = "transparent", colour = NA))
    # Process gtable objects
    # 4. Extract gtable

    g1 <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(lhs))
    g2 <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(rhs))
    # 5. Overlap the panel of the rhs plot on that of the lhs plot
    pp <- c(subset(g1$layout, name == "panel", se = t:r))
    g <- gtable::gtable_add_grob(g1,
        g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
    # Tweak axis position and labels
    ia <- which(g2$layout$name == "axis-l")
    ga <- g2$grobs[[ia]]
    ax <- ga$children[["axis"]]  # ga$children[[2]]
    ax$widths <- rev(ax$widths)
    ax$grobs <- rev(ax$grobs)
    ax$grobs[[1]]$x <- ax$grobs[[1]]$x - grid::unit(1, "npc") + grid::unit(0.15, "cm")
    g <- gtable::gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g <- gtable::gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
    g <- gtable::gtable_add_grob(g, g2$grobs[[7]], pp$t, length(g$widths), pp$b)
    # Display plot with arrangeGrob wrapper arrangeGrob(g)

    out <- gridExtra::arrangeGrob(g)
    class(out) <- c("ggdual_axis", class(out))
    out
}

#' Plots a ggdual_axis Object
#'
#' Plots a ggdual_axis object.
#'
#' @param x The \code{ggdual_axis} object.
#' @param \ldots Other arguments passed to \code{\link[gridExtra]{grid.arrange}}.
#' @method plot ggdual_axis
#' @export
plot.ggdual_axis <- function(x, ...){
    gridExtra::grid.arrange(x, ...)
}

#' Prints a ggdual_axis Object
#'
#' Prints a ggdual_axis object.
#'
#' @param x The \code{ggdual_axis} object.
#' @param \ldots Other arguments passed to \code{\link[gridExtra]{grid.arrange}}.
#' @method print ggdual_axis
#' @export
print.ggdual_axis <- function(x, ...){
    graphics::plot(x, ...)
}

