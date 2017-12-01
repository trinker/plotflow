#' Two Choice Horizontal Stacked Bar
#'
#' Create a two choice horizontal stacked bar chart in the style of base graphics.
#' Default color scheme makes it easy to throw into slides and posters as an
#' infographic style plot.
#'
#' @param n.yes Number yes (integer).  User must supply 2 of the first three
#' parameters (\code{n.yes}, \code{n.no}, or \code{n}).
#' @param n.no Number no (integer).
#' @param n Total number (integer).
#' @param labs  The labels to use for the two choices that correspond to
#' \code{n.yes} and \code{n.no}.
#' @param digits Digits to use in the percents.
#' @param fill The bar fill colors
#' @param color The text color.
#' @param border The border color.
#' @param size The text size.
#' @param include.percent A logical vector of 1-2 (if 1 the second will be
#' \code{FALSE}) stating if percents should be included.  The length of 2
#' corresponds to the 2 choices; \code{n.yes} and \code{n.no}.  If a single value
#' is given it is recycled.
#' @return Returns a \code{ggplot} object.
#' @keywords bar infographic
#' @importFrom utils head
#' @export
#' @examples
#' yes_no_bar(111, 66)
#' yes_no_bar(111, 66, fill=c("pink", "ivory"), color="purple")
#' yes_no_bar(55, n = 166, labs = c("True", "False"))
#' yes_no_bar(2345, 3456, labs = c("Boy", "Girl"), fill=c("lightblue", "pink"),
#'     color="grey50", include.percent = TRUE)
#' yes_no_bar(2345, 3456, labs = c("Boy", "Girl"), fill=c("lightblue", "pink"),
#'     color="grey50", include.percent = FALSE)
#' yes_no_bar(9999, n = 22166, fill=c("green", "pink"),
#'     labs = c("Go:", "Stop"), color = "grey30", digits=0)
yes_no_bar <- function(n.yes, n.no = NULL, n = NULL, labs = c("Yes", "No"),
   digits = 1, fill = c("#51A651", "white"), color = "#CAE4CA",
   border = "grey70", size = 10, include.percent = c(TRUE, FALSE)){

    prop <- diffs <- aes <- pos <- percent <- NULL

    stopifnot(sum(c(!is.null(n.yes), !is.null(n), !is.null(n.no))) > 1)

    if (!is.null(n)) {
        if (!is.null(n.yes)) {
            n.no <- n - n.yes
        } else {
            n.yes <- n - n.no
        }
    }

    if (length(include.percent) == 1) include.percent[2] <- include.percent[1]

    bar <- dplyr::data_frame(
        tagged = factor(labs, levels = labs[2:1]),
        n = c(n.yes, n.no),
        prop = n/sum(c(n.yes, n.no)),
        diffs = diff(c(0, cumsum(prop))),
        pos = head(c(0, cumsum(prop)), -1) + (0.5 * diffs),
        percent = ifelse(include.percent, paste0("  ", pp(prop*100, digits)), "")
    )

    ggplot2::ggplot(bar, ggplot2::aes_string(1, fill ='tagged', y = 'prop')) +
        ggplot2::geom_bar(stat='identity', color=border, size=2) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(label=scales::percent) +
        ggplot2::scale_fill_manual(values=fill[2:1]) +
        ggplot2::geom_text(ggplot2::aes(y =pos, label=ifelse(c(TRUE, FALSE),
             paste0(sprintf("%s", bar[[1]][1]), percent, "\n",  pc(n)),
             paste0(sprintf("%s", bar[[1]][2]),  percent, "\n", pc(n))
        )), size=size, color=color) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank()
        ) +
        ggplot2::guides(fill=FALSE) +
        ggplot2::labs(x=NULL, y=NULL)
}
