#' Plot ggplot2/base R Symbols
#'
#' Makes a plot of ggplot2/base R symbols.
#'
#' @param ggplot2 logical.  If \code{TRUE}
#' \href{http://docs.ggplot2.org/current/}{ggplot2} symbols will be plotted.  If
#' \code{FALSE} base graphics symbols will be plotted.
#' @importFrom graphics box plot text
#' @export
#' @examples
#' qsymbol()
qsymbol <- function(ggplot2 = TRUE){

	x <- y <- symb <- NULL

    if (!ggplot2) {
        plot(x=rep(5:1, 5), y=rep(1:5, each=5), pch=25:1,
            ylim=c(1, 5.25), xlab="", ylab="",
            main="Base Symbols (pch)", axes=FALSE)
        text(25:1, x=rep(5:1, 5), y=rep(1:5, each=5)+.2, cex=.8)
        box()
    } else {
        dat <- data.frame(y = rep(5:1, , each = 5), x = rep(1:5, 5), symb=1:25)

        ggplot2::ggplot(dat, ggplot2::aes(x=x, y=y)) +
            ggplot2::geom_text(ggplot2::aes(label=symb), size=3.25, vjust=2.25) +
            ggplot2::geom_point(ggplot2::aes(shape = symb), size = 5,
                colour = "red", fill = "black") +
            ggplot2::scale_shape_identity() +
            ggplot2::ylab("") + ggplot2::xlab("") +
            ggplot2::theme(axis.text=ggplot2::element_blank(),
                axis.ticks=ggplot2::element_blank()) +
            ggplot2::ggtitle("ggplot2 Symbols (shape)")
    }

}
