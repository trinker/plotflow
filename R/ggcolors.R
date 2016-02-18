#' ggplot2's Default Color Scheme
#' 
#' \code{ggcolors} - Make a palette with \pkg{ggplot2}'s default color scheme.
#' 
#' @param n An integer specifying the number of colors.
#' @return Returns a vector of \code{n} hex colors.
#' @references \url{http://stackoverflow.com/a/8197703/1000343}
#' @export
#' @rdname ggcolors
#' @author John Colby
#' @examples 
#' scales:::show_col(ggcolors(n=9))
#' 
#' n <- 10
#' ggcolors(n)
#' 
#' plot(
#'     1:n, 
#'     pch=16, 
#'     cex=2, 
#'     col= ggcolors(n)
#' )
#' 
#' barplot(
#'     stats::setNames(5:14, LETTERS[5:14]), 
#'     col = ggcolors(n), 
#'     border = ggcolors(n)
#' )
#' 
#' boxplot(
#'     x ~ y, 
#'     data = data.frame(x = rnorm(1000), 
#'     y = sample(LETTERS[5:14], 1000, TRUE)), 
#'     border = ggcolors(n),  
#'     lwd=2
#' )
#' 
#' pie(
#'     stats::setNames(5:14, LETTERS[5:14]), 
#'     col = ggcolors(n), 
#'     border = ggcolors(n)
#' )
ggcolors <- function(n) {
    hues = seq(15, 375, length=n+1)
    grDevices::hcl(h=hues, l=65, c=100)[1:n]
}



#' ggplot2's Default Color Scheme
#' 
#' \code{random_ggcolors} - Make a random n length palette with \pkg{ggplot2}'s 
#' default color scheme.
#' @export
#' @rdname ggcolors
random_ggcolors <- function(n) {
    sample(ggcolors(n))
}



