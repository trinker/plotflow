#' Plot Base R Symbols 
#' 
#' Makes a plot of base R symbols.
#' 
#' @export
#' @examples
#' qcolor
qsymbols <- function(){
    plot(x=rep(5:1, 5), y=rep(1:5, each=5), pch=25:1, 
        ylim=c(1, 5.25), xlab="", ylab="",
        main="Symbols (pch)", axes=FALSE)
    text(25:1, x=rep(5:1, 5), y=rep(1:5, each=5)+.2, cex=.8)
    box()
}