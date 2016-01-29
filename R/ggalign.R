#' Align ggplot2 Graph Edges
#' 
#' Align ggplot2 graph edges.
#' 
#' @param plot1 A ggplot2 plot.
#' @param plot2 A ggplot2 plot.
#' @return Returns a stacked grid object with left/right edges aligned.
#' @references \url{http://stackoverflow.com/a/13295880/1000343}
#' @keywords align
#' @export
#' @author Baptiste Auguie
#' @examples
#' require(ggplot2)
#' A <- ggplot(CO2, aes(x=Plant)) + geom_bar() + coord_flip() 
#' B <- ggplot(CO2, aes(x=Type)) + geom_bar() + coord_flip() 
#' ggalign(A, B)
ggalign <- function(plot1, plot2) {

     gA <- ggplot2::ggplotGrob(plot1)
     gB <- ggplot2::ggplotGrob(plot2)
     maxWidth <- grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
     gA$widths[2:5] <- as.list(maxWidth)
     gB$widths[2:5] <- as.list(maxWidth)
     out <- gridExtra::arrangeGrob(gA, gB, ncol=1)
#      if (plot) print(out)
#      return(invisible(out))
     class(out) <- c("ggalign", class(out))
     out
}

#' Prints a ggalign Object
#'
#' Prints a ggalign object.
#'
#' @param x The ggalign object.
#' @param \ldots ignored.
#' @method print ggalign
#' @export
print.ggalign <- function(x, ...){
    gridExtra::grid.arrange(x)
}










