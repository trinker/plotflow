#' Align ggplot2 Graph Edges
#' 
#' Align ggplot2 graph edges.
#' 
#' @param plot1 A ggplot2 plot.
#' @param plot2 A ggplot2 plot.
#' @param plot logical.  If \code{TRUE} the plot will automatically plot.  
#' The user may wish to set to \code{FALSE} for use in knitr, sweave, etc.
#' to add additional plot layers.
#' @return Returns a stacked grid object with left/right edges aligned.
#' @references \url{http://stackoverflow.com/a/13295880/1000343}
#' @keywords align
#' @export
#' @author Baptiste Auguie
#' @importFrom grid unit.pmax
#' @importFrom ggplot2 ggplotGrob 
#' @import gridExtra
#' @examples
#' require(ggplot2)
#' A <- ggplot(CO2, aes(x=Plant)) + geom_bar() + coord_flip() 
#' B <- ggplot(CO2, aes(x=Type)) + geom_bar() + coord_flip() 
#' ggalign(A, B)
ggalign <- function(plot1, plot2, plot = TRUE) {

     gA <- ggplotGrob(plot1)
     gB <- ggplotGrob(plot2)
     maxWidth <- unit.pmax(gA$widths[2:5], gB$widths[2:5])
     gA$widths[2:5] <- as.list(maxWidth)
     gB$widths[2:5] <- as.list(maxWidth)
     out <- arrangeGrob(gA, gB, ncol=1)
     if (plot) print(out)
     return(invisible(out))

}

