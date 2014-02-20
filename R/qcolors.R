#' Plot Base R colors()
#' 
#' Makes a plot of base R colors from \code{\link[grDevices]{colors}}.
#' 
#' @export
#' @author Earl F. Glynn
#' @references \url{http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.R}
#' @examples
#' qcolor()
qcolor <- function() {
    SetTextContrastColor <- function(color){
      ifelse(mean(col2rgb(color)) > 127, "black", "white")
    }

    TextContrastColor <- unlist( lapply(colors(), SetTextContrastColor) )

    colCount <- 25 
    rowCount <- 27
    plot( c(1,colCount), c(0,rowCount), type="n", ylab="", xlab="",
      axes=FALSE, ylim=c(rowCount,0))
    title("R colors")
    
    for (j in 0:(rowCount-1)) {
      base <- j*colCount
      remaining <- length(colors()) - base
      RowSize <- ifelse(remaining < colCount, remaining, colCount)
      rect((1:RowSize)-0.5,j-0.5, (1:RowSize)+0.5,j+0.5,
        border="black",
        col=colors()[base + (1:RowSize)])
      text((1:RowSize), j, paste(base + (1:RowSize)), cex=0.7,
        col=TextContrastColor[base + (1:RowSize)])
    }
}   