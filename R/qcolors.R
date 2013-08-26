#' Plot Base R colors()
#' 
#' Makes a plot of base R colors from \code{\link[grDevices]{colors}}.
#' 
#' @export
#' @examples
#' qcolor()
qcolor <- function() {
    SetTextContrastColor <- function(color)
    {
      ifelse( mean(col2rgb(color)) > 127, "black", "white")
    }
    # Define this array of text contrast colors that correponds to each
    # member of the colors() array.
    TextContrastColor <- unlist( lapply(colors(), SetTextContrastColor) )
    # 1a. Plot matrix of R colors, in index order, 25 per row.
    # This example plots each row of rectangles one at a time.
    colCount <- 25 # number per row
    rowCount <- 27
    plot( c(1,colCount), c(0,rowCount), type="n", ylab="", xlab="",
      axes=FALSE, ylim=c(rowCount,0))
    title("R colors")
    
    for (j in 0:(rowCount-1))
    {
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