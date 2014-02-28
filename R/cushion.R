#' Determine Max Value + n Extra
#' 
#' Determine max value of a vector plus an additional pproportion.
#' 
#' @param x A vector.
#' @param pad A proportion extra to add.
#' @return Returns the max value of a vector + n extra.
#' @note If x is a factor or character vector counts for each category are used 
#' as \code{max(n)}.
#' @export
#' @examples
#' cushion(as.factor(mtcars$cyl))
#' cushion(mtcars$cyl)
#' cushion(mtcars$cyl, .5)
#'
#' \dontrun{
#' ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) + 
#'     geom_bar()  + 
#'     theme_apa() + 
#'     y0(cushion(as.factor(mtcars$cyl))) +
#'     xlab("Cylinders") + 
#'     ylab("Total")
#' }
cushion <- function(x, pad = .05) {

    if(is.factor(x) | is.character(x)) {
        out <- max(tapply(x, x, length))
    } else {
        out <- max(x)
    }   

    out + out*pad
}


