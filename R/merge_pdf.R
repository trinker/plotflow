#' Merge Different Sized Plots
#'
#' Allows for merging of different sized R plots.
#'
#' @param n.plots The number of plots to be combined.
#' @param file A connection, or a character string naming the file to print to.
#' @param widths A vector of widths equal to \code{n.plots} or a single value
#' that will be used for all plot widths.
#' @param heights A vector of heights equal to \code{n.plots} or a single value
#' that will be used for all plot widths.
#' @param n.lines A vector of integer values indicating the number of lines each
#' plotting sequence will take.  Default is 1 line each.
#' @return Returns a single combined plot of various sizes.
#' @note To use with ggplot the plotting sequence must be wrapped with
#' \code{plot()}.
#' @keywords plot
#' @importFrom grDevices dev.off pdf
#' @export
#' @examples
#' \dontrun{
#' merge_pdf(3, file = "foo.pdf", widths = c(7, 7, 10), heights = c(6, 10, 7))
#' plot(1:10)
#' plot(1:10, pch=19)
#' plot(1:10, col="red", pch=19)
#'
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
#' merge_pdf(2, file = "bar.pdf", widths = c(7, 10), heights = c(6, 10))
#' plot(1:10)
#' print(p)
#' }
merge_pdf <-
function(n.plots, file, widths = 8, heights = 8, n.lines = 1) {
    xs <- c(n.plots, 1)
    if ((!length(widths) %in% xs) || (!length(heights) %in% xs) || (!length(n.lines) %in% xs)) {
        stop("widths and heights must be length 1 or equal to  n.plots")
    }
    if (length(widths) == 1) {
        widths <- rep(widths, n.plots)
    }
    if (length(heights) == 1) {
        heights <- rep(heights, n.plots)
    }
    if (length(n.lines) == 1) {
        n.lines <- rep(n.lines, n.plots)
    }
    if (n.plots < 2) stop("Must have > 2 plots")
    files <- tempfile(fileext = rep.int(".pdf", n.plots))
    invisible(lapply(1:n.plots, function(i) {
        pdf(file=files[i], width = widths[i], height = heights[i])
        # Reads string interactively
        cat(paste("Enter plot ", i, ":\n", sep=""))
        input <- readLines(n=n.lines[i])
        # Executes `input` as a command (possibly, needs extra check)
        eval(parse(text=input))
        dev.off()
    }))
    mergePDF(in.file = files, file = file)
    unlink(files, recursive = TRUE, force = FALSE)
    cat("\n")
    cat(paste(file, "written to:", paste0(getwd(), "/", file, "\n")))
}
