#' Plot to Drop Box
#' 
#' A wrapper to plotting device for easy plotting to Drop Box.
#' 
#' @param name The name of the plot (no file extension needed).
#' @param type The type of plotting device (png, pdf, etc).
#' @param width Width of plot.
#' @param height Height of plot.
#' @param open logical.  If \code{TRUE} provides the code to open the plot in 
#' default browser.
#' @param loc The path to the location of the dropbox public folder.
#' @param key The dropbox key.
#' @param \ldots Other arguments passed to the plotting device in type.
#' @details Plots to Drop Box and returns the URL to the plot (and optionally 
#' opens the plot).  Code attemoted to be copied to the clipboard.
#' @export
#' @examples
#' \dontrun{
#' plot2drop("dfg")
#' plot(1:10)
#' dev.off()
#' }
plot2drop <- 
function(name, type = "png", width = 400, height = 400, open = TRUE, 
	loc = getOption("dropbox_path"), key = getOption("dropbox_key"), ...){
    name <- as.character(substitute(name))
    if (missing(name)) {
        stop("must specify a name")
    }
    if (is.null(getOption("dropbox_path"))) {
        stop("must specify a loc (Dropbox Location)")
    }
    if (is.null(getOption("dropbox_key"))) {
        stop("must specify a key (Dropbox Key)")
    }
    what <- paste0(name, ".", type)
    loc <- file.path(loc, what)
    dev <- match.fun(type)
    dev(loc, width, height, ...)
    w <- "dev.off()"
    x <- paste0("plotflow:::write_clip(\"https://dl.dropboxusercontent.com/u/", key,
        "/", what, "\")")
    y <- NULL
    if (open) {
        y <- paste0("browseURL(\"https://dl.dropboxusercontent.com/u/", key,
          "/", what, "\")\n")
    } 
    z <- paste(w, x, y, sep="\n")
    write_clip(z)
    message(z)
}
