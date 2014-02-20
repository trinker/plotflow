#' Open Help Pages for ggplot2
#' 
#' \code{gghelp} - Open Hadely Wickham's ggplot2 
#' \href{http://docs.ggplot2.org/current/}{web page}.
#' 
#' @param FUN A particular ggplot function to reference.  Default is the index 
#' page.
#' @return Opens a help web page.
#' @rdname help
#' @export
#' @seealso \code{\link[utils]{browseURL}}
#' @examples
#' \dontrun{
#' gghelp() 
#' gghelp("theme")
#' ggcook()
#' }
gghelp <- function(FUN) {
    if(missing(FUN)) FUN <- "" else FUN <- paste0(FUN, ".html")
    browseURL(sprintf("http://docs.ggplot2.org/current/%s", FUN))
}

#' Open Help Pages for ggplot2
#' 
#' \code{ggcook} - Open Winston Chang's ggplot2 
#' \href{http://www.cookbook-r.com/Graphs/}{Cookbook for R page}.
#' 
#' @rdname help
#' @export
ggcook <- function() {
    ## browseURL("http://www.cookbook-r.com/Graphs/#graphs-with-ggplot2")
    browseURL("http://www.cookbook-r.com/Graphs/")
}
