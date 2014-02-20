#' Add Text to a Faceted ggplot2 Plot
#' 
#' A ggplot2 wrapper for adding text to facets.
#' 
#' @param  ggplot2.object a faceted ggplot2 object or an object returned from 
#' qfacet_text
#' @param  x.coord a single x coordinate to be repeated or a vector of x 
#' coordinates equal to the number of facets
#' @param  y.coord a single y coordinate to be repeated or a vector of y 
#' coordinates equal to the number of facets
#' @param  labels a vector of labels to place on each facet
#' @param \ldots additional arguments accepted by geom_text
#' @return Returns a plot of class "gg" "ggplot" with annotations.  Also 
#' invisibly returns a list object of the class qfacet with the following items:
#' \itemize{
#'   \item{original} {the Original ggplot2 object}
#'   \item{new} {the new ggplot object}
#'   \item{dat} {the mini data frame created for the text}
#' }
#' @seealso \code{\link[ggplot2]{geom_text}}
#' @keywords ggplot2 facet text
#' @export
#' @examples
#' #alter mtcars to make some variables factors
#' mtcars[, c("cyl", "am", "gear")] <- lapply(mtcars[, 
#'     c("cyl", "am", "gear")], as.factor)
#'  
#' p <- ggplot(mtcars, aes(mpg, wt, group = cyl)) + 
#'     geom_line(aes(color=cyl)) +
#'     geom_point(aes(shape=cyl)) + 
#'     facet_grid(gear ~ am) +
#'     theme_bw()  
#'  
#' z <- ggfaxt(ggplot2.object = p, x.coor = 33, y.coor = 2.2, 
#'     labels = 1:6, color="red")
#   
#' #approach 1 (alter the text data frame and pass the qfacet object)
#' z$dat[5, 1:2] <- c(15, 5)
#' ggfaxt(z, color="red")
#'  
#' #approach 2 (alter the original ggplot object)
#' ggfaxt(p, x = c(33, 33, 33, 33, 15, 33), 
#'     y = c(2.2, 2.2, 2.2, 2.2, 5, 2.2), 1:6, color="red")
#'     
#' #use "" to not add a label to a facet
#' ggfaxt(ggplot2.object = p, x.coor = 33, y.coor = 2.2,
#'     labels = c("", letters[1:4], ""), color="red")
#'  
#' #all the same things you can pass to geom_text qfacet_text takes
#' ggfaxt(z, labels = paste("beta ==", 1:6), 
#'     size = 3, color = "grey50", parse = TRUE)
#'      
#' #two labels: same plot
#' p <- ggplot(CO2, aes(conc, uptake, group = Plant)) + 
#'     geom_line(aes(color=Plant)) +
#'     facet_grid(Type ~ Treatment) +
#'     theme_bw()  
#'     
#' #plot first text layer
#' z <- ggfaxt(ggplot2.object = p, x.coor = 250, y.coor = 10, 
#'      labels = 1:4, color="red")
#'      
#' #plot second text layer
#' ggfaxt(ggplot2.object = z$new, x.coor = 900, y.coor = 10, 
#'     labels = paste("beta ==", 11:14), color="blue", parse = TRUE)
#'     
#' ## Replace mtcars
#' mtcars <- datasets::mtcars
ggfaxt <-
function(ggplot2.object, x.coord = NULL, y.coord = NULL, 
    labels = NULL, ...) {
	
	x <- y <- NULL
	
    dat <- ggplot2.object$data
    look <- sapply(ggplot2.object$facet[1:2], as.character)
    empt <- function(x) {!identical(x, character(0))}
    who <- sapply(look, empt)
    if (all(who)) {
        rows <- ggplot2.object$facet[[1]][[1]]
        frow <- dat[, as.character(rows)]
        cols <- ggplot2.object$facet[[2]][[1]]
        fcol <- dat[, as.character(cols)]
        len <- length(levels(factor(fcol))) *  length(levels(factor(frow)))
        vars <- data.frame(expand.grid(levels(factor(frow)), levels(factor(fcol))))
        colnames(vars) <- c(as.character(rows), as.character(cols))
    } else {
        if (who[1]) {
            rows <- ggplot2.object$facet[[1]][[1]]
            frow <- dat[, as.character(rows)]    
            len <- length(levels(factor(frow)))
            vars <- data.frame(levels(factor(frow)), stringsAsFactors = FALSE)
            colnames(vars) <- as.character(rows)
        } else {
            cols <- ggplot2.object$facet[[2]][[1]]
            fcol <- dat[, as.character(cols)]  
            len <- length(levels(factor(fcol)))
            vars <- data.frame(levels(factor(fcol)), stringsAsFactors = FALSE)
            colnames(vars) <- as.character(cols)    
        }
    }
    if (any(class(ggplot2.object) %in% c("ggplot", "gg"))) {
        if (is.null(labels)) {
            labels <- LETTERS[1:len]
        }
        if (!length(labels) %in% c(1, len)) {
            stop("labels must be of length 1 or equal to number of facets")
        }
        if (length(x.coord) == 1) {
           x.coord <- rep(x.coord, len)
        }
        if (length(y.coord) == 1) {
           y.coord <- rep(y.coord, len)
        }
        text.df <- data.frame(x = x.coord, y = y.coord, vars, labs=labels)
    } else {
        if (class(ggplot2.object) == "qfacet") {
            text.df <- ggplot2.object$dat
            if (!is.null(x.coord)) {
                text.df$x.coord <- x.coord
            }
            if (!is.null(y.coord)) {
                text.df$y.coord <- y.coord
            }
            if (!is.null(labels)) {
                text.df$labs <- labels
            }
            ggplot2.object <- ggplot2.object$original
        }
    }
    p <- ggplot2.object + geom_text(aes(x, y, label=labs, group=NULL), 
        data=text.df, ...)
    print(p)
    v <- list(original = ggplot2.object, new = p, dat = text.df)
    class(v) <- "qfacet"
    invisible(v)
}
