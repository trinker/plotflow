#' Order a Factor by Numeric Variable(s)
#' 
#' Create a new dataframe with a factor reordered (re-leveled) by numeric 
#' variable(s).
#' 
#' @param fact The factor to be reordered (re-leveled).
#' @param by A formula to order the factor by. 
#' @param dat A \code{data.frame} object.
#' @param FUN A function to compute the summary statistics which can be applied 
#' to all data subsets.
#' @param df logical.  If \code{TRUE} a dataframe is returned.  If \code{FALSE}
#' a factor vector is returned.
#' @return Returns a re-ordered (re-leveled) dataframe, factor vector, or levels.
#' @references The majority of this code is taken directly from Thomas Wutzler's
#' blog post: \url{http://rwiki.sciviews.org/doku.php?id=tips\%3adata-frames\%3asort}
#' @author Thomas Wutzler and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @keywords factor, order, sort, plot
#' @export 
#' @examples
#' \dontrun{
#' ## EXAMPLE 1 - no aggregation ##
#' 
#' ## Make a fake data set
#' dat <- aggregate(cbind(mpg, hp, disp)~carb, mtcars, mean)
#' dat$carb <- factor(dat$carb)
#' 
#' ## compare levels (data set looks the same though)
#' dat$carb
#' order_by(x = dat, carb, ~-hp + -mpg)$carb
#' 
#' library(ggplot2)
#' ## Unordered bars
#' ggplot(dat, aes(x=carb, y=mpg)) + 
#'     geom_bar(stat="identity") + 
#'     coord_flip()
#' 
#' ## Ordered bars
#' ggplot(order_by(x = dat, carb, ~mpg), aes(x=carb, y=mpg)) + 
#'     geom_bar(stat="identity") + 
#'     coord_flip()
#'     
#' ## EXAMPLE 2 - with aggregation ##
#' 
#' ## Return just the vector with new levels
#' order_by(x = dat, carb, ~-hp + -mpg, FALSE)
#' 
#' mtcars2 <- order_by(gear, ~hp + -carb, mtcars, mean)
#' 
#' ## Without re-leveling gear
#' ggplot(mtcars, aes(mpg, hp)) + 
#'     geom_point(aes(color=factor(cyl))) + 
#'     facet_grid(gear~.)
#' 
#' ## After re-leveling gear
#' ggplot(mtcars2, aes(mpg, hp)) + 
#'     geom_point(aes(color=factor(cyl))) + 
#'     facet_grid(gear~.)
#' }
order_by <- function(fact, by, dat, FUN = NULL, df = TRUE){
    if(by[[1]] != "~")
        stop("Argument 'by' must be a one-sided formula.")

    fact <- as.character(substitute(fact))
    # Make the formula into character and remove spaces
    formc <- as.character(by[2]) 
    formc <- gsub(" ", "", formc) 
    # If the first character is not + or -, add +
    if(!is.element(substring(formc, 1, 1), c("+", "-")))
        formc <- paste("+", formc, sep = "")
 
    # Extract the variables from the formula
    vars <- unlist(strsplit(formc, "[\\+\\-]"))    
    vars <- vars[vars != ""] # Remove any extra "" terms

    ## use f aggregating
    if (!is.null(FUN)) {
        x <- eval(parse(text=paste0("aggregate(cbind(", paste(vars, collapse = ", "), ") ~", 
            fact, ", data = dat, FUN = \"", substitute(FUN), "\")")))
    }

    # Build a list of arguments to pass to "order" function
    calllist <- list()
    pos <- 1 # Position of + or -
    for(i in 1:length(vars)){
        varsign <- substring(formc, pos, pos)
        pos <- pos + 1 + nchar(vars[i])
        if(is.factor(x[, vars[i]])){
            if(varsign == "-") {
                calllist[[i]] <- -rank(x[, vars[i]])
            } else {
                calllist[[i]] <- rank(x[, vars[i]])
            }
        } else {
            if(varsign == "-") {
                calllist[[i]] <- -x[, vars[i]]
            } else {
                calllist[[i]] <- x[,vars[i]]
            }
        }
    }
    dat[, fact] <- factor(dat[, fact], levels = x[do.call("order", calllist), fact])
    if (df) {
        dat
    } else {
        dat[, fact]
    }
}




