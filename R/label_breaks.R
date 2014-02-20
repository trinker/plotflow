#' Break Axis Labels
#' 
#' Break factor level labels onto new lines within plots.
#' 
#' @param dataframe A dataframe object.
#' @param split.col The column to break its labels onto separate lines.
#' @return Returns a data.frame with the factor element spaces replaces with \code{\\n}.
#' @author Mollie Taylor and Tyler Rinker <tyler.rinker@@gmail.com>
#' @references \url{http://www.mollietaylor.com/2013/10/line-breaks-between-words-in-axis.html}
#' @export
#' @examples
#' set.seed(1000)
#' dat <- data.frame(Location = rnorm(1:1000, mean = 200, sd = 75),
#'     Type = sample(c("Big Red Monster", "Little Green Alien", 
#'         "One-Eyed, One-Horned Flying Purple People Eater"), 1000, TRUE))
#' 
#' ggplot(dat,  aes(x = Type, y = Location)) + 
#'     geom_boxplot() 
#' 
#' ggplot(label_breaks(dat, 2),  aes(x = Type, y = Location)) + 
#'     geom_boxplot() 
#' 
#' 
#' ggplot(label_breaks(dat, "Type"),  aes(x = Type, y = Location)) + 
#'     geom_boxplot() + 
#'     coord_flip() 
label_breaks <- function(dataframe, split.col) {
    
    levels(dataframe[, split.col]) <- gsub(" ", "\n", 
        levels(dataframe[, split.col]))
    dataframe

}
