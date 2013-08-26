#' Fill facet_wrap By Column
#' 
#' Fill ggplot2's facet_wrap by column rather than row.
#' 
#' @param dataframe A data.frame object.
#' @param fact The factor to be reordered (re-leveled).
#' @param ncol Number of columns to make the plot
#' @export
#' @examples
#' library(ggplot2); library(reshape2)
#' dat <- aggregate(cbind(vs, am, gear, cyl) ~ carb, mtcars, sum)
#' dat$carb <- factor(dat$carb, rev(dat$carb[order(rowSums(dat[, -1]))]))
#' mdat <- melt(dat)
#' 
#' ggplot(mdat, aes(x = variable)) + 
#'   geom_bar(stat="bin", aes(fil=variable, weights = value)) + 
#'   facet_wrap(~carb, ncol = 2) + coord_flip()
#' 
#' ggplot(fill_by_col(mdat, "carb"), aes(x = variable)) + 
#'   geom_bar(stat="bin", aes(fil=variable, weights = value)) + 
#'   facet_wrap(~carb, ncol = 2) + coord_flip()
#'   
#' ggplot(fill_by_col(mdat, "carb", 3), aes(x = variable)) + 
#'   geom_bar(stat="bin", aes(fil=variable, weights = value)) + 
#'   facet_wrap(~carb, ncol = 3) + coord_flip()
fill_by_col <- function(dataframe, fact, ncol = 2) {
    var2 <- dataframe[, fact]
    dat <- matrix(levels(var2), ncol = ncol)
    dataframe[, fact] <- factor(dataframe[, fact], 
        levels = unlist(lapply(1:nrow(dat), function(i) dat[i, ])))
    dataframe
}