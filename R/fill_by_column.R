#' Fill facet_wrap By Column
#'
#' Alter the data set to fill ggplot2's \code{\link[ggplot2]{facet_wrap}} by
#' column rather than row.
#'
#' @param dataframe A data.frame object.
#' @param fact The factor to be reordered (re-leveled) to fill by column.
#' @param ncol Number of columns to make the plot
#' @references \url{http://stackoverflow.com/a/12893273/1000343}
#' @author jem77bfp (stackoverflow.com) and Tyler Rinker <tyler.rinker@@gmail.com>
#' @export
#' @examples
#' library(ggplot2); library(reshape2)
#' dat <- aggregate(cbind(vs, am, gear, cyl) ~ carb, mtcars, sum)
#' dat$carb <- factor(dat$carb, rev(dat$carb[order(rowSums(dat[, -1]))]))
#' mdat <- melt(dat)
#'
#' ggplot(mdat, aes(x = variable)) +
#'   geom_bar(stat="count", aes(fill=variable, weight = value)) +
#'   facet_wrap(~carb, ncol = 2) + coord_flip()
#'
#' ggplot(fill_by_column(mdat, "carb"), aes(x = variable)) +
#'   geom_bar(stat="count", aes(fill=variable, weight = value)) +
#'   facet_wrap(~carb, ncol = 2) + coord_flip()
#'
#' ggplot(fill_by_column(mdat, "carb", 3), aes(x = variable)) +
#'   geom_bar(stat="count", aes(fill=variable, weight = value)) +
#'   facet_wrap(~carb, ncol = 3) + coord_flip()
fill_by_column <- function(dataframe, fact, ncol = 2) {
    var2 <- dataframe[, fact]
    dat <- matrix(levels(var2), ncol = ncol)
    dataframe[, fact] <- factor(dataframe[, fact],
        levels = unlist(lapply(1:nrow(dat), function(i) dat[i, ])))
    dataframe
}
