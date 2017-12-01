#' Rename the Margins of ggplot2 facet_grid
#'
#' Convert the default (all) when setting ggplot2's
#' \code{\link[ggplot2]{facet_grid}} to \code{margins = TRUE}.
#'
#' @param newname The new strip.text name to overwrite (all).
#' @keywords margin
#' @importFrom ggplot2 as_labeller
#' @export
#' @examples
#' mtcars2 <- mtcars
#' mtcars2$gear <- factor(mtcars2$gear)
#' ggplot(mtcars2, aes(cyl)) +
#'     geom_point(stat="bin", size = 2, binwidth = 2,
#'         aes(shape = gear), position = "stack") +
#'     facet_grid(carb ~ gear, margins = TRUE,
#'         labeller=label_rename_margin("Total"))
#'
#' ggplot(mtcars2, aes(cyl)) +
#'     geom_point(stat="bin", size = 2, binwidth = 2,
#'         aes(shape = gear), position = "stack") +
#'     facet_grid(carb ~ gear, margins = "gear",
#'         labeller=label_rename_margin("Total"))
label_rename_margin <- function(newname = "Total") {
    as_labeller(function(x) {
        x[x == "(all)"] <- newname
        x
    })
}
