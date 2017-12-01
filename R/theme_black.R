#' ggplot2 Theme with No Background or Gridlines.
#'
#' A ggplot2 theme with no background and no gridlines.
#'
#' @param base_size The size to use for text.  Various textual components are
#' scaled off of this value.
#' @param base_family The base font family.
#' @author Jon Lefcheck (\url{http://jonlefcheck.net})
#' @references \url{http://jonlefcheck.net/2013/03/11/black-theme-for-ggplot2-2}
#' @export
#' @seealso \code{\link[ggplot2]{theme}}
#' @importFrom ggplot2 theme_grey theme element_blank element_text element_line element_rect %+replace%
#' @examples
#' ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill="white") + theme_black()
#' dat <- data.frame(y = c(austres), time = time(austres))
#' ggplot(dat, aes(time, y)) + scale_x_continuous() +
#'     geom_line(color="lightblue", size=1) + theme_black()
#'
#' \dontrun{
#' library(maps)
#' crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
#' states_map <-map_data("state")
#'
#' ggplot(crimes, aes(map_id = state)) +
#'     geom_map(aes(fill = Murder), map = states_map) +
#'     expand_limits(x = states_map$long, y = states_map$lat) +
#'     theme_black() +
#'     scale_fill_gradient(low="grey10", high="white")
#' }
theme_black <- function(base_size=12, base_family="") {
    theme_grey(base_size=base_size, base_family=base_family) %+replace%
        theme(
            # Specify axis options
            axis.line=element_blank(),
            axis.text.x=element_text(size=base_size*0.8, color="grey55",
                lineheight=0.9, vjust=1),
            axis.text.y=element_text(size=base_size*0.8, color="grey55",
                lineheight=0.9,hjust=1),
            axis.ticks=element_line(color="grey55", size = 0.2),
            axis.title.x=element_text(size=base_size, color="grey55", vjust=1,
                margin=ggplot2::margin(.5, 0, 0, 0, "lines")),
            axis.title.y=element_text(size=base_size, color="grey55", angle=90,
                margin=ggplot2::margin(.5, 0, 0, 0, "lines"), vjust=0.5),
            axis.ticks.length=grid::unit(0.3, "lines"),

            # Specify legend options
            legend.background=element_rect(color=NA, fill="black"),
            legend.key=element_rect(color="grey55", fill="black"),
            legend.key.size=grid::unit(1.2, "lines"),
            legend.key.height=NULL,
            legend.key.width=NULL,
            legend.text=element_text(size=base_size*0.8, color="grey55"),
            legend.title=element_text(size=base_size*0.8, face="bold",hjust=0,
                color="grey55"),
            legend.position="right",
            legend.text.align=NULL,
            legend.title.align=NULL,
            legend.direction="vertical",
            legend.box=NULL,
            # Specify panel options
            panel.background=element_rect(fill="black", color = NA),
            panel.border=element_rect(fill=NA, color="grey55"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.spacing=grid::unit(0.25,"lines"),
            # Specify facetting options
            strip.background=element_rect(fill="grey30", color="grey10"),
            strip.text.x=element_text(size=base_size*0.8, color="grey55"),
            strip.text.y=element_text(size=base_size*0.8, color="grey55",
                angle=-90),
            # Specify plot options
            plot.background=element_rect(color="black", fill="black"),
            plot.title=element_text(size=base_size*1.2, color="grey55"),
            plot.margin=grid::unit(c(1, 1, 0.5, 0.5), "lines")
    )
}
