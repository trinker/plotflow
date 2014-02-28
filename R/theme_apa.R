#' ggplot2 Theme for APA Publications 
#' 
#' A ggplot2 theme with no background and Times New Roman font.
#' 
#' @param plot.box logical.  If \code{TRUE} a full box surrounds the plot area.  If \code{FALSE} only the x and y axis are shown.
#' @export
#' @seealso \code{\link[ggplot2]{theme}}
#' @importFrom ggplot2 theme_bw theme element_blank element_text element_line element_rect
#' @examples
#' \dontrun{
#' ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) + 
#'     geom_bar()  + 
#'     theme_apa() + 
#'     y0(cushion(as.factor(mtcars$cyl))) +
#'     xlab("Cylinders") + 
#'     ylab("Total")
#' 
#' ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) + 
#'     geom_bar()  + 
#'     theme_apa(plot.box=T) + 
#'     y0(cushion(as.factor(mtcars$cyl))) +
#'     xlab("Cylinders") + 
#'     ylab("Total")
#' 
#' ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) + 
#'     geom_bar()  + 
#'     theme_basic() +
#'     theme_apa() + 
#'     y0(cushion(as.factor(mtcars$cyl))) +
#'     xlab("Cylinders") + 
#'     ylab("Total")
#' }
theme_apa <- function(plot.box = FALSE){

    if (Sys.info()["sysname"] != "Windows") {
        windowsFonts <- NULL
    }

    if (Sys.info()["sysname"] == "Windows") {
        windowsFonts(RMN=windowsFont("Times New Roman"))
        RMN <- "RMN"
    } else {
        RMN <- "Times New Roman"
    }

    out <- theme(
        plot.title=element_text(family=RMN, size=14, face="bold", colour="black"),
        axis.title.x=element_text(family=RMN, size=14, colour="black"),
        axis.title.y=element_text(family=RMN, size=14, angle=90, colour="black"),
        axis.text.x=element_text(family=RMN, size=11, colour="black"),
        axis.text.y=element_text(family=RMN, size=11, colour="black"),
        axis.ticks=element_line(colour="black"))  

    if (!plot.box) {
        out <- out + theme(panel.background = element_rect(fill = "white", 
                colour = "black"), panel.border = element_rect(fill = NA, 
                colour = "white"), axis.line = element_line()) 
    } else {
        out <- out + theme(panel.background = element_rect(fill = "white", 
                colour = "white"), panel.border = element_rect(fill = NA, 
                colour = "grey50")) 
    }
    out

}