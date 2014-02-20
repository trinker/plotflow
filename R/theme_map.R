#' ggplot2 Theme for Mapping. 
#' 
#' A ggplot2 theme with no background, gridlines, border, labels, or ticks.
#' 
#' @export
#' @seealso \code{\link[ggplot2]{theme}}
#' @importFrom ggplot2 theme_bw theme element_blank
#' @examples
#' \dontrun{
#' require("maps") 
#' states <- data.frame(map("state", plot=FALSE)[c("x","y")]) 
#' (usamap <- qplot(x, y, data=states, geom="path")) 
#' usamap + theme_map()
#' }
theme_map <- function() {
    theme_bw() +     
    theme(axis.title=element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()   
    )  
}

