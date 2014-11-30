## Gerneric Graphics Device
## 
## \code{splot} - Save plot (splot) to graphics device based on file extension 
## with sensible defaults.
## 
## @param file The name of the output file.
## @param width  The width in inches.
## @param height The height in inches.
## @param cairo logical.  If \code{TRUE} \pkg{cairoDevice} package is used for 
## supported file types.  See \code{\link[cairoDevice]{Cairo}} for more details. 
## @param reduce.margins logical.  If \code{TRUE} margins will be reduced.
## @param \ldots Arguments passed to graphics devices.
## @return Sets up a graphics device using default parameters.
## @keywords plot
## @rdname splot
## @export
## @examples
## \dontrun{
## plot(1:10, 1:10)
## splot()
## 
## ggplot(mtcars, aes(mpg, hp)) + geom_point()
## splot()
## 
## ggplot(mtcars, aes(mpg, hp)) + geom_point()
## splot("out.pdf")
## }
splot <- function(file = "myPlot.png", width = 6.93, height = 6.93, 
	cairo = FALSE, reduce.margins = TRUE, bg = "white", ...){

	req <- NULL
	
    if (reduce.margins) {
        opar <- par()[["mar"]]
        par(mar=c(5,3,2,2)+0.1)
    }

    ext <- tools::file_ext(file)
    size <- sprintf("Image size: width %s inches x height %s inches", 
        width, height)

    gd <- switch(ext, 
        svg = list(con = 1, name = "svg"),
        pdf = list(con = 1, name = "pdf"),
        png = list(con = 72, name = "png"),
        tif = list(con = 72, name = "tiff"),
        jpg = list(con = 72, name = "jpeg"),
        bmp = list(con = 72, name = "bmp"),
        ps = list(con = 1, name = "postscript"),
        stop(sprintf("%s not a supported graphics device", ext))
    )
    if(cairo && ext == "png") gd[["con"]] <- 1
    if (cairo && ext %in% c("pdf", "ps", "svg", "png")) {
#        req <- require(cairoDevice)
        if (!req) {
            warning("carioDevice package not installed, `cario` ignored")
        } else {
            gd[["name"]] <- paste0("Cairo_", gd[["name"]])
        }
    }
    if(cairo && !ext %in% c("pdf", "ps", "svg", "png")) {
        message(sprintf("%s not supported by cairoDevice package", 
            gd[["name"]]))
    }

    width <- width * gd[["con"]]
    height  <- height * gd[["con"]]

    graphics_dev <- match.fun(gd[["name"]])

    if (ext %in% c("svg", "png", "jpg", "bmp", "tiff")){
        dev.print(device=graphics_dev, filename=file, 
            width=width, height=height, bg = bg, ...)
    } else {
        dev.print(device=graphics_dev, file=file, 
            width=width, height=height, bg = bg, ...)
    }

    message(file, " ready to plot on ", gd[["name"]], "\n", size)
}



## Gerneric Graphics Device
## 
## \code{lsplot} - A wrapper for \code{splot} that saves the last plot to 
## a graphics device.
## 
## @note \code{lsplot} only works under 2 conditions (1) the last plotting 
## function has the word "plot" in the function name; (2) no other functions 
## were called after the plot.
## @rdname splot
## @export
lsplot <- function(...) {

    suppressMessages(splot(...) )
    x <- head(ggplot2::last_plot(), -1)
    x <- x[tail(grep("plot", x), 1):length(x)]
    eval(parse(text=paste(x, collapse=" ")), envir=.GlobalEnv)
    dev.off()

}


