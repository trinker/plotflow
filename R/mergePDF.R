#helper function
#' @importFrom tools find_gs_cmd
mergePDF <-
  function(..., file, gsversion = NULL, in.file = NULL) {
    if (is.null(in.file)) {
      in.file <- substitute(...())
    }
    infiles <- unlist(lapply(in.file, function(y) as.character(y)))
    if (is.null(gsversion)) {
      gsversion <- find_gs_cmd()
      if (!nzchar(gsversion))
        stop("Please install Ghostscript and see ?tools::find_gs_cmd")
    }
    pre <- c("-dBATCH", "-dNOPAUSE", "-q", "-sDEVICE=pdfwrite")
    out <- paste0("-sOutputFile=", shQuote(file))
    system2(gsversion, c(pre, out, shQuote(infiles)))
}
