#helper function
mergePDF <-
  function(..., file, gsversion = NULL, in.file = NULL) {
    if (is.null(in.file)) {
      in.file <- substitute(...())
    } 
    infiles <- paste(unlist(lapply(in.file, function(y) as.character(y))), 
        collapse = " ")
    if (is.null(gsversion)) {
      gsversion <- names(which(Sys.which(c("gs", "gswin32c", "gswin64c")) != ""))
      if (length(gsversion) == 0) 
        stop("Please install Ghostscript and ensure it is in your PATH")
      if (length(gsversion) > 1)
        stop("More than one Ghostscript executable was found:", 
             paste(gsversion, collapse = " "), 
             ". Please specify which version should be used with the gsversion argument")
    }   
    pre = " -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile="
    system(paste(paste(gsversion, pre, file, sep = ""), infiles, collapse = " "))
}
