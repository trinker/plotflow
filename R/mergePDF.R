#helper function
mergePDF <-
mergePDF <-
  function(..., file, gsversion = NULL, in.file = NULL) {
    if (is.null(in.file)) {
      in.file <- substitute(...())
    } 
    infiles <- paste(unlist(lapply(in.file, function(y) as.character(y))), 
<<<<<<< HEAD
        collapse = " ")
    version <- names(which(Sys.which(c("gs", "gswin32c", "gswin64c")) != ""))
=======
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
>>>>>>> 5dc7c293e5f0c036d411a8cc419033dcc2ae9d05
    pre = " -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile="
    system(paste(paste(gsversion, pre, file, sep = ""), infiles, collapse = " "))
}
