#helper function
mergePDF <-
function(..., file, os = NULL, in.file = NULL) {
    if (is.null(in.file)) {
        in.file <- substitute(...())
    } 
    infiles <- paste(unlist(lapply(in.file, function(y) as.character(y))), 
        collapse = " ")
    if (is.null(os)) {
        testme <- c(UNIX = "gs -version", 
            Win32 = "gswin32c -version", 
            Win64 = "gswin64c -version")
        os <- names(which(sapply(testme, system, 
            ignore.stderr = TRUE, ignore.stdout = TRUE) == 0))
    }   
    version <- switch(os,
        unix = "gs",
        win32 = "gswin32c",
        win64 = "gswin64c")
    pre = " -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile="
    system(paste(paste(version, pre, file, sep = ""), infiles, collapse = " "))
}
