#helper function
mergePDF <-
function(..., file, os = NULL, in.file = NULL) {
    if (is.null(in.file)) {
        in.file <- substitute(...())
    } 
    infiles <- paste(unlist(lapply(in.file, function(y) as.character(y))), 
        collapse = " ")
    version <- names(which(Sys.which(c("gs", "gswin32c", "gswin64c")) != ""))
    pre = " -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile="
    system(paste(paste(version, pre, file, sep = ""), infiles, collapse = " "))
}
