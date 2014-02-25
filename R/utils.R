write_clip <- function(x) {
    ## The code for this helper function comes from the oveRflow package.  
    ## # https://raw.github.com/sebastian-c/oveRflow/master/R/writeClip.R
    ## This is code I submitted but was modified by the package maintainers.
    ## The idea to keep this function as a modular unit makes sense and was 
    ## subsequently applied to the reports package
	
    OS <- Sys.info()["sysname"]
    
    if(!(OS %in% c("Darwin", "Windows", "Linux"))) {
        stop("Copying to clipboard not supported on your OS")
    }
    
    if (OS != "Windows") {
        writeClipboard <- NULL
    } 
    
    switch(OS, 
        "Darwin"={j <- pipe("pbcopy", "w")                       
            writeLines(x, con = j)                               
            close(j)   
        },
        "Windows"=writeClipboard(x, format = 1),
        "Linux"={
            if(Sys.which("xclip") == "") {
              stop("Clipboard on Linux requires 'xclip'. Try using:\nsudo apt-get install xclip")
            }
            con <- pipe("xclip -i", "w")
            writeLines(x, con=con)
            close(con)
        }
    )
}

read_clip <- function() {
	    ## The code for this helper function comes from the oveRflow package.  
    ## # https://raw.github.com/sebastian-c/oveRflow/master/R/writeClip.R
    ## This is code I submitted but was modified by the package maintainers.
    ## The idea to keep this function as a modular unit makes sense and was 
    ## subsequently applied to the reports package
	
    OS <- Sys.info()["sysname"]

    if (OS != "Windows") {
        readClipboard <- NULL
    } 
    

    switch(OS, 
        "Darwin" = {j <- pipe("pbcopy", "w")                       
            pcon <- pipe("pbpaste")
            out <- scan(pcon, what="character", quiet=TRUE)
            close(pcon)
        },
        "Windows" = {out <- readClipboard()},
        out <- readLines("clipboard")
    )
    out
}

