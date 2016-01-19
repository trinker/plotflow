options(repos = "http://cran.r-project.org")

update_news <- function(repo = basename(getwd())) {
  
    News <- readLines("NEWS")
    
    News <- qdap::mgsub(
        c("<", ">", "&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;", "BUG FIXES", 
            "NEW FEATURES", "MINOR FEATURES", "CHANGES", " TRUE ", " FALSE ", 
            " NULL ", "TRUE.", "FALSE.", "NULL.", ":m:"), 
        c("&lt;", "&gt;", "**&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;**", 
            "**BUG FIXES**", "**NEW FEATURES**", "**MINOR FEATURES**", 
            "**CHANGES**", " `TRUE` ", "`FALSE`.", "`NULL`.", "`TRUE`.", 
            " `FALSE` ", " `NULL` ", " : m : "), 
            News, trim = FALSE, fixed=TRUE)
    
    News <- sub(pattern="issue *# *([0-9]+)", 
        replacement=sprintf("<a href=\"https://github.com/trinker/%s/issues/\\1\">issue #\\1</a>",
        repo), 
        x=News)
    
    News <- sub(pattern="pull request *# *([0-9]+)", 
        replacement=sprintf("<a href=\"https://github.com/trinker/%s/issues/\\1\">pull request #\\1</a>",
        repo), 
        x=News)
 
    News <- gsub(sprintf(" %s", repo), 
        sprintf(" <a href=\"https://github.com/trinker/%s\" target=\"_blank\">%s</a>", 
        repo, repo), News)
    
    cat(paste(News, collapse = "\n"), file = "NEWS.md")
    message("news.md updated")
}

update_version <- function(ver = NULL){
    
	desc <- read.dcf("DESCRIPTION")

	if (is.null(ver)) {
		m <- as.numeric(unlist(strsplit(desc[,"Version"], "\\.")))
		m[3] <- m[3] + 1
		ver <- paste(m, collapse=".")
	}
	
	desc[,"Version"] <- ver  
    write.dcf(desc, "DESCRIPTION")
	
    cit <- suppressWarnings(readLines("inst/CITATION"))
    regex2 <- '(version\\s+)(\\d+\\.\\d+\\.\\d+)'
    cit <- paste(cit, collapse="\n")
	
    cat(sprintf(gsub(regex2, "%s", cit, perl=TRUE), ver, ver), file = "inst/CITATION")
    message(sprintf("Updated to version: %s", ver))
}


update_date <- function(){
    desc <- read.dcf("DESCRIPTION")
	if (Sys.Date() > desc[,"Date"]) {
		desc[,"Date"] <- as.character(Sys.Date())
        write.dcf(desc, "DESCRIPTION")
		message("Date updated")
	} else {
		message("Date is current")
	}
}

update_date()

