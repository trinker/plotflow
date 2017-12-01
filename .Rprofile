if (!require("pacman")) install.packages("pacman")
pacman::p_load(qdap, reports)

update_version <- function(ver = NULL){

    desc <- suppressWarnings(readLines("DESCRIPTION"))
    regex <- "(^Version:\\s+\\d+\\.\\d+\\.)(\\d+)"
    loc <- grep(regex, desc)
    ver <- ifelse(is.null(ver), as.numeric(gsub(regex, "\\2", desc[loc])) + 1, ver)
    desc[loc] <- sprintf(gsub(regex, "\\1%s", desc[loc]), ver)
    cat(paste(desc, collapse="\n"), file="DESCRIPTION")

    cit <- suppressWarnings(readLines("inst/CITATION"))
    regex2 <- '(version\\s+\\d+\\.\\d+\\.)(\\d+)([."])'
    cit <- paste(cit, collapse="\n")
    cat(gsub(regex2, paste0("\\1", ver, "\\3"), cit), file = "inst/CITATION")
    message(sprintf("Updated to version: %s", ver))
}

update_news <- function(repo = basename(getwd())) {

    News <- readLines("NEWS")

    News <- mgsub(
        c("<", ">", "&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;", "BUG FIXES",
            "NEW FEATURES", "MINOR FEATURES", "CHANGES", "IMPROVEMENTS", " TRUE ", " FALSE ",
            " NULL ", "TRUE.", "FALSE.", "NULL.", ":m:"),
        c("&lt;", "&gt;", "**&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;**",
            "**BUG FIXES**", "**NEW FEATURES**", "**MINOR FEATURES**",
            "**CHANGES**", "**IMPROVEMENTS**", " `TRUE` ", "`FALSE`.", "`NULL`.", "`TRUE`.",
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

twitter <- "[![Follow](https://img.shields.io/twitter/follow/tylerrinker.svg?style=social)](https://twitter.com/intent/follow?screen_name=tylerrinker)"


md_toc <- function(path = "README.md", repo = basename(getwd()),
    insert.loc = "Functions"){

    x <- suppressWarnings(readLines(path))



    inds <- 1:(which(!grepl("(^\\s*-)|(\\]\\(#)", x))[1] - 1)

    temp <- gsub("(^[ -]+)(.+)", "\\1", x[inds])
    content <- gsub("^[ -]+", "", x[inds])
    bkna <- grepl("^[^[]", content)

    if (sum(bkna) > 0){
        bkn <- which(bkna)
        for (i in bkn){
            content[i - 1] <- paste(content[i - 1], content[i])
        }
        content <- content[!bkna]
        temp <- temp[!bkna]
    }

    toc <- paste(c("\nTable of Contents\n============\n",
        sprintf("%s[%s](%s)", temp, c(qdapRegex::ex_square(content)), gsub("[;/?:@&=+$,.]", "",
            gsub("\\s", "-", c(tolower(qdapRegex::ex_round(content)))))),
        sprintf("\n%s\n============\n", insert.loc)),
        collapse = "\n"
    )

    x <- x[(max(inds) + 1):length(x)]

    inst_loc <- which(grepl(sprintf("^%s$", insert.loc), x))[1]
    x[inst_loc] <- toc
    x <- x[-c(1 + inst_loc)]

    beg <- grep("^You are welcome", x)
    end <- grep("compose a friendly", x)
    
    x[beg] <- sprintf(contact, repo, repo)
    
    x <- x[!seq_along(x) %in% (1+beg:end)]

    a <- grep("<table>", x)
    if (!identical(integer(0), a)){
        b <- grep("</table>", x)
        inds <- unlist(mapply(function(a, b){ a:b}, a, b))
        x[inds] <- gsub("\\\\_", "_", x[inds])
    }

    x <- gsub("<!-- -->", "", x, fixed=TRUE)
    cat(paste(c(sprintf("%s   %s\n============\n", repo, twitter), x), collapse = "\n"), file = path)
    message("README.md updated")
}

contact <- paste(c(
    "You are welcome to:    ",
    "- submit suggestions and bug-reports at: <https://github.com/trinker/%s/issues>    ",
    "- send a pull request on: <https://github.com/trinker/%s/>    ",
    "- compose a friendly e-mail to: <tyler.rinker@gmail.com>    "
), collapse="\n")
    
    
    