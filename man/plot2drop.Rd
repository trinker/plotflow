\name{plot2drop}
\alias{plot2drop}
\title{Plot to Drop Box}
\usage{
plot2drop(name, type = "png", width = 400, height = 400, open = TRUE,
  loc = getOption("dropbox_path"), key = getOption("dropbox_key"), ...)
}
\arguments{
  \item{name}{The name of the plot (no file extension
  needed).}

  \item{type}{The type of plotting device (png, pdf, etc).}

  \item{width}{Width of plot.}

  \item{height}{Height of plot.}

  \item{open}{logical.  If \code{TRUE} provides the code to
  open the plot in default browser.}

  \item{loc}{The path to the location of the dropbox public
  folder.}

  \item{key}{The dropbox key.}

  \item{\ldots}{Other arguments passed to the plotting
  device in type.}
}
\description{
A wrapper to plotting device for easy plotting to Drop Box.
}
\details{
Plots to Drop Box and returns the URL to the plot (and
optionally opens the plot).  Code attemoted to be copied to
the clipboard.
}
\examples{
\dontrun{
plot2drop("dfg")
plot(1:10)
dev.off()
}
}

