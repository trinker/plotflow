\name{label_rename_margin}
\alias{label_rename_margin}
\title{Rename the Margins of ggplot2 facet_grid}
\usage{
label_rename_margin(newname = "Total")
}
\arguments{
  \item{newname}{The new strip.text name to overwrite
  (all).}
}
\description{
Convert the default (all) when seeting ggplot2's
\code{\link[ggplot2]{facet_grid}} to \code{margins = TRUE}.
}
\examples{
ggplot(mtcars, aes(cyl)) +
    geom_point(stat="bin", size = 2,
        aes(shape = gear, position = "stack")) +
    facet_grid(carb ~ gear, margins = TRUE,
        labeller=label_rename_margin("Total"))

ggplot(mtcars, aes(cyl)) +
    geom_point(stat="bin", size = 2,
        aes(shape = gear, position = "stack")) +
    facet_grid(carb ~ gear, margins = "gear",
        labeller=label_rename_margin("Total"))
}
\references{
\url{http://permalink.gmane.org/gmane.comp.lang.r.ggplot2/5279}
}
\keyword{margin}

