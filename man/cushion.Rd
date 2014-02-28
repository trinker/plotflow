\name{cushion}
\alias{cushion}
\title{Determine Max Value + n Extra}
\usage{
cushion(x, pad = 0.05)
}
\arguments{
  \item{x}{A vector.}

  \item{pad}{A proportion extra to add.}
}
\value{
Returns the max value of a vector + n extra.
}
\description{
Determine max value of a vector plus an additional
pproportion.
}
\note{
If x is a factor or character vector counts for each
category are used as \code{max(n)}.
}
\examples{
cushion(as.factor(mtcars$cyl))
cushion(mtcars$cyl)
cushion(mtcars$cyl, .5)

\dontrun{
ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) +
    geom_bar()  +
    theme_apa() +
    y0(cushion(as.factor(mtcars$cyl))) +
    xlab("Cylinders") +
    ylab("Total")
}
}

