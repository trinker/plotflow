\name{qgrid}
\alias{qgrid}
\title{Quickly Set ggplot2 panel.grid}
\usage{
qgrid(xmaj = "grey94", xmin = "grey94", ymaj = "grey94",
  ymin = "grey94", x, y)
}
\arguments{
  \item{xmaj}{A colour choice for panel.grid.major.x.  Use
  \code{NULL} to remove.}

  \item{xmin}{A colour choice for panel.minor.x.  Use
  \code{NULL} to remove.}

  \item{ymaj}{A colour choice for panel.grid.major.y.  Use
  \code{NULL} to remove.}

  \item{ymin}{A colour choice for panel.minor.y.  Use
  \code{NULL} to remove.}

  \item{x}{A colour choice.  Sets both panel.grid.major.x
  and panel.minor.x. Overides \code{xmin} and \code{xmax}.
  Use \code{NULL} to remove.}

  \item{y}{A colour choice.  Sets both panel.grid.major.y
  and panel.minor.y. Overides \code{ymin} and \code{ymax}.
  Use \code{NULL} to remove.}
}
\description{
Set ggplot2's panel.grid quickly.
}
\examples{
\dontrun{
ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) +
    geom_bar()  +
    theme_apa() +
    y0(cushion(as.factor(mtcars$cyl))) +
    xlab("Cylinders") +
    ylab("Total") + qgrid()

ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) +
    geom_bar()  +
    theme_apa() +
    y0(cushion(as.factor(mtcars$cyl))) +
    xlab("Cylinders") +
    ylab("Total") + qgrid(x=NULL)

ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) +
    geom_bar()  +
    theme_apa() +
    y0(cushion(as.factor(mtcars$cyl))) +
    xlab("Cylinders") +
    ylab("Total") + qgrid(x=NULL, y="red")
}
}
\seealso{
\code{\link[ggplot2]{theme}}
}
\keyword{gridelines}

