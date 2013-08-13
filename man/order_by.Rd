\name{order_by}
\alias{order_by}
\title{Order a Factor by Numeric Variable(s)}
\usage{
  order_by(x, fact, by, df = TRUE)
}
\arguments{
  \item{x}{A \code{data.frame} object.}

  \item{fact}{The factor to be reordered (re-leveled).}

  \item{by}{A formula to order the factor by.}

  \item{df}{logical.  If \code{TRUE} a dataframe is
  returned.  If \code{FALSE} a factor vector is returned.}
}
\value{
  Returns a re-ordered (re-leveled) dataframe or factor
  vector.
}
\description{
  Create a new dataframe with a factor reordered
  (re-leveled) by numeric variable(s).
}
\examples{
\dontrun{
## Make a fake data set
dat <- aggregate(cbind(mpg, hp, disp)~carb, mtcars, mean)
dat$carb <- factor(dat$carb)

## compare levels (data set looks the same though)
dat$carb
order_by(x = dat, carb, ~-hp + -mpg)$carb

library(ggplot2)
## Unordered bars
ggplot(dat, aes(x=carb, y=mpg)) +
    geom_bar(stat="identity") +
    coord_flip()

## Ordered bars
ggplot(order_by(x = dat, carb, ~mpg), aes(x=carb, y=mpg)) +
    geom_bar(stat="identity") +
    coord_flip()

## Return just the vector with new levels
order_by(x = dat, carb, ~-hp + -mpg, FALSE)
}
}
\author{
  Thomas Wutzler and Tyler Rinker <tyler.rinker@gmail.com>.
}
\references{
  The majority of this code is taken directly from Thomas
  Wutzler's blog post:
  \url{http://rwiki.sciviews.org/doku.php?id=tips\%3adata-frames\%3asort}
}
\keyword{factor,}
\keyword{order,}
\keyword{plot}
\keyword{sort,}

