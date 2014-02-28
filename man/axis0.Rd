\name{axis0}
\alias{axis0}
\alias{y0}
\title{Adjust Space Between Axis in ggplot2}
\usage{
axis0(max, scale = "continuous", axis = "y")

y0(max, scale = "continuous")
}
\arguments{
  \item{max}{The upper/right-most edge (upper ylim/xlim
  numeric value).}

  \item{scale}{A ggplot scale type (e.g. "continuous",
  "deiscrete", etc.)}

  \item{axis}{The axis to operate on ("x" or "y").}
}
\description{
\code{axis0} - Adjust Space Between x/y Axis in ggplot2.

\code{y0} - Adjust Space Between y Axis in ggplot2.
}
\examples{
\dontrun{
ggplot(reorder_by(cyl, ~-cyl , mtcars, length), aes(x=as.factor(cyl))) +
    geom_bar()  +
    theme_apa() +
    y0(cushion(as.factor(mtcars$cyl))) +
    xlab("Cylinders") +
    ylab("Total")

library(dplyr)
mtcars2 <- mtcars \%.\% group_by(cyl) \%.\% summarise(n=length(cyl))
ggplot(mtcars2, aes(y=as.factor(cyl), x=n)) +
    geom_point()  +
    theme_apa() +
    x0(16)

ggplot(mtcars, aes(x=hp, y=mpg)) +
  geom_point()  +
  theme_apa() +
  x0(max(mtcars$hp) + 10) +
  y0(max(mtcars$mpg) + 5)
}
}
\references{
\url{http://stackoverflow.com/a/20220737/1000343}.
}
\seealso{
\code{\link[ggplot2]{scale_y_continuous}}
}

