\name{theme_map}
\alias{theme_map}
\title{ggplot2 Theme for Mapping.}
\usage{
theme_map()
}
\description{
A ggplot2 theme with no background, gridlines, border,
labels, or ticks.
}
\examples{
\dontrun{
require("maps")
states <- data.frame(map("state", plot=FALSE)[c("x","y")])
(usamap <- qplot(x, y, data=states, geom="path"))
usamap + theme_map()
}
}
\seealso{
\code{\link[ggplot2]{theme}}
}

