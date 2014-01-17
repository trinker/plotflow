\name{fill_by_col}
\alias{fill_by_col}
\title{Fill facet_wrap By Column}
\usage{
fill_by_col(dataframe, fact, ncol = 2)
}
\arguments{
  \item{dataframe}{A data.frame object.}

  \item{fact}{The factor to be reordered (re-leveled).}

  \item{ncol}{Number of columns to make the plot}
}
\description{
Fill ggplot2's facet_wrap by column rather than row.
}
\examples{
library(ggplot2); library(reshape2)
dat <- aggregate(cbind(vs, am, gear, cyl) ~ carb, mtcars, sum)
dat$carb <- factor(dat$carb, rev(dat$carb[order(rowSums(dat[, -1]))]))
mdat <- melt(dat)

ggplot(mdat, aes(x = variable)) +
  geom_bar(stat="bin", aes(fil=variable, weights = value)) +
  facet_wrap(~carb, ncol = 2) + coord_flip()

ggplot(fill_by_col(mdat, "carb"), aes(x = variable)) +
  geom_bar(stat="bin", aes(fil=variable, weights = value)) +
  facet_wrap(~carb, ncol = 2) + coord_flip()

ggplot(fill_by_col(mdat, "carb", 3), aes(x = variable)) +
  geom_bar(stat="bin", aes(fil=variable, weights = value)) +
  facet_wrap(~carb, ncol = 3) + coord_flip()
}

