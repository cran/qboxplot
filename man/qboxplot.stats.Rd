\name{qboxplot.stats}
\alias{qboxplot.stats}
\title{Helper Function For \code{qboxplot}}
\description{Produce quantile-based box-and-whisker plot(s) of the given (grouped) values.}
\usage{
qboxplot.stats(x, probs, qtype, range, output="all")
}
\arguments{
\item{x}{ a numeric vector of data values from which to calculate the requested statistics. }
\item{probs}{ numeric vector of values in [0,1] specifying the percentiles of the upper hinge, the midpoint (usually the median) and the lower hinge. }
\item{qtype}{ an integer between 1 and 9 indicating which one of the nine quantile algorithms to use (see \code{\link{quantile}}). }
\item{output}{ limit the output to \code{"quantiles"}, \code{"outliers"} or \code{"n"} (see below), or, if set to \code{"all"} (the default), outputs a list containing all three. }
\item{range}{ this determines how far the plot whiskers extend out from the box. If \code{range} is positive, the whiskers extend to the most extreme data point which is no more than \code{range} times the difference in the value of the upper hinge and the value of the lower hinge from the box. A value of zero causes the whiskers to extend to the data extremes. }
}
\value{
List with the following components:
\item{quantiles}{ a matrix, each column contains the extreme of the lower whisker, the lower hinge, the median, the upper hinge and the extreme of the upper whisker for one group/plot. }
\item{outliers}{ a vector with the number of observations in each group. }
\item{n}{ the values of any data points which lie beyond the extremes of the whiskers. }
}
\examples{
x = runif(100)
stats = qboxplot.stats(x, probs=c(0.4,0.5,0.6), qtype=7, range=1.5)
stats
}
\keyword{qboxplot.stats}
