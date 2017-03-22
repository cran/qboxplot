\name{qboxplot}
\alias{qboxplot}
\title{Quantile-Based Boxplots}
\description{Produce quantile-based box-and-whisker plot(s) of the given (grouped) values.}
\usage{
qboxplot(x, range=1.5, probs=c(0.25,0.5,0.75), qtype=7, data=parent.frame(),
         width=NULL, varwidth=FALSE, outline=TRUE, names=NULL, plot=TRUE,
         border=par("fg"), col=NULL, log="", pars=list(boxwex=0.8,
         staplewex=0.5, outwex=0.5), horizontal=FALSE, add=FALSE, at=NULL,
         ...)
}
\arguments{
\item{x}{ a formula, such as \code{y ~ grp}, where \code{y} is a numeric vector of data values to be split into groups according to the grouping variable \code{grp} (usually a factor), or a data frame specifying data from which the boxplots are to be produced. }
\item{range}{ this determines how far the plot whiskers extend out from the box. If \code{range} is positive, the whiskers extend to the most extreme data point which is no more than \code{range} times the difference between the value of the upper hinge and the value of the lower hinge from the box. A value of zero causes the whiskers to extend to the data extremes. }
\item{probs}{ numeric vector of values in [0,1] specifying the percentiles of the upper hinge, the midpoint (usually the median) and the lower hinge. }
\item{qtype}{ an integer between 1 and 9 selecting one of the nine quantile algorithms detailed below to be used (see \code{\link{quantile}}). }
\item{data}{ a data.frame (or list) from which the variables in \code{formula} should be taken. }
\item{width}{ a vector giving the relative widths of the boxes making up the plot. }
\item{varwidth}{ if \code{varwidth} is \code{TRUE}, the boxes are drawn with widths proportional to the square-roots of the number of observations in the groups. }
\item{outline}{ if \code{outline} is not \code{TRUE}, the outliers are not drawn. }
\item{names}{ group labels which will be printed under each boxplot. }
\item{plot}{ if \code{TRUE} then a boxplot is produced. If not, the summaries which the boxplots are based on are returned. }
\item{border}{ an optional vector of colours for the outlines of the boxplots. The values in \code{border} are recycled if the length of \code{border} is less than the number of plots. }
\item{col}{ if \code{col} is non-null it is assumed to contain colors to be used to colour the bodies of the box plots. By default they are in the background colour. }
\item{log}{ character indicating if x or y or both coordinates should be plotted in log scale. }
\item{pars}{ a list of (potentially many) more graphical parameters. }
\item{horizontal}{ logical indicating if the boxplots should be horizontal; default \code{FALSE} means vertical boxes. }
\item{add}{ logical, if \code{TRUE} add boxplot to current plot. }
\item{at}{ numeric vector giving the locations where the boxplots should be drawn; defaults to \code{1:n} where \code{n} is the number of boxes. }
\item{\dots}{ other arguments (see \code{\link{boxplot}}). }
}
\value{
List with the following components:
\item{stats}{ a matrix, each column contains the extreme of the lower whisker, the lower hinge, the midpoint (i.e. probs[2]), the upper hinge and the extreme of the upper whisker for one group/plot. }
\item{n}{ a vector with the number of observations in each group. }
\item{out}{ the values of any data points which lie beyond the extremes of the whiskers. }
\item{group}{ a vector of the same length as \code{out} whose elements indicate to which group the outlier belongs. }
\item{names}{ a vector of names for the groups. }
}
\examples{
#Example 1
data = data.frame(a=runif(10), b=runif(10), c=runif(10))
qboxplot(data, range=1.3, probs=c(0.2,0.5,0.7), qtype=6)

#Example 2
qboxplot(count~spray, data=InsectSprays, col="lightgray")

#Example 3
rb = qboxplot(decrease~treatment, data=OrchardSprays, log="y", col="bisque")
title("")
rb

#Example 4
mat = cbind(Uni05=(1:100)/21, Norm=rnorm(100), "5T"=rt(100,df=5),
            Gam2=rgamma(100,shape=2))
qboxplot(as.data.frame(mat))

#Example 5
qboxplot(len~dose, data=ToothGrowth, boxwex=0.25, at=1:3-0.2,
         subset=ToothGrowth$supp=="VC", col="yellow",
         main="Guinea Pigs' Tooth Growth", xlab="Vitamin C dose mg",
         ylab="Tooth length", xlim=c(0.5,3.5), ylim=c(0,35), yaxs="i")

}
\keyword{qboxplot}
