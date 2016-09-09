\name{aout.kernel}
\alias{aout.kernel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in arbitrary univariate data using kernel density estimation
}
\description{Given the arguments of the \code{density}, \code{aout.kernel} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.kernel(data, alpha, plot = TRUE, plottitle = "", kernel = "gaussian", 
nkernel = 1024, kern.bw = "SJ", kern.adj = 1, 
xlim = NA, ylim = NA, outints = FALSE, w = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a vector. The data set to be examined.
}
  \item{alpha}{an atomic vector. Determines the maximum amount of probability mass the outlier region may contain.
}
  \item{plot}{boolean. If \code{TRUE}, a plot of the data and estimated density with shaded outlier region is printed.
}
  \item{plottitle}{character string. Title of the plot.
}
  \item{kernel}{See \code{kernel} in \code{\link[stats:density]{density}}.
}
  \item{nkernel}{See \code{n} in \code{\link[stats:density]{density}}.
}
  \item{kern.bw}{See \code{bw} in \code{\link[stats:density]{density}}.
} 
  \item{kern.adj}{See \code{adjust} in \code{\link[stats:density]{density}}.
}
  \item{xlim}{a vector. Specify if you want to change the x-limits of the plot.
}
  \item{ylim}{a vector. Specify if you want to change the y-limits of the plot.
}
  \item{outints}{boolean. If \code{TRUE}, then the bounds of the inlier-regions and the chosen bandwidth are shown.
}
  \item{w}{a vector. See \code{weights} in \code{\link[stats:density]{density}}.
}
  \item{\dots}{Further arguments for \code{density} and \code{plot}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
If \code{outints = TRUE}, a list of 
\item{Results}{A data frame containing one row for each observation. The observations are labelled 
whether they are outlying, the value of the estimated density at the observation is shown and the bound
of the outlier identifier.}
\item{Bounds.of.Inlier.Regions}{The bounds of the inlier region(s).}
\item{KDE.Chosen.Bandwidth}{The bandwidth that was chosen by \code{density}.}
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{A. Rehage
}
%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
set.seed(23)
tempx <- rnorm(1000, 0, 1)
tempx[1] <- -2.5
aout.kernel(tempx[1:10], alpha = 0.1, kern.adj = 1, xlim = c(-3,3), outints = TRUE)
# not run:
# aout.kernel(tempx[1:200], alpha = 0.1, kern.adj = 1, xlim = c(-3,3))
}
\keyword{nonparametric}