\name{aout.pois}
\alias{aout.pois}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in Poisson count data
}
\description{Given the parameters of a Poisson distribution, \code{aout.pois} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.pois(data, param, alpha = 0.1, hide.outliers = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a vector. The data set to be examined.
}
  \item{param}{a vector. Contains the parameter of the Poisson distribution: \eqn{\lambda}.
}
  \item{alpha}{an atomic vector. Determines the maximum amount of probability mass the outlier region may contain. Defaults to 0.1.
}
  \item{hide.outliers}{boolean. Returns the outlier-free data if set to \code{TRUE}. Defaults to \code{FALSE}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Data frame of the input data and an index named \code{is.outlier} that flags the outliers with \code{TRUE}. If hide.outliers is set to \code{TRUE}, a simple vector of the outlier-free data.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{A. Rehage
%%  ~~who you are~~
}
%\note{
%%  ~~further notes~~
%}
\seealso{\code{\link{dpois}}
}
\examples{
aout.pois(data = c(discoveries), param = median(discoveries), alpha = 0.01)
}
\keyword{distribution}