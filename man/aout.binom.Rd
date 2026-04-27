\name{aout.binom}
\alias{aout.binom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in Binomial data
}
\description{Given the parameters of a Binomial distribution, \code{aout.binom} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.binom(data, param, alpha = 0.1, hide.outliers = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a vector. The data set to be examined.
}
  \item{param}{a vector. Contains the parameters of the Binomial distribution, \eqn{N} and \eqn{p}.
}
  \item{alpha}{an atomic vector. Determines the maximum amount of probability mass the outlier region may contain. Defaults to 0.1.
}
  \item{hide.outliers}{boolean. Returns the outlier-free data if set to \code{TRUE}. Defaults to \code{FALSE}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Data frame of the input data and an index named \code{is.outlier} that flags the outliers with \code{TRUE}. If hide.outliers is set to \code{TRUE}, a simple vector of the outlier-free data.
}
%\references{
%}
\author{
A. Rehage
}
\seealso{\code{\link{dbinom}}
}
\examples{
data(uis)
medbeck <- median(uis$BECK) 
aout.binom(data = uis$BECK, param = c(54, medbeck/54), alpha = 0.001)
}
\keyword{distribution}
