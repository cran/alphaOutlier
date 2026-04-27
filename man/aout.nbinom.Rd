\name{aout.nbinom}
\alias{aout.nbinom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in negative Binomial data
}
\description{Given the parameters of a negative Binomial distribution, \code{aout.nbinom} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.nbinom(data, param, alpha = 0.1, hide.outliers = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a vector. The data set to be examined.
}
  \item{param}{a vector. Contains the parameters of the negative Binomial distribution: \eqn{N, p}.
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
%% ~put references to the literature/web site here ~
%}
\author{A. Rehage
%%  ~~who you are~~
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{dnbinom}}, \code{\link{daysabs}}
}
\examples{
data(daysabs)
aout.nbinom(daysabs, c(8, 0.6), 0.05)
}
\keyword{distribution}
