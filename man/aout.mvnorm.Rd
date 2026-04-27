\name{aout.mvnorm}
\alias{aout.mvnorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in multivariate normal data
}
\description{Given the parameters of a multivariate normal distribution, \code{aout.mvnorm} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.mvnorm(data, param, alpha = 0.1, hide.outliers = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a data.frame or matrix. The data set to be examined.
}
  \item{param}{a list. Contains the parameters of the normal distribution: the mean vector \eqn{\mu} and the covariance matrix \eqn{\sigma}.
}
  \item{alpha}{an atomic vector. Determines the maximum amount of probability mass the outlier region may contain. Defaults to 0.1.
}
  \item{hide.outliers}{boolean. Returns the outlier-free data if set to \code{TRUE}. Defaults to \code{FALSE}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Data frame of the input data and an index named \code{is.outlier} that flags the outliers with \code{TRUE}. If hide.outliers is set to \code{TRUE}, a data frame of the outlier-free data.
}
\references{Kuhnt, S.; Rehage, A. (2013) The concept of \eqn{\alpha}-outliers in structured data situations. In C. Becker, R. Fried, S. Kuhnt (Eds.): \emph{Robustness and Complex Data Structures. Festschrift in Honour of Ursula Gather.} Berlin: Springer, 91-108.

}
\author{A. Rehage
}
%\note{
%%  ~~further notes~~
%}
\seealso{\code{\link{dnorm}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
temp <- iris[1:51,-5]
temp.xq <- apply(FUN = median, MARGIN = 2, temp)
aout.mvnorm(as.matrix(temp), param = list(temp.xq, cov(temp)), alpha = 0.001)
}
\keyword{distribution}