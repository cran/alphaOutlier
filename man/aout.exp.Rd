\name{aout.exp}
\alias{aout.exp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in exponentially distributed data
}
\description{Given the parameters of an exponential distribution, \code{aout.exp} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.exp(data, param, alpha = 0.1, hide.outliers = FALSE, theta = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a vector. The data set to be examined.
}
  \item{param}{an atomic vector. Contains the parameter of the exponential distribution.
}
  \item{alpha}{an atomic vector. Determines the maximum amount of probability mass the outlier region may contain. Defaults to 0.1.
}
  \item{hide.outliers}{boolean. Returns the outlier-free data if set to \code{TRUE}. Defaults to \code{FALSE}.
}
  \item{theta}{an atomic vector. Determines the lower bound of the support of the exponential distribution. Defaults to 0.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Data frame of the input data and an index named \code{is.outlier} that flags the outliers with \code{TRUE}. If hide.outliers is set to \code{TRUE}, a simple vector of the outlier-free data.
}
\references{Gather, U.; Kuhnt, S.; Pawlitschko, J. (2003) Concepts of outlyingness for various data structures. In J. C. Misra (Ed.): \emph{Industrial Mathematics and Statistics}. New Delhi: Narosa Publishing House, 545-585.
}
\author{A. Rehage
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{dexp}}
}
\examples{
aout.exp(attenu[,5], median(attenu[,5]), alpha = 0.05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
