\name{aout.norm}
\alias{aout.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in normal data
}
\description{Given the parameters of a normal distribution, \code{aout.norm} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.norm(data, param = c(0, 1), alpha = 0.1, hide.outliers = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a vector. The data set to be examined.
}
  \item{param}{a vector. Contains the parameters of the normal distribution: \eqn{\mu, \sigma}.
}
  \item{alpha}{an atomic vector. Determines the maximum amount of probability mass the outlier region may contain. Defaults to 0.1.
}
  \item{hide.outliers}{boolean. Returns the outlier-free data if set to \code{TRUE}. Defaults to \code{FALSE}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Data frame of the input data and an index named \code{is.outlier} that flags the outliers with \code{TRUE}. If \code{hide.outliers} is set to \code{TRUE}, a simple vector of the outlier-free data.
}
\references{Gather, U.; Kuhnt, S.; Pawlitschko, J. (2003) Concepts of outlyingness for various data structures. In J. C. Misra (Ed.): \emph{Industrial Mathematics and Statistics}. New Delhi: Narosa Publishing House, 545-585.
}
\author{A. Rehage
}
%\note{
%%  ~~further notes~~
%}
\seealso{
\code{\link{dnorm}}
}
\examples{iris.setosa <- iris[1:51, 4]
# implosion breakdown point:
aout.norm(data = iris.setosa, param = c(median(iris.setosa), mad(iris.setosa)), 
          alpha = 0.01) 
# better:
aout.norm(data = iris.setosa, param = c(median(iris.setosa), sd(iris.setosa)), 
          alpha = 0.01) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
