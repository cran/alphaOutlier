\name{aout.hyper}
\alias{aout.hyper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in hypergeometric data
}
\description{Given the parameters of a hypergeometric distribution, \code{aout.hyper} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.hyper(data, param, alpha = 0.1, hide.outliers = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a vector. The data set to be examined.
}
  \item{param}{a vector. Contains the parameters of the hypergeometric distribution: \eqn{m, n, k}.
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
\author{
A. Rehage}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{Hypergeometric}}
}
\examples{
set.seed(1)
lotto6aus49 <- rhyper(100, 6, 43, 6) 
aout.hyper(lotto6aus49, c(6, 43, 6), 0.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}