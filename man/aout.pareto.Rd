\name{aout.pareto}
\alias{aout.pareto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in Pareto data
}
\description{Given the parameters of a Pareto distribution, \code{aout.pareto} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.pareto(data, param, alpha = 0.1, hide.outliers = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a vector. The data set to be examined.
}
  \item{param}{a vector. Contains the parameters of the Pareto distribution: \eqn{\lambda, \theta}.
}
  \item{alpha}{an atomic vector. Determines the maximum amount of probability mass the outlier region may contain. Defaults to 0.1.
}
  \item{hide.outliers}{boolean. Returns the outlier-free data if set to \code{TRUE}. Defaults to \code{FALSE}.
}
}
\details{
We use the Pareto distribution with Lebesgue-density \eqn{ f(x) = \frac{\lambda \theta^{\lambda}}{x^{\lambda + 1}}}.
}
\value{Data frame of the input data and an index named \code{is.outlier} that flags the outliers with \code{TRUE}. If hide.outliers is set to \code{TRUE}, a simple vector of the outlier-free data.
}
\references{Gather, U.; Kuhnt, S.; Pawlitschko, J. (2003) Concepts of outlyingness for various data structures. In J. C. Misra (Ed.): \emph{Industrial Mathematics and Statistics}. New Delhi: Narosa Publishing House, 545-585.
}
\author{A. Rehage
}
%\note{
%%  ~~further notes~~
%}
\seealso{\code{\link{citiesData}}}
\examples{
data(citiesData)
aout.pareto(citiesData[[1]], c(1.31, 14815), alpha = 0.01)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
