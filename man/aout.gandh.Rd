\name{aout.gandh}
\alias{aout.gandh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in data from the family of \eqn{g}-and-\eqn{h} distributions
}
\description{Given the parameters of a \eqn{g}-and-\eqn{h} distribution, \code{aout.gandh} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.gandh(data, param, alpha = 0.1, hide.outliers = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a vector. The data set to be examined.
}
  \item{param}{a vector. Contains the parameters of the \eqn{g}-and-\eqn{h} distribution: median, scale, \eqn{g}, \eqn{h}.
}
  \item{alpha}{an atomic vector. Determines the maximum amount of probability mass the outlier region may contain. Defaults to 0.1.
}
  \item{hide.outliers}{boolean. Returns the outlier-free data if set to \code{TRUE}. Defaults to \code{FALSE}.
}
}
\details{The concept of \eqn{\alpha}-outliers is based on the p.d.f. of the random variable. Since for \eqn{g}-and-\eqn{h} distributions this does not exist in closed form, the computation of the outlier region is based on an optimization of the quantile function with side conditions. 
}
\value{Data frame of the input data and an index named \code{is.outlier} that flags the outliers with \code{TRUE}. If hide.outliers is set to \code{TRUE}, a simple vector of the outlier-free data.
}
\references{Xu, Y.; Iglewicz, B.; Chervoneva, I. (2014) Robust estimation of the parameters of g-and-h distributions, with applications to outlier detection. \emph{Computational Statistics and Data Analysis} 75, 66-80.
}
\author{A. Rehage
}
\note{
Makes use of \code{\link[Rsolnp:solnp]{solnp}}.
}
%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
durations <- faithful$eruptions
aout.gandh(durations, c(4.25, 1.14, 0.05, 0.05), alpha = 0.1)
}
\keyword{distribution}
