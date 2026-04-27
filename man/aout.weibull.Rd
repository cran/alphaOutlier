\name{aout.weibull}
\alias{aout.weibull}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in Weibull data
}
\description{Given the parameters of a Weibull distribution, \code{aout.weibull} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.weibull(data, param, alpha = 0.1, hide.outliers = FALSE, lower = auto.l, 
             upper = auto.u, method.in = "Broyden", global.in = "qline", 
             control.in = list(sigma = 0.1, maxit = 1000, xtol = 1e-12, 
                               ftol = 1e-12, btol = 1e-04))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a vector. The data set to be examined.
}
  \item{param}{a vector. Contains the parameters of the Weibull distribution: \eqn{\beta, \lambda}.
}
  \item{alpha}{an atomic vector. Determines the maximum amount of probability mass the outlier region may contain. Defaults to 0.1.
}
  \item{hide.outliers}{boolean. Returns the outlier-free data if set to \code{TRUE}. Defaults to \code{FALSE}.
}
  \item{lower}{an atomic vector. First element of \code{x} from \code{\link[nleqslv:nleqslv]{nleqslv}}.
}
  \item{upper}{an atomic vector. Second element of \code{x} from \code{\link[nleqslv:nleqslv]{nleqslv}}.
}
  \item{method.in}{See \code{method} in \code{\link[nleqslv:nleqslv]{nleqslv}}
%%     ~~Describe \code{method.in} here~~
}
  \item{global.in}{See \code{global} in \code{\link[nleqslv:nleqslv]{nleqslv}}
}
  \item{control.in}{See \code{control} in \code{\link[nleqslv:nleqslv]{nleqslv}}
}
}
\details{The \eqn{\alpha}-outlier region of a Weibull distribution is generally not available in closed form or via the tails, such that a non-linear equation system has to be solved.
}
\value{Data frame of the input data and an index named \code{is.outlier} that flags the outliers with \code{TRUE}. If hide.outliers is set to \code{TRUE}, a simple vector of the outlier-free data.
}
\references{
Dodson, B. (2006) \emph{The Weibull Analysis Handbook}. American Society for Quality, 2nd edition.
}
\author{A. Rehage
%%  ~~who you are~~
}
%\note{
%%  ~~further notes~~
%}
\seealso{\code{\link{dweibull}}, \code{\link[nleqslv:nleqslv]{nleqslv}}
}
\examples{
# lifetime data example taken from Table 2.2, Dodson (2006)
temp <- c(12.5, 24.4, 58.2, 68.0, 69.1, 95.5, 96.6, 97.0, 
          114.2, 123.2, 125.6, 152.7)
aout.weibull(temp, c(2.25, 97), 0.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}