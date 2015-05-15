\name{aout.chisq}
\alias{aout.chisq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in \eqn{\chi^2} data
}
\description{Given the parameters of a \eqn{\chi^2} distribution, \code{aout.chisq} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.chisq(data, param, alpha = 0.1, hide.outliers = FALSE, ncp = 0, lower = auto.l,
           upper = auto.u, method.in = "Newton", global.in = "gline", 
           control.in = list(sigma = 0.1, maxit = 1000, xtol = 1e-12, 
                             ftol = 1e-12, btol = 1e-04))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a vector. The data set to be examined.
}
  \item{param}{an atomic vector. Contains the degrees of freedom of the \eqn{\chi^2} distribution.
}
  \item{alpha}{an atomic vector. Determines the maximum amount of probability mass the outlier region may contain. Defaults to \eqn{0.1}.
}
  \item{hide.outliers}{boolean. Returns the outlier-free data if set to \code{TRUE}. Defaults to \code{FALSE}.
}
  \item{ncp}{an atomic vector. Determines the non-centrality parameter of the \eqn{\chi^2} distribution. Defaults to 0.
}
  \item{lower}{an atomic vector. First element of \code{x} from \code{\link[nleqslv:nleqslv]{nleqslv}}.
}
  \item{upper}{an atomic vector. Second element of \code{x} from \code{\link[nleqslv:nleqslv]{nleqslv}}.
}
  \item{method.in}{See \code{method} in \code{\link[nleqslv:nleqslv]{nleqslv}}.
}
  \item{global.in}{See \code{global} in \code{\link[nleqslv:nleqslv]{nleqslv}}.
}
  \item{control.in}{See \code{control} in \code{\link[nleqslv:nleqslv]{nleqslv}}.
}
}
\details{The \eqn{\alpha}-outlier region of a \eqn{\chi^2} distribution is generally not available in closed form or via the tails, such that a non-linear equation system has to be solved.
}
\value{
Data frame of the input data and an index named \code{is.outlier} that flags the outliers with \code{TRUE}. If hide.outliers is set to \code{TRUE}, a simple vector of the outlier-free data.
}
%\references{
%}
\author{A. Rehage
}
\seealso{\code{\link{dchisq}}
}
\examples{
aout.chisq(chisq.test(occupationalStatus)$statistic, 49)
}
\keyword{distribution}
