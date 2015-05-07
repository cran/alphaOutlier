\name{alphaOutlier-package}
\alias{alphaOutlier-package}
\alias{alphaOutlier}
\docType{package}
\title{
Obtain \eqn{\alpha}-outlier regions for well-known probability distributions
}
\description{
Given the parameters of a distribution, the package uses the concept of \eqn{\alpha}-outliers by Davies and Gather (1993) to flag outliers in a data set. 
}
\details{The structure of the package is as follows: \code{aout.[Distribution]} is the name of the function which returns the \eqn{\alpha}-outlier region of a random variable following \code{[Distribution]}. The names of the distributions are abbreviated as in the \code{d, p, q, r} functions. Use pre-specified or robustly estimated parameters from your data to obtain reasonable results. The sample size should be taken into account when choosing \code{alpha}, for example Gather et al. (2003) propose \eqn{\alpha_N = 1 - (1 - \alpha)^{1/N}}.
}
\author{A. Rehage, S. Kuhnt
}
\references{Davies, L.; Gather, U. (1993) The identification of multiple outliers, \emph{Journal of the American Statistical Association}, \bold{88} 423, 782-792.

Gather, U.; Kuhnt, S.; Pawlitschko, J. (2003) Concepts of outlyingness for various data structures. In J. C. Misra (Ed.): \emph{Industrial Mathematics and Statistics}. New Delhi: Narosa Publishing House, 545-585.
}

\keyword{package
distribution
robust}
\seealso{
\code{\link[nleqslv:nleqslv]{nleqslv}},
\code{\link[Rsolnp:solnp]{solnp}},
\code{\link[quantreg:rq.fit.fnc]{rq.fit.fnc}}
}
\examples{

iris.setosa <- iris[1:51, 4]
aout.norm(data = iris.setosa, param = c(mean(iris.setosa), sd(iris.setosa)), alpha = 0.01)
aout.pois(data = warpbreaks[,1], param = mean(warpbreaks[,1]), alpha = 0.01, 
          hide.outliers = TRUE)
}
