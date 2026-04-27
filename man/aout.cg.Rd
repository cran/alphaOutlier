\name{aout.cg}
\alias{aout.cg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in conditional Gaussian data
}
\description{Given the parameters of a conditional Gaussian distribution, \code{aout.cg} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.cg(data, param, alpha = 0.1, hide.outliers = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a matrix. First column: Class of the value, coded with an integer between 1 and d, where d is the number of classes. Second column: The value as a realization of a univariate normal with parameters \eqn{\mu} and \eqn{\sigma}. The data set to be examined.
}
  \item{param}{a list with three elements:
   \code{p}: d-dimensional vector of probabilities of the classes.
   \code{mu}: d-dimensional vector of univariate mean values of each class.
   \code{sigma}: d-dimensional vector of univariate standard errors of each class
}
  \item{alpha}{an atomic vector. Determines the maximum amount of probability mass the outlier region may contain. Defaults to 0.1.
}
  \item{hide.outliers}{boolean. Returns the outlier-free data if set to \code{TRUE}. Defaults to \code{FALSE}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Data frame of the input data and an index named \code{is.outlier} that flags the outliers with \code{TRUE}. If \code{hide.outliers} is set to \code{TRUE}, a data frame of the outlier-free data.
}
\references{Edwards, D. (2000) \emph{Introduction to Graphical Modelling}. 2nd edition, Springer, New York.

Kuhnt, S.; Rehage, A. (2013) The concept of \eqn{\alpha}-outliers in structured data situations. In C. Becker, R. Fried, S. Kuhnt (Eds.): \emph{Robustness and Complex Data Structures. Festschrift in Honour of Ursula Gather.} Berlin: Springer, 91-108.
}
\author{A. Rehage
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{# Rats' weights data example taken from Edwards (2000)
ratweight <- cbind(Drug = c(1, 1, 2, 3, 1, 1, 2, 3, 1, 2, 3, 3, 1, 2, 2, 3, 1, 
                            2, 2, 3, 1, 2, 3, 3), 
                   Week1 = c(5, 7, 9, 14, 7, 8, 7, 14, 9, 7, 21, 12, 5, 7, 6, 
                             17, 6, 10, 6, 14, 9, 8, 16, 10))
aout.cg(ratweight, 
        list(p = c(1/3, 1/3, 1/3), mu = c(7, 7, 14), sigma = c(1.6, 1.4, 3.3)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}