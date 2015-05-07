\name{aout.logis}
\alias{aout.logis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in logistic data
}
\description{Given the parameters of a logistic distribution, \code{aout.logis} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.logis(data, param, alpha = 0.1, hide.outliers = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a vector. The data set to be examined.
}
  \item{param}{a vector. Contains the parameters of the logistic distribution: \eqn{\mu, \sigma}.
}
  \item{alpha}{an atomic vector. Determines the maximum amount of probability mass the outlier region may contain. Defaults to 0.1.
}
  \item{hide.outliers}{boolean. Returns the outlier-free data if set to \code{TRUE}. Defaults to \code{FALSE}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Data frame of the input data and an index named \code{is.outlier} that flags the outliers with \code{TRUE}. If hide.outliers is set to \code{TRUE}, a simple vector of the outlier-free data.
}
\references{Balakrishnan, N. (1992) Maximum likelihood estimation based on complete and type II censored samples. In N. Balakrishnan (Ed.): \emph{Handbook of the Logistic Distribution}. Dekker, New York, 49-78.

Gather, U.; Kuhnt, S.; Pawlitschko, J. (2003) Concepts of outlyingness for various data structures. In J. C. Misra (Ed.): \emph{Industrial Mathematics and Statistics}. New Delhi: Narosa Publishing House, 545-585.
}
\author{A. Rehage
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{dlogis}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{# Data example from Balakrishnan (1967)
lifetime <- c(785, 855, 905, 918, 919, 920, 929, 936, 948, 950)
aout.logis(lifetime, c(949.9, 63.44))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
