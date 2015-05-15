\name{aout.laplace}
\alias{aout.laplace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in Laplace / double exponential data
}
\description{Given the parameters of a Laplace distribution, \code{aout.laplace} identifies \eqn{\alpha}-outliers in a given data set.
}
\usage{
aout.laplace(data, param, alpha = 0.1, hide.outliers = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a vector. The data set to be examined.
}
  \item{param}{a vector. Contains the parameters of the Laplace distribution: \eqn{\mu, \sigma}.
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
\references{Dumonceaux, R.; Antle, C. E. (1973) Discrimination between the log-normal and the Weibull distributions. \emph{Technometrics}, 15 (4), 923-926.

Gather, U.; Kuhnt, S.; Pawlitschko, J. (2003) Concepts of outlyingness for various data structures. In J. C. Misra (Ed.): \emph{Industrial Mathematics and Statistics}. New Delhi: Narosa Publishing House, 545-585.
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
\examples{
# Using the flood data from Dumonceaux and Antle (1973):
temp <- c(0.265, 0.269, 0.297, 0.315, 0.3225, 0.338, 0.379, 0.380, 0.392, 0.402,
         0.412, 0.416, 0.418, 0.423, 0.449, 0.484, 0.494, 0.613, 0.654, 0.74)
aout.laplace(temp, c(median(temp), median(abs(temp - median(temp)))), 0.05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
