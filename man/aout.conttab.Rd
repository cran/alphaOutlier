\name{aout.conttab}
\alias{aout.conttab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find \eqn{\alpha}-outliers in two-way contingency tables
}
\description{This is a wrapper function for \code{\link{aout.pois}}. We assume that each entry of a contingency table can be seen as a realization of a Poisson random variable. The parameter \eqn{\lambda} of each cell can either be set by the user or estimated. Given the parameters, \code{aout.conttab} identifies \eqn{\alpha}-outliers in a given contingency table.
}
\usage{
aout.conttab(data, param, alpha = 0.1, hide.outliers = FALSE, show.estimates = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a matrix or data.frame. The contingency table to be examined.
%%     ~~Describe \code{data} here~~
}
  \item{param}{a character string from \code{c("ML", "L1", "MP")} or a vector containing the parameters of each cell of the Poisson distribution: \eqn{\lambda}. \code{"ML"} yields the maximum likelihood estimate from the log-linear Poisson model using a suitable design matrix. \code{"L1"} yields the L1-estimate from \code{\link[quantreg:rq.fit.fnc]{rq.fit.fnc}}. \code{"MP"} yields the Median Polish estimate. If the parameter vector is given by the user, it is necessary that the contingency table was filled \code{byrow = FALSE}.
}
  \item{alpha}{an atomic vector. Determines the maximum amount of probability mass the outlier region may contain. Defaults to 0.1.
}
  \item{hide.outliers}{boolean. Returns the outlier-free data if set to \code{TRUE}. Defaults to \code{FALSE}.
}
  \item{show.estimates}{boolean. Returns \eqn{\hat{\lambda}} for each cell if set to \code{TRUE}. Defaults to \code{FALSE}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Data frame of the vectorized input data and, if desired, an index named \code{is.outlier} that flags the outliers with \code{TRUE} and a vector named \code{param} containing the estimated lambdas.
}
\references{Kuhnt, S. (2000) Ausreisseridentifikation im Loglinearen Poissonmodell fuer Kontingenztafeln unter Einbeziehung robuster Schaetzer. Ph.D. Thesis. Universitaet Dortmund, Dortmund. Fachbereich Statistik.

Kuhnt, S.; Rapallo, F.; Rehage, A. (2014) Outlier detection in contingency tables based on minimal patterns. \emph{Statistics and Computing} 24 (3), 481-491.
}
\author{A. Rehage
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link[quantreg:rq.fit.fnc]{rq.fit.fnc}}, \code{\link{aout.pois}}
}
\examples{
aout.conttab(data = HairEyeColor[,,1], param = "L1", alpha = 0.01, show.estimates = TRUE)
aout.conttab(data = HairEyeColor[,,1], param = "ML", alpha = 0.01, show.estimates = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}