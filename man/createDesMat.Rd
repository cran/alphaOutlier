\name{createDesMat}
\alias{createDesMat}
\title{Create design matrix for log-linear models of contingency tables
}
\description{This function creates a design matrix for contingency tables and is particularly useful for log-linear Poisson models. It uses effect coding of the variables: First the rows of the contingency table from top to bottom, then the columns from left to right.
}
\usage{
createDesMat(n, p)
}
\arguments{
  \item{n}{Number of rows of the corresponding contingency table.
}
  \item{p}{Number of columns of the corresponding contingency table.
}
}
\value{A (n+p-1) times (n*p) design matrix.
}
\references{Kuhnt, S.; Rapallo, F.; Rehage, A. (2014) Outlier detection in contingency tables based on minimal patterns. \emph{Statistics and Computing} 24 (3), 481-491.
}
\author{A. Rehage
}
\examples{createDesMat(3, 5)
}
\keyword{design}