\name{print.mdf}
\alias{print.mdf}
\title{\code{print} method for \code{"mdf"} class objects resulted from the \code{mudfold} function.
}
\description{S3 generic function for printing \code{"mdf"} class objects.
}

\usage{
\method{print}{mdf}(x, ...)
}

\arguments{
   \item{x}{Object of class \code{"mdf"}}
   \item{...}{further arguments passed on to the \code{print} method.}
}


\author{Spyros E. Balafas (auth.), Wim P. Krijnen (auth.), Wendy J. Post (contr.), Ernst C. Wit (auth.)

Maintainer: Spyros E. Balafas (\email{s.balafas@rug.nl})
}

\references{
W.H. Van Schuur.(1984). \emph{Structure in Political Beliefs: A New Model for
Stochastic Unfolding with Application to European Party Activists}. CT Press.

W.J. Post. (1992). \emph{Nonparametric Unfolding Models: A Latent Structure Approach}. M
& T series. DSWO Press.

W.J. Post. and T.AB. Snijders (1993). \emph{Nonparametric unfolding models for dichotomous data}. Methodika.


}

\examples{
\dontrun{
data(ANDRICH)
fit <- mudfold(ANDRICH)
fit
print(fit)
}
}
\keyword{methods}


