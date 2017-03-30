\name{print.mdf}
\alias{print.mdf}
\title{\code{print} method for \code{mdf} class objects.
}
\description{S3 method for \code{print} \code{mdf} class objects.
}

\usage{
\method{print}{mdf}(x, Item.info= FALSE, Diagnostics= FALSE,...)
}

\arguments{
   \item{x}{Object of class \code{mdf}}
   \item{Item.info}{logical argument. If \code{Item.info=TRUE} then additional information for each item in the scale is printed.}
   \item{Diagnostics}{logical argument. If \code{Diagnostics= TRUE} then the diagnostic matrices for the item order are printed.}
   \item{...}{other arguments will be ignored.}
}


\author{Spyros E. Balafas.

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
print(fit,Item.info= TRUE, Diagnostics= TRUE)
}
}
\keyword{methods}


