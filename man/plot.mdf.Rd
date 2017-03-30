\name{plot.mdf}
\alias{plot.mdf}
\title{\code{plot} method for \code{mdf} class objects.
}
\description{S3 method for \code{mdf} class objects. \code{plot} method, is plotting the rows of the Conditional Adjacency matrix which are nonparametric as estimates of the item response functions.
}
\usage{
\method{plot}{mdf}(x,select=NULL,...)
}
\arguments{
    \item{x}{Object of class \code{mdf}}
    \item{select}{: in this argument we can provide a subset of item names from the MUDFOLD scale that we wish them to be explicitly plotted. If \code{select=NULL} the estimated IRF for each item in the scale is plotted.}
    \item{\dots}{Ignored}
}

\details{
The \code{plot} method is used to obtain a graphical representation of the estimated item response functions. As estimates of the IRFs are considered the rows of the conditional Adjacency matrix.
}

\author{Spyros E. Balafas.

Maintainer: Spyros E. Balafas (\email{s.balafas@rug.nl})
}

\references{
W.H. Van Schuur.(1984). \emph{Structure in Political Beliefs: A New Model for
Stochastic Unfolding with Application to European Party Activists}. CT Press.

W.J. Post. (1992). \emph{Nonparametric Unfolding Models: A Latent Structure Approach}. M
& T series. DSWO Press.

W.J. Post. and T.AB. Snijders. (1993).Nonparametric unfolding models for dichotomous
data. Methodika.


}

\examples{
\dontrun{
data(ANDRICH)
fit <- mudfold(ANDRICH)
plot(fit)
plot(fit,select="DONTBELIEV")
}
}
\keyword{methods}

