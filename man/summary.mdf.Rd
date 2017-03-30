\name{summary.mdf}
\alias{summary.mdf}

\title{
\code{summary} method for \code{mdf} class objects.
}
\description{S3 method for summarize \code{mdf} class objects
}
\usage{
\method{summary}{mdf}(object,...)
}

\arguments{
   \item{object}{Object of class \code{mdf}}
  \item{\dots}{Ignored}
}

\details{
Summarizing information from the MUDFOLD method object which has been calculated from \code{\link{mudfold}} function.
}

\value{ returns an \code{m+1x9} data frame with parameters concerning the fit of each item in the scale as well as for the quality of the scale as a whole. The columns of the resulting data frame are the following.
    \item{index}{: \code{m} selected items in the estimated MUDFOLD rank order. In the \code{total} cell information for the scale as a whole is given wherever is available.}
    \item{samp.size}{: The sample size of the data.}
    \item{freq}{: observed frequency of positive responses.} 
    \item{prop}{: observed proportion of positive responses.}
    \item{std.err}{: standard error for the proportion of positive responses.}
    \item{Obs.err}{: observed response errors.}
    \item{Exp.err}{: expected response errors.}
    \item{Iso}{: iso statistic.}
    \item{Scalab.H}{: scalability coefficients.}
}



\author{Spyros E. Balafas.

Maintainer: Spyros E. Balafas (\email{sp.balafas@gmail.com})
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
summary(fit)
}
}
\keyword{methods}


