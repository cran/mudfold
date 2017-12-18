\name{summary.mdf}
\alias{summary.mdf}

\title{
\code{summary} method for S3 class \code{mdf} objects.
}
\description{Generic function that is used in order to summarize information from \code{mdf} class objects.
}
\usage{
\method{summary}{mdf}(object,...)
}

\arguments{
   \item{object}{Object of class \code{mdf} resulted from the function \code{mudfold} or \code{as.mudfold}. }
  \item{\dots}{Other arguments passed on to method \code{summary}. Not currently used.}
}

\details{
A summary of the MUDFOLD scale that has been calculated with the \code{\link{mudfold}} function.
}

\value{ returns a data frame with parameters concerning the fit of each item in the scale as well as for the quality of the scale as a whole. The columns of the resulting data frame are the following.
    \item{index}{: \code{m} selected items in the estimated MUDFOLD rank order. In the cell \code{total}, information for the total scale is given wherever this is available.}
    \item{samp.size}{: The sample size of the data.}
    \item{freq}{: observed frequency of positive responses.} 
    \item{prop}{: observed proportion of positive responses.}
    \item{std.err}{: standard error for the proportion of positive responses.}
    \item{Obs.err}{: observed response errors.}
    \item{Exp.err}{: expected response errors.}
    \item{Iso}{: iso statistic.}
    \item{Scalab.H}{: scalability coefficients.}
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
summary(fit)
}
}
\keyword{methods}


