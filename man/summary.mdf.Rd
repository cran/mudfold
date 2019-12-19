\name{summary.mdf}
\alias{summary.mdf}

\title{
\code{summary} method for S3 class \code{"mdf"} objects.
}
\description{Generic function that is used in order to summarize information from \code{"mdf"} class objects.
}
\usage{
\method{summary}{mdf}(object, boot=FALSE, type="perc", ...)
}

\arguments{
   \item{object}{: Object of class \code{"mdf"} resulted from the function \code{mudfold} or \code{as.mudfold}.}
   \item{boot}{: This argument applies when the \code{nboot} argument in the \code{mudfold} function is not \code{NULL}. If \code{boot=FALSE} (default) then no bootstrap information is returned by the summary. When \code{boot=TRUE}, confidence intervals, standard errors, biases, calculated from the bootstrap iterations for each parameter are given with the output. If the bootstrap estimate of the scale does not agree with the the scale of the item selection algorithm, then a summary of the bootstrap estimate of the scale is also given in the output.}
\item{type}{: A string that determines the type of confidence intervals that will be calculated. This argument is passed to the \code{boot.ci} function from the R package \pkg{boot}. Available options are \code{c("norm","basic", "perc", "bca")}. See \code{?boot.ci} for more information.}
  \item{\dots}{Other arguments passed on to the function \code{boot.ci} from the R package \pkg{boot}.}
}

\details{
A summary of the MUDFOLD scale that has been calculated with the \code{\link{mudfold}} function.
}

\value{The output of the \code{summary.mdf()} is a list with two main components. The first component of the list is a \code{data.frame} with scale statistics and the second component is a list with item statistics. If \code{diagnostics=TRUE} another component with diagnostic matrices is also included in the output. When the bootstrap scale estimate does not agree with the obtained MUDFOLD estimate a summary of the bootstrap scale will be given in the output.
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
fit <- mudfold(ANDRICH, nboot=100)
summary(fit, boot=TRUE)
summary(fit, boot=FALSE)

}
}
\keyword{methods}


