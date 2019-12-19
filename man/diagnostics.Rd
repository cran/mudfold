\name{diagnostics}
\alias{diagnostics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MUDFOLD scale diagnostics
}
\description{
This function returns diagnostics for a fitted MUDFOLD scale. Specifically, it returns the iso statistic (see \code{\link{ISO}}) the max statistic (see \code{\link{MAX}}), the matrix with stars at the maximum of each row, as well as a test for conditional independence.
}
\usage{
diagnostics(x, boot, nlambda, lambda.crit, type, k, which, plot)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{: A fitted object of class \code{"mdf"} obtained from the \code{\link{mudfold}} function.
}
\item{boot}{: logical argument that controls if bootstrap confidence intervals and summary for the H coefficients and the ISO and MAX statistics will be returned. If \code{boot=FALSE} (default) no information for bootstrap is returned. When \code{boot=TRUE}, confidence intervals, standard errors, biases, calculated from the bootstrap iterations for each diagnostic are given with the output. 
}
\item{nlambda}{: The number of regularization parameters to be used in \code{cv.glmnet()} function when testing local independence.
}
\item{lambda.crit}{: String that specifies the criterion to be used by cross-validation for choosing the optimal regularization parameter. Available options are "class" (default), "deviance", "auc", "mse", "mae". See the argument \code{'type.measure'} in the \code{cv.glmnet()} function for more details.
}
\item{type}{: The type of bootstrap confidence intervals to be calculated if the argumnet \code{boot=TRUE}. Available options are \code{"norm"}, \code{"basic"}, \code{"perc"} (deafult), and \code{"bca"}. See the argument type of the \code{boot.CI()} for details.
}
\item{k}{: The dimension of the basis in the thin plate regression spline that is used when testing for IRF unimodality. The default value of \code{k} is four.
}
\item{which}{: Which diagnostic should be returned by the function. Available options are \code{"H"}, \code{"LI"}, \code{"UM"}, \code{"ISO"}, \code{"MAX"}, \code{"STAR"}, \code{"all"} (default).
}
\item{plot}{: Logical. Should plots be returned for the diagnostics that can be plotted? Default value is \code{plot=TRUE}.
}

}

\value{
a list of length six where each component is a diagnostic when \code{which="all"}. A list equal to \code{length(which)} when \code{which != "all"}.
}
\references{
W.J. Post. (1992). \emph{Nonparametric Unfolding Models: A Latent Structure Approach}. M
& T series. DSWO Press.

W.J. Post. and T.AB. Snijders. (1993). \emph{Nonparametric unfolding models for dichotomous
data}. Methodika.
}
\author{
Spyros E. Balafas (\email{s.balafas@rug.nl})
}



\examples{
## load the ANDRICH data
data("ANDRICH")

## Fit a MUDFOLD scale to the ANDRICH data
mudf_andrich <- mudfold(ANDRICH)
## Get the diagnostics
diagnostics(mudf_andrich, which = "UM")
}