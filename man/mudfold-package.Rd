\name{mudfold-package}
\alias{mudfold-package}
\docType{package}
\title{MUDFOLD : A Nonparametric Model for Undolding Scale Analysis
}
\description{
MUDFOLD is a nonparametric probabilistic model for unfolding founded by W. Van Schuur and further developed by W.J. Post. MUDFOLD method can be used to analyse proxomity item response data. The package incorporates the main function \code{mudfold} which is used to fit MUDFOLD method to binary questionnaire data with no missing values.

}
\details{
This package contains the main \code{\link{mudfold}} function which is used to determine the order of a maximal subset consisted of \eqn{m \le K} items such that the obtained partitioned data fits sufficiently the unfolding scale assumptions. A binary (i.e, \code{(0,1)}) valued, \code{n x K} matrix or a data frame \code{data} with subjects in the rows and items in the columns must be used as an input. The output of the main function is a list of class \code{.mdf} for which S3 methods for print, summary and plot are available to the user.

MUDFOLD item selection algorithm in the process of building an unfolding scale makes use of scalability coefficients. H coefficients of scalability are analogous to Loevinger's coefficient of homogeneity for a cumulative scale used by Mokken (MH model, DM model) (1972) and from W.Van Schuur in MUDFOLD as a scalability measure. This coefficients in MUDFOLD can be calculated for:

Triples of items,\cr

Items individually,\cr

The whole scale.\cr

The main \code{\link{mudfold}} function makes use of the latter scalability coefficients to determine an unfolding item order on the basis of MUDFOLD model assumptions. Usefull quantitative information for the scale quality is provided in the \code{.mdf} class output of the main function. S3 methods for \code{.mdf} class objectrs are available (see \code{\link{summary.mdf}}, \code{\link{plot.mdf}}, \code{\link{print.mdf}}) with \pkg{mudfold} package.
}

\author{Spyros E. Balafas.

Maintainer: Spyros E. Balafas (\email{s.balafas@rug.nl})
}

\references{
W.H. Van Schuur.(1984). \emph{Structure in Political Beliefs: A New Model for
Stochastic Unfolding with Application to European Party Activists}. CT Press.

W.J. Post. (1992). \emph{Nonparametric Unfolding Models: A Latent Structure Approach}. M
& T series. DSWO Press.

W.J. Post. and T.AB. Snijders (1993). \emph{Nonparametric unfolding models for dichotomous
data}. Methodika.


}

\examples{
\dontrun{
# Capital punishment data from Andrich
data(ANDRICH)
# Fit MUDFOLD method to estimate an item rank order
fit <- mudfold(ANDRICH, lower.boundary= .3, estimation="rank")
# Print the .mdf class object fit
fit
# Summary of the model fit results
summary(fit)
# Visually check the Conditional adjacency matrix
fit$Cond.Adjacency.matrix
# Plot the empirical estimates of the IRFs
plot(fit)
}
}



\keyword{package}
