\name{mudfold-package}
\alias{mudfold-package}
\docType{package}
\title{MUDFOLD : A nonmonotonic nonparametric model for undolding scale analysis.
}
\description{
This package can be used for the purpose of finding unfolding structures from selected items in tests or questionnaires. Such structures, represent the underlying latent ordering of those items. The main function of this package is called \code{\link{mudfold}} and fits the Van Schuur's scale construction method to binary proximity items. The method is called Multiple UniDimensional unFOLDing (MUDFOLD) and is an item selection algorithm belonging in the class of Nonparametric Item Response Theory (IRT) models.
}
\details{
MUDFOLD is a nonparametric probabilistic model for unidimensional unfolding. Originally developed by W. Van Schuur (1984) and further extended following ideas by W.J. Post (1992) who derived testable properties for the model fit. This method can be used to analyse proximity items pressumably generated from a nonmonotonic (unimodal) Item Response Function (IRF). The package incorporates the main function \code{\link{mudfold}} which is used to estimate the MUDFOLD scale to binary valued proximity items. The output of the main function is a list of S3 class \code{.mdf}, for which \code{print()}, \code{summary()} and \code{plot()} generic functions are available to the user.

The data must be given in an \eqn{n \times p} binary \code{matrix} or \code{data.frame} with \eqn{n} respondents in the rows and \eqn{p} items in the columns. Each row of the data corresponds to the selections of the \eqn{i}-th individual on a set of \eqn{p} items. Missing values (NA) are not allowed. 

Ultimate goal for MUDFOLD is to determine a unidimensional rank order of a (sub)set of items such that, they constitute an appropriate scale for measuring a common latent trait of the respondents. The estimation of the item order is done through an heuristic item selection algorithm, which tests iteratively the item fit to the scale with the use of scalability coefficients.

MUDFOLD's H coefficients of scalability are based to Loevinger's coefficient of homogeneity. In MUDFOLD, H coefficients utilize a scalability measure that is used in several criteria in the item selection algorithm. This coefficient in MUDFOLD can be calculated for triples of items, individual items, and the total scale.
}

\author{Spyros E. Balafas (auth.), Wim P. Krijnen (auth.), Wendy J. Post (contr.), Ernst C. Wit (auth.)

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
# Install the R package mudfold
install.packages("mudfold")

# Load the R package mudfold
library(mudfold)

}
}


\keyword{package}
