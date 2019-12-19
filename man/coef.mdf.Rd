\name{coef.mdf}
\alias{coef.mdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generic \code{coef} method for S3 class \code{"mdf"} objects.
}
\description{
This function extracts person and/or item parameters obtained after fitting MUDFOLD to binary preferential-choice data.
}
\usage{
\method{coef}{mdf}(object, type, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{: A fitted object of class \code{"mdf"} obtained from the \code{mudfold} function.
}
  \item{type}{: Argument that controls the type of parameters to be returned. If \code{type="persons"} (default), a vector with the person parameters is returned. When \code{type="items"} then a vector with the item ranks obtained by the MUDFOLD item selection algorithm is returned. If {type="all"} then a list with both person and item coefficients is returned to the user.
}
  \item{\dots}{: not in use at the current version of the package.
}

}

\value{
A vector when \code{type="persons"} or \code{type="items"}. Alist when \code{type="all"}.
}
\references{
W.H. Van Schuur.(1984). \emph{Structure in Political Beliefs: A New Model for
Stochastic Unfolding with Application to European Party Activists}. CT Press.

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

## fit a MUDFOLD scale to the ANDRICH data
mudf_andrich <- mudfold(ANDRICH)

## obtain the parameters from the fitted object
coef(mudf_andrich)
}

