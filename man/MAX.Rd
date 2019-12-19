\name{MAX}
\alias{MAX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Max statistic for a given unfolding scale.
}
\description{
This function calculates the max statistic based on the conditional adjacency matrix (CAM) of a given scale. This statistic quantifies violations of the moving maxima property for the item response functions (Post,1992) and it can be calculated for each item and the whole scale. For each row of the CAM, the max statistic is calculated using both a top-down and a bottom-up method. 

Both methods yield the same max statistic value for the scale, however, the number of items with non-zero max statistisc may change. In this case, the method that yields the smaller number of items with zero max statististic will be prefered. 
}
\usage{
MAX(X, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{: A matrix of class 'cam.mdf' obtained from the function \code{CAM()}. Alternatively, \code{x} can be a fitted object of class \code{"mdf"} resulted from the \code{mudfold()} function. 
}
  \item{type}{: This argument controls the type of the statistic that is returned. If \code{type="item"} (default) then the max statisic for each item in the scale will be calculated. When \code{type="scale"} the MAX statistic for the whole scale will be returned divided by \eqn{\frac{N^2}{2}} which is approximately the total number of violations that can occur to a scale of length \eqn{N}.
}
}
\details{
To come up with a value of the max statistic for each item in a scale with \emph{N} items in total, we need first to locate the maximum position in each row of the CAM \eqn{m_i^*}. Then the max statistic for the item \emph{i} is calculated using a top-down method according to which,

\eqn{
MAX_i = \sum_{k = i+1}^{N}  max(0, m_i^* - m_k^*)
} 

and a bottom-up method according to which,

\eqn{
MAX_i = \sum_{k = 1}^{i-1}  max(0, m_k^* - m_i^*).
}

}
\value{
A vector with the MAX statistic for each item. The sum of the individual MAX statistics for each of the items yields the MAX statistic for the whole scale.
}
\references{
W.J. Post. (1992). \emph{Nonparametric Unfolding Models: A Latent Structure Approach}. M
& T series. DSWO Press.

}
\author{
Spyros E. Balafas (\email{s.balafas@rug.nl})
}



\seealso{
 \code{\link{CAM}}
}
\examples{
## load the ANDRICH data
data("ANDRICH")

## Calculate the CAM for the ANDRICH scale
CAM_andrch <- CAM(ANDRICH)

## Use the CAM to calculate the MAX statistic
## for each item in the ANDRICH scale
MAX(CAM_andrch)

## and the whole scale
MAX(CAM_andrch, type="scale")

}