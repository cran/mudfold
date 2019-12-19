\name{ISO}
\alias{ISO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Iso statistic for a given unfolding scale.
}
\description{
This function calculates the iso statistic based on the conditional adjacency matrix (CAM) of a given scale. In order to quantify if the rows of the CAM show a weakly unimodal pattern, the iso statistic was introduced (Post, 1992). Iso statistic (ISO), is a measure for the degree of unimodality violation in the rows of CAM. ISO can be obtained for each item (\eqn{{ISO}_j}) and their summation results in the total ISO for the scale (\eqn{{ISO}_{tot}}).

To come up with an ISO value for an item \emph{j}, one should first locate the maximum in each row of the CAM. If we index \eqn{m^*} the maximum in row \emph{j} of CAM, the ISO measures deviations from unimodality to the left and right of \eqn{m^*}. The function takes as input objects of class \code{"cam.mdf"} obtained from the function \code{\link{CAM}} or objects of class \code{"mdf"} obtained from the function \code{\link{mudfold}}
}
\usage{
ISO(x, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{: A matrix of class 'cam.mdf' obtained from the function \code{CAM()}. Alternatively, \code{x} can be a fitted object of class \code{"mdf"} resulted from the \code{mudfold()} function. 
}
  \item{type}{: This argument controls the type of the statistic that is returned. If \code{type="item"} (default) then the ISO statisic for each item in the scale. When \code{type="scale"} the ISO statistic for the whole scale will be returned.
}
}
\details{

 \eqn{
ISO_j = \sum_{h\leq k \leq m^*}  max(0, {CAM}_{jh} - {CAM}_{jk}) + \sum_{m^* \leq h \leq k}  max(0,{CAM}_{jk} - {CAM}_{jh} )
}
}
\value{
A vector with the ISO statistic for each item. The sum of the individual ISO statistics for each of the items yield the ISO statistic for the whole scale.
}
\references{
W.J. Post. (1992). \emph{Nonparametric Unfolding Models: A Latent Structure Approach}. M
& T series. DSWO Press.

}
\author{
Spyros E. Balafas (\email{s.balafas@rug.nl})
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{CAM}}
}
\examples{
## load the ANDRICH data
data("ANDRICH")

## Calculate the CAM for the ANDRICH scale
CAM_andrch <- CAM(ANDRICH)

## Use the CAM to calculate the ISO statistic
## for the ANDRICH scale
ISO(CAM_andrch)

}
