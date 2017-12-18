\name{as.mudfold}
\alias{as.mudfold}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for calculating MUDFOLD statistics for a given unfolding scale.
}
\description{
This function calculates the MUDFOLD statistics for proximity item response data, whose columns are assumed to be ordered to the order they are provided. The resulting object from the \code{as.mudfold} function is an object of S3 class \code{.mdf}, for which generic functions \code{print}, \code{summary}, and \code{plot} are available.
}
\usage{
as.mudfold(x,estimation="rank")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{x}{: A binary matrix or data frame containing the responses of \code{nrow(x)} persons
to \code{ncol(x)} items. Missing values in \code{x} are not allowed.
}
\item{estimation}{: This argument controls the nonparametric estimation method for item and subject locations. By deafult this argument equals to \code{"rank"} and implies that a Van Schuur's rank based estimator will be used for estimating the item parameters which are later used as thresholds in order to estimate subject 's parameters. The user can set this argument to \code{"quantile"} and then an estimator based on item rank quantiles proposed by Johnson is applied.
}
}
\details{
The function \code{as.mudfold} calculates MUDFOLD statistics for a given scale. Descriptive statistics, scalability coefficients, iso statistic values, are calculated and the user can obtain a summary table with the \code{summary} function which is designed for \code{.mdf} class objects.
}
\value{
The function \code{as.mudfold} returns a list object of class \code{mdf} with the same components as the core function \code{mudfold} except the information that concerns the item selection algorithm. The list contains the following:
  \item{dat}{The data in which MUDFOLD method has been fitted.}
  \item{starting.items}{The starting set of items.}
  \item{no.items}{The number of items in the starting set (i.e., equal to \code{ncol(x)}).}
  \item{sample.size}{The number of respondents.}
  \item{Best.triple}{\code{NA}}
  \item{iterations.in.sec.step}{\code{NA}}
  \item{mdfld.order}{The item order on the unidimensional scale.}
  \item{length.scale}{The number included in the scale (\code{length(mdfld.order})).}
  \item{item.popularities}{Observed proportion of positive responses for the items included in the scale.}
  \item{item.freq}{Observed frequency of positive responses for the items in MUDFOLD scale.}
  \item{Obs.err.item}{Observed response errors for each item included in the scale.}
  \item{Exp.err.item}{Expected response errors for each item included in the scale.}
  \item{H.item}{Scalability coefficient for each item included in the scale.}
  \item{Item.ISO}{Iso statistic for each item included in the scale.}
  \item{Obs.err.scale}{Observed response errors for the estimated scale.}
  \item{Exp.err.scale}{Expected response errors for the estimated scale}
  \item{Htotal}{Scalability coefficient for the estimated scale.}
  \item{Isototal}{Iso statistic for the estimated scale.}
  \item{Cond.Adjacency.matrix}{Conditional adjacency matrix (CAM) for the estimated scale.}
  \item{Adjacency.matrix}{Adjacency matrix for the estimated scale.}
  \item{Dominance.matrix}{Dominance matrix for the MUDFOLD scale.}
  \item{star}{A matrix with stars placed at the maxima locations of each row of the conditional adjacency matrix. }
  \item{Correlation.matrix}{Correlation matrix for the MUDFOLD scale.}
  \item{uniq}{The set of unique triples. From this set, the best minimal scale for the first step of the item selection algorithm is determined.}
  \item{est.parameters}{A list with two components. The first component refers to item parameters and the other to the subject parameters. The estimates have been obtained with a user specified nonparametric estimation method.}
  \item{call}{The function call.}
}

\author{Spyros E. Balafas (auth.), Wim P. Krijnen (auth.), Wendy J. Post (contr.), Ernst C. Wit (auth.)

Maintainer: Spyros E. Balafas (\email{s.balafas@rug.nl})
}
\references{
W.H. Van Schuur.(1984). \emph{Structure in Political Beliefs: A New Model for
Stochastic Unfolding with Application to European Party Activists}. CT Press.

W.J. Post. (1992). \emph{Nonparametric Unfolding Models: A Latent Structure Approach}. M
& T series. DSWO Press.

W.J. Post. and T.AB. Snijders. (1993).Nonparametric unfolding models for dichotomous
data. Methodika.

M.S. Johnson. (2006). Nonparametric Estimation of Item and Respondent Locations from Unfolding-type Items. Psychometrica
}


\seealso{
\code{\link{mudfold}}}
\examples{
\dontrun{
## pick a number for setting the seed
n.seed <- 11

## Simulate an unfolding scale
simulation <- mudfoldsim(p=6, n=100, gamma1=5, gamma2=-10, zeros=FALSE,seed=n.seed)

## get the data
dat <- simulation$dat

## true order
true_order <- simulation$true_ord

## check MUDFOLD statistics for the simulated order
mud_stats1 <- as.mudfold(dat,estimation="rank")

# get the summary table
summary(mud_stats1)

## check MUDFOLD statistics for the true item order
mud_stats2 <- as.mudfold(dat[,true_order],estimation="rank")

# get the summary table
summary(mud_stats2)
}
}
