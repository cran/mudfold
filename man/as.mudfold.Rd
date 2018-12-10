\name{as.mudfold}
\alias{as.mudfold}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for calculating MUDFOLD statistics for a given unfolding scale.
}
\description{
This function calculates the MUDFOLD statistics for data whose columns are assumed to be ranked to the order they are provided. The resulting object from the \code{as.mudfold} function is an object of S3 class \code{"mdf"}, for which generic functions \code{print}, \code{summary}, and \code{plot} are available.
}
\usage{
as.mudfold(data,estimation="rank")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{data}{: A binary \code{matrix} or \code{data.frame} containing the responses of \code{nrow(data)} persons
to \code{ncol(data)} items. Missing values in \code{data} are not allowed.
}
\item{estimation}{: This argument controls the nonparametric estimation method for person locations. By deafult this argument equals to \code{"rank"} and implies that Van Schuur's estimator will be used in order to infer the person parameters. The user can set this argument to \code{"quantile"} and then an estimator proposed by Johnson is applied.
}
}
\details{
The function \code{as.mudfold} calculates MUDFOLD statistics for a given scale. Descriptive statistics, observed errors, expected errors, scalability coefficients, iso statistic values, are calculated for items and the scale. The user can obtain a summary table for the given scale with the \code{summary} function which is designed for \code{"mdf"} class objects.
}

\value{
The function \code{as.mudfold} returns a list with the same components as the \code{mudfold} function except the information that concerns the item selection algorithm. The list contains the following:
  \item{CALL}{A list where its components provide information for the function call.}
  \item{CHECK}{A list where its components provide information from the data checking step.}
  \item{DESCRIPTIVES}{A list with descriptive statistics for the \code{data}.}
  \item{MUDFOLD_INFO}{A list with three main components. The first component is called \code{triple_stats} and is a list where in each element contains the observed errors, expected errors, and scalability coefficients for each item triple. The second element is called \code{first_step} and informs the user that the first step of the item selection algorithm is not applied in the \code{as.mudfold} function. The third element of this list is called \code{second_step}  and is also a list with the MUDFOLD statistics and parameter estimates for the given scale.}
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
simulation <- mudfoldsim(N=6, n=100, seed=n.seed)

## get the data
dat <- simulation$dat

## true order
true_order <- simulation$true_ord

## check MUDFOLD statistics for the random simulated rank order
mud_stats1 <- as.mudfold(dat)

# get the summary 
summary(mud_stats1)

## check MUDFOLD statistics for the true item rank order
mud_stats2 <- as.mudfold(dat[,true_order])

# get the summary for the true item rank order
summary(mud_stats2)
}
}
