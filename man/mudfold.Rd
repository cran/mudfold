\name{mudfold}
\alias{mudfold}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MUDFOLD item selection algorithm for dichotomous proximity items.
}
\description{
This function incorporates a two step algorithm that determines an unfolding scale from observed binary data \code{x}. In the first step of the algorithm the best scale consisting of \code{3} items is determined, while in the second step, the scale obtained in the first step is expanded iteratively by adding the best fitting item in each iteration. The first step of the algorithm can be skiped with the argument \code{start} which can be used by setting manually an ordered item set that will be extended in the second step of the item selection algorithm. The resulting scale consists of the best \code{m} fitting items based on scalability criteria (where \code{m} \eqn{\le} \code{ncol(x)}). 

In \code{mudfold} function, the user can specify a value \eqn{\lambda_1} that will be used as a lower boundary in the scalability criteria of the search algorithm. By default, the lower boundary for the scalability coefficients is \code{lambda1=0.3} (Mokken,1971). The user can choose a second value \eqn{\lambda_2} (usually negative) that will be used as a lower boundary only for the second step of the algorithm (by default, \code{lambda2=0}). The parameter \eqn{\lambda_2} is used mostly, in order to relax the first scalability criterion of the second step. Generally, values greater than \eqn{0.3} for \eqn{\lambda_1}, and \eqn{\lambda_2} lead to very strict criteria while negative values relax these criteria.

Moreover, the user is able to choose between two nonparametric estimation methods in order to obtain individual and item scale parameters that are estimated based on the resulting item order. The default setting (i.e., \code{estimation="rank"}) uses an estimation proposed by Van Schuur based on item ranks. Alternatively, an estimation method described by M.S. Johnson, which uses item quantiles for estimating person parameters, can be used by setting \code{estimation="quantile"}.
}
\usage{
mudfold(x, estimation="rank", lambda1=.3, lambda2=0,start= NULL,check=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{: A binary matrix or data frame containing the responses of \code{nrow(x)} persons
to \code{ncol(x)} items. Missing values in \code{x} are not allowed.
}
\item{estimation}{: This argument controls the nonparametric estimation method for item and subject locations. By deafult this argument equals to \code{"rank"} and implies that Van Schuur's rank based estimator will be used for estimating the item parameters which are later used as thresholds in order to estimate subject 's parameters. The user can set this argument to \code{"quantile"} and then an estimator based on item rank quantiles proposed by Johnson is applied.
}
\item{lambda1}{: User specified numerical value that is used as a lower boundary for the scalability criterion of the first step of the item selection algorithm, and in the item scalability criterion at the end of the scale expansion. Default value is \eqn{\lambda_1=0.3} but it can be any value between \eqn{-\infty} and \eqn{1} (i.e., \eqn{\lambda_1 \in \left(-\infty,1\right]}). The higher the value of \eqn{\lambda_1} the stricter the scalability criteria of the algorithm. 
}
\item{lambda2}{: User specified numerical value that controls explicitly the first scalability criterion of the scale expansion. In the default settings \eqn{\lambda_2=0}, however, the user can choose a negative value for \eqn{\lambda_2}, which leads to less strict scalability criterion in the beginning of the scale expansion.
}
\item{start}{: A pressumably ordered character vector containing column names of \code{x}, with length greater than or equal to \eqn{3} and less than or equal to the number of columns of \code{x}. This ordered item set is used as a startset for the scale extension phase of MUDFOLD method. If \code{start= NULL} the standard MUDFOLD method is fitted to the data.
}
\item{check}{: A logical argument. If \code{check=TRUE} (default) then the data is checked for errors.
}
}
\details{
In the first stage, \code{mudfold} function seeks the best ordered triple of items. The best triple of items is called \emph{best unique triple} and has to fullfil certain scalability criteria in order to be chosen as the best elementary scale. Specifically, the scalability coefficient of the best unique triple has to be higher than a user specified parameter \eqn{\lambda_1}.

In the second stage of the algorithm the best elementary scale is extended by adding the best fitting item in each replication of the algorithm based again on scalability criteria. The user can potentially specify an explicit lower boundary for the first scalability criterion of the second step through a parameter \eqn{\lambda_2}. 

The parameters \eqn{\lambda_1,\lambda_2} can take any value in \eqn{\left(-\infty,1\right]}. Values larger than \eqn{0.3} for the \eqn{\lambda}'s lead to very strict scalability criteria.
}

\value{
The function \code{mudfold} returns a list object of class \code{mdf} with the following components:
  \item{dat}{The data in which MUDFOLD method has been fitted.}
  \item{starting.items}{The starting set of items.}
  \item{no.items}{The number of items in the starting set (i.e., equal to \code{ncol(data)}).}
  \item{sample.size}{The number of respondents.}
  \item{Best.triple}{The best minimal scale (triple of items) determined in the first step of the item selection algorithm.}
  \item{iterations.in.sec.step}{Number of iterations in the second step of MUDFOLD method.}
  \item{mdfld.order}{The item order resulted from the item selection algorithm.}
  \item{length.scale}{The number included in the scale (\code{length(mdfld.order})).}
  \item{item.popularities}{Observed proportion of positive responses for the items included in the scale.}
  \item{item.freq}{Observed frequency of positive responses for the items in MUDFOLD scale.}
  \item{Obs.err.item}{Observed response errors for each item included in the scale.}
  \item{Exp.err.item}{Expected response errors for each item included in the scale.}
  \item{H.item}{Scalability coefficient for each item included in the scale.}
  \item{Item.ISO}{Iso statistic for each item included in the scale..}
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
Mokken, R. J. (1971). A theory and procedure of scale analysis: With applications in political research (Vol. 1). 
Walter de Gruyter.

W.H. Van Schuur.(1984). \emph{Structure in Political Beliefs: A New Model for
Stochastic Unfolding with Application to European Party Activists}. CT Press.

W.J. Post. (1992). \emph{Nonparametric Unfolding Models: A Latent Structure Approach}. M
& T series. DSWO Press.

W.J. Post. and T.AB. Snijders. (1993).Nonparametric unfolding models for dichotomous
data. Methodika.

M.S. Johnson. (2006). Nonparametric Estimation of Item and Respondent Locations from Unfolding-type Items. Psychometrica


}

\examples{
\dontrun{
#####################################
#### MUDFOLD method on real data ####
#####################################

### MUDFOLD method on ANDRICH data (see Post and Snijders pp.147) ###
data(ANDRICH)

## fit MUDFOLD on ANDRICH data ##
fit_andr <- mudfold(ANDRICH)

## generic functions for the S3 class .mdf object fit ##

# print
fit_andr

# summary
summary(fit_andr)

# plot
plot(fit_andr)

### MUDFOLD method on EURPAR2 data ###
data("EURPAR2")

## fit MUDFOLD on EURPAR2 data ##
fit_eurp <- mudfold(EURPAR2)

# print
print(fit_eurp,Diagnostics = TRUE)

# summary
summary(fit_eurp)

# plot
plot(fit_eurp)

### MUDFOLD method on Plato7 data ###
data("Plato7")

## transform to binary data
## using as threshold the mean
## per row of Plato7

dat_plato <- pick(Plato7)

## fit MUDFOLD on Plato7 data ##
fit_plato <- mudfold(dat_plato)

# print
print(fit_plato)

# summary
summary(fit_plato)

# plot
plot(fit_plato)


##########################################
#### MUDFOLD method on simulated data ####
##########################################

### Data with the responses of
### n=3000 on p=20 items

simulation1 <- mudfoldsim(p=20, n=3000, pgamma1=2, pgamma2=-10, zeros=FALSE,seed = 1)
dat_sim1 <- simulation1$dat

## fit MUDFOLD on simulated data ##
fit.sim1 <- mudfold(dat_sim1)

# print
fit.sim1

# summary
summary(fit.sim1)

# plot
plot(fit.sim1)

### Data with the responses of
### n=3000 on p=26 items

simulation2 <- mudfoldsim(p=26, n=3000, gamma1=2, gamma2=-10, zeros=FALSE,seed = 1)
dat_sim2 <- simulation2$dat

## fit MUDFOLD on simulated data ##
fit.sim2 <- mudfold(dat_sim2)

# print
fit.sim2

# summary
summary(fit.sim2)

# plot
plot(fit.sim2)
}
}

