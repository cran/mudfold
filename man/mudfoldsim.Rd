\name{mudfoldsim}
\alias{mudfoldsim}

\title{Function for constructing artificial item response data generated under a proximity response process.
}

\description{
\code{mudfoldsim} function simulates unfolding data following a unimodal parametric function with flexible set up. User can control the number of respondents, the number of items and fixed parameters of the Item Response Function (IRF) under which the responses are generated. Moreover, the user of the \pkg{mudfold} package can allow (or not) individuals that are endorsing no items.

}


\usage{
mudfoldsim(p=p,n=n,gamma1=5,gamma2=-10,zeros=FALSE,parameters="normal",seed=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{p}{: This argument specifies the number of items (stimuli).
}
\item{n}{: Argument which allows the user to specify the number of respondents in the simulated data. 
}
\item{gamma1}{: Parameter which is used in the IRF under which the data is generated. Default value is 5.
}
\item{gamma2}{: Parameter which is used in the IRF under which the data is generated. Default value is -10.
}
\item{zeros}{: Logical argument. If \code{zeros=FALSE} (default), only individuals who endorse at least one item are allowed. Else, if \code{zeros=TRUE} individuals with no response are allowed.
}
\item{parameters}{: A character string that controls the distribution of the person parameters. If \code{parameters="normal"} (default), individual parameters are drawn from a standard normal distribution. If \code{parameters="uniform"}, the person parameters are uniformly drawn between the minimum and the maximum item parameters respectively.
}
\item{seed}{: An integer to be used in the \code{set.seed} function. If \code{seed=NULL} (default), then the seed is not set.
}
}


\details{
For simulating the response of an individual \eqn{i} with scale parameter \eqn{\theta_i} to an item \eqn{j} with scale parameter \eqn{\beta_j} we use the function \eqn{P(X_j =1 \mid \theta_i, \beta_j)=\frac{1}{1+e^{-\gamma_1 -\gamma_2(\theta_i - \beta_j)^2}}}. The parameters \eqn{\theta_i, \beta_j} are sampled from a standard normal distribution, i.e., \eqn{\theta \sim \mathcal{N}(0,1)}, and \eqn{\beta \sim \mathcal{N}(0,1)}.
}

\value{a list with 11 components. 
\item{obs_ord}{: A character vector with the items in the simulated order.
}
\item{true_ord}{: A character vector with the items in the true order in which they constitute an unfolding scale.
}
\item{items}{: An integer corresponding to the number of the simulated items.
}
\item{sample}{: An integer corresponding to the number of the simulated respondents.
}
\item{gamma1}{: A value that corresponds to the parameter \eqn{\gamma_1} of the IRF.
}
\item{gamma2}{: A value that corresponds to the parameter \eqn{\gamma_2} of the IRF.
}
\item{seed}{: An integer that corresponds to the seed number that is going to be used in the \code{set.seed} function.
}
\item{dat}{: data frame containing the binary responses of \code{n} subjects on \code{K} items under a parametric Item Response Function.
}
\item{probs}{: A matrix containing the probabilities of positive response from \code{n} subjects on \code{K} items under a parametric Item Response Function.
}
\item{item.patameters}{: The simulated item parameters that have been used for sampling the data.
}
\item{subject.parameters}{: The simulated subject parameters that have been used for sampling the data.
}
}

\author{Spyros E. Balafas (auth.), Wim P. Krijnen (auth.), Wendy J. Post (contr.), Ernst C. Wit (auth.)

Maintainer: Spyros E. Balafas (\email{s.balafas@rug.nl})
}

\references{
W.H. Van Schuur.(1984). \emph{Structure in Political Beliefs: A New Model for
Stochastic Unfolding with Application to European Party Activists}. CT Press.

W.J. Post. (1992). \emph{Non parametric Unfolding Models: A Latent Structure Approach}. M
& T series. DSWO Press.

W.J. Post. and T.AB. Snijders. (1993).Non parametric unfolding models for dichotomous
data. Methodika.

}

\examples{
\dontrun{
## Simulate 5 different scenarios 

n.seed <- 10

sim1 <- mudfoldsim(p=6, n=100, gamma1=5, gamma2=-10, zeros=FALSE,seed=n.seed)
sim2 <- mudfoldsim(p=10,n=1000,gamma1=10,gamma2=-100,zeros=FALSE,seed=n.seed)
sim3 <- mudfoldsim(p=15,n=2000,gamma1=50,gamma2=-100,zeros=FALSE,seed=n.seed)
sim4 <- mudfoldsim(p=30,n=2000,gamma1=50,gamma2=-100,zeros=FALSE,seed=n.seed)
sim5 <- mudfoldsim(p=50,n=2000,gamma1=50,gamma2=-100,zeros=FALSE,seed=n.seed)


dat1 <- sim1$dat
dat2 <- sim2$dat
dat3 <- sim3$dat
dat4 <- sim4$dat
dat5 <- sim5$dat

fit1 <- mudfold(dat1)
fit1
fit2 <- mudfold(dat2)
fit2
fit3 <- mudfold(dat3)
fit3
fit4 <- mudfold(dat4)
fit4
fit5 <- mudfold(dat5)
fit5
}
}
