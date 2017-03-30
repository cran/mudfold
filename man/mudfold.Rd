\name{mudfold}
\alias{mudfold}
\title{ This function is used in order to fit H. Van Schuur 's unfolding item selection method to binary item response data.
}
\description{
\code{mudfold} function is used in order to calculate a Multiple UniDimensional unFOLDing (MUDFOLD) method object. In principle is a two stage algorithm which determines an unfolding scale from observed item response binary data \code{X}. The resulting MUDFOLD scale consists of  \eqn{m \le ncol(X)} items in a rank order estimated by \code{mudfold}. 

In \code{mudfold} function the user can choose a value that will be used as a lower boundary for MUDFOLD method scalability criteria. By default, the lower boundary for the scalability coefficients is \code{0.3}. User specified is also the nonparametric estimation method which is used to obtain item and subject parameters. Moreover, the user can also manually determine an ordered item set for the second step of Van Schuur 's item selection algorithm. In the default setting, \code{mudfold} function incorporates a greedy search to find the best starting item set consisted of three items in a "uniquely" represented order.
}

\usage{
mudfold(X=data, estimation="rank", lower.boundary=.3, start= NULL)
}

\arguments{
\item{X}{: A binary matrix or data frame \code{data}, with responses of \code{nrow(data)} respondents
to \code{ncol(data)} items. Missing values are not allowed.
}
\item{estimation}{: This argument controls the nonparametric estimation method for item and subject locations. By deafult this argument equals to \code{"rank"} and implies that Van Schuur's rank based estimator will be used for estimating the item parameters which are later used as thresholds in order to estimate subject 's parameters. The user can set this argument to \code{"quantile"} and then an estimator based on item rank quantiles proposed by Johnson is applied.
}
\item{lower.boundary}{: User specified numerical value for the H coefficient lower boundary. This value is used in several stages of the algorithm for testing the scalability properties of a candidate MUDFOLD scale. Default value is \code{0.3}.
}
\item{start}{: A pressumably ordered character vector containing column names of \code{X}, with length greater than or equal to \code{3} and less than or equal to the number of columns of \code{X}. This ordered item set is used as a startset for the scale extension phase of MUDFOLD method. If \code{start= NULL} the standard MUDFOLD method is fitted to the data.
}
}

\details{
In the first stage \code{mudfold} function establishes a search procedure seeking for the best triple of items which is selected from the starting set of items (see \code{names(data)}). In the second stage the scale is extended in an iterative way. In this iterative process the rest \code{ncol(data) - 3} items are investigated to decide the best fitting each time.

The best triple of items in the first stage of the algorithm is called \code{best unique triple} and has to fullfil certain criteria to be chosen as the best elementary scale that will be extended in the second step of the algorithm. These explicit criteria for the best triple are :

a) positive scalability \code{Htrip} in only one of its permutations and negative in the other two.\cr

b) the highest scalability \code{Htrip} among the triples who fulfill criterion a).\cr

In the second stage of the algorithm the best elementary scale is extended by adding one item in each replication. The best fitting item in the best fitting position is determined based on scalability criteria which have to be fulfilled. These are :

a) The first criterion that a candidate scale from the total \code{(ncol(data)-m)(m+1)} has to accomplish to be admissible for the next steps of the algorithm is:

All the \eqn{{m \choose 2}} triples defined from this specific scale (with respect to the order of the items), which are containing the -candidate- item must have positive \code{Htrip} coefficient.

b) The candidate scales which fulfill the 1st criterion are admissible for the next level of the process. In this level the best fitting scale will be the one that contains the candidate item which is the most uniquely represented.

The latter means that a certain amount of scales "passed" the first criterion. Each of these scales contains a candidate (new) item that can be potentially added in the scale. We calculate how frequently each candidate item is represented among the admissible scales. The scales or the scale which are containing the new item with the minimum appearance frequency among the admissible scales will be chosen. If more than one scales fit the last criterion then the best will be determined on the base of the highest scalability coefficient for the scale as a whole.

c) The last criterion for a candidate scale which fulfills the previous requirements is the value of its H coefficient. The scalability coefficient \code{Hitem} of the candidate item has to be higher than a user specified lower boundary. We choose this lower boundary to be \code{0.3} but this parameter can be determined from the user himself by the \code{lower.boundary} argument in \code{mudfold} function.

\code{mudfold(X=data,lower.boundary=...)}

}

\value{
The function \code{mudfold} returns a list object of class \code{mdf} with the following components:
  \item{dataf}{The data in the obtained MUDFOLD item rank order.}
  \item{no.items}{The number of items in the starting pool (i.e., equal to \code{ncol(data)}).}
  \item{sample.size}{The number of subjects that responded the attitudinal test / survey /questionnaire.}
  \item{Best.triple}{The best minimal scale (triple) determined in the first step.}
  \item{iterations.in.sec.step}{Number of iterations in the second step of MUDFOLD method.}
  \item{mdfld.order}{Items selected for the MUDFOLD scale given in the estimated rank order.}
  \item{length.scale}{The number of items consisting the resulting scale.}
  \item{item.popularities}{Observed proportion of positive responses for the items in MUDFOLD scale.}
  \item{item.freq}{Observed frequency of positive responses for the items in MUDFOLD scale.}
  \item{Obs.err.item}{Observed response errors for each item in MUDFOLD scale.}
  \item{Exp.err.item}{Expected response errors for each item in MUDFOLD scale.}
  \item{H.item}{Scalability coefficient for each item in MUDFOLD scale.}
  \item{Item.ISO}{Iso statistic for each item in MUDFOLD scale.}
  \item{Obs.err.scale}{Observed response errors for the MUDFOLD scale.}
  \item{Exp.err.scale}{Expected response errors for the MUDFOLD scale}
  \item{Htotal}{Scalability coefficient for the MUDFOLD scale.}
  \item{Isototal}{Iso statistic for the MUDFOLD scale.}
  \item{Dominance.matrix}{Dominance matrix for the MUDFOLD scale.}
  \item{Adjacency.matrix}{Adjacency matrix for the MUDFOLD scale.}
  \item{Correlation.matrix}{Correlation matrix for the MUDFOLD scale.}
  \item{Cond.Adjacency.matrix}{Conditional adjacency matrix (CAM) for the MUDFOLD scale.}
  \item{uniq}{The set of unique triples. From this set, the best minimal scale for the first step of MUDFOLD is determined.}
  \item{est.parameters}{A list with two components. One refers to item parameters and the other to the subject parametrs. The estimates have been obtained with a user specified nonparametric estimation method.}
  \item{call}{The function call.}
  
}



\author{Spyros E. Balafas.

Maintainer: Spyros E. Balafas (\email{s.balafas@rug.nl})
}

\references{
W.H. Van Schuur.(1984). \emph{Structure in Political Beliefs: A New Model for
Stochastic Unfolding with Application to European Party Activists}. CT Press.

W.J. Post. (1992). \emph{Nonparametric Unfolding Models: A Latent Structure Approach}. M
& T series. DSWO Press.

W.J. Post. and T.AB. Snijders. (1993).Nonparametric unfolding models for dichotomous
data. Methodika.

}

\examples{
\dontrun{
# Apply mudfold function to obtain the best fitting
# MUDFOLD scale
data(ANDRICH)
## FIT MUDFOLD AND OBTAIN VAN SCUUR'S ESTIMATES ##
fit1 <- mudfold(X=ANDRICH, estimation="rank")
fit1$est.parameters
## FIT MUDFOLD AND OBTAIN JOHNSON'S ESTIMATES ##
fit2 <- mudfold(X=ANDRICH, estimation="quantile")
fit2$est.parameters
## check the class of the 
class(fit1)
## S3 Methods for .mdf class objects ##
# print
fit1
# summary
summary(fit1)
# plot
plot(fit1)
}
}



