\name{EURPAR2}
\alias{EURPAR2}
\docType{data}
\title{
Preferences of European party activists.
}
\description{
European party activists preferences for two political parties in the European parliament in 1980. A sample consisted of 1786 individuals are asked to endorse 2 out of 6 (pick 2/6 case) political parties from the European parliament.
}
\usage{data("EURPAR2")}
\format{
  A data frame with 1786 observations (responses) on the following 6 variables (items).
  \describe{
    \item{\code{communists}}{Communistic political party;}
    \item{\code{socdemocr}}{Social Democratic political party;}
    \item{\code{demprogres}}{Progressive Democratic political party;}
    \item{\code{liberals}}{Liberal Democratic political party;}
    \item{\code{christians}}{Christian Democratic political party;}
    \item{\code{conservat}}{Conservative political party;}
  }
}
\details{
The data has been first studied by Van Schuur (1984) and further by W. J. Post (1992).
}
\source{
W.H. Van Schuur.(1984). \emph{Structure in Political Beliefs: A New Model for Stochastic Unfolding with Application to European Party Activists}. CT Press.
}
\author{Spyros E. Balafas.

Maintainer: Spyros E. Balafas (\email{s.balafas@rug.nl})
}
\references{
W.H. Van Schuur.(1984). \emph{Structure in Political Beliefs: A New Model for Stochastic Unfolding with Application to European Party Activists}. CT Press.

W.J. Post. (1992). \emph{Nonparametric Unfolding Models: A Latent Structure Approach}. M & T series. DSWO Press.
}
\examples{
data(EURPAR2)
str(EURPAR2)
}
\keyword{datasets}
