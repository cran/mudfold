\name{mudfoldsim}
\alias{mudfoldsim}

\title{Function for constructing artificial item response data generated under a proximity response process.
%%  ~~function to do ... ~~
}
\description{
\code{mudfoldsim} function simulates unfolding data following a unimodal paremetric function with flexible set up. User can control the number of respondents, the number of items and fixed parameters of the Item Response Function (IRF) under which the responses are generated. Moreover, the user of \pkg{mudfold} package can allow or not individuals that are not endorsing any item to be simulated.
}
\usage{
mudfoldsim(items=8, sample.size=1000, pgamma1=5, pgamma2=-10, zeros=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{items}{: This argument specifies the number of items (stimuli). By default the responses of individuals on 8 items are simulated.
}
\item{sample.size}{: Argument which allows the user to specify the number of respondents in the simulated data. In the default setting of \code{mudfoldsim} function the responses of 1000 individuals are simulated.
}
\item{pgamma1}{: Parameter which is used in the IRF under which the data is generated. Default value is 5.
}
\item{pgamma2}{: Parameter which is used in the IRF under which the data is generated. Default value is -10.
}
\item{zeros}{: Logical argument. If \code{zeros=FALSE} (default), only individuals who endorse at least one item are allowed. Else, if \code{zeros=TRUE} individuals with no response are allowed.
}
}

\value{a list with three components. 
\item{df}{: data frame containing the binary responses of \code{n} subjects on \code{K} items under a parametrized Item Response Function.}
\item{item.patameters}{: The simulated item parameters that have been used for building the data.}
\item{subject.parameters}{: The simulated subject parameters that have been used for building the data.}
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

S. Balafas. (2015). \emph{Mudfold: A non parametric model for unfolding scale analysis}


}

\examples{
\dontrun{
## Create three different data sets and fit mudfold()
unfData <- mudfoldsim(items=6, sample.size=100, pgamma1=5, pgamma2=-10, zeros=FALSE)$df
unfData1 <- mudfoldsim(items=15, sample.size=2000, pgamma1=50, pgamma2=-100, zeros=FALSE)$df
unfData2 <- mudfoldsim(items=10, sample.size=2000, pgamma1=10, pgamma2=-100, zeros=FALSE)$df

## Create some random item order for the data
ord_data <- sample(colnames(unfData))
ord_data1 <- sample(colnames(unfData1))
ord_data2 <- sample(colnames(unfData2))

## Fit MUDFOLD to the simulated data and print the output (if such)
fit_simdata <- mudfold(unfData[,ord_data])
fit_simdata
fit_simdata1 <- mudfold(unfData1[,ord_data1])
fit_simdata1
fit_simdata2 <- mudfold(unfData2[,ord_data2])
fit_simdata2


}
}
