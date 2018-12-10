\name{ANDRICH}
\alias{ANDRICH}
\docType{data}
\title{Andrich's attitude scale towards capital punishment
}
\description{ D. Andrich's (1988) scale designed to measure the attitude from a sample of students towards
capital punishment. The data set contains the dichotomous responses of 54 students on 8 statements concerning capital punishment.
}
\usage{data(ANDRICH)}
\format{
  A data frame with 54 observations on the following 8 variables.
  \describe{
    \item{\code{HIDEOUS}}{a column vector containing the binary responses on the statement:

    "Capital punishment is one of the most hideous practices of our time"}

    \item{\code{LIFESACRED}}{a column vector containing the binary responses on the statement:

    "The state cannot teach the sacredness of human life by destroying it"}

    \item{\code{INEFFECTIV}}{a column vector containing the binary responses on the statement:

    "Capital punishment is not an effective deterrent to crime"}

    \item{\code{DONTBELIEV}}{a column vector containing the binary responses on the statement:

    "I do not believe in capital punishment but i am not sure it is not necessary"}

    \item{\code{WISHNOTNEC}}{a column vector containing the binary responses on the statement:

    "I think capital punishment is necessary but i wish it were not"}

    \item{\code{MUSTHAVEIT}}{a column vector containing the binary responses on the statement:

    "Until we find a more civilized way to prevent crime we must have capital punishment"}

    \item{\code{DETERRENT}}{a column vector containing the binary responses on the statement:

    "Capital punishment is justified because it does act as a deterrent to crime"}

    \item{\code{CRIMDESERV}}{a column vector containing the binary responses on the statement:

    "Capital punishment gives the criminal what he deserves"}
  }
}
\details{
The persons who responded to the statements for the analysis were 54 graduate students taking an introductory course in educational measurement and statistics. They responded simply by agreeing (1) or disagreeing (0) with each statement, with no restrictions placed on how many statements should receive an \emph{Agree} response.
}
\source{
D. Andrich. (1988). \emph{The Application of an Unfolding Model of the PIRT Type to the Measurement of Attitude}. Applied psychological measurement 12.1: 33-51.
}


\references{
D. Andrich. (1988). \emph{The Application of an Unfolding Model of the PIRT Type to the Measurement of Attitude}. Applied psychological measurement 12.1 (1988): 33-51.

W.J. Post. (1992). \emph{Nonparametric Unfolding Models: A Latent Structure Approach}. M & T series. DSWO Press.

W.J. Post. and T.AB. Snijders. (1993). \emph{Nonparametric unfolding models for dichotomous
data}. Methodika.

}
\examples{
\dontrun{
data(ANDRICH)
str(ANDRICH)
}
}
\keyword{datasets}
