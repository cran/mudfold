\name{MUDFOLData}
\alias{MUDFOLData}
\docType{data}
\title{Simulated data set obtained from the \code{\link{mudfoldsim}} function. 
}
\description{
 Artificial binary data containing the responses of 3000 individuals on 15 proximity items. The responses are given in a random item order.
}
\usage{data("MUDFOLData")}
\format{
  A data frame with 3000 observations on the following 15 variables.
  \describe{
    \item{\code{N}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{N}}
    \item{\code{E}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{E}}
    \item{\code{A}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{A}}
    \item{\code{I}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{I}}
    \item{\code{O}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{O}}
    \item{\code{F}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{F}}
    \item{\code{M}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{M}}
    \item{\code{G}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{G}}
    \item{\code{L}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{L}}
    \item{\code{H}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{H}}
    \item{\code{K}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{K}}
    \item{\code{J}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{J}}
    \item{\code{D}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{D}}
    \item{\code{B}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{B}}
    \item{\code{C}}{a binary variable that contains simulated dichotomous responses of 3000 individuals on item \code{C}}
  }
}
\details{
This data has been generated under a parametric unimodal function which is implemented in \pkg{mudfold} package with \code{\link{mudfoldsim}} function. The IRF parameters for this data were \code{pgamma1=10} and \code{pgamma2=-100}. The responses are given in a random item order. The true scale that sufficiently measures the assummed latent trait is given in an alphabetical order, i.e., \code{A<B<...<O}. }
\author{Spyros E. Balafas.

Maintainer: Spyros E. Balafas (\email{s.balafas@rug.nl})
}
\seealso{
 \code{\link{mudfoldsim}}
 }
\examples{
data(MUDFOLData)
str(MUDFOLData)
}
\keyword{datasets}
