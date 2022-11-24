\name{plot.mdf}
\alias{plot.mdf}
\title{\code{plot} function for \code{"mdf"} class objects.
}
\description{Generic function for plotting S3 class \code{"mdf"} objects. This function, is plotting the rows of the conditional adjacency matrix (CAM) which are nonparametric estimates of the item response functions. The plot is produced using the \code{ggplot} function from the package \pkg{ggplot2}.
}
\usage{
\method{plot}{mdf}(x, select, plot.type, ...)
}
\arguments{
    \item{x}{Object of class \code{mdf}}
    \item{select}{: in this argument the user can provide a subset of items he would like them to be explicitly plotted. If the \code{select} argument is empty the estimated IRF for every item in the scale is plotted. When \code{plot.type="persons"} this argument is ignored.}
    \item{plot.type}{: Determines the type of plot that is returned. By default, \code{plot.type="IRF"}, which returns the estimated IRFs for the items in the MUDFOLD scale. The user can set \code{plot.type="scale"} in order to get plotted the unidimensional MUDFOLD scale. Setting \code{plot.type="persons"} will return the distribution of the person parameters on the latent scale.}
    \item{\dots}{Other arguments passed on to \code{ggplot} plotting method.}
}

\details{
The \code{plot} method is used to obtain a graphical representation of the estimated rank order of the items, the item response functions, and the distribution of the person parameters. As estimates of the IRFs are considered the rows of the CAM. For interpolating the missing diagonal elements of the CAM, we make use of the \code{na.approx} function from the package \pkg{zoo}.
}

\author{Spyros E. Balafas (auth.), Wim P. Krijnen (auth.), Wendy J. Post (contr.), Ernst C. Wit (auth.)

Maintainer: Spyros E. Balafas (\email{s.balafas@rug.nl})
}

\references{
W.H. Van Schuur.(1984). \emph{Structure in Political Beliefs: A New Model for
Stochastic Unfolding with Application to European Party Activists}. CT Press.

W.J. Post. (1992). \emph{Nonparametric Unfolding Models: A Latent Structure Approach}. M
& T series. DSWO Press.

W.J. Post and T.AB. Snijders. (1993).Nonparametric unfolding models for dichotomous
data. Methodika.

A. Zeileis and G. Grothendieck. (2005). zoo: S3 Infrastructure for Regular and Irregular Time Series. Journal of
Statistical Software, 14(6), 1-27. doi:10.18637/jss.v014.i06

H. Wickham. (2009). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York.

H. Wickham. (2007). Reshaping Data with the reshape Package. Journal of Statistical Software, 21(12), 1-20. URL
http://www.jstatsoft.org/v21/i12/.


  


}

\examples{
\dontrun{
data(ANDRICH)
fit <- mudfold(ANDRICH)
plot(fit, plot.type= "scale")
plot(fit, plot.type= "IRF")
plot(fit, plot.type= "persons")
plot(fit, select="DONTBELIEV", plot.type= "IRF")
}
}
\keyword{methods}

