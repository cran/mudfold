\name{Loneliness}
\alias{Loneliness}
\docType{data}
\title{De Jong-Gierveld loneliness scale 
}
\description{ De Jong-Gierveld loneliness scale that consists of eleven ordinal items. Five of these items are positively formulated and six are negatively formulated. Each of the items has three possible response categories.
}
\usage{data(Loneliness)}
\format{
  A data frame with 3987 observations on the following 11 variables.
  \describe{
    \item{\code{A}}{: a column vector containing the ordinal responses on the statement:
    
"There is always someone I can talk to about my day to day problems (+)"}

    \item{\code{B}}{a column vector containing the ordinal responses on the statement:

    "I miss having a really close friend (-)"}

    \item{\code{C}}{a column vector containing the ordinal responses on the statement:

    "I experience a general sense of emptiness (-)"}

    \item{\code{D}}{a column vector containing the ordinal responses on the statement:

    "There are plenty of people I can lean on in case of trouble (+)"}

    \item{\code{E}}{a column vector containing the ordinal responses on the statement:

    "I miss the pleasure of company of others  (-)"}

    \item{\code{F}}{a column vector containing the ordinal responses on the statement:

    "I find my circle of friends and acquaintances too limited  (-)"}

    \item{\code{G}}{a column vector containing the ordinal responses on the statement:

    "There are many people that I can count on completely (+)"}

    \item{\code{H}}{a column vector containing the ordinal responses on the statement:

    "There are enough people that I feel close to (+)"}
    
    \item{\code{I}}{a column vector containing the ordinal responses on the statement:

    "I miss having people around (-)"}
      \item{\code{J}}{a column vector containing the ordinal responses on the statement:

    "Often I feel rejected  (-)"}
      \item{\code{K}}{a column vector containing the ordinal responses on the statement:

    "I can call on my friends whenever I need them (+)"}

}
}
\details{
Each item in the scale has three possible levels of response, i.e., "no" (=1), "more or less" (=2), "yes" (=3). The data is a subset of the NESTOR study (see C. P. Knipscheer, J. d. Jong-Gierveld, T. G. van Tilburg, P. A. Dykstra, et al. (1995))
}
\source{
G. J. De Jong and T. van Tilburg (1999). Manual of the loneliness scale. Amsterdam: VU University Amsterdam. 
}


\references{
C. P. Knipscheer, J. d. Jong-Gierveld, T. G. van Tilburg, P. A. Dykstra, et al. (1995). Living arrange-ments and social networks of older adults.Amsterdam: VU University Amsterdam.


J. de Jong-Gierveld and F. Kamphuls (1985).  The development of a rasch-type loneliness scale.Applied psychological measurement, 9(3):289-299. 

G. J. De Jong and T. van Tilburg (1999). Manual of the loneliness scale. Amsterdam: VU University Amsterdam. 

W. J. Post, M. A. van Duijn, and B. van Baarsen (2001). Single-peaked or monotone tracelines? onthe choice of an irt model for scaling data. InEssays on item response theory, pages 391-414.Springer.

}
\examples{
\dontrun{
data(Loneliness)
str(Loneliness)
}
}
\keyword{datasets}
