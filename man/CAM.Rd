\name{CAM}
\alias{CAM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Conditional adjacency matrix (CAM) for dichotomously scored items. 
}
\description{
This function is used to calculate the conditional adjacency matrix (CAM) from a binary valued matrix with the responses of \emph{n} individuals to \emph{N} items (Post,1992). CAM in its \emph{(i,j)}th element contains the conditional frequency that a subject from the sample will choose the row item \emph{i} given that the column item \emph{j} is chosen. The probability \eqn{Pr(X_i=1 | X_j=1)} is estimated from the data by dividing the joint frequency of choosing both items \emph{i} and \emph{j} by the relative frequency of choosing item \emph{j}. Different orderings of the columns of the input matrix will result into different CAM matrices.
}
\usage{
CAM(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{: A binary matrix or data frame containing the responses of \code{nrow(data)} persons
to \code{ncol(data)} items. In this case, missing values in \code{x} are not allowed. Alternatively, \code{x} can be a fitted object of class \code{"mdf"} from the \code{mudfold()} function. Then the function will extract the CAM for the obtained MUDFOLD scale.
}
}
\details{
It calculates the CAM based on the following equation,

\eqn{
{CAM}_{ij}=\ \frac{\sum_{k=1}^n x_{ki}  x_{kj} /  n}{\sum_{k=1}^n x_{kj} /n} = \frac{\sum_{k=1}^n x_{ki}  x_{kj} }{\sum_{k=1}^n \ x_{kj} }, \ {for}\ i\neq j.
}
}
\value{
A matrix of class 'cam.mdf', with \code{ncol(x)} rows and \code{ncol(x)} columns with missing values on the diagonal elements when \code{x} is a matrix or data frame. When \code{x} is an object of class \code{"mdf"} the dimension of the output matrix depends on the length of the obtained MUDFOLD scale. Rows and columns of the resulting CAM are ordered in the order of the columns of \code{x} when \code{x} is a matrix. When \code{x} is a fitted MUDFOLD object then the rows and columns of CAM are ordered in the obtained MUDFOLD order.
}
\references{
W.J. Post. (1992). \emph{Nonparametric Unfolding Models: A Latent Structure Approach}. M
& T series. DSWO Press.

W.J. Post. and T.AB. Snijders. (1993). \emph{Nonparametric unfolding models for dichotomous
data}. Methodika.
}
\author{
Spyros E. Balafas (\email{s.balafas@rug.nl})
}



\examples{
## load the ANDRICH data
data("ANDRICH")

## Calculate the CAM for the ANDRICH scale
CAM_andrch <- CAM(ANDRICH)

## Extract CAM from a fitted mudfold object 
mudf_andrich <- mudfold(ANDRICH)
CAM_andrch_mudfold <- CAM(mudf_andrich)
}

