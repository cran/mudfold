\name{pick}
\alias{pick}
\title{
Transform items to preference binary data.
}
\description{
Function \code{pick} can be used to transform quantitative or ordinal type of variables, into binary form (i.e., \code{0},\code{1}). When \code{byItem=FALSE}, then the underlying idea is that the individual selects those items with the higher preference. This is done through user provided cut-off values, or by assuming a \emph{pick} \code{k} \emph{out of} \code{N} response process, where, each continuous response vector takes a \code{1} at its \code{k} higher values. Dichotomization can be performed row-wise (default) or column-wise.
}
\usage{
pick(data , k=NULL, cutoff=NULL, byItem=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{: A matrix or data frame containing the continuous or discrete responses of \code{nrow(data)} persons/judges
to \code{ncol(data)} items. Missing values in \code{data} are not allowed.
}
  \item{k}{: An integer (\eqn{1 \le} \code{k} \eqn{\le} \code{ncol(data)}) that restricts the number of items a person can pick (default \code{k=NULL}). This argument, is used if one wants to transform the data into \emph{pick} \code{k} \emph{out of} \code{N} form. If \code{k} is provided by the user, \code{cutoff} should be \code{NULL} and vice verca. By default, this process is applied to the matrix \code{data} rowise. The user can restrict the number 
}
  \item{cutoff}{:The value(s) that will be used as thresholds. The length of this argument should be equal to 1 (the same threshold for all rows (or columns) of \code{data}) or equal to \code{K} where \code{K=nrow(data)} or \code{K=ncol(data)} when \code{byItem=TRUE}.
}
\item{byItem}{: logical argument. If byItem=TRUE, the dichotomization is performed columnwise. In the default byItem=FALSE, the function determines the ones rowise.
}
}
\details{
Binary transformation of continuous or discrete variables with \eqn{\rho\ge 3} number of levels. Two different methods are available for the transformation. 

The first method uses the argument \code{k} in the \code{pick} function, and assumes a \emph{pick} \code{k} \emph{out of} \code{N} response process. Such type of response processes are met in surveys and questionnaires, in which respondents are asked to pick exactly the \code{k} most preferred items. The value for \code{k} is an integer between 1 and \code{ncol(data)}. By choosing an integer for \code{k}, this function ''picks'' the \code{k} higher values in each row (if \code{byItem=FALSE}) of \code{data}. The \code{k} higher values in each row become 1 and the rest \code{ncol(data)-k} elements are set to 0. Obviously, if \code{k=ncol(data)}, then the resulting matrix will only consists of 1's and no 0's.

The second method is based on thresholding in order to binarize the data. For this method, the user should provide threshold(s) with the parameter \code{cutoff} in the \code{pick} function (default \code{cutoff=NULL}). If one value is provided in the \code{cutoff} parameter, i.e., \code{cutoff=}\eqn{\alpha}, then \eqn{\alpha} is used as threshold in each row \eqn{i} (if \code{byItem=FALSE}) of the data matrix \code{data} such that, any value greater than or equal to \code{cutoff} in row \eqn{i} becomes 1 and 0 else. Additionally, the user can provide row (or column) specific cut off values, i.e., \code{cutoff=}\eqn{\alpha} with \eqn{\alpha=(\alpha_1,...,\alpha_K)} where \eqn{\alpha_i} is the cut-off value for the row or column \eqn{i}. In this case, if \eqn{x_{ij}\ge \alpha_i} then \eqn{x_{ij}=1} and \eqn{x_{ij}=0} else.

The two methods cannot be used simultaneously. Only one of the parameters \code{k} and \code{cutoff} can be different than \code{NULL} each time. If both parameters are equal \code{NULL} (default), then a row specific cut off is determined automatically for each row \eqn{i} of \code{data}, such that, \eqn{\alpha_i= \bar{data_i}}. The dichotomization is performed by row of \code{data}, except the case, \code{byItem=TRUE}.

When the argument \code{k} is used, it can be the case that more than \code{k} values can be picked (i.e., ties). In this case, the choice on which item will be picked is being made after we add a small amount of noise in each observation of row or column \eqn{i}. This is done with the function \code{jitter}.
}
\value{
Binary valued (i.e., 0-1) data with the same dimensions as the input.
}

\section{Warning}{
\strong{!!!} This function should be used with care. Dichotomization may distort the data structure and lead to potential information loss. In the case of polytomous items, the user is suggested to consider polytomous unfolding models that take into account different levels of measurement. \strong{!!!}}

\author{Spyros E. Balafas (auth.), Wim P. Krijnen (auth.), Wendy J. Post (contr.), Ernst C. Wit (auth.)

Maintainer: Spyros E. Balafas (\email{s.balafas@rug.nl})
}

\examples{
\dontrun{ 
### simulate some data with 3 discrete variables with three levels
### and 1 variable with 4 levels
d1 <- cbind(sample(1:3,20,replace = TRUE),
            sample(1:3,20,replace = TRUE,prob = c(0.3,0.3,0.4)),
            sample(1:3,20,replace = TRUE,prob = c(0.2,0.4,0.4)),
            sample(1:4,20,replace = TRUE,prob = c(.1,.3,.4,.2)))


### apply pick on d1 ###  
# binarize at the mean of 
# each row and column
d1_rowmean <- pick(d1)
d1_colmean <- pick(d1,byItem = TRUE)

# binarize at the cutoff=2 
d1_cut <- pick(d1,cutoff = 2,byItem = TRUE)

# binarize at different cutoffs (per row) 
# for example at the median of each row
med_cuts <- apply(d1,1,median)
d1_cuts <- pick(d1,cutoff = med_cuts)

# binarize at different cutoffs (per column) 
# for example at the median of each column
med_cuts_col <- apply(d1,2,median)
d1_cuts_col <- pick(d1,cutoff = med_cuts_col,byItem = TRUE)


# binarize at the k=2 higher values
# per row and column
d1_krow <- pick(d1,k = 2)
d1_kcol <- pick(d1,k = 2,byItem = TRUE)
}
}