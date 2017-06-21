\name{mychol}
\alias{mychol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function that finds cholesky decomposition for a hermitian positive def matrix
}
\description{
function that finds cholesky decomposition for a hermitian positive def matrix. Can be used for many applications
}
\usage{
mychol(A)
}

\arguments{
  \item{A}{
Insert hermitian positive definite matrix, and 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
value returned is a matrix 
}
\references{

}
\author{
Santiago Paz
}
\note{
%%  ~~further notes~~
}

\section{Warning }{Matrix not valid} 

\seealso{
Nothing
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (A) 
{
    l <- matrix(0, nrow(A), nrow(A))
    l[1, 1] <- sqrt(A[1, 1])
    for (i in 2:nrow(A)) {
        for (j in 1:i - 1) {
            l[i, j] <- 1/l[j, j] * (A[i, j] - sum(l[i, 1:j - 
                1] * l[j, 1:j - 1]))
            l[i, i] <- sqrt(A[i, i] - sum(l[i, 1:i - 1]^2))
        }
    }
    return(l)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }% use one of  RShowDoc("KEYWORDS")
