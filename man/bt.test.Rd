\name{bt.test}
\alias{bt.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Test goodness-of-fit of the Bradley-Terry model.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function tests the assumptions on the strict linear dominance structure made by the Bradley-Terry model.  Specifically, that if Pr(A beats B) > 0.5 and Pr(B beats C) > 0.5, then Pr(A beats C) > 0.5 and Pr(A beats C) > Pr(B beats C).
}
\usage{
condTest(conf.mat, baseline = 1, maxLength = 3, reps = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conf.mat}{
  Either a square matrix with positive integer values or an object of class "conf.mat".}
  \item{baseline}{
  An integer between 1 and n, where n is the number of competitors.  This value
  can slightly effect the Bradley-Terry estimates, but usually by a negligable amount.}
  \item{maxLength}{
  An integer between 1 and n, where n is the number of competitors.  This value indicates the maximum length of dominance paths used in the percolation estimates.}
  \item{reps}{
  A positive integer.  Sets the number of simulations used to estimate the distribution of the test-statistics under the Bradley-Terry model.}
  }
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{stat}{The value of the test statistic.}
\item{dist}{A vector of the simulated values for the sampling distribution
of the test statistic under the Bradley-Terry model.}
}
\references{
%% ~put references to the literature/web site here ~
Shev, A.; Fujii, K.; Hsieh, F.; McCowan B. Systemic Testing on Bradley-Terry Model against Nonlinear Ranking Hierarchy. PLoS ONE 9(12): e115367.
}
\author{
Aaron Shev
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
