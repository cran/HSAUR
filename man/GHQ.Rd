\name{GHQ}
\alias{GHQ}
\docType{data}
\title{ General Health Questionnaire }
\description{
  Data from an psychiatric screening questionnaire 
}
\usage{data("GHQ")}
\format{
  A data frame with 22 observations on the following 4 variables.
  \describe{
    \item{\code{GHQ}}{the General Health Questionnaire score.}
    \item{\code{sex}}{a factor with levels \code{female} and \code{male}}
    \item{\code{cases}}{the number of diseased subjects.}
    \item{\code{non.cases}}{the number of healthy subjects.}
  }
}
\details{

  The data arise from a study of a psychiatric screening questionnaire 
  called the GHQ (General Health Questionnaire, see Goldberg, 1972). 
  Here the main question of interest is to see how caseness is related to 
  gender and GHQ score.

}
\source{

  D. Goldberg (1972). \emph{The Detection of Psychiatric Illness by Questionnaire}, 
  Oxford University Press, Oxford, UK.

}
\examples{

  data("GHQ", package = "HSAUR")
  male <- subset(GHQ, sex == "male")
  female <- subset(GHQ, sex == "female")
  layout(matrix(1:2, ncol = 2))
  barplot(t(as.matrix(male[,c("cases", "non.cases")])), main = "Male", xlab = "GHC score")
  barplot(t(as.matrix(male[,c("cases", "non.cases")])), main = "Female", xlab = "GHC score")

}
\keyword{datasets}
