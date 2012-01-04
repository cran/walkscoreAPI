\name{walkscoreAPI-package}
\alias{walkscoreAPI-package}
\alias{walkscoreAPI}
\docType{package}
\title{Walk Score and Transit Score API}
\description{A collection of functions to perform the Application Programming Interface (API) calls associated with the Walk Score website (www.walkscore.com) within the R environment. These functions can be used to query the Walk Score and Transit Score database for a wide variety of information using R scripts. This package includes the simple Walk Score and Transit Score API calls, which return the scores associated with an input location, as well as calls which return some data used to calculate the scores. These functions are especially useful for mass data collection and gathering Walk Score and Transit Score values for large lists of locations. }
\details{
\tabular{ll}{
Package: \tab walkscoreAPI\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2012-01-04\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
Every function in this package requires the use of a Walk Score API key number, enetered as a parameter. The key is free to obtain with limited use, and can be requested here: http://www.walkscore.com/professional/api.php. The easiest way to enter the key is to store the string as a variable and entering that variable as a parameter for the function calls.
}
\author{ John Whalen

Maintainer: <whalenjf@gmail.com>
}
\references{ Visit www.walkscore.com for information on Walk Score and to obtain an API key  }

\keyword{ package }


\examples{
\dontrun{
test <- geoloc("350 5th Avenue New York NY", "Google API key")
getWS(test$coordinates[1],test$coordinates[2], "Walk Score API key")
getTS(test$coordinates[1],test$coordinates[2],"New York","NY","Walk Score API key")
}
}
