\name{checkTSsupport}
\alias{checkTSsupport}
\title{Check Transit Score Support}
\description{A function to check if a city of interest is among the cities supported by the Transit Score.}
\usage{
checkTSsupport(city, state, key)
}
\arguments{
  \item{city}{  name of city of interest (string)  }
  \item{state}{  postal abbreviation of city's state (string)  }
  \item{key}{  your Walk Score API key (string)  }
}
\details{ Transit Score only works in select cities. }
\value{TRUE if city is supported, FALSE otherwise
}
\references{  http://www.walkscore.com/professional/public-transit-api.php  }
\author{  John Whalen  }
\note{  Visit www.walkscore.com for information on Walk Score and to obtain an API key  }


\seealso{
 \code{\link{getTScities}}
}

\examples{
\dontrun{
checkTSsupport("Buffalo","NY","your key")
}
}