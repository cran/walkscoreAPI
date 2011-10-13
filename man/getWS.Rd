\name{getWS}
\alias{getWS}
\title{Walk Score API Call}
\description{A function to perform the basic Walk Score API call.}
\usage{getWS(x, y, key)}
\arguments{
  \item{x}{  longitude of query location (numeric)  }
  \item{y}{  latitude of query location (numeric)  }
  \item{key}{  your Walk Score API key (string), see Details below  }
}
\details{Note that the call uses longitude and latitude coordintes and not addresses like the website interface. It is strongly recomended that Google Geolocation is used to convert addresses to coordinates because this is the method used by the Walk Score website, and will result in the same Walk Score as entering the address into the website interface. The function "geoloc" in this package is a tool for using the Google Geolocation API.}
\value{ Otherwise Returns an object of class \code{WalkScore}, basically a list of the following elements:
  \item{status}{  Status code of the request. Status of 1 indicates a successful call. See the Walk Score API page for interpretation of other codes.  }
  \item{walkscore}{  Walk Score of query location.  } 
  \item{description}{  Qualitative description of location.  }
  \item{updated}{  Date and time of most recent update to this location's Walk Score.  }
  \item{snappedLong}{  grid point longitude to which the input was snapped to.  }
  \item{snappedLat}{  grid point latitude to which the input was snapped to.  }
}
\references{  http://www.walkscore.com/professional/api.php  }
\author{  John Whalen  }
\note{  Visit www.walkscore.com for information on Walk Score and to obtain an API key  }


\seealso{
  \code{\link{geoloc}}}

\examples{
\dontrun{
getWS(-73.98496,40.74807,"your key")
}
}