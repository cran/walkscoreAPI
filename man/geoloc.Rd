\name{geoloc}
\alias{geoloc}
\title{Google Geolocation API Call}
\description{A function to perform the Google Geolocation API call, and return the longitude and latitude coordinates of the query location. }
\usage{
geoloc(address, apikey)
}
\arguments{
  \item{address}{  query address (string). Do not use commas, and zip codes are not required, e.g. "1600 Pennsylvania Ave Washington DC".}
  \item{apikey}{  your Google API key (string)  }
}
\details{Use of this function requires a Google API key, which is different from the Walk Score API key. Get one here: http://code.google.com/apis/maps/signup.html}
\value{Returns a vector of two numbers, the first representing the longitude and the second representing the latitude. Prints accuracy rating of geolocation.
}
\references{  http://code.google.com/apis/gears/api_geolocation.html  }
\author{John Whalen}
\note{ For description of Google Geolocation see here: http://code.google.com/apis/gears/api_geolocation.html
}


\examples{
\dontrun{
geoloc("350 5th Avenue New York NY","your Google API key")
}
}