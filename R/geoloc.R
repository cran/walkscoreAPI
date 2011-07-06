geoloc <-
function(address,apikey){
    place <- address
    place <- gsub(" ","+",place)

    URL <- paste("http://maps.google.com/maps/geo?q=",place,"&output=json&oe=utf8\
      &sensor=true_or_false&key=",apikey,sep="")

    X <- character(0)
    X <- c(X, scan(file = URL, what = "", sep = "\n", quiet = TRUE))

    coord <- grep("\"coordinates\":",X)
    coord <- strsplit(X[coord],"\"coordinates\"")
    coord <- coord[[1]][2]
    coord <- gsub(": [ ","",coord,fixed=TRUE)
    coord <- strsplit(coord[[1]],",")

    long <- as.numeric(coord[[1]][1])
    lat <- as.numeric(coord[[1]][2])

    acc <- grep("Accuracy",X)
    acc <- strsplit(X[acc]," : ")
    acc <- gsub(",","",acc[[1]][2])
    acc <- as.numeric(acc)

    print(paste("Accuracy: ", acc, sep=""))
    return(c(long,lat))
}

