makeVector <- function(x = numeric()) {
  m <- NULL  ## begins by setting the mean to NULL as a placeholder for a future value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }   ## this defines a function to set the vector, x, to a new vector, y, and resets the mean, m, to NULL
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
  ##The following function calculates the mean of the vector created with the above function
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
