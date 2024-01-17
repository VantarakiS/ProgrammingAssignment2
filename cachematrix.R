makeCacheMatrix <- function(x = matrix()) {
  
  inver <- NULL
  
  setMtrx <- function(Value) {
    x <<- Value
    inver <<- NULL
  }
  
  getMtrx <- function() {
    x
  }
  
  setInver <- function(newinver) {
    inver <<- newinver
  }
  
  getInver <- function() {
    inver
  }
  
  list(setMtrx = setMtrx,
       getMtrx = getMtrx,
       setInver = setInver,
       getInver = getInver)
}

cacheSolve <- function(cacheMtrx, ...) {
  cachedInver <- cacheMtrx$getInver()
  
  if (!is.null(cachedInver)) {
    message("Loading data...")
    return(cachedInver)
  }
  
  mInverse <- cacheMtrx$getMtrx()
  inverMtrx <- solve(mInverse, ...)
  
  cacheMtrx$setInver(inverMtrx)
  
  inverMtrx
}

