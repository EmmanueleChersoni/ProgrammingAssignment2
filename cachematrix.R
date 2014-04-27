## The first function is very similar to the one used for caching the mean of a vector.
## It takes an argument of type matrix and returns a list of 4 functions:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        	m <- NULL
        	set <- function(y) {
                			x <<- y
                			m <<- NULL
        					}
        	get <- function() x
        	setinv <- function(solve) m <<- solve
        	getinv <- function() m
        	list(set = set, get = get, setinv = setinv, getinv = getinv)
											}

## The second function queries in the cache of the vector created by the makeCacheMatrix.
## If there is an inverse matrix already stored, it will be returned.
## Otherwise, the inverse matrix will be computed (with the function solve) and saved in the cache of the vector (with the function setinv).
## Finally, the result of the computation is returned.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        		  if(!is.null(m)) {
                	  message("getting cached data")
                	  return(m)
        					}
        		  data <- x$get()
        		  m <- solve(data, ...)
        	        x$setinv(m)
        		  m
								}
