# Two functions to provide functionality for calculation of inverse matrix and cached storage.
## Call function cacheSolve with argument x is function makeCacheMatrix(<matrix>). 
## The inverse of <matrix> is calculated and stored or retrieved from memory
## based on code written by RD Peng as part of R Programming course

# makeCacheMatrix
## This function is a holder for 4 functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of a matrix
## 4. get the inverse of a matrix
## with this function it is possible to cache a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
	  ###set function
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

	  ###get function
        get <- function() x

	  ###set the inverse to inverse argument 	
        setInverse <- function(inverse) i <<- inverse

	  ###get the inverse calculated earlier
        getInverse <- function() i

        ###set values of resulting variables from makeCacheMatrix function
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# cacheSolve
## This function checks if the inverse of the matrix was already calculated
## If already calculated: get from cache. Otherwise, calculate new.
cacheSolve <- function(x, ...) {
        i <- x$getInverse()

	  ###check if value is already calculated. if not calculated: i = NULL (by default)
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

	  data <- x$get()    ### if not calculated before, get matrix first
        i <- solve(data)   ### calculate the inverse of matrix x	
        x$setInverse(i)    ### set the inverse matrix i into the cache
		
	  ### Return a matrix that is the inverse of 'x' 
        i
}
