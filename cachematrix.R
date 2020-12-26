## Below are two functions that can be used to store an invertible matrix and cache it's inverse

## The first function takes an invertible matrix as argument (x) and returns a list of functions that can be used to 
## 1. Set the value of the matrix x
## 2. Get the value of the matrix x
## 3. Set the value of the inverse in the global environment
## 4. Get the value of the inverse if it's already present in the global environment

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
          }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## The second function checks if the value of the inverse matrix is available in global environment. 
## If it is, the function returns this value, else it solves for the inverse and returns the output.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
          }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
