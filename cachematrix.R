
## makeCacheMatrix is a function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function (x = matrix()){
    # Initiate i value to NULL
    i <- NULL
    # Define function to set the value of the matrix
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    # Define the function to get the value of matrix
    get <- function()x
    # Set the value of inverse
    setinverse <- function(inverse){i <<- inverse}
    # Get the value of inverse
    getinverse <- function(){i}
    # return a list of of all the four functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    # in environemnt, call function getinverse and assign the value to i
    i <- x$getinverse()
    # if the inverse value is not empty, we just return the value
    if(!is.null(i)){
        message("getting cached data")
      ## Return a matrix that is the inverse of 'x'
      return(i)
    }
    # if the inverse value is empty, we need to calculate the value, cache it and then return it.
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
