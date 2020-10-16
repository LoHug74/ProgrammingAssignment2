# The cacheMatrix file contains two functions, 
# makeCacheMatrix() and cacheSolve(). 
# The first function in the file, makeVector() creates an R object that stores a matrix and its inverse 
# The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() in order to retrieve the inverse
# from the cached value that is stored in the makeCacheMatrix() object's environment

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                      s <- NULL
                      set <- function(y) {
                              x <<- y # Assign the input argument to the x object in the parent environment
                              s <<- NULL # clears any value of m that had been cached by a prior execution of cacheSolve
                              }
                      get <- function() x # retrieves x from the parent environment of makeCacheMatrix
                      setinv <- function(inv) s <<- inv #  assign the inv argument to the value of s in the parent environment
                      getinv <- function() s # retrieves s from the parent environment of makeCacheMatrix
                      list(
                            set = set, get = get,
                            setinv = setinv,
                            getinv = getinv
                           )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                s <- x$getinv() # retrieve the inverse from the object passed in as the argument. First, it calls the getinv() function from makeCacheMatrix on the input object
                if(!is.null(s)) { # checks if s is not null, then retrieve the cached value
                                message("getting cached data")
                                return(s)
                                }
                data <- x$get() # here s is null, so retrieves x from cached env. 
                s <- solve(data, ...) # computes the inverse of the retrieved x
                x$setinv(s) # calls setinv on s, to cach for later usage
                s # returns s, ie, the inverse of x
}