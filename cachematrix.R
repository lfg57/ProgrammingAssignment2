## The following functions are used to compute the inverse of a square matrix
## and cache the result so the computation does not have to be done repeatedly.

## 1. `makeCacheMatrix()` creates a special "matrix" object that can cache its
##    inverse.
## 2. `cacheSolve()` computes the inverse of the special "matrix" returned by
##    `makeCacheMatrix`. If the inverse has already been calculated (and the 
##    matrix has not changed), then `cachesolve()` retrieves the inverse from
##    the cache.


## `makeCacheMatrix()` takes a square matrix `X` as argument, which is assumed
## to be invertible, and returns a list containing functions to##
## 1. set the matrix,
## 2. get the matrix,
## 3. set the inverse of the matrix, and
## 4. get the inverse of the matrix.
## ---------------------------------------------------------------------------
makeCacheMatrix <- function(X = matrix()) {
    invX <- NULL                                # Set the inverse to NULL.
    set <- function(Y) {                        # Call set(arg) to:
        X <<- Y                                 # - assign 'arg' to X, and
        invX <<- NULL                           # - reset the inverse to NULL.
    }
    get <- function() { X }                     # Call get() to return the matrix X.
    setinv <- function(inv) { invX <<- inv }    # Call setinv(arg) to set invX to inv.
    getinv <- function() { invX }               # Call getinv() to return invX.
    list(set = set, get = get,                  # Return a list containing the defined
         setinv = setinv, getinv = getinv)      #  functions.
}


## `cacheSolve()` takes the list of functions returned by `makeCacheMatrix()` as
## argument and returns the inverse of the given square matrix `X`.
## ---------------------------------------------------------------------------
cacheSolve <- function(L, ...) {
    ## Return a matrix that is the inverse of 'X'
    invX <- L$getinv()                          # Call L$getinv() to get the inverse.
    if (!is.null(invX)) {                       # If invX is not NULL, return invX
        message("getting cached data")          #  and end.
        return(invX)
    }                                           # Otherwise,
    data <- L$get()                             # - call L$get() to get the matrix X,
    invX <- solve(data, ...)                    # - calculate the inverse of the matrix,
    L$setinv(invX)                              # - set the value of the inverse in the
    invX                                        #   cache and return the inverse of X.
}
