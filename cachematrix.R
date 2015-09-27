## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Initializing variable inversedMatrix that has an inversed matrix of itself.
        ## It is used as cache with an environmental value.
        inversedMatrix <- NULL
        
        ## Resetting a matrix of makeCacheMatrix.
        set <- function(y) {
                x <<- y
                inversedMatrix <<- NULL
        }
        ## Getting a matrix x.
        get <- function() x
        ## Setting an inversed Matrix with assigning a value to an object in an environment .
        setsolve <- function(solve) inversedMatrix <<- solve
        ## Getting an inversed Matrix of this
        getsolve <- function() inversedMatrix
        ## create list object to make methods of an object of OOP
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## Write a short comment describing this function
## parameter x is a object that was made by makeCacheMatrix() with a square matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        
        if(!is.null(m)) { ## check existing a cached data.
                message("getting cached matrix")
                return(m)  ## returnning a cached data.
        }
        ## Getting a matrix in x
        data <- x$get()
        ## computing inversing a matrix.
        m <- solve(data)
        ## Setting inversed matrix into cache of x
        x$setsolve(m)
        m  ## print inversed matrix
}
