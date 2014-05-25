################################################################################
## These functions store or calculate the inverse of a matrix.                ##
## Note: The descriptions and code are simply modified versions of the samples #
##       provided to the class in order to produce the requested results.     ##
##       Coursera: R Programming, Week 3, Assignment 2. User: TinaVee         ##
################################################################################


## The first function, `makeCacheMatrix` creates a special "matrix" object that 
## can cache its inverse.

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { #stores original matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x # get original matrix
        setinverse <- function(solve) m <<- solve # solves and stores inverse
        getinverse <- function() m # get inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinverse`
## function.  

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) { #check to see if inverse is stored
                message("getting cached data")
                return(m) #return stored inverse
        }
        #if inverse is not stored, solve inverse
        data <- x$get() 
        m <- solve(data, ...)
        x$setinverse(m) 
        m #return calculated inverse
}
