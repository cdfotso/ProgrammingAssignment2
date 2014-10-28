## Put comments here that give an overall description of what your
## functions do

##These functions compute the matrix inverse, 
##with a mechanism of cache to not compute more than once an inverse that has already been computed

## Write a short comment describing this function

##This function creates a special matrix, which is a
##list containing the methods needed to get and set the matrix and it inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <-function(inverse) inv <<- inverse
        getinverse <-function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

##This function returns the inverse of a matrix from the
##cache if it has been already computed. If not, it will first
##compute it, store the result in the cache and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached inverse matrix")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinverse(inv)
        inv
}
