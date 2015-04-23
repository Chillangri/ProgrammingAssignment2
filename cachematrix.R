## Overall description for using this functions
##   1) Assing a function of makeCacheMatrix to an arbitrary list object
##      for example: > t <- makeCacheMatrix()
##   2) Create a special matrix using method 'set' with an arbitrary matrix
##      for example: > tempX <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4,0), nrow=3)
##                   > t$set(tempX)
##   3) Using cacheSolve() function, find an inverse
##      if there is no chched inverse value, this function will find inverse
##      for example: > chcheSolve(t)
##                         [,1] [,2] [,3]
##                   [1,]  -24   18    5
##                   [2,]   20  -15   -4
##                   [3,]   -5    4    1
##       if there is a cached value, this function will show the cached value
##      for example: > chcheSolve(t)
##                   getting cached data
##                         [,1] [,2] [,3]
##                   [1,]  -24   18    5
##                   [2,]   20  -15   -4
##                   [3,]   -5    4    1


## Sample inversible matrix testing to find a inverse matrix
#   tempX <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4,0), nrow=3)

## Function Name: makeCacheMatrix
##   1) makeCahceMatrix() is a closure function
##   2) This function creates a special matrix object that can cache its inverse in a variable of mtx
makeCacheMatrix <- function(x = matrix()) {
    mtx <- NULL
    set <- function(y) {
        x <<- y
        mtx <<- NULL
    }
    get <- function() {
        x
    }
    solveinverse <- function(solve) {
        mtx <<- solve
    }
    getinverse <- function() {
        mtx
    }
    
    list(set = set,
    get = get,
    solveinverse = solveinverse,
    getinverse = getinverse)
}


## Function Name: cacheSolve
##   1) This function use an instance of makeCacheMatrix() as a parameter
##   2) If there is a cached value, this function will just return it.
##   3) If there is no cached valeu, this function sovle it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mtx <- x$getinverse()
    if(!is.null(mtx)) {
        message("getting cached data")
        return(mtx)
    }
    data <- x$get()
    mtx  <- solve(data, , ...)
    x$solveinverse(mtx)
    mtx
}
