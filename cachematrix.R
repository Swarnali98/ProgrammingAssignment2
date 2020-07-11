## Inverse of matrix
## Two functions will be created

##"makeCacheMatrix" Creates a special matric that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
j <- NULL
        set <- function(y){
                x <<- y
                j <<- NULL
}
get <- function()x
        setInverse <- function(inverse) j <<- inverse  ##set the value of the matrix
        getInverse <- function()j                      ##get the value of the matrix
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)   ##set the value of the inverse (setInverse) and to get the value of the inverse (getInverse)
        }

## Computes the inverse of the matrix made by makeCacheMatrix
## `cacheSolve`: This function computes the inverse of the special
##  "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then
##  `cacheSolve` should retrieve the inverse from the cache.
##  `cacheSolve` should retrieve the inverse from the cache with the
##  message "getting cached date" .  Inverse is calculated using function
##  'solve()'


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j <- x$getInverse()
        if(!is.null(j)){
                message("getting cached data")
                return(j)
                }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j
}
