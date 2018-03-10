## This function creates a special matrix object that catches its inverse without much work

## We will create makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)

}


## cacheSolve is function that computes the inverse of the special "matrix" created above 
## If the inverse has already been calculated (and the matrix has not changed),
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
          if(!is.null(inv)) {
                    message("retrieving cached result")
                    return(inv)
                  }
          my_mat <- x$get()
          inv <- solve(my_mat, ...)
          x$set_inv(inv)
          inv
}
##.............................................................................
## Testing the function
## Ctreate a 8 by 8 matrix from 64 normal random values
matrix1 <- matrix(rnorm(64),8,8)
## Apply the function makeCacheMatrix to create the special matrix that caches its inverse
matrix2 <- makeCacheMatrix(matrix1)
## get the inverse from the cache

cacheSolve(matrix2)
