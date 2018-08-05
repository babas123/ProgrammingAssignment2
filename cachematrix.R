## The first function, makeCacheMatrix creates a special “matrix”, 
## which is a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the Inverse of the matrix
## get the value of the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

            inverse_matrix <- NULL
            set <- function(y) {
                   x <<- y
                   inverse_matrix <<- NULL

            }

            get <- function() x
            setInv <- function(solve_matrix) inverse_matrix <<- solve_matrix
            getInv <- function() inverse_matrix
            list(set = set, get = get, setInv = setInv, getInv = getInv)


}


## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {

        inverse_matrix <- x$getInv()

       if(!is.null(inverse_matrix)){
                message("getting cached data")
                return(inverse_matrix)
        }
        MatrixA <- x$get()
        inverse_matrix <- solve(MatrixA)
        x$setInv(inverse_matrix)
        inverse_matrix   
}

