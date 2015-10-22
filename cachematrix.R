## makeCacheMatrix is the initating function, taking a matrix and assigning
## the matrix and several functions to a list variable. This interacts with
## cacheSolve, where the inverted matrix will be calculated and, in future
## iterations, recalled without computation.

## Proper use:
## m <- makeCacheMatrix(<input matrix>)

## makeCacheMatrix requires an invertable matrix as an input.
## Assigning makeCacheMatrix to a variable m will allow use of subfunctions
## using m$get.matrix() and m$get.inv() etc..

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set.matrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        get.matrix <- function() x
        set.inv <- function(inv) i <<- inv
        get.inv <- function() i
        list(set.matrix = set.matrix, get.matrix = get.matrix, 
             set.inv = set.inv, get.inv = get.inv)
}


## After a matrix has been processed by makeCacheMatrix and stored to a
## variable, cacheSolve will determine the inverse matrix and store it.

## Proper use (continued from above):
## cacheSolve(m)

cacheSolve <- function(x, ...) {
        i <- x$get.inv()
        if(!is.null(i)) {
                message("data already cached, retrieving...")
                return(i)
        }
        matrix <- x$get.matrix()
        i <- solve(matrix)
        x$set.inv(i)
        i
}
