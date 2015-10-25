## These functions allow a user to create a matrix and calculate its inverse. If an 
## inverse for a specific matrix has already been calculated, then the function will
## return the cached value of the inverse of the specific matrix.

## This function creates an object that can store a matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inversematrix <- NULL
        set <- function(y){
                x <<- y
                inversematrix <<- NULL
        }
        get <- function() x
        setinversematrix <- function(solve) inversematrix <<- solve
        getinversematrix <- function() inversematrix
        list(set=set, get=get,
             setinversematrix=setinversematrix,
             getinversematrix=getinversematrix)
}

## This function calculates the inverse of the matrix created above. If the inverse
## has already been calculated, then the function will retrieve the results from the
## above function.

cacheSolve <- function(x, ...) {
        
        inversematrix <- x$getinversematrix()
        ## Checks to see if cached solution exsits. If it does, the function returns
        ## the cached solution
        if(!is.null(inversematrix)){
                message("getting cached inverse solution of matrix")
                return(inversematrix)
        }
        ## Calculates the inverse of the matrix if cache solution does not exist.
        data <- x$get()
        inversematrix <- solve(data, ...)
        x$setinversematrix(inversematrix)
        inversematrix
}
