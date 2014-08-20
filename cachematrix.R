## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
    # This should store the inverse of the matrix we are interested in
    minv <- NULL    

    # Hard reset of the matrix inverse to allow us to recache the inverse
    set <- function(y)   
        {
            x <<- y
            minv <<- NULL
        }

    # Return the input matrix
    get <- function() x     

    # Set the inverse (without calculations). This is just an assignment operation
    setinv <- function( matrixinv ) minv <<- matrixinv

    # Return the inverse of the matrix
    getinv <- function() minv

    # return an object of functions
    list(set = set, get = get,
         setinv = setinv,
          getinv = getinv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    # Just get the inverse (without calculating it explicitly)
    minv <- x$getinv()
    
    # If the inverse is not null, it means that it has already been calculated
    # therefore we can just retriee that value from cache
    if(!is.null(minv))   
        {
            # print a message indicating it's cached and then return it
            message("getting cached data")
            return(minv)
        }

    # If we make it to this point it means the inverser has not been calculated
    # therefore we need to calculate it.

    # Get the matrix first
    data <- x$get()

    # Now solve for the inverse assuming it exists
    minv <- solve(data, ...)

    # Set this value in the Make Vector object, i.e. cacheing
    x$setinv(minv)

    #return the inverse
    minv
    
}

mat<- matrix( c(1,2,3,4), 2,2)

mymat<-makeCacheMatrix(mat)

cacheSolve(mymat)
cacheSolve(mymat)
