# This function returns a list of functions that act on
# the matrix object (whose inverse we wish to cache)
# The formal argument,x, is the matrix of interest.
# The description of the functions are listed below

makeCacheMatrix <- function(x = matrix())
{
    # This should store the inverse of the matrix we are interested in
    # It is set to NULL
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


# This function will calculate the inverse of the matrix and returns it if it hasn't been cached.
# If it has been already calculated, it returns the cached value instead of performing the
# calculation again.
#
# It takes the object makeCacheMatrix as an argument (called x) 
# and grants it access to the matrix as well as the functions
# to perform cacheing or retrieving cached data.
#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    # Just get the inverse (without calculating it explicitly)
    minv <- x$getinv()
    
    # If the inverse is not null, it means that it has already been calculated
    # therefore we can just retriee that value from cache
    if(!is.null(minv))   
        {
            # print a message indicating it's cached and then return it
            message("getting cached matrix inverse")
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


# Test matrix
mat<- matrix( c(1,2,3,4), 2,2)

# Create the matrix
mymat<-makeCacheMatrix(mat)

# Cache it
cacheSolve(mymat)

# Retrieve it since it's cached. 
cacheSolve(mymat)
