## The makeCacheMatrix function will utilize the caching mechanism to
## store the matrix and its inverse. It has four functions named
## set, get, setInv, getInv.
## Function set: set the matrix to the value in the argument
## Function get: return the stored matrix
## Funciton setInv: set the inverse of the matrix to the value in the argument
## Function getInv: return the inverse of the matrix
## Considering the concept in object orient programming, it acts must like
## a class with methods defined in.

makeCacheMatrix <- function(x = matrix()) {
# A matrix passed to this function
        
        inv <- NULL
        # The default/initilized value of matrix inverse 'inv' is NULL
        
        set <- function(y) {
                # Set the matrix and inverse, in cached mode
                
                x <<- y
                # Assign the passed value of variable 'y' to 'x'
                
                inv <<- NULL
                # Set the 'inv' to NULL, because 'x' has been assigned
                # to a new value, the inverse needs to be reset
        }
        
        get <- function() x
        # Return the matrix 'x'
        
        setInv <- function(inverse) inv <<- inverse
        # Set the matrix inverse to the value passed in, cached mode
        
        getInv <- function() inv
        # Return the inverse of matrix
        
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        # Define all the functions contained in makeCacheMatrix function
}

## Below cacheSolve function will make use of makeCacheMatrix function.
cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'
        
        inv <- x$getInv()
        # Get the inverse of matrix 'x' by calling the 
        # 'getInv()' function
        
        if(!is.null(inv)) {
                # If the returned result is not NULL, then the
                # inverse was computed before and it's already cached.
                
                message("Getting cached data")
                # Display a message indicating the data was cached.
                
                return(inv)
                # Return the result and exit current function
        }
        
        data <- x$get()
        # When above code executed, which means the above if
        # condition was not fullfilled. Meaning the matrix
        # inverse was NULL and it was not computed and cached before.
        # Assign the matrix to 'data' by calling function 'get()'.
        
        inv <- solve(data)
        # Calculate the inverse of matrix and assign it to 'inv',
        # time consumming if 'data' is big.
        
        x$setInv(inv)
        # Calling the 'setInv' function to set the inverse, the
        # '<<-' will make sure the value of inverse will be cached.
        # So next time it'll return a cached result instead of
        # calculating again.
        
        inv
        # Return the inverse of matrix
}
