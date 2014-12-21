# This piece of code performs simple inverse matrix caching.
# function makeCacheMatrix is for basic methods initialization
# function cacheSolve returns an inverse of a square invertable matrix (no checks on matrix conformity is performed)
#                     calculating it on the first run and returning cached value on all successive
# At the bottom there is a sample test code (in comments).

makeCacheMatrix <- function (x = matrix()) {

        # Constructs 3 basic methods: get, setinv, getinv for handling cached (square) matrix
        
        n   <- nrow(x)
        inv <- matrix(NA, n, n)                                 # Initialize inverse matrix

        get    <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        list(get = get,
             setinv = setinv,
             getinv = getinv)        
}

cacheSolve <- function (x, ...) {
        
        # Returns a matrix that is an inverse of 'x'
        
        x.inv <- x$getinv()
        if(all(is.na(x.inv))) {                                 # All NAs mean that inverse has not been evaluated yet
                message("Calculating Inverse...")
                x.inv <- x$get()
                x.inv <- solve(x.inv)                           # If matrix appears to be noninvertable 'solve' returns an error
                x$setinv(x.inv)
        }
        return(x.inv)
}

# # Sample code for testing (can be easily uncommented by <shift><ctrl>C in RStudio)
# 
# a  <- matrix(runif(16), 4, 4) + diag(nrow = 4) * 3              # Make sample invertable matrix
# x  <- makeCacheMatrix(a)
# b1 <- cacheSolve(x)                                             # First run (calculate inverse using 'solve()')
# b2 <- cacheSolve(x)                                             # Second run (get inverse from cache)
# 
# cat("Check: The message 'Calculating Inverse...' appears ONCE illustrating that the second time an inverse was taken from cache\n\n")
# cat("Check: a %*% b1  should give identity matrix ensuring that 'b1' is an inverse of 'a'\n")
# print(a %*% b1)
# cat("\n")
# cat("Check: b1 - b2 should give all zero matrix, ensuring that cache also did provide an (inverse) matrix\n")
# print(b1 - b2)