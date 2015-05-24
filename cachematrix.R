## Put comments here that give an overall description of what your
## functions do
#
# This is just a demo of using lexical scoping in
# R. We'll enable a function to be repeatedly
# called without incurring the computational
# overhead of recalculating a result each time.

# We do all of this by creating a list and a
# "constructor function" that has a pair of
# setter and getter functions for both the
# matrix and its inverse. We also initialize
# 'm' with either the matrix provided or a simple
# default, and 'im' to NULL; the function that
# solves can put the solved inverse there.
#
# Seems neater to me to use anonymous functions
# in the list.
# 
makeCacheMatrix <- function(initialMatrix = matrix()) {
    m <- initialMatrix
    im <- NULL
    list(
            set = function(newMatrix)
                {
                    m <<- newMatrix
                    im <<- NULL
                },
            get = function() m,
            setinv = function(inverse) im <<- inverse,
            getinv = function() im
         )
}


# Take one of our special matrix and some extra
# arguments for solve(). First we check if this
# matrix has already be solved. If not, squirrel
# away the solution and return it.
#
# While we use the extra args for solve(), an
# improvement to this function would be to test
# whether the arguments provided matched the
# the prior arguments--we might need to resolve
# the matrix depending on what arguments are used.
#
# But the best way to do this is to keep all
# inverses in a list we could look up by arguments.
# That's a bit beyond where we are now.
#
cacheSolve <- function(x, ...)
{
    inverse <- x$getinv()
    if ( is.null(inverse) )
    {
        print("Solving")
        inverse <- x$setinv(solve(x$get(), ...))
    }

    inverse
}
