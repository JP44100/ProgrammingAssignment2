
## GENERAL DESCRIPTION
##
## Assignment: Caching the Inverse of a Matrix
##
## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##
## For this assignment, we assume that the matrix supplied is always invertible.
##
## NOTA-BENE All my comments associated with statements below are done "AS FAR AS I UNDERSTAND"


## DESCRIPTION of makeCacheMatrix function
##
## The makeCacheMatrix function creates a special "matrix", which is really a list containing
##	1 : a function to set the value of the matrix
##	2 : a function to get the value of the matrix
##	3 : a function to set the value of the inverse of the matrix
##	4 : a function to get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	setmat <- function(y)						# set a copy (= a cache) of the matrix (initialise it when $setmat is "called")
	{
		x <<- y							# the copy of the matrix itself
		inv <<- NULL						# initialise its inverse to NULL
	}
	getmat <- function() x						# get the matrix to inverse (return it when $getmat is "called")
	setinv <- function(mat) inv <<- mat				# set a copy (a cache) of the inverse of the matrix (initialise it when $setinv is "called")
	getinv <- function() inv					# get the inverse of the matrix (return it when $getinv is "called")
	list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)	# list of pairs name=function

}


## DESCRIPTION of cacheSolve function
##
# The cacheSolve function calculates the inverse of the special "matrix" created with the makeCacheMatrix function.
# However, it first checks to see if the invers has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv <- x$getinv()						# try to get the inverse of the matrix passed as argument to the cacheSolve function
	if(!is.null(inv))						# inverse as already been computed and is returned
	{
		message("getting cached data")
		return(inv)
	}
	data <- x$getmat()						# fetch the matrix
	inv <- solve(data, ...)						# compute the inverse using the solve() function in R
	x$setinv(inv)							# store the inverse in the special "matrix" object
	inv								# return the inverse of the matrix
}


## A FEW TRIES BELOW

# # try 1 : it seems that in this case m is seen as 2 different matrices
# m <- matrix(4:1, nrow = 2, ncol = 2)
# dimnames(m) <- list(paste("R", 1:nrow(m), sep=""), paste("C", 1:ncol(m), sep=""))	# check an idea to quickly set dimnames in any case and it works
# cacheSolve(makeCacheMatrix(m))
# cacheSolve(makeCacheMatrix(m))

# # try 2
# m <- matrix(1:4, nrow = 2, ncol = 2)
# dimnames(m) <- list(paste("R", 1:nrow(m), sep=""), paste("C", 1:ncol(m), sep=""))
# m
# m1 <- makeCacheMatrix(m)
# cacheSolve(m1)								# first call inverse of m1 is computed
# cacheSolve(m1)								# second call inverse of m1 is got from the cache

# m <- matrix(c(1, 3, 2, 4), nrow = 2, ncol = 2)
# dimnames(m) <- list(paste("R", 1:nrow(m), sep=""), paste("C", 1:ncol(m), sep=""))
# m
# m2 <- makeCacheMatrix(m)
# cacheSolve(m2)								# first call inverse of m1 is computed
# cacheSolve(m2)								# second call inverse of m1 is got from the cache

