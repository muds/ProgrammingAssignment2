### Caching the inverse of a matrix
### -------------------------------
### This R script creates two functions, makeCacheMatrix() and cacheSolve(),
### that allow the inverse of a matrix to be quickly retrieved from a cache
### when available, and otherwise calculated once and stored in the cache for
### later retrieval.
###
### Usage
### -----
###   * To create a matrix 'm' with a cacheable inverse from a standard R
###     matrix 'x':
###
###       cx1 <- makeCacheMatrix(x)
###       cx2 <- makeCacheMatrix(); cx2$set(x)
###
###   * To obtain the inverse matrix y such that xy == I:
###
###       y <- cacheSolve(cx)
###


######################################################################
# Function:
#   makeCacheMatrix
#
# Arguments:
#   x, an R matrix whose inverse is to be cached after it is
#      first calculated
#
# Description:
#   Returns a list object with four methods that can be used to get
#   and set the matrix and its cached inverse.
#
# Note:
#   Calling the getinv() method returns NULL if cacheSolve() has not
#   been called, regardless of the invertibility of the matrix x.
makeCacheMatrix <- function (x = matrix()) {
  # Before cacheSolve() has been called, our default initial cache
  # for the inverse is NULL, stored in the local variable xinv
  xinv <- NULL
  # The method set() allows a new R matrix to be assigned to this
  # cacheMatrix. This resets the cached inverse to NULL.
  set <- function (y) {
    # Store the supplied matrix in the local variable 'x'
    x <<- y
    # Since the inverse of this new matrix hasn't been calculated yet,
    # set the cache to NULL
    xinv <<- NULL
  }
  # The get() method simply returns the local copy of the matrix 'x'
  get <- function () x
  # The setinv() method caches the supplied matrix, and assumes that it
  # is the inverse of x. NB: No check is done to see if this is true,
  # or even if m is a matrix!
  setinv <- function (m) xinv <<- m
  # The getinv() method returns whatever value or object is cached in the
  # local 'xinv' variable
  getinv <- function () xinv
  
  # What is actually returned when makeCacheMatrix() is called is the
  # following list of its method functions
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


######################################################################
# Function:
#   cacheSolve
#
# Arguments:
#   cx, the cacheMatrix object (previously created by makeCacheMatrix)
#       whose inverse is to be obtained
#
# Description:
#   Returns the inverse of the R matrix object stored within the supplied
#   cacheMatrix object. The inverse is taken from the cached value if that
#   is not NULL. Otherwise, the inverse is calculated using the R solve()
#   command, and both saved to the cache and returned by cacheSolve.
#
cacheSolve <- function(cx, ...) {
  # Retrieve the currently cached value of the index
  xinv <- cx$getinv()
  # Test whether the current cahce is NULL
  if (is.null(xinv)) {
    # When the cache is NULL, the inverse has be calculated, so
    # alert the user that this is happening
    message('Calculating matrix inverse')
    # Retrieve the R matrix whose inverse is desired
    xmat <- cx$get()
    # Perform the possibly intensive inverse calculation, and save
    # the result into the local 'xinv' variable (which was previously NULL)
    xinv <- solve(xmat)
    # Store this inverse in the cache
    cx$setinv(xinv)
  } else {
    # When the cached value isn't NULL, on the other hand, alert the
    # user that the cached value is being retrieved. Since it is already
    # stored localling in 'xinv', there's nothing more to do here.
    message('Getting cached inverse')
  }
  
  # Return the inverse, which was either calculated or retreived from the
  # cache according to which part of the if-else statement executed
  xinv
}
