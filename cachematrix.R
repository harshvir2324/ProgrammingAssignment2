## Put comments here that give an overall description of what your
## functions do

##cacheSolve is a function which computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
## I simply set the input x as a matrix
## and then set the solved value "s" as a null
## then I changed every reference to "mean" to "solve"
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## Same here, changed "mean" to "solve" and "m" to "s"
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

##source("ProgrammingAssignment2/cachematrix.R")
##my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##my_matrix$get()
##[,1] [,2]
##[1,] 1 3
##[2,] 2 4
##my_matrix$getInverse()
##NULL
##cacheSolve(my_matrix)
##[,1] [,2]
#[1,] -2 1.5
#[2,] 1 -0.5
#cacheSolve(my_matrix)
#getting cached data
#[,1] [,2]
#[1,] -2 1.5
#[2,] 1 -0.5
#my_matrix$getInverse()
#[,1] [,2]
#[1,] -2 1.5
#[2,] 1 -0.5
#my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
#my_matrix$get()
#[,1] [,2]
#[1,] 2 1
#[2,] 2 4
#my_matrix$getInverse()
#NULL
#cacheSolve(my_matrix)
#[,1] [,2]
#[1,] 0.6666667 -0.1666667
#[2,] -0.3333333 0.3333333
#cacheSolve(my_matrix)
#getting cached data
#[,1] [,2]
#[1,] 0.6666667 -0.1666667
#[2,] -0.3333333 0.3333333
#my_matrix$getInverse()
#[,1] [,2]
#[1,] 0.6666667 -0.1666667
#[2,] -0.3333333 0.3333333
