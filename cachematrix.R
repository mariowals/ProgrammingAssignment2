## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function receives a numeric matrix as a parameter; it assumes that the matrix is invertible.
#In this function are defined 4 methods: set and get the matrix received, as well as, set and get the inverted matrix. 
#It uses <<- to store information in cache
makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set_mat <- function(n) {
        m <<- n
        inv <<- NULL
    }
    get_mat <- function() {
        m
    }
    set_inv_mat <- function(inverse) {
        inv <<- inverse
    }
    get_inv_mat <- function() {
        inv
    }
    list(set_mat = set_mat, get_mat = get_mat,
         set_inv_mat = set_inv_mat, get_inv_mat = get_inv_mat)
}


## Write a short comment describing this function
#This function invokes the get inverted matrix method on makeCacheMatrix to calculate inverted matrix
#In case the information for inverted matrix is stored in cahce, it will be used instead of calculated
#This function returns the inverted matrix using solve function
cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- m$get_inv_mat()
    if (!is.null(inv)) {
        message("Data alredy in cache, skipping calculation.")
        return(inv)
    }
    data <- m$get_mat()
    inv <- solve(data, ...)
    m$set_inv_mat(inv)
    inv
}