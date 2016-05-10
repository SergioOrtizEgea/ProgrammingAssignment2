## Put comments here that give an overall description of what your
## functions do

#The function makeVector creates a list that contains the 
#following functions:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  
  #Declaring the variable that contains the inverted matrix
  inv_mat <- NULL
  
  # 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  
  # 2. get the value of the matrix
  get <- function() x
  
  # 3. set the value of the inverse matrix
  set_inverse <- function(inverse){
    inv_mat <<- inverse
  }
  
  # 4. get the value of the inverse matrix
  get_inverse <- function() inv_mat
  
  #Function list return
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


# cacheSolve:
# Once the matrix has been created this function 
#calculates inverse and caches the result. If the matrix
#is square the function calculates the result with solve 
#if the matrix is not square it solves the inverse in a
#Moore-Penrose sense.
#
#If the inverse has bee previously calculated then the 
#previously calculated value is returned. In addition,
#a message will report the previous calculated value
#
#Example Square Matrix:
# M <- matrix(1:4, nrow=2, ncol=2)
# cache_matrix <- makeCacheMatrix(M)
# inv_M <- cacheSolve(cache_matrix)
# inv_M will return:
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#
# inv_M_2<- cacheSolve(m)
# This should display a "Getting cached matrix"
#
#inv_M_2 will return
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#
#Example Rectangular Matrix:
# M2 <- matrix(1:6, nrow=3, ncol=2)
#M2 will print
#      [,1] [,2]
#[1,]    1    4
#[2,]    2    5
#[3,]    3    6
# cache_matrix2 <- makeCacheMatrix(M2)
# inv_M2 <- cacheSolve(cache_matrix2)
# inv_M2 will return:
#          [,1]       [,2]       [,3]
#[1,] -0.9444444 -0.1111111  0.7222222
#[2,]  0.4444444  0.1111111 -0.2222222


cacheSolve <- function(x, ...) {
        
    #Pulling up the matrix 
    inv_mat <- x$get_inverse()
    
    #If it the inverse of the matrix has been 
    #performed a message and the inverted matrix
    #is returned
    if(!is.null(inv_mat)) {
      message("Getting cached matrix")
      return(inv_mat)
    }
    
    #If the operation has not been performed, then
    #the matrix is obtained
    data <- x$get()
    
    dim_data <- dim(data)
    
    #Finding the inverse of the matrix data:
    #If data is a squared matrix then we use solve
    #Else we use ginv to solve it in a Moore-Penrose sense
    if(dim_data[1] == dim_data[2]){
      inv_mat <- solve(data, ...)}
    else{
      library(MASS)
      inv_mat <-ginv(data)}
    
    #Cache the inverse
    x$set_inverse(inv_mat)
    
    #Return of the value
    inv_mat
  }

