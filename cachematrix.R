## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly 
## following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix<-function(m=matrix()){
        inverse_m<-NULL
        set_mat<-function(mat){
                m<<-mat
                inverse_m<<-NULL
        }
        get_mat<-function() m
        
        set_invmat<-function(invmat) inverse_m<<-invmat
        
        get_invmat<-function() inverse_m
        
        list(set_mat=set_mat,get_mat=get_mat,
             set_invmat=set_invmat,get_invmat=get_invmat)
        
        
}



# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve<-function(x){
        inverse_m<-x$get_invmat()
        if(!is.null(inverse_m)){
                message("getting cached data.")
                return(inverse_m)
        }
        m<-x$get_mat()
        inverse_m<-solve(m)
        x$set_invmat(inverse_m)
        inverse_m
        
}
