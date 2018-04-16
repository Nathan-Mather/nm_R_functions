
#================================#
# ==== ROXYGEN documentation ====
#================================#

#' nm_find_linear_combos
#' 
#' Takes a data.table or Matrix, finds perfectly colinear rows. Returns a list of all linear combination dependencies. For example, a list element of (2,3,4) 
#' implies that columns 2,3, and 4 are linearly dependent. The function then iteratively removes one column from each dependency and checks for remaining colinearities.
#' a list of the columns that were removed in those iterations is also returned. 
#'
#'@param data a data.table, Matrix, or and data that can be converted to a matrix with as.matrix. 

#'@export
#'@return linearCombos If there are linear combinations, this will be a list with elements for each dependency that contains vectors of column numbers.
#'@return remove a list of column numbers that can be removed to counter the linear combinations
#'
#'@author Nathan Mather
#' This is substitutible with caret::findlinearcombos but is faster and uses rook pivoting and LU decomposition rather that QR decomposition. This is not 
#' an original algorithm, just an implimentation in R. Much of this code and processes is directly from caret::findlinearcombos


#===================#
# ==== function ====
#===================#

# define function
nm_find_linear_combos <- function(in_data){
  
  # check the class of the input data 
  if(is(in_data, "matrix")){
   
    # rename in_data 
    in_matrix <- in_data
    
  }else{
    
    # convert in_data to numeric 
    in_data[] <- lapply(in_data[],  as.numeric)
    
    # check for NA values 
    if(any(is.na(in_data))){ stop("Your Matrix has NA values, either NA's where entered or coerced when classes were converted to numeric")}
  
    # convert in_data to matrix 
    in_matrix <- as.matrix(in_data)
  
  }

  # set up inner function, may need to be run more than once 
  # create inner function equivilent to findlinearcombos enumLC
  ea_enumLC <- function(in_matrix = NULL){

  
  #==============================#
  # ==== start rook pivoting ====
  #==============================#
    
    # lu fractorization in matrix package does not do complete pivoting (column pivoting). So, use rook pivoing. A fast, numerically stable way to get 
    # an upper triangular matrix that will work, i.e. the zeros on the diagonal equal the rank and correspond to linearly dependent columns 
    # notes for coding are from  " http://www.math.sjsu.edu/~foster/m143m_f13/gaussian_elimination_algorithms_4.pdf "

    # get number of columns and rows. These will be used as parameters in the rook pivoting code 
    n <- ncol(in_matrix)
    r  <- nrow(in_matrix)
    
    # check if its a wide matrix 
    if(n>r){
      
      # if yes, we need to fix that. Find the ratio of columns to rows and roudn up
      n_stacks <- ceiling(n/r)
      
      # stack the matrix on itself until it is no longer wide 
      rook_m <- do.call(rbind, replicate(n_stacks, in_matrix, simplify = FALSE))
      
    }else{

      # copy the original matrix so it isnt altrerd. We will need to come back to the original matrix after determining which columns are dependent 
      rook_m <- in_matrix
      
    }
    
    # make vector to track column chages. This is VERY important. Rook pivoting will swap columns in the matrix. So, if we find that the 3rd column 
    # in the output matrix is depdnent, this may correspond to a different column in the original matrix. To check, we will look at the 3rd entry of 
    # this vector. That will tell us which original matrix column corresponds to column 3 of the output matrix.  
    col_vector <- c(1:n)
    
    # DO this for every column. For every column we will search for a potential row and column pivot.
    for(k in 1:n){
    
      # We start our search for a row and pivot column at the diagonal of column K. we are allowed to search for pivot row Rho and pivot column Phi
      # in the lower right corver from this diagonal. i.e. from rook_m[k:r,k:n]
      rho_old <- k
      phi_old <- k
  
      # start a while loop for column k. keep finding rho and phi until they are stable. That is until the new rho and phi we find is the same as the ones from the previous iteration.
      continue <- TRUE
      while(continue){
        
        
        #===================#
        # ==== find rho ====
        #===================#
      
          # search column phi for maximal value, get rho coordinates of this maximal value #check not sure if this should be r or n becuase the thing I looked at used a square matrix 
          # if multiple values in this row take the max, select the min (i.e. the closest to K)
          rho_new <- k-1 + min(which.max(rook_m[k:r, phi_old]))
        
        #===================#
        # ==== find phi ====
        #===================#
 
          # earch row rho from column k-n for maximal entry, get coordinates of that entry. If multiple enteries contain the max value, just return the smaller one (closest to K). This choice is arbitrary 
          phi_new <- k-1 + min(which.max(rook_m[rho_new,k:n]))
          
            
          # if rho and phi are unchanged (stable) stop,  otherwise continue 
          if(rho_new == rho_old & phi_new == phi_old){
            continue <- FALSE
              
            }else{
              rho_old <- rho_new
              phi_old <- phi_new
            }
        # end while loop for column k
        }  
      
      
      #=============================#
      # ==== swap rows and cols ====
      #=============================#
        #once rho and phi are stable, we want to swap columns and rows 
      
        # switch rows k and rho
        if(rho_new != k){
          
          rook_m[c(k, rho_new), ] <- rook_m[c(rho_new, k), ]
        }
        
        # switch columns k and phi
        if(phi_new != k){
          
          rook_m[, c(k, phi_new) ] <- rook_m[, c(phi_new, k) ]
          
          # we need to keep track of the column pivots we do 
          # store the original column number that is curently in postion k of the rook pivot matrix. This is important because its possible that 
          # column K has already been swaped with a previous column. So rook matrix column K may not equal K 
          old_k <- col_vector[k] 
          
          # replace the column number at position K with the column number at postion phi_new
          col_vector[k] <- col_vector[phi_new]
          
          # replace the column vector at position phi_new with the column vector from position K 
          col_vector[phi_new] <- old_k
        }
        
      # end for loop over columns   
      } 


  #======================================#
  # ==== do rest of LU factorization ====
  #======================================#

    # the rook pivoting takes care of pivots, but we still need row reduction. Use Matrix:lu for this.
		# store the output of lu as matrix
		lu_decomp <-Matrix::lu(rook_m, warnSing=0)
		
		# use a form of gaussian elimination to find dose matrix as a product of upper and lower row reduced matrices.
    # this is our upper triangular matrix that should be rank revealing. NOTE AGAIN: the columns have been reorderd to col_vector. 
    # they are not in the original order. col_vector is your cross walk back to the original columns. 
		u <- as.matrix(Matrix::expand(lu_decomp)$U)

  	# get diagonal of upper triangular matrix 
  	 tdc_decomp_diag <- diag(u)
  	 
  	#Take that diagonal vector and turn any non-zero entries into a one 
		#MSC edit additional -- change the criterion from 0 to a very very small number
		# tdc_decomp_diag_one<-(abs(tdc_decomp_diag)>0.00000001)*1es 
    diag_zero_one <- as.integer(abs(tdc_decomp_diag)>0.0000002) 
    
    # if no zeros in the diagonal, no dependancies 
    if(!(any(diag_zero_one == 0))){
     
      # return an empty list and exit function
      return(list())
     
    }
  	
    # get original columns that are dependent, it is important to look in the col_vector because rook pivoting swaps column order
    # First we look at which columns in the upper triangualr matrix have zero in their diagonal. then, we look at those positions 
    # in col_vector to get the original column numbers that are dependent. AFter we have the list of column numbres in the original matrix, sort them.
    # The subset below will order them in the same order as the list so This is just so that the subset keeps the original columns in numerical order.  
    dep_cols <- sort(col_vector[which(diag_zero_one ==0)])
    
    # get original columns that are dependent, same logic with col_vector applies 
    ind_cols <-  sort(col_vector[which(diag_zero_one !=0)])
  
    #Make a matrix of only the dosage variables with the RHS dosage variables on it. These are the "independant" columns. The ones with a non-zer0 
    # (changed to one for simplicity) in the diagonal of tdc_decomp_diag_one, the upper triangular matrix 
    tdc_dose_deq_rhs = as.matrix(in_matrix[ , ind_cols])
  
    #Make a matrix of only the dosage variables with the LHS dosage variables on it. These are the "dependent" columns. The ones with a 0 in the 
    # diagonal of the reduced row form 
    tdc_dose_deq_lhs = as.matrix(in_matrix[ , dep_cols])
  
  
      
    #Run an OLS regression that will output the dependence equations.  The resulting matrix should have as many columns
    #as there are dependence equations, and as many rows as there are dosage variables that do not go on the LHS
    #of the dependence equations
    
    # we want to find linear combinations of the independent columns that make up the dependent columns. To do these we need to solve the matrix equation.
    # LHS = B*RHS or Dependent = B*independent. Sincethe RHS is not invertible we find the Moore–Penrose psudo inverse, 
    # (this is equivilent to an OLS regression) to solve for B
    B <- Matrix::solve(Matrix::crossprod(tdc_dose_deq_rhs, tdc_dose_deq_rhs))%*%(Matrix::crossprod(tdc_dose_deq_rhs, tdc_dose_deq_lhs))
     
    # zap small values
    B[abs(B) < 1e-6] <- 0 
    	 
      
    # get column and row names for B
    output  <-    lapply(1:dim(tdc_dose_deq_lhs)[2], function(i) c(dep_cols[i], ind_cols[which(B[,i] != 0)]))
    
    return(output)
  }

  #================================#
  # ==== run ea_enumLC on data ====
  #================================#


    # run function on data 
    lcList <- ea_enumLC(in_matrix = in_matrix)
    
    # run while loop 
    initialList <- lcList
       badList <- NULL
       if(length(lcList) > 0)
       {
          while(length(lcList) > 0)
          {
             # keep removing linear dependencies until it resolves
            
            # grap all dependent columns 
             tmp <- unlist(lapply(lcList, function(x) x[1])) 
             
            # remove duplicates or NA values 
             tmp <- unique(tmp[!is.na(tmp)])
             
             # combine dependent columns from current iteration with list of bad columns 
             badList <- unique(c(tmp, badList))
             
             # put this in here for cases where only one column remains as independent, R changes this from a matrix to a numeric vector
             lcList <- ea_enumLC(as.matrix(in_matrix[,-badList]))
             
          }
       }else{ badList <- NULL}
       
       # return list 
       list(linearCombos = initialList, remove = badList)

# close entire function 
}
    

