

# set rook pivoting code 
rook_pivot <- function(dose_to_pivot = NULL){
  
  # get number of columns and rows. These will be used as parameters in the rook pivoting code 
  n <- ncol(dose_to_pivot)
  r  <- nrow(dose_to_pivot)
  
  # copy the original matrix so it isnt altrerd. We will need to come back to the original matrix after determining which columns are dependent 
  rook_m <- dose_to_pivot
  
  # make vector to track column chages. 
  col_vector <- c(1:n)
  
  # DO this for every column. For every column we will search for a potential row and column pivot. 
  for(k in 1:n){
    
    # We start our search for a row and pivot column at the diagonal of column K. we are allowed to search for pivot row Rho and pivot column Phi
    # in the lower right corver from this diagonal. i.e. from rook_m[k:r,k:n]
    rho_old <- k
    phi_old <- k
    
    # start a while loop for column k. keep finding rho and phi until they are stable. That is until the new rho and phi we find is the same as the
    # ones from the previous iteration. 
    continue <- TRUE
    while(continue){
      
      #===================#
      # ==== find rho ====
      #===================#
      
      # search column phi for maximal value, get rho coordinates of this maximal value #check not sure if this should be r or n   becuase the thing I 
      # looked at used a square matrix  if multiple values in this row take the max, select the min (i.e. the closest to K)
      rho_new <- k-1 + min(which.max(rook_m[k:r, phi_old]))
      
      #===================#
      # ==== find phi ====
      #===================#
      
      # earch row rho from column k-n for maximal entry, get coordinates of that entry. If multiple enteries contain the max value,
      # Just return the smaller one (closest to K). This choice is arbitrary 
      
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
  
  # return pivoted matrix and pivot vector 
  return_list <- list()
  return_list[["column_vector"]] <- col_vector
  return_list[["rook_matrix"]] <- rook_m
  
  return(return_list)
}

