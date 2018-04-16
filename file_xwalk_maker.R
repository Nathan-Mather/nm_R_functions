#=================================#
# ==== function documentation ====
#=================================#

#'
#'@param path: the path for the folder you want to make a file xwalk for. 
#'
#' @return returns: a data.table with the file paths and file types of each file in a folder 

#===================================================#
# ==== script to source for xwlk maker function ====
#===================================================#

  # ste function perameters 
  ea_file_xwalk <- function(path = NULL){
    
    # get all file paths 
    file_names_full <- list.files(path, recursive = TRUE, all.files = FALSE)
    
    # create xwalk and enter base path and full extension 
    file_xwalk <- data.table(base_path = path, full_file_extension = file_names_full)
    
    # create full path 
    file_xwalk[, full_path :=paste0(base_path, full_file_extension)]
   
    # run while loop to split up file path 
    continue_loop <- TRUE
    n <- 1
    
    while(continue_loop){
      
    # make variable name for extension n 
    path_n <- paste0("path_", n)
    
    # save section of extension as a new variable 
    file_xwalk[, noquote(path_n) := ea_scan(full_file_extension, n,"/")]
    
    # check if there are any entreis in column, if all NA end loop, 
    continue_loop <- !(all(is.na(file_xwalk[[path_n]])))
    
    # drop extra row if all NA
    if(continue_loop == FALSE){
      file_xwalk[, noquote(path_n) := NULL] 
    }
    
    # add one to n for next itereation 
    n <- n + 1L

    }
  
  #================================================#
  # ==== create file type .xlsx, .csv .txt etc ====
  #================================================#

    # find the number of elements after spliting by .'s in each file path tp find location of file type 
    type_loc_function <- function(data){length(ea_split(data, "."))}
    type_loc_list <- unlist(lapply(file_xwalk$full_file_extension, type_loc_function))
    file_xwalk[, type_loc := type_loc_list]
  
    
    # loop around type locations to fill in file type 
    for(m in unique(type_loc_list)){
    file_xwalk[ type_loc == m, type := ea_scan(full_file_extension, m, "\\.")]
    
  }
    
    # drop type_loc var 
    file_xwalk[, type_loc := NULL]

    
  #================================#
  # ==== list xlsx sheet names ====
  #================================#

    # subset data to xlsx files 
    xlsx_sub <- file_xwalk[type == "xlsx"]
    
    # loop over xlsx files
    for(file in xlsx_sub$full_path){
      
   
      # get the sheet names 
      sheet_names_m <- openxlsx::getSheetNames(file)
      
    
      # fill in the sheet names
      file_xwalk[full_path == file, sheet_names := paste0(sheet_names_m, collapse = "+")]
    }
  
   
  
  #===============================#
  # ==== list xls sheet names ====
  #===============================#

    # subset to xls files 
    xls_sub <- file_xwalk[type == "xls"]
  
    # loop over xls fiels 
    for(file in xls_sub$full_path){
      
      tryCatch({
        
        # try to get sheet names 
        sheet_names_m <-  gdata::sheetNames(file)
        
        # if it worked, fill in the sheet names 
        file_xwalk[full_path == file, sheet_names := paste0(sheet_names_m, collapse = "+")]
      
      # if it errors 
      }, error = function(error){
        
        # if it doesn't work, make a note of it 
        file_xwalk[full_path == file, sheet_names := NA]
        
      })
 
      


      
    }
   return(file_xwalk[])
  }

  