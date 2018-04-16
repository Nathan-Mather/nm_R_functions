#===============================#
# ==== ea_load_any function ====
#===============================# 

#=================================#
# ==== function documentation ====
#=================================#

#' ea_load_any 
#'supports: supports the following file types .csv .CSV .txt .xlsx .xls .rdata .dat .dta (versions 5-13) .sas7bdat
#'read_functions_used: fread, read_excel, read.xls, read_fwf, load, read.dta, read.dta13, read_sas
#'
#'
#'@param path: the path for the file 
#'@param opt_csv_has_one_col: base function assumes that the data should have more than one column. If your data does have one column, set to TRUE
#'@param opt_xlsx_func: option to select function to read in excel files. supports "read_excel" and "read.xlsx". "read_excel" is default because it is faster but is known to
#' miss data if a column starts off with a bunch of blank rows. So, read.xlsx is a slower potentially more reliable option 
#' @param opt_print Option to print row and column count, and load time
#' @param opt_xlsx_sheet a specific sheet name or a vector of sheet names to load in. If none are entered the function will auto-detect sheet names, load them all in, and store them in a list 
#' @param fwf_length: vector of lengths for the fixed width file  
#' @param fwf_names: vector of names for the fixed width file 
#'
#' @return returns: Ususally a data.table. For xlsx or xls files with more than one sheet, it returns a list of data.tables. One for each sheet. 

#==============================#
# ==== initialize function ====
#==============================#


ea_load_any <- function(path = NULL,
                        opt_csv_has_one_col = FALSE,
                        opt_xlsx_func = "read_excel",
                        opt_xlsx_sheet = NULL,
                        fwf_length = NULL,
                        fwf_names = NULL,
                        opt_print    = TRUE){
  
  
    # store initial time
    time_a <- Sys.time()
  
  #==============================#
  # ==== check the file type ====
  #==============================#

  

    # find the number of elements after spliting by .'s 
    type_loc <- length(ea_split(path, "."))

    # split string and return last element after period  
    type  <-  ea_scan(path, type_loc, "\\.")
    


  #====================#
  # ==== ~load csv ====
  #====================#
    
    # if .csv or .txt then read in useing fread
    if(type %chin% c("txt", "csv", "CSV")){
      
      # load data 
      tryCatch({
        
        # load in data using fread and its built in feature to auto detect seperator 
        out_data <- data.table::fread(path, colClasses = "character")
        
      # if it errors, print out warning and try to manually detect seperator 
      }, error = function(error){
        
        # add note that regular fread failed 
        warning("auto-fread failed, manually detecting separator")
        
        # manually check for correct seperator
        for(sep_type in c(";", "\t", ",", "^")){
          
          # check the number of seperations in line and 2
          lines_1_2 <- readLines(path, n = 2)
          n_fields <- utils::count.fields(textConnection(lines_1_2), sep = sep_type)
          closeAllConnections()
          
          # if there exists more than one field and the number of fields are the same in line 1 and 2, assign sep type 
          if(n_fields[[1]] > 1 & n_fields[1] == n_fields[2]){ 
            
            # assign sep type
            in_sep <- sep_type 
          }
        # close sep type loop 
        }
        
        # print a note of which seperator was used 
        print(paste0("used seperator ", in_sep))
      
        # load in with manually found sep 
        out_data <- data.table::fread(path, sep = in_sep, colClasses = "character")
  
      })
      
    # ALSO if there is only one column, probably an issue (was for core data). So try manual sep here too
    if(length(colnames(out_data)) == 1){
      
      if(opt_csv_has_one_col == FALSE){
      
        # print warning 
        warning("auto-fread data only had one column, manually detecting separator. if data is supposed to have one column set opt_csv_has_one_col = TRUE")
 
          # check for correct seperator
          for(sep_type in c(";", "\t", ",", "^")){
            
            # check the number of seperations in line and 2
            lines_1_2 <- readLines(path, n = 2)
            n_fields <- utils::count.fields(textConnection(lines_1_2), sep = sep_type)
            closeAllConnections()
            
            # if the exists more than one field and the number of fields are the same in line 1 and 2, assign sep type 
            if(n_fields[[1]] > 1 & n_fields[1] == n_fields[2]){ 
              
              # assign sep type
              in_sep <- sep_type 
            }
          # close sep type loop 
          }
          
          # print a note of which seperator was used 
          print(paste0("used seperator ", in_sep))
        
          # load in with manually found sep 
          out_data <- fread(path, sep = in_sep, colClasses = "character")
      
    }}
      
  }

    #==============================#
    # ==== ~ load xlsx and xls ====
    #==============================#
    
    # if xlsx, read in all the sheets 
    if(type %chin% c("xlsx", "xls")){
      
      
      # check if a specific sheet name was entered 
      if(!(is.null(opt_xlsx_sheet))){
        
        sheet_names <- opt_xlsx_sheet
      
      }else{
      
        # check file type to use proper function to get sheet names 
        if(type == "xlsx"){
          
          # grab sheet names (xlsx)
          sheet_names <- openxlsx::getSheetNames(path) 
          
        }else{
          
          # grab sheet names (xls)
          sheet_names <- gdata::sheetNames(path)
          
        }
      
      # close sheet name section
      }
        
      # set flag for at least one sheet loading correctly 
      flag_some_data_loaded <- FALSE

      # if there is more than one sheet, we will need to return a list 
      if(length(sheet_names) != 1){
       
        # create list to store all sheets 
        out_data <- ea_init_list(names = sheet_names) 
      }
      
      # loop over sheet names to load each sheet 
      for(sheet_m in sheet_names){
        
        # try to load in sheets, if it errors print warning. (empty sheet names show up so errors here are common )
        tryCatch({
          
          # check file type to use proper loadiing function 
          if(type == "xlsx"){
            
            # check the option to use read.xlsx
            if(opt_xlsx_func == "read_excel"){
            
              # try to load data read_excel (for xlsx) 
              out_data_sheet_m <- data.table(readxl::read_excel(path, sheet = sheet_m))
              
            }
            if(opt_xlsx_func == "read.xlsx"){
              
              # try to load data read_excel (for xlsx) 
              out_data_sheet_m <- data.table(xlsx::read.xlsx(path, sheetName = sheet_m))
            }


              
          }else{
            
            # load in with read.xls
            out_data_sheet_m <- data.table(gdata::read.xls(path, sheet = sheet_m))
          }

          if(length(sheet_names) != 1){
            
            # put data into list 
            out_data[[sheet_m]] <- out_data_sheet_m
            
          }else{
            
            # just rename data 
            out_data <- out_data_sheet_m
          }

          
          # trip flag for some data loaded in 
          flag_some_data_loaded <- TRUE
          
          # remove data, for next loop 
          rm(out_data_sheet_m)
          
        # if sheet cannot be loaded 
        }, error = function(error){
          
          # print warning that given sheet could not be loaded 
          warning(sheet_m, " has ", error)
          
          # close tryCatch
          })
        
      }
        
    # see if any sheets read in 
    if(!flag_some_data_loaded){

          # if not, error
          stop("no sheets loaded correctly")
        }
        
      # close loop 
      }


    
    #===================#
    # ==== ~load dat ====
    #===================#
    
    # if DAT file, read in using read_fwf
    if(type == "dat"){

      # laod in data using read_fwf and fwf info from xwalk 
      out_data <- data.table(readr::read_fwf(path,
                                    col_positions = readr::fwf_widths(fwf_length,
                                                               fwf_names),
                                    col_types = paste(rep("c", length(fwf_names)), collapse="")))
    }
    
    
    #======================#
    # ==== load r data ====
    #======================#
    
    # if rdata file 
    if(type == "rdata"){
      
      # load file
      out_data <- get(load(path))
    }
    
    #===============================#
    # ==== load stata .dta file ====
    #===============================#

    if(type == "dta"){
      
      # different packages needed for stata 5-12 and 13. try 5-12 first 
      tryCatch({
      
        # load stata data 
        out_data <- data.table(foreign::read.dta(path))
        
      }, error = function(error){
        
        # use other package to load stata 13 files 
        out_data <- data.table(readstata13::read.dta13(path))

      })
      
      
    }
    
    
    #=========================#
    # ==== load SAS files ====
    #=========================#

    if(type == "sas7bdat"){
      
      out_data <- data.table(haven::read_sas(path))
      
    }
   
  
    #=========================================#
    # ==== error if type is not supported ====
    #=========================================#

    if(!type %chin% c("sas7bdat", "dta", "rdata", "dat", "xlsx", "xls", "txt", "csv", "CSV" )){
      
      stop(paste0("file type ", type, " NOt currently supported. Only sas7bdat, dta, rdata, dat, xlsx, xls, txt, csv, CSV, currently supported."))
      
    }
    
  #==========================================#
  # ==== print out times and return data ====
  #==========================================#

    # store time to load
    time_to_load <- round(as.numeric(Sys.time() - time_a, units = "secs"), 1)
    
    # find class of object 
    class_r_object <- class(out_data)
    
    # print statistics
    if(opt_print == TRUE){
      
      # if the object is a data frame or data table
      if("data.table" %chin% c(class_r_object) | "data.frame" %chin% c(class_r_object)){
      print(paste0("Loaded ", prettyNum(nrow(out_data), big.mark = ","), " rows and ", prettyNum(ncol(out_data), big.mark = ","), " columns in ", time_to_load, " seconds"))
      }
      
      # if the object is a list
      if("list" %chin% c(class_r_object)){
      print(paste0("Loaded list in ", time_to_load, " seconds"))
      }
      
    }
    

      # Return data
      return(out_data)
    
    
  # close funciton 
}
