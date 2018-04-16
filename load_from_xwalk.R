#=================================#
# ==== function to load files ====
#=================================#

  # docuentation 
  
  # in_file_xwalk: the xwalk with file loactaions and info for loading. Made useing ea_file_xwalk
  # in_var_fwf_length: the variable name for the column in the xwalk specifying the object name of fwf_length
  # data for reading in fixed width files 
  # in_var_fwf_names: the variable name for the column in the file_xwalk specifying the object name of the 
  # fwf variable names for reading in fixed width files 
  # data_subgroup_length: how many path elements should make up the data list subgroups? default is one. 
  # opt_keep_file_type_in_name: should file type (.txt .csv .rdata) be including in the list element name 
  # for the data? 
  
  
  # set function parms 
  ea_load_from_xwalk <- function(in_file_xwalk = NULL,
                                 in_var_fwf_length = "fwf_length",
                                 in_var_fwf_names = "fwf_var_names",
                                 data_subgroup_length = 1,
                                 opt_keep_file_type_in_name = FALSE,
                                 opt_allow_csv_of_one_col = FALSE){
    
    # copy file xwalk 
    file_xwalk <- copy(in_file_xwalk)
    
    #====================================================================#
    # ==== add columns in file_xwalk to keep track of laoding issues ====
    #====================================================================#

    
      # keeps track of loading errors 
      file_xwalk[, load_errors := ""]
      
      # shows if some data was succesfully loaded in
      file_xwalk[, some_data_loaded_in := "no"]
      
      # if auto-fread fails, this tells you the separator found manually 
      file_xwalk[, manual_sep_used := ""]
      
      # add column in file_xwalk to save name of data set in output list 
      file_xwalk[, output_list_name := ""]
      
      # get list of path chunk variable names 
      path_chunks_vars <- grep("path_", colnames(file_xwalk), value = TRUE)
      
      # create column in file xwalk with path roots. path root length is determined by "data_subgroup_length"
      root_vars <- path_chunks_vars[1:data_subgroup_length]
      
      # get one character of root path variables seperated by commas 
      root_vars_comma <- paste0(root_vars, collapse = ", ")
      
      # create a text string. the text is a command that will be run on next line 
      path_root_command_line <- paste0("paste(", root_vars_comma, ", sep = '_' )")
      
      # run the line of text created above 
      file_xwalk[, path_root := eval(parse(text = path_root_command_line))]
      
      # create list to store data 
      all_data <- vector("list", length(unique(file_xwalk$path_root)))
      
      # add list elements for all path_root options. (organizes data in a clear way )
      names(all_data) <- unique(file_xwalk$path_root)
      
    
    #================================#
    # ==== start loop over xwalk ====
    #================================#
    
    # loop over the xwalk to load every line 
    for(m in 1:nrow(file_xwalk)){
      
      # print m for qc of function
      print(m)
      
      # grab type perameteres 
      type_m <- file_xwalk$type[[m]]
      
      # grab full path 
      full_path_m <- file_xwalk$full_path[[m]]
      
      # get list of actual paths for row m
      path_chunks_m <- unlist(lapply(path_chunks_vars, function(x) file_xwalk[[x]][[m]]))
      
      # remove NA path chunks 
      path_chunks_m <- subset(path_chunks_m, !is.na(path_chunks_m))
      
      # crete a unique name out of paths data_subgroup_length-n since path_root will be used to organize list structuer 
      data_set_name <- paste0(path_chunks_m[-data_subgroup_length], collapse = "_")
      
      # if option to keep file type in the name is false
      if(!(opt_keep_file_type_in_name)){
     
        # remove the file type from the data set name 
        data_set_name <- paste0(head(ea_split(data_set_name, "."), -1), collapse = "_")   
      }
     
      # add data set name to xwalk 
      file_xwalk[full_path == full_path_m, output_list_name := data_set_name]
      
      # save path_root_m
      path_root <- paste0(path_chunks_m[1:data_subgroup_length], collapse = "_")
      
      #====================#
      # ==== ~load csv ====
      #====================#
      
      # if .csv or .txt then read in useing fread
      if(type_m %chin% c("txt", "csv", "CSV")){
        
        # load data 
        tryCatch({
          
          # load in data using fread and its built in feature to auto detect seperator 
          data_m <- fread(full_path_m, colClasses = "character")
          
        # if it errors, put note in xwalk 
        }, error = function(error){
          
          # add note that regular fread failed 
          file_xwalk[full_path == full_path_m, load_errors := "auto-fread failed, manually detected sep"]
    
           # manually check for correct seperator
              for(sep_type in c(";", "\t", ",", "^")){
                
                # check the number of seperations in line and 2
                lines_1_2 <- readLines(full_path_m, n = 2)
                n_fields <- count.fields(textConnection(lines_1_2), sep = sep_type)
                closeAllConnections()
                
                # if the exists more than one field and the number of fields are the same in line 1 and 2, assign sep type 
                if(n_fields[[1]] > 1 & n_fields[1] == n_fields[2]){ 
                  
                  # assign sep type
                  sep_m <- sep_type 
                }
              # close sep type loop 
              }
              
                # add a note of which seperator was used 
                file_xwalk[full_path == full_path_m, manual_sep_used :=  sep_m]
              
                # load in with manually found sep 
                data_m <- fread(full_path_m, sep = sep_m, colClasses = "character")
                 
              # remove sep type for other itterations 
              rm(sep_m) 
              
            # if this metthod errors as well, make a note of it 
            }, error = function(error){
              
              
              # if it cannot load make note in the xwalk 
              file_xwalk[full_path == full_path_m, load_errors := paste0("CANNOT LOAD, error: ", error)]
  
            })
        

        # if data was not loaded in, try to manually find seperator 
        if(!(exists("data_m"))){
          
            # try detecting file seperator manually (different than how fread does it)
            tryCatch({
              
              # check for correct seperator
              for(sep_type in c(";", "\t", ",", "^")){
                
                # check the number of seperations in line and 2
                lines_1_2 <- readLines(full_path_m, n = 2)
                n_fields <- count.fields(textConnection(lines_1_2), sep = sep_type)
                closeAllConnections()
                
                # if the exists more than one field and the number of fields are the same in line 1 and 2, assign sep type 
                if(n_fields[[1]] > 1 & n_fields[1] == n_fields[2]){ 
                  
                  # assign sep type
                  sep_m <- sep_type 
                }
              # close sep type loop 
              }
              
                # add a note of which seperator was used 
                file_xwalk[full_path == full_path_m, manual_sep_used :=  sep_m]
              
                # load in with manually found sep 
                data_m <- fread(full_path_m, sep = sep_m, colClasses = "character")
                
            # if this metthod errors as well, make a note of it 
            }, error = function(error){
              
              
              # if it cannot load make note in the xwalk 
              file_xwalk[full_path == full_path_m, load_errors := paste0("CANNOT LOAD, error: ", error)]
  
            })
        # remove sep type for other itterations 
        rm(sep_m) 
      
          
        }
        
      # check if data exists 
      if(exists("data_m")){
        
      # and if there is only one column, this is probably also an issue 
      if(length(colnames(data_m)) == 1){
        
      # and check if option to allow these is FLASE 
      if(opt_allow_csv_of_one_col == FALSE){
        
        # print message
        message("auto-fread data only had one column, manually detecting separator. if data is supposed to have one column set opt_csv_has_one_col = TRUE")
 
      
         # try detecting file seperator manually (different than how fread does it)
            tryCatch({
              
              # check for correct seperator
              for(sep_type in c(";", "\t", ",", "^")){
                
                # check the number of seperations in line and 2
                lines_1_2 <- readLines(full_path_m, n = 2)
                n_fields <- count.fields(textConnection(lines_1_2), sep = sep_type)
                closeAllConnections()
                
                # if the exists more than one field and the number of fields are the same in line 1 and 2, assign sep type 
                if(n_fields[[1]] > 1 & n_fields[1] == n_fields[2]){ 
                  
                  # assign sep type
                  sep_m <- sep_type 
                }
              # close sep type loop 
              }
              
                # add a note of which seperator was used 
                file_xwalk[full_path == full_path_m, manual_sep_used :=  sep_m]
              
                # load in with manually found sep 
                data_m <- fread(full_path_m, sep = sep_m, colClasses = "character")
                
            # if this metthod errors as well, make a note of it 
            }, error = function(error){
              
              
              # if it cannot load make note in the xwalk 
              file_xwalk[full_path == full_path_m, load_errors := paste0("CANNOT LOAD, error: ", error)]
  
            })
        # remove sep type for other itterations 
        rm(sep_m) 
        
      # close if statements 
      }}}
  # close csv txt section  
  } 
      #==============================#
      # ==== ~ load xlsx and xls ====
      #==============================#

      
      # if xlsx, read in all the sheets 
      if(type_m %chin% c("xlsx", "xls")){
        
        # grab sheet names
        sheet_names_m <- ea_split(file_xwalk$sheet_names[[m]], "+")
        
        #======================================================#
        # ==== ~~if sheet names exist, read in using those ====
        #======================================================#
          
          # check if sheet names exist 
          if(!is.na(sheet_names_m) & sheet_names_m != "sheet names cannot be loaded"){
            
            # loop over sheet names to load each sheet 
            for(sheet in sheet_names_m){
              
              # try to load in sheets, if it errors just make a note of it in the xwalk. (empty sheet names show up so errors here are common )
              tryCatch({
                
                # check if xls or xlsx so use correct read function 
                
                if( type_m == "xlsx"){
                
                  # try to load data
                  data_m_sheet_m <- data.table(read_excel(full_path_m, sheet = sheet))
                }
                
                if(type_m == "xls"){
                  
                  # load in with read.xls
                  data_m_sheet_m <- data.table(gdata::read.xls(path, sheet = sheet))
                }
            
                
                # put into list here 
                all_data[[path_root]][[paste0(data_set_name, "_", sheet)]] <- data_m_sheet_m
                
                # fill in xwalk 
                file_xwalk[full_path == full_path_m, some_data_loaded_in := "yes"]
                
                # remove data 
                rm(data_m_sheet_m)
                
              # if sheet cannotbe loaded 
              }, error = function(error){
                
                # add note to xwalk 
                file_xwalk[full_path == full_path_m, load_errors :=  paste0(load_errors, sheet, " has error ", error)]
                
                # close tryCatch
                })
              
            # close loop 
            }
            
            # fill in xwalk 
            if(file_xwalk[full_path == full_path_m, some_data_loaded_in == "no"]){
              
              # no sheets loaded, print warning 
              print(paste0(full_path_m, " did not load "))
            }
            
            # all xlsx data needs are handled above 9easier this way b/c ofsheets) so if .xlsx than skip to next element in loop
            next() 
            
          # if sheet names do not exist 
          }else{
            
           
        #==============================#
        # ==== ~~if no sheet names ====
        #==============================#
        
        # try to load
        tryCatch({
          
          #if xls
          if(type_m == "xls" ){
            
            # load in with read.xls
            data_m <- data.table(gdata::read.xls(full_path_m))
          }
          
          # if  xlsx
          if(type_m == "xlsx"){
            
            # laod in with read_excel 
            data_m  <- data.table(read_excel(full_path_m))
          }

          
          # add reminder that only one sheet is loaded 
          file_xwalk[full_path == full_path_m, load_errors := "WARNING no sheets detected, only first sheet is loaded"]
          
        # if it errors just make a note of it in the xwalk.
        }, error = function(error){
          
          # make note in load errors 
          file_xwalk[full_path == full_path_m, load_errors :=  paste0(load_errors, "CANNOT LOAD, error: ", error)]
  
          })
        
        
      }
      
            
            
             
    }
      
      
      #===================#
      # ==== ~load dat ====
      #===================#
      
      # if DAT file, read in using read_fwf
      if(type_m %chin% c("dat")){
        
        # try grab needed fwf info
        tryCatch({
          
          # get fwf info needed to load it in 
          fwf_length <- get(file_xwalk[[in_var_fwf_length]][[m]])
          fwf_names <- get(file_xwalk[[in_var_fwf_names]][[m]])
          
        
        # if it errors, put message in xwalk that this is the issue 
        }, error = function(error){
          
          # add message 
          file_xwalk[full_path == full_path_m, load_errors := "issue with needed fwf info"]
          })
  
        # try to load in data 
        tryCatch({
          
        # laod in data using read_fwf and fwf info from xwalk 
        data_m <- data.table(read_fwf(full_path_m,
                                      col_positions = fwf_widths(fwf_length,
                                                                 fwf_names),
                                      col_types = paste(rep("c", length(fwf_names)), collapse="")))
      
        # if it errors, make notes that it couldn't be loaded in 
        }, error = function(error){ 
          
          # make note in load errors 
          file_xwalk[full_path == full_path_m, load_errors := paste0("CANNOT LOAD, error: ", error)]
          
          })
        
        
        
        
      }
      
      
      #=======================#
      # ==== ~load r data ====
      #=======================#
      
      
      # if rdata file 
      if(type_m == "rdata"){
        
        tryCatch({
          
          # load rdata 
          data_m <- ea_load(full_path_m)
          
        # if it errors, make note
        }, error = function(error){
            
          # make note in load errors 
          file_xwalk[full_path == full_path_m, load_errors := paste0("CANNOT LOAD, error: ", error)]
  
        # cloase tryCatch  
        })
        
      }
      
      
      
      
    #===============================#
    # ==== load stata .dta file ====
    #===============================#

    if(type_m == "dta"){
      
      # different packages needed for stata 5-12 and 13. try 5-12 first 
      tryCatch({
      
        # load stata data 
        data_m <- data.table(foreign::read.dta(path))
        
      }, error = function(error){
        
        # use other package to load stata 13 files 
        data_m <- data.table(readstata13::read.dta13(path))
        
      }, error = function(error){
        
         # make note in load errors 
        file_xwalk[full_path == full_path_m, load_errors := paste0("CANNOT LOAD, error: ", error)]
      })
      
      
    }
  
      
      #================================#
      # ==== organize data results ====
      #================================#
      
         # check if any data was successfully loaded in 
        if(exists("data_m")){
        
          # put data in list 
          all_data[[path_root]][[data_set_name]] <- data_m
          
          # fill in xwalk 
          file_xwalk[full_path == full_path_m, some_data_loaded_in := "yes"]
          
      
        }else{
          
          # else print warning 
          print(paste0(full_path_m, " did not load "))
        }
      
        # remove data_m to be safe, for next itteration 
        rm(data_m)
    
      
        # close loop  
    } 
    
    
  #===================================#
  # ==== organise and return data ====
  #===================================#
  
    # create list to output results of function 
    output_list <- ea_init_list(names = c("updated_file_xwalk", "loaded_data_list"))
    
    # save the new, updated file xwalk 
    output_list[["updated_file_xwalk"]] <- file_xwalk
    
    # save all the data loaded in 
    output_list[["loaded_data_list"]] <- all_data
    
    # retun both of these things as a list 
    return(output_list[])
  
  # close function   
  }
  



