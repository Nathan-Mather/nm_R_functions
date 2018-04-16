


#'@param in_data_1 first data set
#'@param in_data_2 second data set
#'@param in_char_vars_merge_by you character vectors you want a rough match/merge on 
#'@param in_vars_exact_matchvariables that you want to require to be the same. i.e. they need an exact match
#'@param string_comparison_function a function for matching strings. defaults to RecordLinkage::jarowinkler
#'@param out_all_matches_for The function grabs out the best match for each variable in data set 1, if you want to see 
# all potential matches for a particular sting, put it as a vector here. as of right nowOne of the elements 
# in this vector will need to showb up in every in_char_vars_merge_by
#'@return returns an x merge with all elements in data set 1 paired with their best estimated match in data set 2. 
#'Elements from data set 2 can be matched to more than one element in data set 1 


 # define perameters of function
  ea_name_match <- function(in_data_1 = NULL,
                            in_data_2 = NULL,
                            in_char_vars_merge_by = NULL,
                            in_vars_exact_match = NULL,
                            string_comparison_function = RecordLinkage::jarowinkler,
                            out_all_matches_for = NULL){
    # copy data 
    data_1 <- copy(in_data_1)
    data_2 <- copy(in_data_2)
    
    # subset to needed merge variables 
    data_1_sub <- data_1[, c(in_char_vars_merge_by, in_vars_exact_match), with = FALSE]
    
    data_2_sub <- data_2[, c(in_char_vars_merge_by, in_vars_exact_match), with = FALSE]
    
    # put linkage variables to lower case (compare.linkage is case sensitive)
    for(m in 1:length(in_char_vars_merge_by)){
      
      data_1_sub[, noquote(in_char_vars_merge_by[[m]]) := tolower(get(in_char_vars_merge_by[[m]]))]
      data_2_sub[, noquote(in_char_vars_merge_by[[m]]) := tolower(get(in_char_vars_merge_by[[m]]))]
    }
    
  
    # try compare.linkage 
    if(length(in_vars_exact_match) != 0){
      linkage_output <- RecordLinkage::compare.linkage(data_1_sub, data_2_sub,  blockfld = in_vars_exact_match, phonetic = FALSE, strcmp = TRUE, strcmpfun = string_comparison_function )
      
    }else{
      linkage_output <- RecordLinkage::compare.linkage(data_1_sub, data_2_sub,  blockfld = FALSE, phonetic = FALSE, strcmp = TRUE, strcmpfun = string_comparison_function )
      
      }

    # grab the pairs from the function output
    pairs <- data.table(linkage_output$pairs)
    
    # sum the match rating (gives 0-1 score for each string comapred) #check may be something better than that if match rate is a non-linear scale 
    pairs[, sum_match := eval(parse(text = paste(in_char_vars_merge_by, collapse = " + ")))]

    
    # create data sets to grab out row names with values of interest  
    data_1_sub[, id1 := as.numeric(rownames(data_1_sub))]
    data_2_sub[, id2 := as.numeric(rownames(data_2))]
    
    
    # if option to grab out all matches for some strings is on, grab pairs for requested names 
    if(length(out_all_matches_for) != 0){
      all_matches_of_interest_1 <- copy(data_1_sub)
      all_matches_of_interest_2 <- copy(data_2_sub)
      for(m in 1:length(in_char_vars_merge_by)){
        
        all_matches_of_interest_1 <- all_matches_of_interest_1[get(in_char_vars_merge_by[[m]]) %in% out_all_matches_for, ]
        
        all_matches_of_interest_2 <- all_matches_of_interest_2[get(in_char_vars_merge_by[[m]]) %in% out_all_matches_for, ]
      }
      
    }
    
    
    # make row number id's in  original datasets 
    data_1[, id1 := as.numeric(rownames(data_1))]
    data_2[, id2 := as.numeric(rownames(data_2))]
    
    # rename 0-1 string match ratings 
    rating_vars <- paste0(in_char_vars_merge_by, "_merge_rating")
    data.table::setnames(pairs, in_char_vars_merge_by, rating_vars)
    
    # remove extra columns 
    pairs <- subset(pairs, select = c( "sum_match", "id1", "id2", rating_vars))
    
    # prepare vars for merge, label data set origin (so we knolw which  string is from which data_set)
    vars_set_1 <- paste0(in_char_vars_merge_by, "_1")
    data.table::setnames(data_1, in_char_vars_merge_by, vars_set_1)
    
    vars_set_2 <- paste0(in_char_vars_merge_by, "_2")
    data.table::setnames(data_2, in_char_vars_merge_by, vars_set_2)
    
    # merge on datasets by row number 
    names_matched_full_set <- ea_merge(pairs, data_1, c("id1"), opt_print = 0)
    names_matched_full_set <- ea_merge(names_matched_full_set, data_2, c("id2", in_vars_exact_match), opt_print = 0)

    # grab out all matches for specific string if option is on 
    if(length(out_all_matches_for) != 0){
      names_matched_of_interest <- names_matched_full_set[ id1 %in% all_matches_of_interest_1$id1 | id2 %in% all_matches_of_interest_2$id2, ]
      
      # put character comparisons next to each other 
      for(m in 1:length(in_char_vars_merge_by)){
        ea_colorder(names_matched_of_interest, c(vars_set_1[m], vars_set_2[m]))
      }
      
      }
    
    # set order for no dups, ensures the highest match is kept
    names_matched_full_set_2 <- data.table::setorder(names_matched_full_set, -sum_match)
    
    # eliminate duplicates, should get highest rating since it is arranged by sum match
    names_matched_full_set <- ea_no_dups(names_matched_full_set, "id1")
    
    # drop row number ID's 
    names_matched_full_set[, id1 := NULL]
    names_matched_full_set[, id2 := NULL]
    
    # put character comparisons next to each other 
    for(m in 1:length(in_char_vars_merge_by)){
      ea_colorder(names_matched_full_set, c(vars_set_1[m], vars_set_2[m]))
    }
    
    # if option to return all ratings of a specific string is on, return a list of sets
    if(length(out_all_matches_for) != 0){
      return_list <- list()
      return_list[["names_matched_full_set"]] <- names_matched_full_set
      return_list[["names_matched_of_interest"]] <- names_matched_of_interest
      
      return(return_list)
    }else{
      # otherwise, return data set
      return(names_matched_full_set)
    }

}