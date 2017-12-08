min_max_value_finder <- function(input_matrix, num_min_maxes){
  #fnd the min and max values in a matrix
  
  #note: the maximumum and minimum values will henceforth both be referred to in this 
  #function simply as the max or maximum value
  
  #find the absolute value of all elements of input_matrix
  absolute_value_matrix <- abs(input_matrix)
  
  #create a matrix with 0 rows (for preallocation purposes)
  min_max_matrix <- matrix(data = NA_real_, nrow = 0, ncol = 5)
  #change it to to a df so we can have strings and numbers simultaneously
  min_max_df <- as.data.frame(min_max_matrix)
  
  
  #-----------------------------------------------------------------------------------------------------
  temp_row_number <- 0
  
  #when the have found the num_highest_values highest values in the input matrix we stop.
  #more values may be returned than the number entered for num_highest_value in the case
  #that we have ties in the last iteration
  while(temp_row_number < num_min_maxes){
    #finds the maximum value and records row and column number in which it occurs
    #(multiple rows created in the case of a tie) (na.rm important-ignore NA's !!)
    temp_result <- which(absolute_value_matrix == max(absolute_value_matrix, na.rm = TRUE), arr.ind = TRUE)
    #number of rows (pairs of stocks) in temp result (if there are no ties,
    #then temp_row_number will equal 1)
    temp_result_rows <- nrow(temp_result)
    
    #in case of ties for highest value we need this loop
    #in this loop we find the col and row names and the element value and add
    #this info to max_cor_matrix
    for(i in 1:temp_result_rows){
      #first add a row of NA_real_ to min_max_df so that we have somewhere to
      #append the values
      min_max_df <- rbind(min_max_df, NA_real_)
      #find out which row of min_max_df we are going to be adding to
      min_max_df_temp_row <- nrow(min_max_df)
      
      #collecting the values to fill in min_max_df
      #in case we have more than 1 row (case of a tie for highest value)
      #then we need to make sure we record both instances (hence i), 
      #also row is col 1 and col is col as defined by arr.ind
      temp_row_num <- temp_result[i,1]
      temp_col_num <- temp_result[i,2]
      #we find the corresponding value in input_matrix based on row and column indices
      temp_value <- input_matrix[temp_row_num, temp_col_num]
      #find the row and column names 
      temp_row_name <- rownames(input_matrix)[temp_row_num]
      temp_col_name <- colnames(input_matrix)[temp_col_num]
      
      #fill in min_max_df with the above information (the columns numbers
      #correspond to the row names added below)
      min_max_df[min_max_df_temp_row,1] <- temp_value 
      min_max_df[min_max_df_temp_row,2] <- temp_row_num
      min_max_df[min_max_df_temp_row,3] <- temp_col_num
      min_max_df[min_max_df_temp_row,4] <- temp_row_name
      min_max_df[min_max_df_temp_row,5] <- temp_col_name
      
      #replace the maximum value (the one currently being looked at)
      #with NA so that it won't be counted as the highest value again in 
      #the next iteretation, and we can find the next highest value
      absolute_value_matrix[temp_row_num,temp_col_num] <- NA
    }
    
    #temp_row_number is updated in each iteration of the while loop, so that
    temp_row_number <- temp_row_number + temp_result_rows
  }
  
  #add column names to min_max_df
  #general note: predictor_stock is the row name and reponse_stock is the column name
  min_max_df_names <- c("value","row_num","col_num","predictor_stock","response_stock")
  colnames(min_max_df) <- min_max_df_names
  
  return(min_max_df)
  
}