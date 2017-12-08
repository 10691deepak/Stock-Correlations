correlation_generator = function(predictor_stock_matrix, response_stock_matrix, rolling_correlation=FALSE, 
                                 correlation_width=5, correlation_method = "pearson"){
  #correlation_generator : enter two data frames and a correlation or a rolling correlation is created
  #
  # Description:
  #    A predictor stock data frame and a response stock data frame are entered.  A rolling 
  #    correlation is then found between each column (stock) of predictor_stock_matrix and each column 
  #    (stock) of response_stock_matrix.
  #
  # Inputs:
  #    predictor_stock_matrix:
  #       Data-type:   numeric-only data frame or a numeric matrix
  #       Description: a data frame containing stock prices or changes in stock prices.  
  #                    it is intended that the result (or a portion of the result (several stocks pairs)) 
  #                    of double_difference_correlation be used as the input for predictor_stock_matrix.
  #    response_stock_matrix:
  #       Data-type:   numeric-only data frame or a numeric matrix
  #       Description: a data frame containing stock prices or changes in stock prices.  
  #                    it is intended that the result (or a portion of the result (several stocks pairs)) 
  #                    of double_difference_correlation be used as the input for response_stock_matrix.
  #    rolling_correlation:                     
  #       Data-type:   logical (TRUE or FALSE)                          
  #       Description: if TRUE a rolling correlation will be conducted. If FALSE a standard correlation
  #                    correlation will be conducted
  #    correlation_width:
  #       Data-type:   whole number, greater than 1 & <= number of rows in response_stock_matrix &  
  #                    predictor_stock_matrix
  #       Description: this is the number of periods used when creating the rolling correlation.
  #                    the minimum number is 2, but it should be larger in order to create a meaningful
  #                    correlation.
  #    correlation_method:
  #       Data-type:   string (either "pearson", "spearman", or "kendall")
  #       Description: this is the correlation method which will be used.
  #         
  # Outputs:
  #    cor_matrix:
  #       Data-type: matrix
  #       Description: this a correlation matrix which represents the rolling correlation between 
  #                    the predictive stocks
  #
  # Hints:
  #    -various values for correlation_width should be tested
  #    -be mindful of p-values when using smallvalues for correlation_width
  #    -check that the correlation still exist when using larger values for cor width (check
  #     this by using a smaller value for correlation_width and observe the values in the most recent
  #     time periods)
  #    -predictor_stock_matrix and response_stock_matrix must have the same number of rows but not necessarily the same
  #     number of columns (although they will if you are using the result from double_differenc_generator)
  #
  # required R-libraries: none
  # Other R-files required: none
  # Subfunctions: none
  #
  # See also: OTHER_FUNCTION_NAME1,  OTHER_FUNCTION_NAME2

  ##
  
  
  #---------------------------------------------------------------------------------------------------------
  #some data for testing
  #a <- c(3,8,5,3,6,2,4)
  #b <- c(7,2,1,9,5,3,5)
  #c <- c(8,4,2,7,2,3,5)
  #d <- c(2,3,4,2,7,5,8)
  #sample_data <- data.frame(a,b,c,d)
  #x <- c(4,7,5,9,0,6,7)
  #y <- c(6,8,4,1,0,3,5)
  #z <- c(4,6,5,2,5,8,0)
  #sample_data_2 <- data.frame(x,y,z)
  
  #---------------------------------------------------------------------------------------------------------
  #case that rolling correlation is FALSE
  if(rolling_correlation == FALSE){
    #simply find the correlation between the stocks matrices
    cor_matrix <- cor(predictor_stock_matrix, response_stock_matrix, method=correlation_method)
  }
  else{  
    #---------------------------------------------------------------------------------------------------------
    #preallocating cor_matrix (some of these values are used elsewhere also)
    
    #find the dimensions of the inputs
    num_predictor_stock_matrix_cols <- ncol(predictor_stock_matrix)
    num_response_stock_matrix_cols <- ncol(response_stock_matrix)
    df_rows <- nrow(predictor_stock_matrix)
    
    #given m columns (stocks) in predictor_stock_matrix and n columns (stocks) in response_stock_matrix,
    #we will have m*n pairs of correlations (m*n columns)
    cor_col_number = num_predictor_stock_matrix_cols * num_response_stock_matrix_cols
    
    #given m rows in predictor_stock_matrix and response_stock_matrix and a correlation_width of n, the number of rows in the correlation matrix
    #will be defined as:    num_rows = (m+1)-n
    cor_row_number = (df_rows + 1) - correlation_width
    
    #preallocate cor_matrix for speed
    cor_matrix <- matrix(data = NA_real_, nrow = cor_row_number, ncol = cor_col_number)
    
    #------------------------------------------------------------------------------------------------------------
    #creating vector of column names
    
    #preallocate col_name_list (creates a vector with cor_col_number entries)
    col_name_list <- vector("list", cor_col_number)
    #set the counter for column names
    column_name_counter <- 1
    for(i in 1:(num_predictor_stock_matrix_cols)){
      for(j in 1:(num_response_stock_matrix_cols)){
        #setting cor_matrix names
        temp_predictor_name <- toString(colnames(predictor_stock_matrix)[i])
        temp_response_name <- toString(colnames(response_stock_matrix)[j]) 
        #colnames(cor_matrix)[column_name_counter] <- paste(temp_predictor_name,"_vs_", temp_reponse_name)
        temp_name <- paste(temp_predictor_name,"_vs_", temp_response_name, sep = "")
        col_name_list[[column_name_counter]] <- temp_name
        column_name_counter <- column_name_counter + 1
      }
    }
    
    #--------------------------------------------------------------------------------------------------------------
    #creating vector of row names
    
    #preallocate row_name_list (creates a vector with cor_col_number entries)
    row_name_list <- vector("list", cor_row_number)
    #create a list of row names for cor_matrix
    for(i in 1:cor_row_number){
      #the row in which the correlation starts
      start_row <- i
      #the row in which the correlation stops
      #(ie. 3 period correlation starts in row in 1 and stops in row 3)
      stop_row <- (start_row - 1) + correlation_width
      
      #make the row numbers strings
      start_row_string <- toString(start_row) 
      stop_row_string <- toString(stop_row)   
      
      #rownames(cor_matrix)[i] <- paste("period_",start_row_string,"_to_period_",stop_row_string)
      row_name_list[[i]] <- paste("period_",start_row_string,"_to_period_",stop_row_string,sep = "")
    }
    
    #-------------------------------------------------------------------------------------------------------------
    #add column and row names to cor_matrix
    colnames(cor_matrix) <- col_name_list
    rownames(cor_matrix) <- row_name_list
    
    #-------------------------------------------------------------------------------------------------------------
    #creating the rolling correlation matrix
    
    #we loop from 1 to cor_row_number and in each iteration of the loop, the row in cor_matrix in which
    #we are appending the correlation values is updated.  Also, the column in cor_matrix in which we
    #are appending the correlation values is reset to 1 after each iteration.
    #this loop is also responsible for which rows are being used when the correlation is made.
    for(i in 1:cor_row_number){
      #col_counter is used to keep track of which column we are in. it resets every time we change rows.
      #(ie. every time we come back to this first for loop)
      temp_col <- 1
      #this loop is responsible for which column in predictor_stock_matrix is used when creating the correlation
      for(j in 1:ncol(predictor_stock_matrix)){
        #this loop is responsible for which column in response_stock_matrix is used when creating the correlation
        for(k in 1:ncol(response_stock_matrix)){
          #start_cor is the row in which the correlation stops
          start_cor <- i
          #stop_cor is the row in which the correlation stops, this is defined as (start_cor-1)+correlation_width
          #(ie. start_row=8, correlation_width=3, thus start_cor=10, which gives us a correlation over 3 time periods)
          stop_cor <- (start_cor-1)+correlation_width
          
          #the correlation from row i to row (i-1)+correlation_width between column j of predictor_stock_matrix
          #and rcolumn k of response_stock_matrix
          temp_cor_value <- cor(predictor_stock_matrix[start_cor:stop_cor,j] , response_stock_matrix[start_cor:stop_cor,k], method = correlation_method)
          
          #store the correlation value in the aprropriate cell
          #the row is determined by the outer most for loop (i)
          #and column is is reset to 1 every time we change rows.
          cor_matrix[i,temp_col] <- temp_cor_value
          
          #add 1 to temp_col each time, to keep track of which column we are in
          temp_col <- temp_col + 1
        }
      }
    }
  }
  
  return(cor_matrix)
  
}