double_difference_generator <- function(stock_data_frame, predictor_diff, response_diff, lag_period=0, time_col=FALSE, simple_naming=FALSE){
  #double_difference_correlation : different difference between predictor and response variables, then find correlation
  #
  # Description:
  #    enter a data frame or matrix of stocks (potentially including a column representing 
  #    the time period (date)), the data frame is duplicated and the stocks from one copy are used as 
  #    predictor variables while the stocks in the other copy are used as response variables.
  #    for both the predictor and response data frame a (potentially different) difference period is 
  #    entered for each. It is ensured that stocks in both the predictor data frame and response 
  #    data frame are aligned correctly based upon the values entered for predictor_diff, response_diff
  #    and lag_period
  #
  # Inputs:
  #    stock_data_frame:
  #       data-type:   numeric-only data frame (with possibly a date column) or a numeric matrix
  #       description: A data frame containing only numeric time-series data, except for
  #                    possibly a column containing the time period. each column represents a stock.
  #                    rows represent the time period.
  #    predictor_diff:
  #       data-type:   whole number, greater than 1 & less than number of rows in stock_data_frame
  #       description: the period length between which rows should be subtracted in the predictor stocks. 
  #                    For example, predictor_diff=2 would indicate row 3-1, row 4-2, row 5-3, etc.
  #    response_diff:
  #       data-type:   whole number, greater than 1 & less than number of rows in stock_data_frame
  #       description: the period length between which rows should be subtracted in the response stocks. 
  #                    For example, predictor_diff=2 would indicate row 3-1, row 4-2, row 5-3, etc.
  #    lag_period:
  #       data-type:   whole number, less than the number of rows in stock_data_frame
  #       description: this is the difference between predictor_diff ends and the corresponding
  #                    response_diff begins
  #    time_col:
  #       data-type:   either logical (but only FALSE) or character (string)
  #       description: if there is no time column, enter FALSE. Otherwise, enter the name of the
  #                    time (date) column as a string
  #    simple_naming:
  #       data-type:   logical (TRUE / FALSE)
  #       description: if TRUE only the stock name will be used in the output matrix.  if FALSE
  #                    then, all pertinent info shall be included in output column names.
  #
  #
  # Outputs:
  #    stock_difference_list:
  #       Data-type: list
  #       Description: a list containing two matrices.  One is the so-called predictor matrix and
  #                    one is the so-called response matrix.
  #
  # Hints:
  #    -example 1 for clarity:  given that predictor_diff = 2 & response_diff = 3 & lag_period = 0:
  #     a 2 day change in stock x is paired with a 3 day change in stock y
  #     ie.  change in price from day from day 1 to day to day 3 in stock x is paired with 
  #     the change from day 3 to day 6 in stock y etc.
  #
  #    -example 2 for clarity:  given that predictor_diff = 2 & response_diff = 3 & lag_period = 2:
  #     a 2 day change in stock x is paired with a 3 day change in stock y
  #     with a lag period between them of 2 days
  #     ie.  change from day from day 1 to day to day 3 in stock x is paired with 
  #     the change from day 5 to day 8 in stock y etc.
  #
  #    -predictor_diff + response_diff + lag_period must be <= nrow(stock_data_frame) - 2
  #     in order to be able to find a correlation (since you need columns of at least 
  #     length 2 to find a correlation) AND IDEALY IT SHOULD BE MUCH SMALLER in order to 
  #     generate a correlation which is meaningful
  #
  #    -simple_naming should likely be set to false, unless it this function is being called
  #     from another function which records pertinent information.
  #
  # required R-libraries: none
  # Other R-files required: none
  # Subfunctions: none
  #
  # See also:
 
  
  
  #---------------------------------------------------------------------------------------------
  #some data for testing
  #a <- c(3,8,5,3,6,2,4)
  #b <- c(7,2,1,9,5,3,5)
  #c <- c(8,4,2,7,2,3,5)
  #d <- c(2,3,4,2,7,5,8)
  #sample_data <- data.frame(a,b,c,d)
  
  
  #---------------------------------------------------------------------------------------------
  #first we check for input errors
  #make sure lag_period is a number
  if(data.class(lag_period) != "numeric"){
    stop("lag_period must be a whole number")
  }
  #check that lag_period + 2 < number of rows in stock_data_frame
  if(nrow(stock_data_frame) <= lag_period + 2){
    stop("lag_period + 2 must be < number of rows in stock_data_frame")
  }
  #make sure lag_period is greater than 0
  if(lag_period < 0){
    stop("lag_period must be greater than 0")
  }
  #make sure lag_period is a whole_number
  if(lag_period%%1 != 0){
    stop("lag_period must be a whole number")
  }
  #make sure predictor_diff is a number
  if(data.class(predictor_diff) != "numeric"){
    stop("predictor_diff must be a whole number")
  }
  #make sure predictor_diff is >= 1
  if(predictor_diff < 1){
    stop("predictor_diff must be at least 1")
  }
  #make sure predictor_diff is a whole_number
  if(predictor_diff%%1 != 0){
    stop("predictor_diff must be a whole number")
  }
  #make sure predictor_diff + 2 < number of rows in stock_data_frame
  if(nrow(stock_data_frame) <= predictor_diff + 2){
    stop("predictor_diff + 2 must be < number of rows in stock_data_frame")
  }
  #make sure response_diff is a number
  if(data.class(response_diff) != "numeric"){
    stop("response_diff must be a whole number")
  }
  #make sure response_diff is >= 1
  if(response_diff < 1){
    stop("response_diff must be at least 1")
  }
  #make sure response_diff is a whole_number
  if(response_diff%%1 != 0){
    stop("response_diff must be a whole number")
  }
  #make sure response_diff + 2 < number of rows in stock_data_frame
  if(nrow(stock_data_frame) <= predictor_diff + 2){
    stop("response_diff + 2 must be < number of rows in stock_data_frame")
  }
  #predictor_diff + response_diff + lag_period must be <= nrow(stock_data_frame) - 2
  if(predictor_diff + response_diff + lag_period > nrow(stock_data_frame) - 2){
    stop("predictor_diff + response_diff + lag_period + 2 must be <= nrow(stock_data_frame)")
  }
  #make sure time_col is either FALSE or a string
  if(time_col != FALSE && data.class(time_col) != "character"){
    stop("time_col must be either FALSE or the name of the date column (as a string)")
  }
  
  #----------------------------------------------------------------------------------------------
  #check if there is a date column, and delete it if it exists
  if(time_col != FALSE){
    stock_data_frame[, time_col] <- NULL
  }
  
  
  #---------------------------------------------------------------------------------------------
  #create the difference predictor and response matrices
  predictor_matrix <- diff(as.matrix(stock_data_frame), lag = predictor_diff)
  response_matrix <- diff(as.matrix(stock_data_frame), lag = response_diff)
  
  
  #---------------------------------------------------------------------------------------------
  #naming the columns in predictomatrix and response matrix
  #find number of rows and columns in input_data
  number_stocks <- ncol(stock_data_frame)
  #create string version of predictor_diff and response_diff for naming purposes later on
  predictor_diff_string <- toString(predictor_diff)
  response_diff_string <- toString(response_diff)
  lag_period_string <- toString(lag_period)
  
  
  #------------------------------------------------------------------------------------------------------------------------------------------------
  #case simple_naming == FALSE
  if(simple_naming == FALSE){
    #change predictor_matrix and response_matrix column names for clarity
    for(i in 1:number_stocks){
      #setting predictor_matrix names
      temp_predictor_name <- colnames(predictor_matrix)[i]
      colnames(predictor_matrix)[i] <- paste("predictor:",temp_predictor_name,", diff=", predictor_diff_string,", lag_period=",lag_period_string,sep = "")
    
      #setting response_matrix names
      temp_response_name <- colnames(response_matrix)[i]
      colnames(response_matrix)[i] <- paste("response:",temp_response_name,", diff=", response_diff_string,", lag_period=",lag_period_string,sep = "")
    }
  }

  
  #-----------------------------------------------------------------------------------------------------------
  #formula for setting the correct number of rows in predictor_data_frame and response_data_frame 
  #and ensuring that the correlations match up correctl
  #predictor_data_frame: subtract response_diff # of rows from end of predictor_data_frame
  #response_data_frame: subtract predictor_diff # of rows from beginning of response_data_frame
  #this ensures same number of rows, and that rows match correctly for correlation
  #(the lag is 0 at this point)
  
  #removing from end of predictor data frame
  predictor_matrix <- head(predictor_matrix, - response_diff)
  #removing from beginning of response data frame
  response_matrix <- tail(response_matrix, - predictor_diff)
  
  
  #-----------------------------------------------------------------------------------------------------------
  #preallocate correlation matrix for speed
  #nrow and ncol both = number_stocks since this value represents the number of stocks
  correlation_matrix <- matrix(data = NA, nrow = number_stocks, ncol = number_stocks)
  #get column names
  correlation_column_names <- colnames(response_matrix)
  #set column names
  colnames(correlation_matrix) <- correlation_column_names
  
  
  #------------------------------------------------------------------------------------------------------------
  #set the lag period (the difference between when predictor_diff ends and response_diff starts)
  #if lag_period != 0
  if(lag_period != 0){
    #remove first lag_period rows from response_matrix
    response_matrix <- response_matrix[-1:-lag_period,]
    #remove last lag_period rows from predictor_matrix
    predictor_matrix <- head(predictor_matrix, -lag_period)
  }
  
  
  #-------------------------------------------------------------------------------------------------------------
  #create a list containing predictor_matrix and response_matrix so that only one object need be returned
  
  #set predictor_matrix and response_matrix equal to themselves in order to determine that the names are 
  #retained when the list is created
  stock_difference_list <- list(predictor_matrix=predictor_matrix, response_matrix=response_matrix )
  
  #return the result
  return(stock_difference_list)
  
}

