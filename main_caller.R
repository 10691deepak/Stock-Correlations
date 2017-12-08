#timing the function
start_time <- Sys.time()

a <- c(3,8,5,3,6,2,4,6,7)
b <- c(7,2,1,9,5,3,5,1,9)
c <- c(8,4,2,7,2,3,5,0,1)
d <- c(2,3,4,2,7,5,8,1,0)
sample_data <- data.frame(a,b,c,d)


#add the data you want to use here
#stock_data_frame <- sample_data  # <- for testing
stock_data_frame <- stock_matrix


#number of periods (rows) in stock_data_frame
total_num_stock_periods <- nrow(stock_data_frame)

#initialize our vectors which will make up the data frame pertaining to the strongest correlations
correlation_value <- c()
predictor_stock <- c()
response_stock <- c()
num_periods <- c()
lag_periods <- c()
predictor_difference <- c()
response_difference <- c()



#number of days considered in the in the correlation
for(i in 1:8){
  #note: market is open about 250 days per year
  #only the most recent n periods will be considered when making the correlation
  #*************************IMPORTANT*****************************
  #correlation_periods_matrix is NOT the number of days.... it is the number of periods
  #after the data has been processed in double difference generator.  
  #ie: if correlation_periods = 5, and temp_predictor_diff = 1, temp_response_diff=4, and lag_period = 2,
  #then we would need the last 12 days (5+1+4+2)
  correlation_periods_matrix <- as.matrix(c(15,30,40,50,60,125,250,300))
  correlation_periods <- correlation_periods_matrix[i]
  
  
  #number of difference periods in predictive matrix
  for(j in 1:4){
    temp_predictor_diff <- j
    #number of difference periods in response matrix
    for(k in 1:4){
      temp_response_diff <- k
      #lag periods (num periods between predictive matrix and
      #response matrix)
      for(l in 0:3){
        temp_lag_periods <- l
        
        #we want the number of rows in the output of double_difference_generator
        #(double_difference_result) to be equal to correlation period
        #the number of rows returnd by double_difference_generator is defined by the following equation:
        # nrow(double_difference_result) = nrow(stock_data_frame) - temp_predictor_diff - temp_response_diff - temp_lag_periods
        #so the minimum number of rows we need is given by:
        temp_necessary_rows <-  correlation_periods + temp_predictor_diff + temp_response_diff + temp_lag_periods
        
        #---------------------------------------------------------------------------------------------------------------
        #check if there are enough rows in stock_data_frame given temp_predictor_diff, temp_response_diff and temp_lag_periods values
        #case that there are too few rows in stock_data_frame
        if(temp_necessary_rows > total_num_stock_periods){
          temp_final_result <- "too few rows in stock data frame"
          
          #-------------------------------------------------------------------------------------------------------------
          #update the vectors
          correlation_value <- c(correlation_value, NA)
          predictor_stock <- c(predictor_stock,NA)
          response_stock <- c(response_stock,NA)
          num_periods <- c(num_periods,correlation_periods)
          lag_periods <- c(lag_periods,temp_lag_periods)
          predictor_difference <- c(predictor_difference,temp_predictor_diff)
          response_difference <- c(response_difference,temp_response_diff)
          
          
        }
        #case that there are enough rows in stock_data_frame
        else{
          #necessary row matrix containts only the last temp_necessary_rows rows of stock_data_frame
          #in order to the number of rows produced in double_difference_generator is equal to
          #correlation_periods
          temp_stock_data_frame <- tail(stock_data_frame, temp_necessary_rows)
          
          #-------------------------------------------------------------------------------------------------------------
          #call double_difference_generator
          double_difference_result <- double_difference_generator(temp_stock_data_frame, predictor_diff = temp_predictor_diff,
                                                                  response_diff = temp_response_diff, lag_period = temp_lag_periods, 
                                                                  time_col = "date", simple_naming = TRUE)
          #get the result from the list double_difference_result
          predictor_matrix <- double_difference_result$predictor_matrix
          response_matrix <- double_difference_result$response_matrix
          
          #-------------------------------------------------------------------------------------------------------------
          #call correlation_generator using user input and the result from double_difference_generator
          temp_correlation_result <- correlation_generator(predictor_matrix, response_matrix, correlation_method = "spearman")
          
          #-------------------------------------------------------------------------------------------------------------
          #call min_max_value_finder to find min and max values from temp_correlation_result
          min_max_result <- min_max_value_finder(temp_correlation_result, 5)
          #find the number of rows in min_max_result
          num_min_max_rows <- nrow(min_max_result)
          
          #-------------------------------------------------------------------------------------------------------------
          #update the vectors
          #for(m in 1:num_min_max_rows){   # <-  ++++++++++++++++++++++++++  correct line
          for(m in 1:5){
            correlation_value <- c(correlation_value, min_max_result$value[m])
            predictor_stock <- c(predictor_stock, min_max_result$predictor_stock[m])
            response_stock <- c(response_stock, min_max_result$response_stock[m])
            num_periods <- c(num_periods,correlation_periods)
            lag_periods <- c(lag_periods,temp_lag_periods)
            predictor_difference <- c(predictor_difference,temp_predictor_diff)
            response_difference <- c(response_difference,temp_response_diff)
          }
        } 
      }
    }
  }
}


#append the vectors together to create a data frame
result_data_frame <- data.frame(correlation_value, predictor_stock, response_stock, num_periods,
                                lag_periods, predictor_difference, response_difference)

#sort result_data_frame in reverse order according to
#absolute value of correlation_value
final_result_data_frame_2 <- result_data_frame[rev(order(abs(result_data_frame$correlation_value), na.last = FALSE)),]



#timing the function
stop_time <- Sys.time()

run_time  <- stop_time - start_time




