stock_matrix_generator <- function(min_period_length, trade_volume=FALSE, close_price=TRUE, open_price=FALSE, high_price=FALSE, low_price=FALSE, close_minus_open=FALSE, high_minus_low=FALSE){
  
  
  #stock_matrix_generator: create matrix of stock data from csv files
  #
  # Description:
  #    read in csv files and create data frame of stock data.  The stocks will be merged and matched
  #    based on the date column.  You may choose which variables you would like to include
  #    in the output matrix
  #
  # Inputs:
  #    min_period_length:
  #       Data-type: integer
  #       Description: the minimum number of periods of data for a stock must have to be included
  #                    matrix. should be at least 30. may not exceed
  #    trade_volume:
  #       Data-type: TRUE / FALSE
  #       Description: TRUE is default
  #    close_price:
  #       Data-type: TRUE / FALSE
  #       Description: TRUE is default 
  #    open_price:
  #       Data-type: TRUE / FALSE
  #       Description: FALSE is default
  #    high_price:
  #       Data-type: TRUE / FALSE
  #       Description: FALSE is default
  #    low_price:
  #       Data-type: TRUE / FALSE
  #       Description: FALSE is default
  #    close_minus_open:
  #       Data-type: TRUE / FALSE
  #       Description: FALSE is default
  #    high_low:
  #       Data-type: TRUE / FALSE
  #       Description: FALSE is default
  
  #
  # Outputs:
  #    stock_data_frame:
  #       Data-type: data frame
  #       Description: a data containing the information you have specified. number of rows
  #                    min_period_length input.
  #
  # Hints:
  #    -the number of rows stock_data_frame will not necessarily be equal to min_period_length
  #       it could be both longer or shorter
  #    -this code is intended to be used with one specific set of csv files.  it must be 
  #       adapted for other uses.
  #
  # required R-libraries: data.table
  # Other R-files required: none
  # Subfunctions: none
  #
  # See also: 
  #
  # Author: Hunter McCawley
  # email: huntermccawley@hotmail.com
  # Date Created: 26.04.2016
  # Last Revision: 29.04.2016
  # Revision Notes: revised to allow more input specifications
  #    
  ##
  
  #wd set to location containing stock data
  setwd("C:/Users/deepa/Desktop/Stock Market/Stock Data")
  
  #load data.table for fread
  library(data.table)
  #creates a list of all csv names in the folder
  csv_names = list.files(pattern=".csv")
  
  #data frame is preallocated, temporary data frame will be appended to this inside the loop
  preallocated_data_frame <- data.frame(assign(csv_names[2], fread(csv_names[2], header=FALSE, select = c(1,6,7))))
  #column names
  date_column <- "date"
  close_column <- paste(csv_names[2],'_close',sep = "")
  volume_column <- paste(csv_names[2],'_volume',sep = "")
  #naming the columns
  colnames(preallocated_data_frame, do.NULL = FALSE)
  colnames(preallocated_data_frame) <- c(date_column,close_column,volume_column)
  
  
  #loop through all names from csv_names
  for (i in 1:length(csv_names)){ 
    
    #create data frame for purpose of checking if the stock is long enough 
    #(based min_period_length entry)
    stock_length_data_frame <- data.frame(fread(csv_names[i], header=FALSE, select = c(1)))
    
    #-----------------------------------------------------------------------
    #we only fuk wit stocks with at least min_period_length  
    stock_length <- nrow(stock_length_data_frame)
    if(stock_length >= min_period_length){
      
      #-----------------------------------------------------------------------
      #find the stock name, remove extra shit from the string name
      stock_name <- sub("[A-z]*_","",csv_names[i])
      stock_name <-  sub("\\.[A-z]*$","",stock_name)
      
      #----------------------------------------------------------------------------------------------------------
      
      #data frame created for the date column
      date_column_data_frame <- data.frame(fread(csv_names[i], header=FALSE, select = c(1)))
      #column name string created
      date_column_name <- "date" #1st col
      #column name changed
      colnames(date_column_data_frame, do.NULL = FALSE)
      colnames(date_column_data_frame) <- c(date_column_name)
      #date_column_data_frame added to temp_data_frame
      #(temp date frame only exists in this level of the for-loop we are in)
      temp_data_frame <- date_column_data_frame
      
            
      if(close_price == TRUE){
        #data frame created for the date column (6th col)
        close_column_data_frame <- data.frame(fread(csv_names[i], header=FALSE, select = c(6)))
        #column name string created
        close_column_name <- paste(stock_name,'_close',sep = "")  
        #column name changed
        colnames(close_column_data_frame, do.NULL = FALSE)
        colnames(close_column_data_frame) <- c(close_column_name)
        #temp data frame updated
        temp_data_frame <- data.frame(temp_data_frame, close_column_data_frame)
      }
      
      if(open_price == TRUE){
        #data frame created for the date column (3rd col)
        open_column_data_frame <- data.frame(fread(csv_names[i], header=FALSE, select = c(3)))
        #column name string created
        open_column_name <- paste(stock_name,'_open',sep = "")
        #column name changed
        colnames(open_column_data_frame, do.NULL = FALSE)
        colnames(open_column_data_frame) <- c(open_column_name)
        #temp data frame updated
        temp_data_frame <- data.frame(temp_data_frame, open_column_data_frame)
      }
      
      if(trade_volume == TRUE){
        #data frame created for the date column (7th col)
        volume_column_data_frame <- data.frame(fread(csv_names[i], header=FALSE, select = c(7)))
        #column name string created
        volume_column_name <- paste(stock_name,'_volume',sep = "")
        #column name changed
        colnames(volume_column_data_frame, do.NULL = FALSE)
        colnames(volume_column_data_frame) <- c(volume_column_name)
        #temp data frame updated
        temp_data_frame <- data.frame(temp_data_frame, volume_column_data_frame)
      }
      
      if(high_price == TRUE){
        #data frame created for the date column (4th col)
        high_column_data_frame <- data.frame(fread(csv_names[i], header=FALSE, select = c(4)))
        #column name string created
        high_column_name <- paste(stock_name,'_high',sep = "")
        #column name changed
        colnames(high_column_data_frame, do.NULL = FALSE)
        colnames(high_column_data_frame) <- c(high_column_name)
        #temp data frame updated
        temp_data_frame <- data.frame(temp_data_frame, high_column_data_frame)
      }
      
      if(low_price == TRUE){
        #data frame created for the date column (5th col)
        low_column_data_frame <- data.frame(fread(csv_names[i], header=FALSE, select = c(5)))
        #column name string created
        low_column_name <- paste(stock_name,'_low',sep = "")
        #column name changed
        colnames(low_column_data_frame, do.NULL = FALSE)
        colnames(low_column_data_frame) <- c(low_column_name)
        #temp data frame updated
        temp_data_frame <- data.frame(temp_data_frame, low_column_data_frame)
      }
      
      #--------------------------------------------------------------------------------------------------
      #creating high_minus_low if true. ignored if false.
      if(high_minus_low == TRUE){
        
        #in the case that high_price is false, this variable must now be created for first time
        if(high_price == FALSE){
          #data read in for the high_price column (4th col)
          high_column_data_frame <- data.frame(fread(csv_names[i], header=FALSE, select = c(4)))
          #column name string created
         # high_column_name <- paste(stock_name,'_high',sep = "")
          #column name changed
        #  colnames(high_column_data_frame, do.NULL = FALSE)
        #  colnames(high_column_data_frame) <- c(high_column_name)
        }
        
        #in the case that low_price is false, this variable must now be created for first time
        if(low_price == FALSE){
          #data read in for the low_price column (5th col)
          low_column_data_frame <- data.frame(fread(csv_names[i], header=FALSE, select = c(5)))
          #column name string created
        #  low_column_name <- paste(stock_name,'_low',sep = "")
          #column name changed
        #  colnames(low_column_data_frame, do.NULL = FALSE)
        #  colnames(low_column_data_frame) <- c(low_column_name)  
        }
        
        #data frame created for high_minus_low
        high_minus_low_data_frame <- high_column_data_frame - low_column_data_frame
        #column name string created
        high_minus_low_column_name <- paste(stock_name,'_high_minus_low',sep = "")
        #column name changed
        colnames(high_minus_low_data_frame, do.NULL = FALSE)
        colnames(high_minus_low_data_frame) <- c(high_minus_low_column_name)
        #temp data frame updated
        temp_data_frame <- data.frame(temp_data_frame, high_minus_low_data_frame)
      
      }
      
      #creating close_minus_open if true. ignored if false.
      if(close_minus_open == TRUE){
        
        #in the case that close_price is false, this variable must now be created for first time
        if(close_price == FALSE){
          #data frame created for the close price column (6th col)
          close_column_data_frame <- data.frame(fread(csv_names[i], header=FALSE, select = c(6)))
          #column name string created
        #  close_column_name <- paste(stock_name,'_close',sep = "")
          #column name changed
          #colnames(close_column_data_frame, do.NULL = FALSE)
          #colnames(close_column_data_frame) <- c(close_column_name)
        }
        
        #case that low_price is false, this variable must now be created for first time
        if(open_price == FALSE){
          #data frame created for the open_price column (3rd col)
          open_column_data_frame <- data.frame(fread(csv_names[i], header=FALSE, select = c(3)))
          #column name string created
          #open_column_name <- paste(stock_name,'_open',sep = "")
          #column name changed
          #colnames(open_column_data_frame, do.NULL = FALSE)
          #colnames(open_column_data_frame) <- c(open_column_name)  
        }
        
        #data frame created for high_minus_low
        close_minus_open_data_frame <- close_column_data_frame - open_column_data_frame
        #column name string created
        close_minus_open_column_name <- paste(stock_name,'_close_minus_open',sep = "")
        #column name changed
        colnames(close_minus_open_data_frame, do.NULL = FALSE)
        colnames(close_minus_open_data_frame) <- c(close_minus_open_column_name)
        #temp data frame updated
        temp_data_frame <- data.frame(temp_data_frame, close_minus_open_data_frame)
        
      }

      
      
      preallocated_data_frame <- merge(preallocated_data_frame, temp_data_frame)
    }
  }
  
  #remove initialized columns from preallocated_data_frame
  preallocated_data_frame <- subset(preallocated_data_frame, select=-c(table_aa.csv_volume,table_aa.csv_close))
  
  #sort alphabetically
  ordered_data_frame <- preallocated_data_frame[ , order(names(preallocated_data_frame))]
  
  #make date column column number one
  #first extract date column data
  date_column <- data.frame(ordered_data_frame[,c("date")])
  #reset the name of date column
  colnames(date_column) <- c("date")
  #add date_column back to ordered_data_frame so it is now in first position
  stock_data_frame <- data.frame(date_column, ordered_data_frame)
  
  #drop the duplicate date column
  stock_data_frame <- subset(stock_data_frame, select=-c(date.1))
  
  #change name for output
  #stock_data_frame <- preallocated_data_frame
  #stock_data_frame <- ordered_data_frame
  return(stock_data_frame)
}

