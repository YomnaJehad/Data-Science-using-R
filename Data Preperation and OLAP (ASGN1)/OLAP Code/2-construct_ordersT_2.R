# Function to generate the Orders table (Fact Table)
gen_orders <- function(no_of_recs) {

  # Generate transaction data randomly
  
  #Dimensions
  Store_Location <- sample(Store_Location_Table$Store_Location_Key, no_of_recs, replace=T, prob=c(2,2,1))
  Pizza_Size     <- sample(Pizza_Size_Table$Pizza_Size_Key,         no_of_recs, replace=T, prob=c(2,2,1,1,3))
  Dough_Type     <- sample(Dough_Type_Table$Dough_Type_Key,         no_of_recs, replace=T, prob=c(2,2,1))
  Cheese_Type    <- sample(Cheese_Type_Table$Cheese_Type_Key,       no_of_recs, replace=T, prob=c(1,3,2))
  Topping_Type   <- sample(Topping_Type_Table$Topping_Type_Key,     no_of_recs, replace=T, prob=c(2,2,1,3))
  
  #Attributes
  Quantity <- sample(c(1, 2, 3, 4, 5), no_of_recs, replace=T)
  Date     <- sample (c(1:365), no_of_recs, replace=T)
  
  #Calculations
  Profit   <- (Pizza_Size_Table[Pizza_Size,]$Pizza_Size_Key * 10 + Dough_Type_Table[Dough_Type,]$Dough_Type_Key * 5 + Cheese_Type_Table[Cheese_Type,]$Cheese_Type_Key * 5 + Topping_Type_Table[Topping_Type,]$Topping_Type_Key * 10)*Quantity 
  
  #Orders dataframe creation
  orders   <- data.frame(Store_Location_Key= Store_Location,
                         Pizza_Size_Key    = Pizza_Size,
                         Dough_Type_Key    = Dough_Type,
                         Cheese_Type_Key   = Cheese_Type,
                         Topping_Type_Key  = Topping_Type,
                          
                         Quantity_Sold     = Quantity,
                         Date              = Date,
                         Profit            = Profit)

  # Sort the records by Date order
  orders <- orders[order(orders$Date),]
  row.names(orders) <- NULL
  
  write.csv(Store_Location, 'C:/Users/admin/Desktop/DSASGN1/csvfiles/store_location.csv' ,row.names =TRUE)
  write.csv(Pizza_Size, 'C:/Users/admin/Desktop/DSASGN1/csvfiles/Pizza_Size.csv' ,row.names =TRUE)
  write.csv(Dough_Type, 'C:/Users/admin/Desktop/DSASGN1/csvfiles/Dough_Type.csv' ,row.names =TRUE)
  write.csv(Cheese_Type, 'C:/Users/admin/Desktop/DSASGN1/csvfiles/Cheese_Type.csv' ,row.names =TRUE)
  write.csv(Topping_Type, 'C:/Users/admin/Desktop/DSASGN1/csvfiles/Topping_Type.csv' ,row.names =TRUE)
  
  write.csv(orders, 'C:/Users/admin/Desktop/DSASGN1/csvfiles/orders.csv' ,row.names =TRUE)
  
  #if we want to read again we can use this
  #orders<-read.csv('C:/Users/admin/Desktop/DSASGN1/csvfiles/orders.csv')
  
  return(orders)

  }

# Now create the orders fact table
orders_fact <- gen_orders(500)

# Look at a few records
head(orders_fact)


