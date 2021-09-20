# Setup the dimension tables

Pizza_Size_Table <- 
  data.frame(Pizza_Size_Key =c(1, 2, 3, 4, 5),
             Pizza_Size_Name=c("personal", "small", "medium", "large", "xlarge")
             )

Dough_Type_Table <- 
  data.frame(Dough_Type_Key =c(1, 2, 3),
             Dough_Type_Name=c("whole wheat thin", "white regular", "stuffed crust")
             )

Topping_Type_Table <- 
  data.frame(Topping_Type_Key =c(1, 2, 3, 4),
             Topping_Type_Name=c("tomatoes", "pepper", "onions", "pepperoni")
            )

Cheese_Type_Table <- 
  data.frame(Cheese_Type_Key =c(1, 2, 3),
             Cheese_Type_Name=c("swiss", "cheddar", "mozzarella")
            )

Store_Location_Table <- 
  data.frame(Store_Location_Key=c(1, 2, 3),
             Address=c("3 Abbas St.", "4 Makram St.", "5 Tayaran St."),
             City=c("Cairo", "Alex", "Mansoura"),
             Province=c("Central Egypt", "Northern Egypt", "Eastern Egypt"),
             Country_Key=c("1", "1", "1"),
             Region=c("Middle East", "Middle East", "Middle East"),
             Manager_ID=c("1", "2", "3")
            )

# Extra tables from the snowflake diagram
Country_Table <- 
  data.frame(Country_ID=c(1, 2, 3),
             Country_Name=c("Egypt", "Lebanon", "Japan"),
             Country_Postal_Code=c("1111", "2222", "3333")
  )

Managers_Table <- 
  data.frame(Manager_ID=c(1, 2, 3),
             Manager_Name=c("Ahmed", "Mohammed", "Mahmoud"),
             Manager_Email=c("ahmed@debi.eg", "mohammed@debi.eg", "mahmoud@debi.eg")
             
  )
