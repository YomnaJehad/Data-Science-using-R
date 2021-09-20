
# Build up a cube
profit_cube <- 
    tapply(orders_fact$Profit, 
           orders_fact[,c("Store_Location_Key","Pizza_Size_Key","Dough_Type_Key","Cheese_Type_Key","Topping_Type_Key","Quantity_Sold","Date")], 
           FUN=function(x){return(sum(x))})

# Showing the cells of the cube
profit_cube

dimnames(profit_cube)
