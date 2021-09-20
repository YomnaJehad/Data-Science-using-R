# Profit of each combination of Cheese_Type and Topping_Type in each Store Location;
# To explore which combination is popular in which store 

apply(profit_cube, c("Cheese_Type_Key", "Topping_Type_Key", "Store_Location_Key"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
