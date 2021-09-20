# Profit of Pizza Size sold in each store and collapse the other dimensions;
apply(profit_cube, c("Pizza_Size_Key", "Store_Location_Key"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
