# function set to work only with this data set, since the process repeats in 
# different scripts
fix_trait_labels = function(x){
  
  x = gsub("_", " ", x)

  x = ClimMobTools:::.title_case(x)
  
  x
}

# return number of decimals 
# https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r/5173906
decimalplaces <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

