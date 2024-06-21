# function set to work only with this data set, since the process repeats in 
# different scripts
fix_trait_labels = function(x){
  
  x = gsub("_", " ", x)

  x = ClimMobTools:::.title_case(x)
  
  x
}
