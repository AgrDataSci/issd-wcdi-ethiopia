# ...............................
# ...............................
# get list of column names in the data sets to 
# harmonize them
library("jsonlite")
library("gosset")
library("jsonlite")

list.files("data")

projects = read.csv("data/issd-projects-climmob-v2.csv")

keep = projects$Collected > 0

projects = projects[keep, ]

dat_list = list()

for(i in seq_along(projects$Project)) {
  
  dat = fromJSON(paste0("data/raw/ET-",
                        projects$Project[i],
                        ".json"))
  
  pack = fromJSON(paste0("data/raw/ET-package_",
                         projects$Project[i],
                         ".json"))
  
  
  names_dat = colnames(dat)
  
  trait_index = grep("caracteristica", names_dat)
  
  traits = as.vector(unlist(dat[4, trait_index]))
  
  traits = tolower(gsub("[-(A/B/C)]", "", traits))
  
  traits_best = paste0(traits, "_best")
  
  traits_worst = paste0(traits, "_worst")
  
  names(dat)[trait_index] = traits
  
  names(dat)[trait_index + 1] = traits_best
  
  names(dat)[trait_index + 2] = traits_worst
  
  dat = dat[-trait_index]
  
  dat$crop = projects$Crop[i]
  
  dat = merge(pack, dat, by = "codigo_paquetes", all.y = TRUE)
  
  names(dat) = make_clean_names(names(dat))
  
  dat_list[[i]] = dat
  
}

head(dat_list[[1]])

uniquenames = lapply(dat_list, function(x){
  names(x)
})


uniquenames = unique(unlist(uniquenames))

check = which(unlist(lapply(dat_list, function(x){
  "overall_performance_worst_1" %in% names(x)
})))

dat_list[check[1]]

# make a table that is going to be harmonized by hand
work = data.frame(uniquenames,
                  finalname = "")

# write.csv(work, "data/harmonized-names.csv", row.names = FALSE)


