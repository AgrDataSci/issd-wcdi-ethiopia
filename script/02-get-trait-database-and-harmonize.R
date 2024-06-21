# ...............................
# ...............................
# this script will put the data together combining variable ids 
# and will standardize the list of varieties
library("jsonlite")
library("janitor")
library("gosset")


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
# work = data.frame(uniquenames,
#                   finalname = "")
# write.csv(work, "data/harmonized-names.csv", row.names = FALSE)

names_final = read.csv("data/harmonized-names.csv")

# run over names_final and replace names
for (i in seq_along(names_final$uniquenames)) {
  
  original_name = names_final$uniquenames[i]
  
  final_name = names_final$finalname[i]
  
  dat_list = lapply(dat_list, function(x){
    names(x)[names(x) == original_name] = final_name
    x
  })
  
}

dat = rowbind(dat_list)


table(dat$crop)

table(dat$gender)


plot(as.numeric(dat$trial_longitude),
     as.numeric(dat$trial_latitude))

hist(as.numeric(dat$trial_longitude))
summary(as.numeric(dat$trial_longitude))

dat$longitude = as.numeric(dat$trial_longitude)

dat$longitude = ifelse(dat$longitude < 32 |
                         dat$longitude > 48, NA,
                       dat$longitude)

dat$latitude = as.numeric(dat$trial_latitude)

dat$latitude = ifelse(dat$latitude < 3 |
                         dat$latitude > 14, NA,
                       dat$latitude)

plot(as.numeric(dat$longitude),
     as.numeric(dat$latitude))

keep = !grepl("drop", names(dat))

dat = dat[keep]


sort(unique(unlist(dat[,c("variety_a","variety_b","variety_c")])))

varieties = data.frame(crop = rep(dat$crop, 3),
                       variety = unlist(dat[,c("variety_a","variety_b","variety_c")]),
                       variety_harmonized = "")


varieties$variety = tolower(varieties$variety)

varieties$variety = gsub("  ", " ", varieties$variety)

varieties = split(varieties, varieties$crop)

varieties = lapply(varieties, function(x){
  x = x[!duplicated(x$variety), ]
  x = x[order(x$variety), ]
})

varieties = do.call("rbind", varieties)

varieties = na.omit(varieties)

write.csv(varieties, "data/variety-names-tricot-ethiopia.csv", row.names = FALSE)

write.csv(dat, "data/tricot-data-preclean.csv", row.names = FALSE)

