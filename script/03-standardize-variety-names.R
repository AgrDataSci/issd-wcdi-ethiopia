# ...............................
# ...............................
# this script will put the data together combining variable ids 
# and will standardize the list of varieties
library("jsonlite")
library("janitor")
library("gosset")
library("readxl")
library("ClimMobTools")
source("script/helper-01-function.R")

dat = read.csv("data/tricot-data-preclean.csv")

table(dat$crop)

variety = read_excel("data/variety-names-tricot-ethiopia.xlsx", sheet = 1)

head(variety)

names(variety) = make_clean_names(names(variety))

head(variety)

variety = variety[!is.na(variety$variety_harmonized), ]

unique(dat$crop)

unique(variety$crop) %in% unique(dat$crop)

variety$variety_harmonized = ClimMobTools:::.title_case(tolower(variety$variety_harmonized))

variety$variety_harmonized = gsub("- ", "-", variety$variety_harmonized)

variety$variety_harmonized = gsub(" -", "-", variety$variety_harmonized)

# run over the list of varieties and crops 
# and replace names with correct nomenclature
names(dat)

pack_index = paste0("variety_", letters[1:3])

dat[pack_index] = lapply(dat[pack_index], tolower)

variety$variety %in% unlist(dat[pack_index])

for(i in seq_along(variety$variety)) {
  
  dat$variety_a = ifelse(dat$variety_a == variety$variety[i] & 
                           dat$crop == variety$crop[i],
                         variety$variety_harmonized[i],
                         dat$variety_a)
  
  dat$variety_b = ifelse(dat$variety_b == variety$variety[i] & 
                           dat$crop == variety$crop[i],
                         variety$variety_harmonized[i],
                         dat$variety_b)
  
  dat$variety_c = ifelse(dat$variety_c == variety$variety[i] & 
                           dat$crop == variety$crop[i],
                         variety$variety_harmonized[i],
                         dat$variety_c)
  
  
  dat$crop = ifelse(dat$variety_a == variety$variety_harmonized[i] &
                      dat$crop == variety$crop[i],
                    variety$crop_harmonized[i],
                    dat$crop)
  
  
}

# check how many entries per crop are not in the list
outlist = unlist(dat[pack_index]) %in% unique(variety$variety)

var = unlist(dat[pack_index])[outlist]

crop = rep(dat$crop, 3)[outlist]

table(var, crop)

# remove entries that don't have an existing cleared variety
dat[pack_index] = lapply(dat[pack_index], function(x){
  x[!x %in% variety$variety_harmonized] = NA
  x
})

keep = apply(dat[pack_index], 1, function(x){
  sum(is.na(x))
}) 

keep = keep < 2  

dat = dat[keep, ]

sort(table(dat$crop))

# check overall performance data
sel = union(names(dat)[1:8], 
            names(dat)[grepl("overall_", names(dat))])
sel = union(sel, c("longitude", "latitude"))
sel = union(sel, names(dat))

dat = dat[sel]

dat[dat == ""] = NA

dat[dat == "undefined"] = NA

dat$overall_performance_best = ifelse(is.na(dat$overall_performance_best), 
                                      dat$overall_performance_best_1,
                                      dat$overall_performance_best)

dat$overall_performance_worst = ifelse(is.na(dat$overall_performance_worst),
                                       dat$overall_performance_worst_1,
                                       dat$overall_performance_worst)


rmv = grep("worst_1|best_1", names(dat))

dat = dat[-rmv]

names(dat)

# ..............................
# ..............................
# check planting dates #####
unique(dat$planting_date)



# ..............................
# ..............................
# check GPS data #####
# remove points with no decimals
dat$longitude[is.na(dat$longitude)] = 0
keep = unlist(lapply(dat$longitude, decimalplaces)) > 0

dat$latitude[is.na(dat$latitude)] = 0

keep = unlist(lapply(dat$latitude, decimalplaces)) > 0 & keep

table(keep)

dat$longitude[!keep] = NA
dat$latitude[!keep] = NA

#dat$latitude[dat$latitude == 12.41176562] = NA
#dat$longitude[is.na(dat$latitude)] = NA

plot(dat$longitude, dat$latitude)

#write.csv(dat, "data/tricot-data.csv", row.names = FALSE)



