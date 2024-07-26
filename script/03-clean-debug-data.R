# ...............................
# ...............................
# this script will put the data together combining variable ids 
# and will standardize the list of varieties
library("jsonlite")
library("janitor")
library("gosset")
library("readxl")
library("ClimMobTools")
library("tidyverse")
source("script/helper-01-function.R")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

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

# read file with planting dates 
list.files("data")

pdates = read.csv("data/planting-dates-fixed-by-ethiopian-team.csv")

names(pdates)

head(pdates)

names(pdates) = make_clean_names(names(pdates))

names(pdates)

pdates$avg = as_date(pdates$new_edited_date, format = "%d/%m/%Y")
pdates$min = as_date(pdates$planting_date_min_edited, format = "%d/%m/%Y")
pdates$max = as_date(pdates$planting_date_max_edited, format = "%d/%m/%Y")

x = rep(pdates$trial, 3)
y = c(pdates$avg, pdates$max, pdates$min)

boxplot(y ~ x)

# ......................................
# ......................................
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
# sort(unique(dat$planting_date))
# 
# sum(is.na(dat$planting_date))
# 
# boxplot(as.Date(dat$planting_date) ~ dat$trial)
# 
# boxplot(as.Date(dat$planting_date) ~ dat$year)
# 
# dat$year = as.numeric(gsub("([0-9]+).*$", "\\1", dat$trial))
# 
# date = as.Date(dat$planting_date)
# 
# date = ifelse(date > "2020-01-01", NA, date)
# 
# boxplot(date ~ dat$trial)
# 
# quantile(as.integer(as.Date(dat$planting_date)), na.rm = T)
# 
# pdates = 
#   dat %>% 
#   group_by(trial) %>% 
#   summarise(crop = unique(crop), 
#             planting_date_avg = mean(as.Date(planting_date), na.rm = TRUE),
#             planting_date_min = min(as.Date(planting_date), na.rm = TRUE),
#             planting_date_max = max(as.Date(planting_date), na.rm = TRUE))
# 
# write.csv(pdates, "data/planting-dates.csv", row.names = TRUE)

# we will use the planting windows provided by the 
# Ethiopian team as most of the dates are not provided or 
# not recorded well due to differences with the Ethiopian calendar
trial = pdates$trial

for (i in seq_along(trial)) {
  
  s = sd(c(pdates$avg[i], pdates$min[i], pdates$max[i]))
  n = sum(dat$trial == trial[i])
  
  d = as.integer(rnorm(n, sd = s))
  
  dates = pdates$avg[i] + d
  
  dat$planting_date[dat$trial == trial[i]] = dates
  
}

dat$planting_date = as_date(as.integer(dat$planting_date))

dat$year = year(dat$planting_date)

boxplot(as.Date(dat$planting_date) ~ dat$trial)

# ..............................
# ..............................
# check GPS data #####
# remove points with no decimals
dat$longitude[is.na(dat$longitude)] = 0
keep = unlist(lapply(dat$longitude, decimalplaces)) > 0

dat$latitude[is.na(dat$latitude)] = 0

keep = unlist(lapply(dat$latitude, decimalplaces)) > 0 & keep

table(keep)

plot(dat$longitude, dat$latitude)

dat$longitude[!keep] = NA
dat$latitude[!keep] = NA

dat$latitude = ifelse(dat$latitude > 11.5 & dat$latitude < 12 &
                        dat$longitude > 39, NA, dat$latitude)
dat$longitude[dat$longitude > 43] = NA
dat$latitude[is.na(dat$longitude)] = NA

plot(dat$longitude, dat$latitude)

plot_map(dat, xy = c("longitude", "latitude"))

xy = 
  dat %>% 
  group_by(district, village) %>% 
  summarise(district = unique(district),
            village = unique(village),
            district_harmonized = "",
            village_harmonized = "",
            longitute = mean(longitude, na.rm = TRUE),
            latitude = mean(latitude, na.rm = TRUE))

# write.csv(xy, "data/coordinates.csv", row.names = TRUE)

#write.csv(dat, "data/tricot-data.csv", row.names = FALSE)

sum(is.na(dat$longitude))

names(dat)

sel = union(c("trial", "year", "crop", "package_id", "longitude", "latitude", "planting_date",
        "district", "village", "age", "gender"),
      names(dat))


dat = dat[sel]


sum(is.na(dat$planting_date))


sum(is.na(dat$longitude))


dat$district = tolower(dat$district)

dat$village = tolower(dat$village)

dat$village = str_replace_all(dat$village, "[[:punct:]]", " ")

dat$district = str_replace_all(dat$district, "[[:punct:]]", " ")

dat$location = paste(dat$district, dat$village)

locations = unique(paste(dat$district, dat$village))

for (i in seq_along(locations)) {
  
  dat$longitude = ifelse(is.na(dat$longitude) & dat$location == locations[i],
                         mean(dat$longitude[dat$location == locations[i]], na.rm = TRUE),
                         dat$longitude)
  
  dat$latitude = ifelse(is.na(dat$latitude) & dat$location == locations[i],
                         mean(dat$latitude[dat$location == locations[i]], na.rm = TRUE),
                         dat$latitude)
  
}


sum(is.na(dat$longitude))

# do it by trial
for (i in seq_along(trial)) {
  
  dat$longitude = ifelse(is.na(dat$longitude) & dat$trial == trial[i],
                         mean(dat$longitude[dat$trial == trial[i]], na.rm = TRUE),
                         dat$longitude)
  
  dat$latitude = ifelse(is.na(dat$latitude) & dat$trial == trial[i],
                        mean(dat$latitude[dat$trial == trial[i]], na.rm = TRUE),
                        dat$latitude)
  
}

sum(is.na(dat$longitude))


plot_map(dat, xy = c("longitude", "latitude"))


sum(is.na(dat$year))

table(is.na(dat$year), dat$crop)

dat = dat[order(dat$crop),]

write.csv(dat, "data/tricot-data.csv")


