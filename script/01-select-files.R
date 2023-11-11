# remove files from raw data

list.files("data")

projects = read.csv("data/issd-projects-climmob-v2.csv")

project_names = projects$Project

project_names = paste0("data/raw/",
                       rep(c("ET-package_", "ETH-"), times = length(project_names)), 
                       rep(project_names, each = 2), 
                       ".json")

project_names

files = list.files("data/raw", full.names = TRUE)

rmv = !files %in% project_names

rmv

files = files[rmv]

files

file.remove(files)


