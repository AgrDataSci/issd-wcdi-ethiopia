

dat = read.csv('metadata/metadata-tricot-data.csv')

index = grep("_best|_worst", dat$Name)

traits = ClimMobTools:::.title_case(gsub("_|_best|_worst", " ", dat$Name[index]))

quest = paste0("which variety was XXX preferred among the three tested for ", traits)

best = grep("_best", dat$Name[index])

quest[best] = gsub("XXX", "the most", quest[best])

quest = gsub("XXX", "the least", quest)

dat[index, "Description"] = quest

dat[index, "ValueType"] = "vocabulary encoding scheme"

dat[index, "ControlledVocabulary"] = "A|B|C"

write.csv('metadata/metadata-tricot-data.csv')