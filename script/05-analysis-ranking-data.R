# .......................................
# .......................................
# Analyse the data to test these hypothesis 
# 1. Men and women smallholders don’t differ significantly in 
# their preference of varieties and traits;
# 2. Yield as a trait projects strongest on their overall preference for varieties; 
# 3. They appreciate novelty in terms of the age of varieties; and 
# 4. They prefer superiority over diversity in their choice of varieties.
# .......................................
# .......................................
library("ggrepel")
library("tidyverse")
library("patchwork")
library("magrittr")
library("ggfortify")
library("gosset")
library("PlackettLuce")
library("ClimMobTools")
library("readxl")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")
source("script/helper-01-function.R")

# .......................................
# .......................................
# Organize the traits by data collection moment ####
# read the file 
list.files("data")

tricot = read.csv("data/tricot-data.csv")

sum(is.na(tricot$gender))

tricot = tricot[!is.na(tricot$gender), ]

tricot$gender[tricot$gender == "F"] = "Woman"

tricot$gender[tricot$gender == "M"] = "Man"

tricot$gender[tricot$gender == "Woman"] = "Women"

tricot$gender[tricot$gender == "Man"] = "Men"

names(tricot)

# read table with variety data
variety = read_excel("data/variety-names-tricot-ethiopia.xlsx", sheet = 1)

head(variety)

names(variety) = make_clean_names(names(variety))

variety$variety_harmonized = ClimMobTools:::.title_case(tolower(variety$variety_harmonized))

variety$variety_harmonized = gsub("- ", "-", variety$variety_harmonized)

variety$variety_harmonized = gsub(" -", "-", variety$variety_harmonized)

sort(table(tricot$crop))

x = table(tricot$crop, tricot$gender)

write.csv(data.frame(x), "output/gender-dist.csv")

tricot = tricot[tricot$crop != "oat", ]

rmv = grep("perception", names(tricot))

tricot = tricot[,-rmv]

crop = unique(tricot$crop) 

for (f in seq_along(crop)) {
  
  print(crop[f])
  
  outputdir = paste0("output/", crop[f])
  
  dir.create(outputdir, 
             recursive = TRUE,
             showWarnings = FALSE)
  
  dat = tricot[tricot$crop == crop[f], ]
  
  nas = apply(dat, 2, function(x){
    sum(is.na(x))
  })
  
  rmv = nas == nrow(dat)
  
  dat = dat[!rmv]
  
  # variety info
  variety_f = variety[variety$crop == crop[f], ]
  
  variety_f = variety_f[!is.na(variety_f$variety_harmonized), ]
  
  variety_f = variety_f[!duplicated(variety_f$variety_harmonized), ]
  
  # get the list of traits
  trait_list = getTraitList(dat, 
                            pattern = c("_best", "_worst"))
  
  traits = unlist(lapply(trait_list, function(x) x$trait_label))
  
  traitlabels = fix_trait_labels(traits)
  
  traitlabels
  
  trait_list = getTraitList(dat,
                            pattern = c("_best", "_worst"),
                            trait.labels = traitlabels)
  
  pack_index = paste0("variety_", letters[1:3])
  
  itemnames = sort(unique(unlist(dat[, pack_index])))
  
  itemnames
  
  # get the reference trait
  ov = which(traitlabels == "Overall Performance")
 
  # get one variety to be the reference 
  keep = variety_f$variety_harmonized %in% itemnames
  
  variety_f = variety_f[keep, ]
  
  ref = variety_f$variety_harmonized[which.min(variety_f$variety_release_date)[1]]
  
  # make the rankings
  R = lapply(trait_list, function(x){
    rank_tricot(items = pack_index,
                input = x$string,
                data = dat,
                validate.rankings = TRUE)
  })
  
  
  pdf(file = paste0(outputdir, "/trial-network.pdf"),
      width = 9,
      height = 9)
  plot(network(R[[ov]]))
  dev.off()
  
  # ........................................
  # .......................................
  # ANOVA  ####
  # this will be used to test whether the varieties are different 
  # from each other and we will retain the ones with alpha > 0.05
  anov = lapply(R, function(x){
    a = anova(PlackettLuce(x))
    a[2, ]
  })
  
  anov = do.call("rbind", anov)
  
  anov[,1] = traitlabels
  
  write.csv(anov, 
            paste0(outputdir, "/anova.csv"))
  
  # filter the traits with alpha < 0.05
  keep = union(ov, which(anov[, 5]  < 0.05))

  anov = anov[keep, ]

  traitlabels = traitlabels[keep]

  trait_list = trait_list[keep]

  R = R[keep]
  
  # get the reference trait
  ov = which(traitlabels == "Overall Performance")
  
  traitlabels
  
  # ........................................
  # ........................................
  # Calculate PL model  ####
  mod = lapply(R, PlackettLuce)
  
  # plot log-worth coefficients
  worth = worth_map(mod, labels = traitlabels) +
    labs(x = "", y = "") +
    theme(axis.text.y = element_text(vjust = 0.5, hjust = 1))
  
  ggsave(paste0(outputdir, "/log-worth-map.pdf"),
         plot = worth,
         height = 10,
         width = 22,
         units = "cm")
  
  # ...................................
  # ...................................
  # test the first hypothesis ####
  # 1. Men and women smallholders don’t differ significantly in 
  # their preference of varieties and traits;
  # Likelihood ratio will check whether the ranks are significantly distinct 
  # from each gender
  llr = lapply(R, function(x){
    likelihood_ratio(x, split = dat$gender)
  })
  
  llr = do.call("rbind", llr)
  
  llr$trait = traitlabels
  
  # now check if there is difference in rankings for the top 25% of 
  # varieties, this because the difference might not be in the 
  # best varieties but in the worst
  # here we will see if the top varieties are the same for both genders
  # and the percent of varieties in the top that occurr in man and woman group
  gender_class = table(dat$gender)
  
  top = ceiling(length(itemnames) * 0.25)
  
  top_share = lapply(R, function(x){
    
    g = dat$gender == names(gender_class)[[1]]
    
    m1 = PlackettLuce(x[g,])
    
    m2 = PlackettLuce(x[!g,])
  
    c1 = coefficients(m1, log = F)
      
    c2 = coefficients(m2, log = F)
    
    sum(names(rev(sort(c1)))[1:top] %in% names(rev(sort(c2)))[1:top]) / top
      
  })
  
  share_top = data.frame(trait = traitlabels, 
                         share_top = unlist(top_share),
                         n_top = top,
                         crop = crop[f])
  
  
  llr = merge(llr, share_top, by = "trait")
  
  write.csv(llr, 
            paste0(outputdir, "/likelihood-ratio.csv"))
  
  
  # ...................................
  # ...................................
  # test the second hypothesis ####
  # 2. Yield as a trait projects strongest on their overall preference for varieties; 
  # first we test it using Kendall tau 
  k = 10
  n = nrow(R[[1]])
  set.seed(657)
  folds = sample(rep(1:k, times = ceiling(n / k), length.out = n))
  
  kendall = data.frame()
  
  # run over folds
  for (i in seq_len(k)) {
    
    kt = lapply(R[-ov], function(x){
      kendallTau(x[folds != i, ],
                 R[[ov]][folds != i, ])
    })
    
    kt = do.call("rbind", kt)
    
    kt$trait = traitlabels[-ov]
    
    kt$group = "All"
    
    kendall = rbind(kendall, kt)
    
  }
  
  # now run over the groups
  for (i in seq_along(gender_class)) {
    
    R_subset = lapply(R, function(x){
      x = x[dat$gender == names(gender_class[i])]
    })
    
    k = 10
    n = nrow(R_subset[[1]])
    set.seed(657)
    folds = sample(rep(1:k, times = ceiling(n / k), length.out = n))
    
    # run over folds
    for (j in seq_len(k)) {
      
      kt = lapply(R_subset[-ov], function(x){
        kendallTau(x[folds != j, ],
                   R_subset[[ov]][folds != j, ])
      })
      
      kt = do.call("rbind", kt)
      
      kt$trait = traitlabels[-ov]
      
      kt$group = names(gender_class)[i]
      
      kendall = rbind(kendall, kt)
      
    }
    
  }
  
  kendall$crop = crop[f]
  
  kendall_plot = 
    ggplot(kendall, aes(y = trait, x = kendallTau, fill = group)) +
    geom_boxplot() +
    theme_minimal() +
    scale_fill_manual(values = c("grey50","#225ea8", "#e31a1c")) +
    labs(y = "", x = "Kendall correlation") +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          text = element_text(colour = "grey20"))
  
  
  ggsave(paste0(outputdir, "/kendall-correlation.pdf"),
         plot = kendall_plot,
         height = 15,
         width = 19,
         units = "cm")
  
  write.csv(kendall, paste0(outputdir, "/kendall-correlation.csv"))
  
  # ...........................................
  # ...........................................
  # View these trait correlations using PCA segmented by gender 
  # first fit a PL tree to see if gender influences 
  # variety performance
  n_points = 3
  plots_gender = list()
  
  for (i in seq_along(gender_class)) {
    
    # run the model 
    R_subset = lapply(R, function(x){
      x = x[dat$gender == names(gender_class[i])]
      x = na.omit(x)
      x
    })
    
    mod_i = lapply(R_subset, PlackettLuce)
    
    # get probabilities
    coeffs = lapply(mod_i, function(x){
      resample(x, bootstrap = TRUE, seed = 1424, n1 = n_points) 
    })
    
    coeffs = do.call(cbind, coeffs)
    
    rmv = which(names(coeffs) == "item")[-1]
    
    coeffs = coeffs[-rmv]
    
    names(coeffs)[-1] = traitlabels
    
    pc = princomp(coeffs[, -1], cor = TRUE)
    
    pcplot = plot_pca(pc, scale = 6) + 
      labs(title = paste0("(", LETTERS[i], ") ", 
                          names(gender_class[i]),
                          ", n = ",
                          as.integer(gender_class[i])))
    
    plots_gender[[i]] = pcplot
    
  }
  
  p = plots_gender[[1]] + plots_gender[[2]] 
  
  ggsave(paste0(outputdir, "/biplot-trait-performance-gender.pdf"),
         plot = p,
         height = 18,
         width = 35,
         units = "cm")
  
  
  # ...................................
  # ...................................
  # test the third hypothesis ####
  # 3. They appreciate novelty in terms of the age of varieties; 
  
  # now check if there is difference in rankings for the top 25% of 
  # varieties, this because the difference might not be in the 
  # best varieties but in the worst
  # here we will see if the top varieties are the same for both genders
  # and the percent of varieties in the top that occurr in man and woman group
  
  top_share = lapply(R, function(x){
    
    g = dat$gender == "Man"
    
    m1 = PlackettLuce(x[g,])
    
    m2 = PlackettLuce(x[!g,])
    
    c1 = coefficients(m1, log = F)
    
    c2 = coefficients(m2, log = F)
    
    sum(names(rev(sort(c1)))[1:top] %in% names(rev(sort(c2)))[1:top]) / top
    
  })
  
  share_top = data.frame(trait = traitlabels, 
                         share_top = unlist(top_share),
                         n_top = top,
                         crop = crop[f])
  
  
  
  

  
}



