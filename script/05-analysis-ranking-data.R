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
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")
#source("/Users/kauedesousa/Library/Mobile Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/modules/01_functions.R")
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

names(tricot)

sort(table(tricot$crop))

table(tricot$crop, tricot$gender)

tricot = tricot[tricot$crop != "oat", ]

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
  
  ov = which(traitlabels == "Overall Performance")
  
  # make the rankings
  R = lapply(trait_list, function(x){
    rank_tricot(items = pack_index,
                input = x$string,
                data = dat,
                validate.rankings = TRUE)
  })
  
  # ........................................
  # .......................................
  # Kendall tau  ####
  kt = lapply(R, function(x){
    kendallTau(x,
               R[[ov]])
  })
  
  kt = do.call("rbind", kt)
  
  kt
  
  kt$trait = traitlabels
  
  write.csv(kt, 
            paste0(outputdir, "/kendall-tau-cor.csv"),
            row.names = FALSE)
  
  
  keep = kt$`Pr(>|z|)`  < 0.05
  
  #keep = kt$kendallTau > 0.3 & keep
  
  kt = kt[keep, ]
  
  # filter traits
  keep = traitlabels %in% kt$trait
  
  traitlabels = traitlabels[keep]
  
  trait_list = trait_list[keep]
  
  R = R[keep]
  
  # ........................................
  # .......................................
  # Likelihood ratio  ####
  itemnames
  
  traitlabels
  
  # test likelihood ratio
  # this will check whether the ranks are significantly distinct 
  # from each group
  llr = lapply(R, function(x){
    likelihood_ratio(x, split = dat$gender)
  })
  
  llr = do.call("rbind", llr)
  
  llr$trait = traitlabels
  
  write.csv(llr, 
            paste0(outputdir, "/likelihood-ratio-clusters.csv"))
  
  # ........................................
  # .......................................
  # PCA with log-worth  ####
  mod = lapply(R, PlackettLuce)
  
  #n = sum(trait_list[[ov]]$keep)
  
  n = 250
  
  coeffs = lapply(mod, function(x){
    resample(x, bootstrap = TRUE, seed = 1424, n1 = n)
  })
  
  coeffs = do.call(cbind, coeffs)
  
  rmv = which(names(coeffs) == "item")[-1]
  
  coeffs = coeffs[-rmv]
  
  names(coeffs)[-1] = traitlabels
  
  pc = princomp(coeffs[-1], cor = FALSE)
  
  pcplot = plot_pca(pc)
  
  pcplot
  
  ggsave(paste0(outputdir, "/biplot-performance-all-traits.pdf"),
         plot = pcplot,
         height = 21,
         width = 21,
         units = "cm")
  
  # ...........................................
  # ...........................................
  # PCA segmented by gender ####
  # first fit a PL tree to see if gender influences 
  # variety performance
  gender_class = table(dat$gender)
  gender_class
  plots_gender = list()
  worth_plots = list()
  allmodels = list()
  
  for (i in seq_along(gender_class)) {
    
    R_subset = lapply(R, function(x){
      x = x[dat$gender == names(gender_class[i])]
      na.omit(x)
    })
    
    mod = lapply(R_subset, PlackettLuce)
    
    worth_plots[[i]] = worth_map(mod, labels = traitlabels) +
      labs(x = "", y = "", 
           title = paste0("(", LETTERS[i], ") ", 
                          names(gender_class[i]),
                          ", n = ",
                          as.integer(gender_class[i])))
    
    # get probabilities# gettraitlabels probabilities
    coeffs = lapply(mod, function(x){
      resample(x, bootstrap = TRUE, seed = 1424, n1 = 300) 
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
  
  p
  
  ggsave(paste0(outputdir, "/biplot-trait-performance-gender.pdf"),
         plot = p,
         height = 18,
         width = 35,
         units = "cm")
  
  
  p = worth_plots[[1]] + worth_plots[[2]]
  
  ggsave(paste0(outputdir, "/trait-performance-gender.pdf"),
         plot = p,
         height = 15,
         width = 35,
         units = "cm")
  
  # .........................................
  # .........................................
  # PlackettLuce tree ####
  G = rank_tricot(items = pack_index,
                  input = trait_list[[ov]]$string,
                  data = dat,
                  validate.rankings = TRUE,
                  group = TRUE)
  
  pld = data.frame(G, Gender = as.factor(dat$gender))
  
  # head(pld)
  # 
  # tree = pltree(G ~ Gender, data = pld, gamma = TRUE)
  # 
  # tree
  # 
  # ptree = plot(tree, log = TRUE, ci.level = 0.05, ref = "Local")
  # ptree
  # ggsave(filename = "output/pltree-marketability-covars.pdf",
  #        plot = ptree,
  #        units = "cm",
  #        height = 30,
  #        width = 40)
  
  # .........................................
  # .........................................
  # Pairwise comps matrix using worth ####
  models = list()
  plots = list()
  
  for(k in seq_along(gender_class)) {
    
    g = pld[pld$Gender == names(gender_class[k]), "G"]
    
    mod = PlackettLuce(g)
    
    models[[k]] = mod
    
    coefs = coefficients(mod, log = FALSE)
    
    pair_worth = pairwise_probs(coefs)
    
    # plot the results
    lvls = dimnames(pair_worth)[[1]]
    
    pair_dat = data.frame(player1 = rep(lvls, times = length(lvls)), 
                          player2 = rep(lvls, each = length(lvls)),
                          worth = as.vector(pair_worth))
    
    pair_dat
    
    pair_dat$player1 = factor(pair_dat$player1, levels = lvls)
    
    pair_dat$player2 = factor(pair_dat$player2, levels = rev(lvls))
    
    pair_dat$worth = round(pair_dat$worth, 2)
    
    p = ggplot(pair_dat, 
               aes(x = player2, 
                   y = player1,
                   fill = worth,
                   label = worth)) +
      geom_tile(show.legend = FALSE) + 
      geom_text() +
      scale_fill_gradient2(low = "#b2182b", 
                           high = "#2166ac", 
                           na.value = "white") +
      scale_x_discrete(position = "top") +
      theme_bw() +
      theme(axis.text = element_text(color = "grey10"),
            strip.text.x = element_text(color = "grey10"),
            axis.text.x = element_text(angle = 90, hjust = 0),
            panel.grid = element_blank()) +
      labs(x = "", 
           y = "",
           title = paste0("(", LETTERS[i], ") ", 
                          names(gender_class[k]),
                          ", n = ",
                          as.integer(gender_class[k])),
           fill = "")
    
    plots[[k]] = p
    
  }
  
  p = plots[[1]] + plots[[2]] 
  
  p
  
  ggsave(paste0(outputdir, "/pairwise-probabilities-overall.pdf"),
         plot = p,
         width = 35,
         height = 18,
         units = "cm",
         dpi = 600)
  
  # .........................................
  # .........................................
  # Regret ####
  reg = regret(models)
  
  write.csv(reg, 
            paste0(outputdir, "/regret-analysis.csv"), row.names = FALSE)
  

  
  pdf(file = paste0(outputdir, "/trial-network.pdf"),
      width = 9,
      height = 9)
  plot(network(pld$G))
  dev.off()
  
}

