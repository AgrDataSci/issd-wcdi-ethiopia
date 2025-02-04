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

tricot = tricot[tricot$crop != "wheat", ]

tricot$crop = ifelse(tricot$crop == "haricotbean",
                     "commonbean",
                     tricot$crop)

rmv = grep("perception", names(tricot))

tricot = tricot[,-rmv]

crop = unique(tricot$crop) 

age_crops = data.frame()

kendall_crops = data.frame()

ll_crops = data.frame()

same_variety = data.frame()

tricot = tricot[, -grep("nutritional_quality|flour_and_baking", names(tricot))]


# run over the crops and perform the analysis for each of them 
# but also retain the results to produce charts with all crops together

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
  
  variety_f$variety = variety_f$variety_harmonized
  
  variety_f$release_year = variety_f$variety_release_date
  
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
  
  if (length(traitlabels) <= 1) {
    next
  }
  
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
  # and the percent of varieties in the top that occur in man and woman group
  gender_class = table(dat$gender)
  
  top = ceiling(length(itemnames) * 0.25)
  
  top_share = lapply(R, function(x){
    
    g = dat$gender == names(gender_class)[[1]]
    
    m1 = PlackettLuce(x[g,])
    
    m2 = PlackettLuce(x[!g,])
    
    c1 = coefficients(m1, log = F)
      
    c2 = coefficients(m2, log = F)
    
    a = sum(names(rev(sort(c1)))[1:top] %in% names(rev(sort(c2)))[1:top]) / top
    
    # run n permutations to test whether agreement 
    # is random 
    perms = c()
    
    nperm = 250
    
    for(pe in seq_len(nperm)) {
     
      m3 = PlackettLuce(x[sample(1:nrow(x), sum(!g)), ])
      
      c3 = coefficients(m3, log = F)
      
      perms[pe]  = sum(names(rev(sort(c1)))[1:top] %in% names(rev(sort(c3)))[1:top]) / top
        
    }
    
    nabove = sum(abs(perms) >= abs(a))
    
    pval = (nabove + 1) / (nperm + 1) 
    
    data.frame(share_top = a, 
               mean_random = mean(perms),
               sd_random = sd(perms),
               pval = pval)
        
  })
  
  top_share = do.call("rbind", top_share)
  
  share_top = data.frame(crop = crop[f], 
                         trait = traitlabels, 
                         n_top = top)
  
  share_top = cbind(share_top, top_share)
  
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
    ggplot(kendall, aes(y = trait, x = kendallTau, 
                        fill = group, color = group)) +
    geom_boxplot(alpha = 0.2) +
    theme_minimal() +
    scale_fill_manual(values = c("grey50","#225ea8", "#e31a1c")) +
    scale_color_manual(values = c("grey50","#225ea8", "#e31a1c")) +
    labs(y = "", x = "Kendall correlation") +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          text = element_text(colour = "grey20"))
  
  kendall_plot
  
  ggsave(paste0(outputdir, "/kendall-correlation.pdf"),
         plot = kendall_plot,
         height = 15,
         width = 19,
         units = "cm")
  
  write.csv(kendall, paste0(outputdir, "/kendall-correlation.csv"))
  
  # run anovas over the traits
  kendall_anovas = data.frame()
  
  for(z in seq_along(traitlabels)) {
    
    if (traitlabels[z] == traitlabels[ov]) next
    
    x = summary(aov(glm(kendallTau ~ group, 
                        data = kendall[kendall$trait == traitlabels[z], ])))
    
    x = as.data.frame(x[[1]])[1,]
    
    x$trait = traitlabels[z]
    
    kendall_anovas = rbind(kendall_anovas, x)
    
  }
  
  kendall_anovas$stars = gosset:::.stars_pval(kendall_anovas$`Pr(>F)`)
  
  write.csv(kendall_anovas, paste0(outputdir, "/kendall-anovas-gender.csv"))
  
  # ...........................................
  # ...........................................
  # View these trait correlations using PCA segmented by gender 
  # first fit a PL tree to see if gender influences 
  # variety performance
  n_points = 5
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
  
  p = plots_gender[[1]] + plots_gender[[2]] + plot_layout(ncol = 2) 
  
  p
  
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
  g = dat$gender == names(gender_class)[[1]]
  
  dat$year[is.na(dat$year)] = min(dat$year, na.rm = TRUE)
  
  y = unique(dat$year)
  
  # run over years of trials
  age_top = data.frame()
  
  m_age = median(y[1] - variety_f$release_year, na.rm = T)
  
  for(i in seq_along(y)) {
    
    m1 = PlackettLuce(R[[ov]][g & dat$year == y[i], ])
    
    m2 = PlackettLuce(R[[ov]][!g & dat$year == y[i], ])
    
    c1 = rev(sort(coefficients(m1, log = F)))
    
    c2 = rev(sort(coefficients(m2, log = F)))
    
    age = data.frame(variety = c(names(c1[1:top]),
                                 names(c2[1:top])),
                     group = rep(names(gender_class), each = top),
                     crop = crop[f])
    
    age = merge(age,
                variety_f[,c("variety", "release_year")], 
                by = "variety", 
                all.x = TRUE)
    
    age$age = y[i] - age$release_year
    
    age_top = rbind(age_top, age)
    
  }
  
 
  age_top$age[age_top$age < 0] = 0
  
  age_top$diff = age_top$age - m_age
  
  age_plot = 
    ggplot(age_top, aes(x = group, y = diff)) +
    geom_boxplot() +
    theme_minimal() +
    labs(x = "", y = "Difference in variety age (median - age)")
  
  age_plot
  
  age_top$pval_ttest = summary(aov(lm(diff ~ group, data = age_top)))[[1]][1, 5]
  
  
  ggsave(paste0(outputdir, "/age-preferred-varieties.pdf"),
         plot = age_plot,
         height = 12,
         width = 10,
         units = "cm")
  
  # ......................................
  # ......................................
  # Test hypothesis 4 ####
  # 4. They prefer superiority over diversity in their choice of varieties.
  stability = data.frame(rank = rep(1:top, times = 3),
                         group = rep(c("All", names(gender_class)), each = top),
                         crop = crop[f])
  
  for(i in seq_along(y)) {
    
    m1 = PlackettLuce(R[[ov]][dat$year == y[i], ])
    
    m2 = PlackettLuce(R[[ov]][g & dat$year == y[i], ])
    
    m3 = PlackettLuce(R[[ov]][!g & dat$year == y[i], ])
    
    c1 = rev(sort(coefficients(m1, log = F)))
    
    c2 = rev(sort(coefficients(m2, log = F)))
    
    c3 = rev(sort(coefficients(m3, log = F)))
    
    s = data.frame(variety = c(names(c1[1:top]),
                               names(c2[1:top]),
                               names(c3[1:top])))
    
    stability = cbind(stability, s)
    
    names(stability)[names(stability) == "variety"] = paste0("year", i)
    
  }
  
  stability = split(stability, stability$group)
  
  same = lapply(stability, function(x){
   
    v = unlist(x["year1"])
    
    s = c()
    
    for(i in 2:length(y)) {
      s[i] = sum(unlist(x[paste0("year", i)]) %in% v) / top
    }
    
    s
    
  })
  
  same = data.frame(same_variety = unlist(same), 
                            group = rep(c("All", names(gender_class)), each = length(y)),
                            year = rep(1:length(y), times = length(gender_class) + 1),
                            crop = crop[f])
             
  
  same = na.omit(same)
  
  write.csv(same, paste0(outputdir, "/top-varieties-over-seasons.csv"))
  
  # add results to a large data.frame
  kendall_crops = rbind(kendall_crops, kendall)
  
  ll_crops = rbind(ll_crops, llr)
  
  age_crops = rbind(age_crops, age_top)
  
  same_variety = rbind(same_variety, same)
  
}


save(kendall_crops, ll_crops, age_crops, same_variety, file = "output/results.rda")

write.csv(kendall_crops, "output/kendall-correlation.csv", row.names = FALSE)

write.csv(ll_crops, "output/share-top-varieties.csv", row.names = FALSE)

write.csv(age_crops, "output/age-varieties.csv", row.names = FALSE)

write.csv(same_variety, "output/same-variety-over-the-years.csv", row.names = FALSE)

head(ll_crops)

shapes = c(0:6,15:17, 8, 13, 10)
set.seed(123)
shapes = sample(shapes)

ll_crops = read.csv("output/share-top-varieties.csv")

length(unique(ll_crops$crop))

table(ll_crops$trait, ll_crops$crop)

sel_traits = 
  ll_crops %>% 
  group_by(trait) %>% 
  summarise(n = length(crop)) %>% 
  filter(n > 3) 


keep = ll_crops$trait %in% sel_traits$trait

ll_crops = ll_crops[keep, ]

ll_crops$trait = factor(ll_crops$trait, levels = rev(union(c("Overall Performance", "Yield",
                                                             "Biomass Yield", "Taste", 
                                                             "Market Value"),
                                                       sel_traits$trait)))

set.seed(750)
z = runif(length(ll_crops$share_top), -0.04, 0.07)
ll_crops$share_top = ll_crops$share_top + z

ll_crops$share_top[ll_crops$share_top > 1] = 0.99

ggplot(ll_crops, aes(y = trait, 
                     x = share_top,
                     shape = crop)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Variety overlap in the 4th quartile",
       y = "") +
  scale_shape_manual(values = shapes) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(color = "grey5", size = 13))


ggsave("output/varieties-preferred-men-women.pdf",
       plot = last_plot(),
       height = 18,
       width = 18,
       units = "cm")

ggsave("output/varieties-preferred-men-women.png",
       plot = last_plot(),
       dpi = 500,
       height = 18,
       width = 18,
       units = "cm")

kendall_crops = read.csv("output/kendall-correlation.csv")

head(kendall_crops)

keep = kendall_crops$trait %in% sel_traits$trait

kendall_crops = kendall_crops[keep, ]


kendall_crops$trait = factor(kendall_crops$trait, levels = rev(union(c("Overall Performance", "Yield",
                                                             "Biomass Yield", "Taste", 
                                                             "Market Value"),
                                                           sel_traits$trait)))

kendall_crops$kendallTau[kendall_crops$kendallTau < 0] = NA

kendall_crops %>%
  group_by(trait, crop, group) %>%
  summarise(mean = mean(kendallTau)) %>%
  ungroup() %>%
  ggplot(aes(y = trait, x = mean,
                          shape = crop)) +
  geom_point() +
  facet_grid(~ group) +
  theme_minimal() +
  scale_shape_manual(values = shapes) +
  labs(x = "Kendall correlation",
       y = "") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(color = "grey10"))

ggsave("output/kendall-correlation.pdf",
       plot = last_plot(),
       height = 18,
       width = 30,
       units = "cm")

ggsave("output/kendall-correlation.png",
       plot = last_plot(),
       height = 18,
       width = 30,
       units = "cm")

head(age_crops)

ggplot(age_crops, aes(y = crop, x = diff, color = group)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "", y = "Difference in variety age (median - age)") +
  scale_color_manual(values = c("#225ea8", "#e31a1c")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(color = "grey10"))


ggsave("output/variety-age.pdf",
       plot = last_plot(),
       height = 15,
       width = 18,
       units = "cm")




