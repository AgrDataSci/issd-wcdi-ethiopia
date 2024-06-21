# ..................................
# This script analyses the correlation in trait preference

# load packages
library("ClimMobTools")
library("gosset")
library("janitor")
library("magrittr")
library("tidyverse")
library("reshape2")
library("heatmap3")
library("corpcor")
library("GeneNet")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

#Define new function to obtain a Kendall tau correlation matrix and effective N matrix
kendallTauCorMatrix <- function(L){
  
  n_i <- length(L)
  
  #Check if the list is longer than 3 and check if it contains rankings
  stopifnot(n_i>2, inherits(L[[1]], "rankings"), inherits(L[[2]], "rankings"))
  
  #Prepare the correlation matrix to be filled with values
  r <- matrix(NA, nrow=n_i, ncol=n_i)
  rownames(r) <- names(L)
  colnames(r) <- names(L)
  
  #Prepare the effective N matrix
  N <- matrix(NA, nrow=n_i, ncol=n_i)
  rownames(N) <- names(L)
  colnames(N) <- names(L)
  
  #Fill the matrix
  for(i in 1:n_i){
    for (j in 1:i){ #Fills lower triangle only, to avoid calculating the same value twice
      
      rNij <- kendallTau(L[[i]], L[[j]]) #TODO deal with NA values
      r[i,j] <- as.numeric(rNij[1])
      N[i,j] <- as.numeric(rNij[2]) 
      
    }
    
  }
  
  #Make matrices symmetric    
  r[upper.tri(r)] <- t(r)[upper.tri(r)]
  N[upper.tri(N)] <- t(N)[upper.tri(N)]
  
  #Wrap result
  results <- list(Kendall_tau_corr=r,Effective_N=N)
  return(results)
  
}

dat = read.csv("data/tricot-data-ethiopia.csv")

dat$gender[dat$gender == "M"] = "Man"
dat$gender[dat$gender == "F"] = "Woman"
dat$gender[dat$gender == ""] = NA

dat = dat[!is.na(dat$gender), ]

# remove empty places in varieties names
pack_index = paste0("variety_", letters[1:3])

keep = apply(dat[pack_index], 1, function(x){
  !sum(is.na(x)) > 1
})

dat = dat[keep, ]

# ...............................................
# ...............................................
# Kendall partial correlation ####
# run over crops to filter the traits 
# used in each crop
crops = unique(dat$crop)

gender = unique(dat$gender)

for(v in seq_along(crops)) {
  
  print(crops[v])
  
  dat_v = dat[dat$crop == crops[v], ]
  
  traits_list = getTraitList(dat_v, c("_best", "_worst"))
  
  # drop traits not used
  keep = lapply(traits_list, function(x){
    sum(x$keep) > 5
  })
  
  keep = unlist(keep)
  
  traits_list = traits_list[keep]
  
  traits = unlist(lapply(traits_list, function(x){
    x$trait_label
  }))
  
  reference = which(traits == "overall_performance")

  ngender = table(dat_v$gender)

  trait_cor = data.frame()
  
  for (g in seq_along(gender)) {
    
    R = lapply(traits_list, function(x){
      rank_tricot(data = dat_v[dat_v$gender == gender[g], ],
                  items = pack_index,
                  input = x$string,
                  validate.rankings = TRUE)
    })
    
    rN = kendallTauCorMatrix(R)
    
    r = rN[[1]]
    
    N = rN[[2]]
    
    # Test with the full matrix, just to see if it works
    pr = cor2pcor(r)
    
    # Simple way to work with these values is to convert to Pearson's r
    r_pearson = sin(3.141592654 * r * .5)
    
    dimnames(r_pearson) = list(traits, traits)
    
    #calculate pearson correlation P-value 
    ttv = list()
    ttest_pvalue_matrix = matrix(NA, nrow = length(traits), ncol = length(traits))
    
    #Fill the matrix
    for(i in seq_along(traits)){
      for (j in seq_along(traits)){ 
        
        keep_tt = r_pearson[j,i] *sqrt((mean(N) - 2)/ (1-(r_pearson[j,i]^2)))
        ttv = keep_tt
        keep_tt_value = 2 * (pt(-abs(ttv), mean(N) - 2))
        ttest_pvalue_matrix[j,i] = keep_tt_value
      }
    }
    
    cor_g = data.frame(trait= traits, 
                       cor = r_pearson[, reference],
                       pval = ttest_pvalue_matrix[, reference],
                       gender = gender[g])
    
    cor_g = cor_g[-reference, ]
    
    trait_cor = rbind(trait_cor, cor_g)
    
  }
  
  write.csv(trait_cor,
            paste0("output/trait-correlation-gender-", crops[v], ".csv"),
            row.names = FALSE)
  
  # get the traits with p < 0.1 in at least one gender
  keep = trait_cor$pval < 0.1
  
  keep = unique(trait_cor$trait[keep])
  
  keep = trait_cor$trait %in% keep
  
  trait_cor = trait_cor[keep, ]
  
  # force order of labels using the first wrap
  ord = order(trait_cor[trait_cor$gender == "Man", "cor"])
  
  lev = trait_cor[trait_cor$gender == "Man", "trait"][ord]
  
  trait_cor$trait = factor(trait_cor$trait, levels = lev)
  
  trait_cor$gender = ifelse(trait_cor$gender == "Man",
                            paste0(trait_cor$gender, " (n = ", ngender[1], ")"),
                            ifelse(trait_cor$gender == "Woman",
                                   paste0(trait_cor$gender, " (n = ", ngender[2], ")"),
                                   trait_cor$gender))
  
  # make a bar plot plot
  cor_plot =
    ggplot(data = trait_cor,
           aes(y = cor,
               x = trait,
               fill = trait)) +
    geom_chicklet(show.legend = FALSE) +
    coord_flip() +
    facet_grid(~ gender) +
    scale_fill_manual(values = rev(col_pallet(length(traits)))) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          strip.background =element_rect(fill="white"),
          text = element_text(color = "grey10"),
          strip.background.x = element_blank(),
          strip.placement = "outside",
          legend.position = "bottom",
          strip.text = element_text(size = 16, color = "grey10"),
          legend.text = element_text(size = 16, color = "grey10"),
          axis.text = element_text(size = 16, color = "grey10"),
          axis.title = element_text(size = 16, color = "grey10"),
          legend.title = element_blank()) +
    labs(x = "",
         y = "Correlation with Overall Preference")
  
  cor_plot
  
  ggsave(paste0("output/trait-correlation-gender-plot-", crops[v], ".pdf"),
         plot = cor_plot,
         height = 8,
         width = 14)
  
  
}


