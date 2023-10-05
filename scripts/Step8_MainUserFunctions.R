
# ------------ Replication Data for Religious Belief Alignment from Adolescence to Emerging Adulthood ------------ #
# ------------------------------------ Turgut Keskinturk - Bogazici University - 2021 ---------------------------- #

# ---------------------------------------------------------------------------------------------------------------- #

# Step 7: Main User Functions

# this R file contains user-written functions for the main analyses. I am immensely indebted to Daniel DellaPosta,
# whose replication files (see below) provide significant improvements for the initial code:

# "Pluralistic Collapse: The 'Oil Spill' Model of Mass Opinion Polarization" American Sociological Review.

# ---------------------------------------------------------------------------------------------------------------- #

# function 1: gather correlations from the observed samples

compute_correlations_all = function(dinput) {
  
  pairs_g = vector()
  for(a in ages) {
    d1 <- subset(dinput, agecats==a) # reduce for each age
    if(!(empty(d1))) {
      for(y in 1:ncol(d1)) {
        for(x in 1:ncol(d1)) {
          if(y>x) {
            n = pairwiseCount(d1[,y], d1[,x])
            if(n>thresh) {
              var1 <- colnames(d1)[x]
              var2 <- colnames(d1)[y]
              pairs_g <- append(pairs_g, paste(var1, var2, a, n, sep=","))
            }
          }
        }
      }
    }
  }
  
  compute_correlations_1 = function(i) {
    var1 <- str_split(pairs_g[i], ",")[[1]][1] 
    var2 <- str_split(pairs_g[i], ",")[[1]][2]
    ages <- str_split(pairs_g[i], ",")[[1]][3]
    d1 <- subset(dinput, agecats==ages) # reduce for each age 
    d1 <- remove_empty(d1, which=c("cols"))
    
    # get zero-order correlations
    c = cor.test(d1[,names(d1)==var1], d1[,names(d1)==var2],  
                 use = "pairwise.complete.obs", method="pearson")$estimate
    p = cor.test(d1[,names(d1)==var1], d1[,names(d1)==var2],  
                 use = "pairwise.complete.obs", method="pearson")$p.value
    corr = paste(var1, var2, ages, c, p, sep=",")
    return(corr)
  }
  
  result <- lapply(1:length(pairs_g), compute_correlations_1)
  x = vector()
  y = vector()
  age = vector()
  c = vector()
  p = vector()
  
  for(j in 1:length(result)) {
    r = str_split(result[[j]], pattern=",")[[1]]
    x = append(x, r[1])
    y = append(y, r[2])
    age = append(age, r[3])
    c = append(c, r[4])
    p = append(p, r[5])
  }
  
  x = as.character(x)
  y = as.character(y)
  age = as.numeric(age)
  c = as.numeric(c)
  p = as.numeric(p)
  r = data.frame(x, y, age, c, p)
  r$j = paste(r$y, "_", r$x, sep="") # group variables
  r$abs_c=abs(r$c) # absolute correlations
  
  modeldata <- r %>%
    subset(x != "agecats") %>%
    subset(y != "agecats")
  return(modeldata)
}

# function 2: bootstrapping samples

compute_correlations_bs1 = function(dinput) {
  
  sub = subset(df_all, df_all$agecats==dinput)
  sub = remove_empty(sub, which=c("cols"))
  b = bootstrap(sub, 1000)
  b_corrs = list()
  
  for(j in 1:1000) {
    bs = as.data.frame(b$strap[[j]])
    b_corrs[[j]] = modeldata_all[modeldata_all$age==dinput, c("x", "y", "age", "j")]
    b_corrs[[j]]$abs_c = NA
    
    for(c in 1:nrow(b_corrs[[j]])) {
      var1 = as.character(b_corrs[[j]]$x[c])
      var2 = as.character(b_corrs[[j]]$y[c])
      b_corrs[[j]]$abs_c[c] = abs(cor.test(bs[,names(bs)==var1], 
                                           bs[,names(bs)==var2],  
                                           use = "pairwise.complete.obs", 
                                           method="pearson")$estimate)
    }
  }
  save(b_corrs, file=paste("./data/bootstrapped_corrs_", dinput, ".saved", sep=""))
  return(b_corrs)
}

# function 3: gather correlations from bootstrapped samples

compute_correlations_bs2 = function(dinput) {
  for(a in ages) {
    load(file = paste("./data/bootstrapped_corrs_", a, ".saved", sep=""))
    b_corrs[[dinput]]$age = a
    modeldata_bdf = rbind(modeldata_bdf, b_corrs[[dinput]])
  }
  return(modeldata_bdf)
}

# function 4: return network graph objects with correlations higher than 10%

return_graph_object <- function(dinput) {
  group <- modeldata_all %>%
    group_by(age) %>%
    subset(age == dinput) %>%
    filter(abs_c >= 0.10) %>%
    filter(x != "agecats" & y != "agecats")
  graph <- graph_from_data_frame(data.frame(w1 = group$x, 
                                            w2 = group$y, 
                                            weight = group$abs_c), 
                                 directed = FALSE)
  return(graph)
}

# function 5: simulating models with fewer items

run_simulate_sample <- function(dinput, times) {
  newsample <- sample(varlist, times)
  modeldnew <- dinput %>%
    mutate(fi = x %in% newsample & y %in% newsample) %>%
    subset(fi == TRUE)
  fitted_model <- lmer(abs_c ~ age_r + I(age_r^2) + (1 + age_r | j), 
                       data = modeldnew, 
                       control = lmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb")))
  coef1 = summary(fitted_model)$coefficients[2,1]
  coef2 = summary(fitted_model)$coefficients[3,1]
  tval1 = summary(fitted_model)$coefficients[2,3]
  tval2 = summary(fitted_model)$coefficients[3,3]
  fitted_coefficients <- data.frame(coef1, coef2, tval1, tval2)
  return(fitted_coefficients)
}
