
# ------------ Replication Data for Religious Belief Alignment from Adolescence to Emerging Adulthood ------------ #
# ------------------------------------ Turgut Keskinturk - Bogazici University - 2021 ---------------------------- #

# Step 3: Robustness Tests 1 - Sampling

# -------------------------------------------------------- PART 1 ------------------------------------------------ #

modeldata_bdf <- data.frame()

# parallel computing for bootstrapping

library(parallel) # for parallel computing in Windows
copies_of_r_cluster <- makeCluster(8) # number of clusters allocated to the apply functions

clusterExport(copies_of_r_cluster, 
              c("df_all", "modeldata_all", "ages", "thresh", "modeldata_bdf", 
                "compute_correlations_bs1", "compute_correlations_bs2"))
clusterEvalQ(copies_of_r_cluster, 
              c("df_all", "modeldata_all", "ages", "thresh", "modeldata_bdf", 
                "compute_correlations_bs1", "compute_correlations_bs2"))
clusterEvalQ(copies_of_r_cluster, {
  library(psych)
  library(plyr)
  library(janitor)
  library(stringr)
  library(ggpubr)
  library(sjstats)
})

# you can turn off the parallel computing and use only "lapply", but even with parallel computing it takes a while.

modeldata_bls <- parLapply(copies_of_r_cluster, ages, 
                           fun = compute_correlations_bs1) # bootstrap 1,000 files for each age
save(modeldata_bls, file="./data/modeldata_bootstrapped1.saved")
modeldata_bdf <- parLapply(copies_of_r_cluster, 1:1000, 
                           fun = compute_correlations_bs2) # create data-frames for bootstrapped files
save(modeldata_bdf, file="./data/modeldata_bootstrapped2.saved")
stopCluster(copies_of_r_cluster)

# -------------------------------------------------------- PART 2 ------------------------------------------------ #

# bootstraps & changes in the structure of correlations: constraint and centralization

nt_cor <- matrix(NA, nrow = length(ages), ncol = 1001) # empty matrix
nt_cor[,1] = sort(ages)
nt_cen <- matrix(NA, nrow = length(ages), ncol = 1001) # empty matrix
nt_cen[,1] = sort(ages)

for (i in 1:1000) {
  print(i)
  e = modeldata_bdf[[i]]
  e$x = modeldata_all$x[match(e$j, modeldata_all$j)]
  e$y = modeldata_all$y[match(e$j, modeldata_all$j)]
  e$weight = e$abs_c
  e$weight[e$weight<0] = 0
  e$weight[e$weight>1] = 1
  
  matrix_initial <- 1
  for (a in ages) { # this creates a network for each age group and calculates raw correlations and eigenvector centralization
    sub = e[e$age == a,]
    g = graph_from_data_frame(sub[, c("x", "y", "weight")], directed = FALSE)
    nt_cor[matrix_initial, i+1] = 2*sum((E(g)$weight)) / ((vcount(g)*(vcount(g)-1)))
    nt_cen[matrix_initial, i+1] = centralize(eigen_centrality(g, weights = E(g)$weigth)$vector, 
                                             theoretical.max = 20, normalized = TRUE)
    matrix_initial = matrix_initial + 1
  }
}
rm(matrix_initial, e, sub, i, g, a) # clean-up

# -------------------------------------------------------- PART 3 ------------------------------------------------ #

detach(package:plyr) # for dplyr to work properly

nt_cen_grouped <- data.frame(ages)
nt_cen_grouped$mean <- NA
nt_cor_grouped <- data.frame(ages)
nt_cor_grouped$mean <- NA
for (a in 1:length(ages)) {
  nt_cen_grouped$mean[a] <- mean(nt_cen[a, 2:1001], na.rm = TRUE)
  nt_cor_grouped$mean[a] <- mean(nt_cor[a, 2:1001], na.rm = TRUE)
  nt_cen_grouped$sd[a] <- sd(nt_cen[a, 2:1001], na.rm = TRUE)
  nt_cor_grouped$sd[a] <- sd(nt_cor[a, 2:1001], na.rm = TRUE)
}
rm(a) # clean-up again!

# visualizing centralization and constraint

viz_central <- ggplot(nt_cen_grouped, aes(x = ages, y = mean)) +
  geom_smooth(se = F, col = "black", size = 1, method = "lm", 
              formula = y ~ splines::bs(x, 3)) +
  geom_smooth(aes(x = ages, y = mean + 1.96*sd),
              se = F, col = "gray50", size = 0.5, method = "lm",
              formula = y ~ splines::bs(x, 3), linetype = "dashed") +
  geom_smooth(aes(x = ages, y = mean - 1.96*sd),
              se = F, col = "gray50", size = 0.5, method = "lm",
              formula = y ~ splines::bs(x, 3), linetype = "dashed") +
  ggtitle("Centralization") +
  xlab("Age") +
  ylab(element_blank()) +
  ylim(0.15, 0.45) +
  theme_minimal() +
  theme(text=element_text(family = "Arial Narrow")) +
  scale_x_continuous(breaks = round(seq(min(modeldata_all$age), max(modeldata_all$age), by = 1),1))

viz_correlations <- ggplot(nt_cor_grouped, aes(x = ages, y = mean)) +
  geom_smooth(se = F, col = "black", size = 1, method = "lm", 
              formula = y ~ splines::bs(x, 3)) +
  geom_smooth(aes(x = ages, y = mean + 1.96*sd),
              se = F, col = "gray50", size = 0.5, method = "lm",
              formula = y ~ splines::bs(x, 3), linetype = "dashed") +
  geom_smooth(aes(x = ages, y = mean - 1.96*sd),
              se = F, col = "gray50", size = 0.5, method = "lm",
              formula = y ~ splines::bs(x, 3), linetype = "dashed") +
  ggtitle("Constraint") +
  xlab("Age") +
  ylab(element_blank()) +
  ylim(0.15, 0.45) +
  theme_minimal() +
  theme(text=element_text(family = "Arial Narrow")) +
  scale_x_continuous(breaks = round(seq(min(modeldata_all$age), max(modeldata_all$age), by = 1),1))

png("./figures/FIG04.png", units="in", width=10, height=5, res=500)
grid.arrange(viz_correlations, viz_central, nrow = 1)
dev.off()

# linear regressions for collapsed data frames

nt_cen_grouped <- nt_cen_grouped %>%
  mutate(ages_r = ages - 13)
nt_cor_grouped <- nt_cor_grouped %>%
  mutate(ages_r = ages - 13)
summary(lm(mean ~ ages_r + I(ages_r^2), nt_cen_grouped))
summary(lm(mean ~ ages_r + I(ages_r^2), nt_cor_grouped))

rm(copies_of_r_cluster, modeldata_bdf, modeldata_bls, 
   nt_cen, nt_cen_grouped, nt_cor, nt_cor_grouped, viz_central, viz_correlations) # clean-up
  
# ---------------------------------------------------------------------------------------------------------------- #
