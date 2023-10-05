
# ------------ Replication Data for Religious Belief Alignment from Adolescence to Emerging Adulthood ------------ #
# ------------------------------------ Turgut Keskinturk - Bogazici University - 2021 ---------------------------- #

# Step 6: Item Selection

# -------------------------------------------------------- PART 1 ------------------------------------------------ #

# 5,000 draws from the item list and correlation coefficients

varlist <- c("faith", "prayansr", "miracle", "comitgod", "aftrlife", "angels", "demons", 
             "miracles", "god", "godclose", "judgeday", "godview", "spiritua", "okayconv", "viewrel", 
             "congmust", "okaypick", "learnrel", "doubts", "onlyone")

set.seed(211) # simulations for item heterogeneity
modeldata_sim_items <- (replicate(5000, run_simulate_sample(modeldata_all, 10)))
modeldata_sim_coefs <- as.data.frame(t(matrix(unlist(modeldata_sim_items), nrow = 4)))
colnames(modeldata_sim_coefs)[1] <- "a1coef"
colnames(modeldata_sim_coefs)[2] <- "a2coef"
colnames(modeldata_sim_coefs)[3] <- "a1tval"
colnames(modeldata_sim_coefs)[4] <- "a2tval"
modeldata_sim_coefs <- modeldata_sim_coefs %>%
  mutate(a1sig = as.numeric(a1tval >= 1.96 | a1tval <= -1.96),
         a2sig = as.numeric(a2tval >= 1.96 | a2tval <= -1.96))
print(describe(modeldata_sim_coefs), digits = 5)

# -------------------------------------------------------- PART 2 ------------------------------------------------ #

# visualize the item selection analyses

s1 <- ggplot(modeldata_sim_coefs, aes(x = a1coef)) +
  geom_histogram(fill = "grey66", color = "grey20", bins = 25, alpha = 0.75) +
  xlab("Coefficients") +
  ylab("Models") +
  scale_x_continuous(breaks = c(0.0125, 0.0200, 0.0275, 0.0350)) +
  theme_minimal() +
  ggtitle("Age") +
  theme(text=element_text(family = "Arial Narrow"))

s2 <- ggplot(modeldata_sim_coefs, aes(x = a2coef)) +
  geom_histogram(fill = "grey66", color = "grey20", bins = 25, alpha = 0.75) +
  xlab("Coefficients") +
  ylab("Models") +
  scale_x_continuous(breaks = c(-0.0016, -0.0012, -0.0008, -0.0004)) +
  theme_minimal() +
  ggtitle("Age Squared") +
  theme(text=element_text(family = "Arial Narrow"))

png("./figures/FIG07.png", units="in", width=15, height=5, res=500)
grid.arrange(s1, s2, ncol = 2)
dev.off()

rm(curWarnings, modeldata_sim_coefs, modeldata_sim_items, s1, s2)

# ---------------------------------------------------------------------------------------------------------------- #
