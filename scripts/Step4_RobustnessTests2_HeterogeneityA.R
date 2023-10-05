
# ------------ Replication Data for Religious Belief Alignment from Adolescence to Emerging Adulthood ------------ #
# ------------------------------------ Turgut Keskinturk - Bogazici University - 2021 ---------------------------- #

# Step 4: Robustness Tests 2 - Heterogeneity A

# -------------------------------------------------------- PART 1 ------------------------------------------------ #

# first, load the necessary packages and the data

df_pop <- read.dta13("./data/NSYR_ALL.dta") %>% subset(select = -c(ids, waves))
thresh <- 25 # min number of respondents for correlations to be gathered

library(plyr) # let's load this again

# -------------------------------------------------------- PART 2 ------------------------------------------------ #

# subgroups comparison

category_lists <- c("gender", "rwhites", "south", "noaffil", "peers", "lifetrauma", "evercollege", "hardships", "famfaith", 
                    "married", "famcollege", "income", "attend", "youthgr", "reltraining", "conversion")
category_label <- c("Female", "White", "South", "Non-Affiliated", "Peers Networks", "Trauma", "College", 
                    "Financial Hardship", "Parent Faith", "Parent Married", "Parent College", "Parent Income", "Attendance", 
                    "Youth Group", "Religious Education", "Conversion") # new labels for the variables

modeldata_subpops <- data.frame()
for (cat in category_lists) { # this for loop calculates correlations for each subgroup
  print(cat)
  cat <- paste(cat)
  group1 <- compute_correlations_all(
    df_pop %>% filter(!!as.symbol(cat) == 1) %>%
      subset(select = c(agecats, faith, prayansr, miracle, comitgod, aftrlife, angels, demons, miracles, god, godclose, 
                        judgeday, godview, spiritua, okayconv, viewrel, congmust, okaypick, learnrel, doubts, onlyone))
  ) %>% mutate(category = cat, group = 1)
  group0 <- compute_correlations_all(
    df_pop %>% filter(!!as.symbol(cat) == 0) %>%
      subset(select = c(agecats, faith, prayansr, miracle, comitgod, aftrlife, angels, demons, miracles, god, godclose, 
                        judgeday, godview, spiritua, okayconv, viewrel, congmust, okaypick, learnrel, doubts, onlyone))
  ) %>% mutate(category = cat, group = 0)
  modeldata_subpops = rbind(modeldata_subpops, group1)
  modeldata_subpops = rbind(modeldata_subpops, group0)
  rm(group1, group0)
}
rm(cat) # some clean-up again

# group and summarize the correlations

detach(package:plyr) # for dplyr to work properly

modeldata_subpops_grouped <- modeldata_subpops %>%
  mutate(category = as.factor(category)) %>% group_by(age, group, category) %>%
  summarize(mcor = mean(abs_c, na.rm = TRUE), 
            mse = sd(abs_c)/sqrt(sum(!is.na(abs_c)))) %>%
  mutate(source = factor(category, 
                         levels = category_lists,
                         labels = category_label)) # summary of alignment trajectories across groups

# visualize the correlations

png("./figures/FIG05.png", units="in", width=10, height=12.5, res=500)
modeldata_subpops_grouped %>%
  ggplot(aes(x = age, y = mcor, group = factor(group))) +
  geom_smooth(aes(y = mcor, col = factor(group)), method = "lm", formula = y ~ splines::bs(x, 3), 
              size = 1.5, se = FALSE) +
  scale_color_manual(values = c("gray50", "gray15"),
                     labels = c("Group = 0", "Group = 1")) +
  ylim(0, 0.5) +
  xlab("Age") +
  ylab("Pairwise Correlations") +
  scale_x_continuous(breaks = round(seq(min(modeldata_subpops$age), 
                                        max(modeldata_subpops$age), by = 3),3)) +
  facet_wrap(~ source, nrow = 4) +
  theme_bw() +
  theme(text=element_text(family = "Arial Narrow")) +
  theme(legend.position = "bottom",
        legend.title = element_blank())
dev.off()

# -------------------------------------------------------- PART 3 ------------------------------------------------ #

# fit multilevel models for each subgroup and compare for model fits

modeldata_subpops <- modeldata_subpops %>% # 
  mutate(age_r = age - 13)
modeldata_subpops_fitted_l <- lapply(split(modeldata_subpops, modeldata_subpops$category), lmer, # Linear Fits
                                     formula = abs_c ~ age_r * group + 
                                       (1 + age_r | j), 
                                     control = lmerControl(optimizer = "Nelder_Mead"))
modeldata_subpops_fitted_q <- lapply(split(modeldata_subpops, modeldata_subpops$category), lmer, # Quadratic Fits
                                     formula = abs_c ~ age_r * group + I(age_r^2) * group + 
                                       (1 + age_r | j), 
                                     control = lmerControl(optimizer = "Nelder_Mead"))
modeldata_subpops_fitted_c <- lapply(split(modeldata_subpops, modeldata_subpops$category), lmer, # Cubic Fits
                                     formula = abs_c ~ age_r * group + I(age_r^2) * group + I(age_r^3) * group + 
                                       (1 + age_r | j), 
                                     control = lmerControl(optimizer = "Nelder_Mead"))

modeldata_subpops_bic <- data.frame()
  for (i in 1:16) {
    modeldata_subpops_bic[i, 1] <- BIC(modeldata_subpops_fitted_l[[i]])
    modeldata_subpops_bic[i, 2] <- BIC(modeldata_subpops_fitted_q[[i]])
    modeldata_subpops_bic[i, 3] <- BIC(modeldata_subpops_fitted_c[[i]])
    modeldata_subpops_bic[i, 4] <- names(modeldata_subpops_fitted_c[i])
  }
rm(i)
modeldata_subpops_bic$min <- max.col(-modeldata_subpops_bic[, 1:3])

# -------------------------------------------------------- PART 4 ------------------------------------------------ #

# whether signs differ between population groups

modeldata_subpops_gr1 <- modeldata_subpops %>% filter(group == 1)
modeldata_subpops_gr0 <- modeldata_subpops %>% filter(group == 0)

modeldata_logics <- inner_join(modeldata_subpops_gr1, modeldata_subpops_gr0, 
                               by = c("age", "category", "j"), suffix = c(".H", ".L")) %>%
  mutate(pos.H = (c.H > 0), pos.L = (c.L > 0)) %>%
  mutate(similar = pos.H == pos.L) %>%
  filter(p.H < 0.05 | p.L < 0.05) %>%
  group_by(age, category) %>% dplyr::summarize(totalsimilar = sum(similar), n = n()) %>%
  mutate(logics = totalsimilar/n)

modeldata_logics_summary <- modeldata_logics %>%
  group_by(category) %>% dplyr::summarise(
    logicsM = mean(logics, na.rm = TRUE), logicSD = sd(logics, na.rm = TRUE)) %>%
  mutate(category = factor(category,
                           levels = category_lists,
                           labels = category_label))

attach(modeldata_logics_summary)
modeldata_logics_summary <- modeldata_logics_summary[order(logicsM), ]
detach(modeldata_logics_summary)

rm(df_pop, modeldata_logics, modeldata_logics_summary,
   modeldata_subpops, modeldata_subpops_bic, category_label, category_lists,
   modeldata_subpops_fitted_c, modeldata_subpops_fitted_l, modeldata_subpops_fitted_q,
   modeldata_subpops_gr0, modeldata_subpops_gr1, modeldata_subpops_grouped)

# ---------------------------------------------------------------------------------------------------------------- #
