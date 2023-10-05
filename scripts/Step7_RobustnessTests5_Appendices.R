
# ------------ Replication Data for Religious Belief Alignment from Adolescence to Emerging Adulthood ------------ #
# ------------------------------------ Turgut Keskinturk - Bogazici University - 2021 ---------------------------- #

# Step 7: Appendices

# -------------------------------------------------------- PART 1 ------------------------------------------------ #

# first, load the necessary packages and the data

df_pop <- read.dta13("./data/NSYR_ALL.dta")
df_chg <- read.dta13("./data/NSYR_varchange.dta")
thresh <- 25

# -------------------------------------------------------- PART 2 ------------------------------------------------ #

# number of belief changes per year of age

change_model <- glm(totalchanges ~ agecats + factor(waves), 
                    family = "poisson", data = df_chg)

png("./figures/FIGA1.png", units="in", width=10, height=5, res=500)
plot_model(change_model, type = "pred", terms = "agecats", robust = TRUE) +
  xlab("Age") +
  ylab("The Number of Belief Changes") +
  ggtitle(element_blank()) +
  scale_x_continuous(breaks = round(seq(min(df_chg$agecats), 
                                        max(df_chg$agecats), by = 1),1)) +
  scale_y_continuous(breaks = c(4, 5, 6, 7, 8), limits = c(4, 8)) +
  theme_minimal() +
  theme(text=element_text(family = "Arial Narrow"))
dev.off()

# -------------------------------------------------------- PART 3 ------------------------------------------------ #

# panel attrition: drop-outs & all-ins

df_pop <- df_pop %>% 
  dplyr::group_by(ids) %>%
  dplyr::mutate(counts = n(),
                now3 = as.numeric(waves %in% c(1, 2)),
                now4 = as.numeric(waves %in% c(1, 2, 3))) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(drop = as.numeric(counts < 4))

library(plyr)

# those who have dropped out of the surveys & those who have not

df_drops0 <- df_pop %>% # present in all waves
  filter(drop == 0) %>%
  subset(select = c(agecats, faith, prayansr, miracle, comitgod, aftrlife, angels, demons, miracles, god, godclose, 
                    judgeday, godview, spiritua, okayconv, viewrel, congmust, okaypick, learnrel, doubts, onlyone)) %>%
  as.data.frame() %>%
  compute_correlations_all()
df_drops0$drop <- 0

df_drops1 <- df_pop %>% # drop in just one of the waves
  filter(drop == 1) %>%
  subset(select = c(agecats, faith, prayansr, miracle, comitgod, aftrlife, angels, demons, miracles, god, godclose, 
                    judgeday, godview, spiritua, okayconv, viewrel, congmust, okaypick, learnrel, doubts, onlyone)) %>%
  as.data.frame() %>%
  compute_correlations_all()
df_drops1$drop <- 1

df_drops2 <- df_pop %>% # not present after wave 2
  filter(now3 == 1) %>%
  subset(select = c(agecats, faith, prayansr, miracle, comitgod, aftrlife, angels, demons, miracles, god, godclose, 
                    judgeday, godview, spiritua, okayconv, viewrel, congmust, okaypick, learnrel, doubts, onlyone)) %>%
  as.data.frame() %>%
  compute_correlations_all()
df_drops2$drop <- 2

df_drops3 <- df_pop %>% # not present after wave 3
  filter(now4 == 1) %>%
  subset(select = c(agecats, faith, prayansr, miracle, comitgod, aftrlife, angels, demons, miracles, god, godclose, 
                    judgeday, godview, spiritua, okayconv, viewrel, congmust, okaypick, learnrel, doubts, onlyone)) %>%
  as.data.frame() %>%
  compute_correlations_all()
df_drops3$drop <- 3

modeldata_drops <- rbind(df_drops0, df_drops1, df_drops2, df_drops3)
rm(df_drops0, df_drops1, df_drops2, df_drops3) # clean-up

# visualize the trajectories

png("./figures/FIGA4.png", units="in", width=10, height=7.5, res=500)
modeldata_drops %>%
ggplot(aes(x = age, y = abs_c, group = drop, linetype = factor(drop))) + 
  geom_smooth(col = "black", size = 0.5,
              method = "lm", formula = y ~ splines::bs(x, 3), se = F) +
  xlab("Age") +
  ylab("Pairwise Correlations") +
  scale_x_continuous(breaks = round(seq(min(modeldata_drops$age), 
                                        max(modeldata_drops$age), by = 3),3)) +
  theme_bw() +
  theme(text=element_text(family = "Arial Narrow")) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_linetype_manual(values = c(1, 2, 3, 4), 
                        labels = c("No Attrition", 
                                   "Present in All But One Wave", 
                                   "Not Present After W = 2",
                                   "Not Present After W = 3"))
dev.off()

# -------------------------------------------------------- PART 4 ------------------------------------------------ #

# panel attrition: different groups

category_lists <- c("gender", "rwhites", "south", "noaffil", "peers", "lifetrauma", "evercollege", "hardships", "famfaith", 
                    "married", "famcollege", "income", "attend", "youthgr", "reltraining", "conversion")
category_label <- c("Female", "White", "South", "Non-Affiliated", "Peers Networks", "Trauma", "College", 
                    "Financial Hardship", "Parent Faith", "Parent Married", "Parent College", "Parent Income", "Attendance", 
                    "Youth Group", "Religious Education", "Conversion") # new labels for the variables

df_counts <- df_pop %>% group_by(agecats) %>% # percentage for groups
  summarise_all(mean, na.rm = TRUE) %>%
  melt(id.vars = "agecats") %>%
  mutate(variable = factor(variable, 
                           levels = category_lists,
                           labels = category_label)) %>% 
  na.omit()

hor_lines <- df_pop %>% group_by(waves) %>% # percentage for groups at wave = 1
  summarise_all(mean, na.rm = TRUE) %>%
  melt(id.vars = "waves") %>%
  filter(waves == 1) %>%
  mutate(variable = factor(variable, 
                           levels = category_lists,
                           labels = category_label)) %>%
  na.omit()

df_counts <- left_join(df_counts, hor_lines, by = "variable")

png("./figures/FIGA2.png", units="in", width=10, height=10, res=500)
ggplot(df_counts, aes(x = agecats, y = value.x)) + 
  geom_line(size = 0.75) + 
  geom_hline(aes(yintercept = value.y), linetype = "dashed", size = 0.75, col = "gray") +
  geom_ribbon(aes(ymin = value.x, ymax = value.y), fill = "gray", alpha = 0.5) +
  facet_wrap(~ variable, nrow = 4) + theme_bw() + ylim(0, 1) +
  theme(text=element_text(family = "Arial Narrow")) +
  scale_x_continuous(breaks = round(seq(min(df_counts$agecats), 
                                        max(df_counts$agecats), by = 3),3)) +
  labs(x = "Age", y = "Percentage")
dev.off()

# -------------------------------------------------------- PART 5 ------------------------------------------------ #

# attrition effects

df_lesspeople <- read.dta13("NSYR_ALL.dta") %>% # this is for belief-only data
  filter(evercollege == 0 & rwhites == 0 & income == 0) %>%
  subset(select = c(agecats, faith, prayansr, miracle, comitgod, aftrlife, angels, demons, miracles, god, godclose, 
                    judgeday, godview, spiritua, okayconv, viewrel, congmust, okaypick, learnrel, doubts, onlyone))
df_morepeople <- read.dta13("NSYR_ALL.dta") %>% # this is for belief-only data
  filter(evercollege == 1 & rwhites == 1 & income == 1) %>%
  subset(select = c(agecats, faith, prayansr, miracle, comitgod, aftrlife, angels, demons, miracles, god, godclose, 
                    judgeday, godview, spiritua, okayconv, viewrel, congmust, okaypick, learnrel, doubts, onlyone))

thresh <- 10
modeldata_lesspeople <- compute_correlations_all(df_lesspeople) %>% mutate(category = "Low Attrition")
modeldata_morepeople <- compute_correlations_all(df_morepeople) %>% mutate(category = "High Attrition")
modeldata_summary <- rbind(modeldata_lesspeople, modeldata_morepeople)

detach(package:plyr)

png("./figures/FIGA3.png", units="in", width=5, height=5, res=500)
modeldata_summary %>%
  group_by(age, category) %>% dplyr::summarise(mean = mean(abs_c, na.rm = TRUE)) %>%
  ggplot(aes(x = age, y = mean, group = category)) +
  geom_smooth(aes(x = age, y = mean, col = category), 
              method = "lm", formula = y ~ splines::bs(x, 3), size = 1.5, se = FALSE) +
  scale_color_manual(values = c("gray50", "gray15"),
                     labels = c("Group = 0", "Group = 1")) +
  xlab("Age") +
  ylab("Pairwise Correlations") +
  ylim(0, 0.5) +
  scale_x_continuous(breaks = round(seq(min(modeldata_lesspeople$age), 
                                        max(modeldata_lesspeople$age), by = 3),3)) +
  theme_bw() +
  theme(text=element_text(family = "Arial Narrow")) +
  theme(legend.position = "bottom",
        legend.title = element_blank())
dev.off()

rm(change_model, df_chg, df_counts, df_lesspeople, df_morepeople,
   df_pop, hor_lines, modeldata_drops, modeldata_lesspeople, modeldata_morepeople,
   modeldata_summary, category_label, category_lists)

# ---------------------------------------------------------------------------------------------------------------- #
