
# ------------ Replication Data for Religious Belief Alignment from Adolescence to Emerging Adulthood ------------ #
# ------------------------------------ Turgut Keskinturk - Bogazici University - 2021 ---------------------------- #

# Step 5: Robustness Tests 3 - Heterogeneity B

library(lavaan) # for schematic invariance tests

# -------------------------------------------------------- PART 1 ------------------------------------------------ #

# re-scale the variables

df_cca1 <- read.dta13("./data/NSYR_ALL.dta") %>%
  subset(select = c(ids, agecats, waves, 
                    faith, prayansr, miracle, comitgod, aftrlife, angels, demons, miracles, god, godclose, judgeday, 
                    godview, spiritua, okayconv, viewrel, congmust, okaypick, learnrel, doubts, onlyone))
df_cca2 <- read.dta13("./data/NSYR_ALL.dta") %>%
  subset(select = c(ids, agecats, waves, 
                    faith, prayansr, miracle, comitgod, aftrlife, angels, demons, miracles, god, godclose, judgeday, 
                    godview, spiritua, okayconv, viewrel, congmust, okaypick, learnrel, doubts, onlyone)) %>% 
  mutate(
    prayansr = recode(prayansr, "0" = 1, "1" = 5),
    miracle = recode(miracle, "0" = 1, "1" = 5),
    comitgod = recode(comitgod, "0" = 1, "1" = 5),
    aftrlife = recode(aftrlife, "1" = 1, "2" = 3, "3" = 5),
    angels = recode(angels, "1" = 1, "2" = 3, "3" = 5),
    demons = recode(demons, "1" = 1, "2" = 3, "3" = 5),
    miracles = recode(miracles, "1" = 1, "2" = 3, "3" = 5),
    god = recode(god, "1" = 1, "2" = 3, "3" = 5),
    godclose = ifelse(godclose > 5, 5, godclose),
    judgeday = recode(judgeday, "0" = 1, "1" = 5),
    godview = recode(godview, "0" = 1, "1" = 5),
    spiritua = recode(spiritua, "1" = 1, "2" = 3, "3" = 5),
    okayconv = recode(okayconv, "0" = 1, "1" = 5),
    viewrel = recode(viewrel, "1" = 1, "2" = 3, "3" = 5),
    congmust = recode(congmust, "0" = 1, "1" = 5),
    okaypick = recode(okaypick, "0" = 1, "1" = 5),
    learnrel = recode(learnrel, "1" = 1, "2" = 1, "3" = 3, "4" = 5),
    doubts = recode(doubts, "1" = 1, "2" = 3, "3" = 5, "4" = 5),
    onlyone = recode(onlyone, "0" = 1, "1" = 5)
  ) %>% filter(waves == 1) %>% # variables are rescaled such that the lowest score takes 1 and the highest score takes 5
  subset(select = -c(agecats, waves)) %>% na.omit()

# -------------------------------------------------------- PART 2 ------------------------------------------------ #

# let us find schematic classes through an inductive process

## correlational class analyses
cca <- df_cca2 %>% 
  subset(select = -c(ids)) %>% 
  cca(filter.value = 0.05, zero.action = "ownclass")

## merge and drop
df_cca2$member <- cca$membership
df_cca2 <- df_cca2 %>% group_by(member) %>%
  mutate(counts = n()) %>%
  ungroup() %>%
  filter(counts != 1) %>% subset(select = -c(counts))

## invariance
cfa_model <- 'rel =~ faith + prayansr + miracle + comitgod + aftrlife + angels + demons + miracles + god + godclose +
              judgeday + godview + okayconv + viewrel + congmust + okaypick + learnrel + spiritua + doubts'

fit_cfa1 <- cfa(cfa_model, data = df_cca2, group = "member", group.equal = c("lv.variances", "lv.covariances"))
fit_cfa2 <- cfa(cfa_model, data = df_cca2, group = "member")

anova(fit_cfa1, fit_cfa2) # schemas significantly differ

## merge with the master file
df_cca2 <- df_cca2 %>% subset(select = c(ids, member))
df_cca1 <- df_cca1 %>% left_join(df_cca2, by = "ids")

## plot schematic classes
cca_plot1 <- qgraph::qgraph(cca$modules[[1]]$cormat, layout = "spring", theme = "gray", title = "Group 1",
                            labels = colnames(cca$modules[[1]]$cormat), esize = 7.5*exp(-18/90)+1,
                            DoNotPlot = TRUE)
cca_plot2 <- qgraph::qgraph(cca$modules[[3]]$cormat, layout = "spring", theme = "gray", title = "Group 2",
                            labels = colnames(cca$modules[[2]]$cormat), esize = 7.5*exp(-18/90)+1,
                            DoNotPlot = TRUE)
cca_plot3 <- qgraph::qgraph(cca$modules[[4]]$cormat, layout = "spring", theme = "gray", title = "Group 3",
                            labels = colnames(cca$modules[[3]]$cormat), esize = 7.5*exp(-18/90)+1,
                            DoNotPlot = TRUE)
cca_plot4 <- qgraph::qgraph(cca$modules[[5]]$cormat, layout = "spring", theme = "gray", title = "Group 4",
                            labels = colnames(cca$modules[[4]]$cormat), esize = 7.5*exp(-18/90)+1,
                            DoNotPlot = TRUE)

png("./figures/FIGA6.png", units="in", width=10, height=10, res=500)
par(mfrow = c(2, 2))
plot(cca_plot1); plot(cca_plot2); plot(cca_plot3); plot(cca_plot4)
dev.off()

df_cca1 %>%
  filter(waves == 1) %>% group_by(member) %>%
  summarise(m = mean(agecats)) # no age differences between groups

# -------------------------------------------------------- PART 3 ------------------------------------------------ #

# correlations for each group

modeldata_cca <- data.frame()
df_cca1 <- df_cca1[is.na(df_cca1$member) == FALSE, ]
thresh <- 20
library(plyr)

for (m in unique(df_cca1$member)) { # this for loop calculates correlations for each class
  print(m)
  group <- compute_correlations_all(
    df_cca1 %>% filter(member == m) %>%
      subset(select = c(agecats, faith, prayansr, miracle, comitgod, aftrlife, angels, demons, miracles, god, godclose, 
                        judgeday, godview, spiritua, okayconv, viewrel, congmust, okaypick, learnrel, doubts, onlyone))
  ) %>% mutate(category = m)
  modeldata_cca = rbind(modeldata_cca, group)
}
rm(group, m) # some clean-up again

# -------------------------------------------------------- PART 4 ------------------------------------------------ #

# the change in correlations for each group

detach(package:plyr)

png("./figures/FIG06.png", units="in", width=10, height=7.5, res=500)
modeldata_cca %>%
  group_by(age, category) %>%
  summarise(
    mean_abs = mean(abs_c, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = age, y = mean_abs, group = category, linetype = factor(category))) +
  geom_smooth(col = "black", size = 0.5,
              method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE) +
  ylim(0.10, 0.45) +
  xlab("Age") +
  ylab("Pairwise Correlations") +
  scale_x_continuous(breaks = round(seq(min(modeldata_cca$age), 
                                        max(modeldata_cca$age), by = 3),3)) +
  theme_bw() +
  theme(text=element_text(family = "Arial Narrow")) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_linetype_manual(values = c(1, 3, 4, 5), 
                        labels = c("Group 1", "Group 2", "Group 3", "Group 4"))
dev.off()

rm(cca, cca_plot1, cca_plot2, cca_plot3, cca_plot4, df_cca1, df_cca2, 
   fit_cfa1, fit_cfa2, modeldata_cca, cfa_model) # clean-up

# ---------------------------------------------------------------------------------------------------------------- #
