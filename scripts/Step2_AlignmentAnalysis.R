
# ------------ Replication Data for Religious Belief Alignment from Adolescence to Emerging Adulthood ------------ #
# ------------------------------------ Turgut Keskinturk - Bogazici University - 2021 ---------------------------- #

# Step 2: Alignment Analysis

# -------------------------------------------------------- PART 1 ------------------------------------------------ #

rm(list = ls()) # clean-up

# first, load the necessary packages and the data

pacman::p_load(
  readstata13,
  tidyverse,
  plyr,
  igraph,
  qgraph,
  ggraph,
  hrbrthemes,
  sjPlot,
  lme4,
  optimx,
  psych,
  sjstats,
  extrafont,
  janitor,
  stringr,
  ggpubr,
  reshape2,
  gridExtra,
  coefplot,
  corclass,
  RColorBrewer,
  grid
) # use the same session for each script

df_all <-
  read.dta13("./data/NSYR_ALL.dta") %>% # this is for belief-only data
  subset(
    select = c(
      agecats,
      faith,
      prayansr,
      miracle,
      comitgod,
      aftrlife,
      angels,
      demons,
      miracles,
      god,
      godclose,
      judgeday,
      godview,
      spiritua,
      okayconv,
      viewrel,
      congmust,
      okaypick,
      learnrel,
      doubts,
      onlyone
    )
  )
ages <- unique(df_all$agecats) # vector of unique ages
thresh <-
  50 # min number of respondents for correlations to be gathered

source("./scripts/Step8_MainUserFunctions.R") # load the necessary functions

# -------------------------------------------------------- PART 2 ------------------------------------------------ #

# let's collect and visualize the correlations

modeldata_all <-
  compute_correlations_all(df_all) # compute correlation coefficients

png(
  "./figures/FIG01.png",
  units = "in",
  width = 7.5,
  height = 5,
  res = 500
)
modeldata_all %>%
  ggplot(aes(x = abs_c)) +
  geom_histogram(aes(y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..]),
                 bins = 8, color = "grey20") +
  xlab("Pairwise Correlations") +
  ylab("Percentage of Pairwise Correlations") +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  facet_wrap(~ age) +
  theme_minimal() +
  theme(text = element_text(family = "Arial Narrow")) +
  theme(strip.text = element_text(hjust = 1))
dev.off()

# visualize age = 13 belief network with correlations higher than 10%

set.seed(112)
png(
  "./figures/FIG02.png",
  units = "in",
  width = 10,
  height = 7.5,
  res = 500
)
ggraph(return_graph_object(13), layout = "fr") +
  geom_edge_link(aes(width = weight),
                 alpha = 0.75,
                 edge_color = "grey20") +
  geom_node_point(
    shape = 21,
    colour = "black",
    fill = "white",
    size = 15,
    show.legend = FALSE,
    stroke = 0.75
  ) +
  geom_node_text(
    aes(label = name),
    color = "black",
    show.legend = FALSE,
    family = "Arial Narrow",
    size = 3
  ) +
  scale_edge_width(
    breaks = c(0.1, 0.2, 0.3, 0.4),
    labels = c(
      "0.10 < |cor| \u2264 0.20",
      "0.20 < |cor| \u2264 0.30",
      "0.30 < |cor| \u2264 0.40",
      "0.40 < |cor|"
    ),
    name = "Tie Strengths",
    range = c(0.1, 3),
    limits = c(0, 1)
  ) +
  theme_graph(background = "white") +
  theme_graph() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )
dev.off()

# visualize age = 27 belief network with correlations higher than 10%

set.seed(211)
png(
  "./figures/FIG03.png",
  units = "in",
  width = 10,
  height = 7.5,
  res = 500
)
ggraph(return_graph_object(27), layout = "fr") +
  geom_edge_link(aes(width = weight),
                 alpha = 0.75,
                 edge_color = "grey20") +
  geom_node_point(
    shape = 21,
    colour = "black",
    fill = "white",
    size = 15,
    show.legend = FALSE,
    stroke = 0.75
  ) +
  geom_node_text(
    aes(label = name),
    color = "black",
    show.legend = FALSE,
    family = "Arial Narrow",
    size = 3
  ) +
  scale_edge_width(
    breaks = c(0.1, 0.2, 0.3, 0.4),
    labels = c(
      "0.10 < |cor| \u2264 0.20",
      "0.20 < |cor| \u2264 0.30",
      "0.30 < |cor| \u2264 0.40",
      "0.40 < |cor|"
    ),
    name = "Tie Strengths",
    range = c(0.1, 3),
    limits = c(0, 1)
  ) +
  theme_graph(background = "white") +
  theme_graph() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )
dev.off()

# -------------------------------------------------------- PART 3 ------------------------------------------------ #

# model correlations with multilevel mixed effects estimations

modeldata_all <- modeldata_all %>%
  mutate(age_r = age - 13) # change the starting point to 0 (i.e., age 13 becomes 0)

mixedmodel1 <- lmer(
  abs_c ~ (1 + age_r | j) + age_r,
  data = modeldata_all,
  control = lmerControl(optimizer = "Nelder_Mead")
)
tab_model(
  mixedmodel1,
  digits = 4,
  digits.p = 4,
  digits.re = 4,
  show.se = TRUE
)

mixedmodel2 <- lmer(
  abs_c ~ (1 + age_r | j) + age_r + I(age_r ^ 2),
  data = modeldata_all,
  control = lmerControl(optimizer = "Nelder_Mead")
)
tab_model(
  mixedmodel2,
  digits = 4,
  digits.p = 4,
  digits.re = 4,
  show.se = TRUE
)

mixedmodel3 <-
  lmer(
    abs_c ~ (1 + age_r | j) + age_r + I(age_r ^ 2) + I(age_r ^ 3),
    data = modeldata_all,
    control = lmerControl(optimizer = "Nelder_Mead")
  )
tab_model(
  mixedmodel3,
  digits = 4,
  digits.p = 4,
  digits.re = 4,
  show.se = TRUE
)

AIC(mixedmodel1)
BIC(mixedmodel1)
AIC(mixedmodel2)
BIC(mixedmodel2)
AIC(mixedmodel3)
BIC(mixedmodel3) # quadratic model is the best model

# coefficients for the mixed model 4 (age as dummy variables)

mixedmodel4 <- lmer(
  abs_c ~ (1 + age_r | j) + factor(age_r),
  data = modeldata_all,
  control = lmerControl(optimizer = "Nelder_Mead")
)
tab_model(
  mixedmodel4,
  digits = 4,
  digits.p = 4,
  digits.re = 4,
  show.se = TRUE
)

png(
  "./figures/FIGA5.png",
  units = "in",
  width = 10,
  height = 5,
  res = 500
)
coefplot(
  mixedmodel4,
  intercept = FALSE,
  innerCI = 0,
  outerCI = 1.96,
  lwdOuter = 0.75,
  title = "",
  xlab = "Coefficients",
  ylab = "Age",
  horizontal = TRUE,
  color = "black",
  newNames = c(
    "factor(age_r)1" = "14",
    "factor(age_r)2" = "15",
    "factor(age_r)3" = "16",
    "factor(age_r)4" = "17",
    "factor(age_r)5" = "18",
    "factor(age_r)6" = "19",
    "factor(age_r)7" = "20",
    "factor(age_r)8" = "21",
    "factor(age_r)9" = "22",
    "factor(age_r)10" = "23",
    "factor(age_r)11" = "24",
    "factor(age_r)12" = "25",
    "factor(age_r)13" = "26",
    "factor(age_r)14" = "27",
    "factor(age_r)15" = "28"
  )
) +
  theme_minimal() + theme(legend.position = "none")
dev.off()

rm(mixedmodel1, mixedmodel2, mixedmodel3, mixedmodel4) # clean-up for later scripts

# ---------------------------------------------------------------------------------------------------------------- #
