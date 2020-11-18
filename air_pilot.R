setwd("~/Documents/TECL/AIR")

# Packages used for this script
library(pacman)
p_load(tidyverse, magrittr, effects)

# Load the data
df = read_csv("air_data.csv")

# Get rid of familiarization trials
df %<>% dplyr::filter(trial != "F1")
df %<>% dplyr::filter(trial != "F2")

# Recode NA's
df %<>% replace_na(list(gen_self = 0, 
                        gen_neg = 0, 
                        gen_group = 0, 
                        gen_mean = 0, 
                        gen_affil = 0, 
                        gen_goal = 0, 
                        gen_novel = 0))

# Turn true/false into numbers
df %<>% mutate(genn = if_else(df$gen, 1, 0))

# New data frame with summary stats by individual
df_2 = df %>% group_by(pid, gen_face) %>% summarise(mean = mean(genn), sd = sd(genn))

# Linear model with outputs of participants' scores
mod = df_2 %>% lm(mean ~ gen_face, data = .)
mod %>% summary()

# Effects plot
plot(effect(term = "gen_face", mod = mod))

df %>% ggplot(aes(x = gen_face, y = genn, group = gen_face, fill = gen_face)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(0.9)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.9)) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Cue presented at generalization trial") +
  ylab("Proportion of generalizations about moral violation")

