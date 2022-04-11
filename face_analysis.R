setwd("~/Documents/TECL/AIR")

# Packages used for this script
library(pacman)
p_load(tidyverse, magrittr, emmeans, effects, wesanderson)

# Load the data
df_air = read_csv("air_data.csv")
df = read_csv("face_data.csv")

# Get rid of familiarization trials
df %<>% dplyr::filter(trial != "F1")
df %<>% dplyr::filter(trial != "F2")

df_air %<>% dplyr::filter(trial != "F1")
df_air %<>% dplyr::filter(trial != "F2")

# Recode NA's
df %<>% replace_na(list(gen_self = 0, 
                        gen_neg = 0, 
                        gen_group = 0, 
                        gen_mean = 0, 
                        gen_affil = 0, 
                        gen_goal = 0, 
                        gen_novel = 0))

df_air %<>% replace_na(list(gen_self = 0, 
                        gen_neg = 0, 
                        gen_group = 0, 
                        gen_mean = 0, 
                        gen_affil = 0, 
                        gen_goal = 0, 
                        gen_novel = 0))

df %<>% mutate(gen_bool = case_when(
  gen == "TRUE" ~ TRUE,
  gen == "FALSE" ~ FALSE,
  gen == "N/A" ~ NA
))

# Turn true/false into numbers
df %<>% mutate(genn = case_when(
  gen == TRUE ~ 1,
  gen == FALSE ~ 0,
  gen == NA ~ NA_real_
))

df_air %<>% mutate(genn = if_else(df_air$gen, 1, 0))

# New data frame with summary stats by individual
df_2 = df %>% group_by(pid, gen_face, version) %>% summarise(mean = mean(genn), sd = sd(genn))

# Linear model with outputs of participants' scores
mod = df_2 %>% lm(mean ~ gen_face * version, data = .)
mod %>% summary()
# Model including the prior violation or non-violation
mod_2 = df_2 %>% lmer(mean ~ gen_face * version + (1|pid), data = .)
mod_2 %>% summary()

semi.r = function(y, x, given){  # this function will compute the semi-partial r
  ryx  = cor(y, x)
  ryg  = cor(y, given)
  rxg  = cor(x, given)
  num  = ryx - (ryg*rxg)
  dnm  = sqrt( (1-rxg^2) )
  sp.r = num/dnm
  return(sp.r)
}

# Model including the version has better fit
anova(mod_2, mod)

# Pairwise contrasts for prior moral violations
mod_2_emm = emmeans(mod_2, by = "gen_face", "version")
pairs(mod_2_emm)

# Reorder these so they look the same for both graphs
df$gen_face = factor(df$gen_face, levels = c("N", "H", "M"))
df_air$gen_face = factor(df_air$gen_face, levels = c("neutral", "mean"))

# Rename the versions for the graph
df$version = factor(df$version)
levels(df$version) = c("Violation", "Non-Violation")

# Effects plot
plot(effect(term = "gen_face", mod = mod))

# Visualize data from Study 2 and Study 1
df %>% ggplot(aes(x = gen_face, y = genn, group = gen_face, fill = gen_face)) +
  facet_grid(.~version) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(0.9)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.9)) +
  xlab("Cue presented at prediction trial") +
  ylab("Proportion of inferences of moral violation") +
  ylim(0,1) +
  scale_fill_manual(values = c("#F3C09D", "#F7995C", "#AD5013")) +
  scale_x_discrete(labels = c("Neutral", "Hidden", "Mean")) +
  ggtitle("Inference of moral violation by cue type", 
          subtitle = "Prior moral violation (left) and no prior violation (right)") +
  bbc_style() +
  theme(legend.position = "none")
  

df %>% ggplot(aes(x = gen_face, y = genn, group = gen_face, fill = gen_face)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(0.9)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.9)) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Cue presented at prediction trial") +
  ylab("Proportion of inferences of moral violation") +
  ylim(0,1) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels = c("Mean", "Neutral", "Hidden"))

df %>% ggplot(aes(x = version, y = genn, group = version, fill = version)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(0.9)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.9)) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Prior behaviour on trial") +
  ylab("Proportion of inferences of moral violation") +
  ylim(0,1) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels = c("Prior violation", "No prior violation"))

df %<>% mutate(normn = case_when(
  norm == TRUE ~ 1,
  norm == FALSE ~ 0,
  norm == NA ~ NA_real_
))

# Build contrasts
df %<>% mutate(face1 = case_when(
  gen_face == "M" ~ 0.5,
  gen_face == "N" ~ -0.5,
  gen_face == "H" ~ 0,
))

df %<>% mutate(face2 = case_when(
  gen_face == "M" ~ 0,
  gen_face == "N" ~ -0.5,
  gen_face == "H" ~ 0.5,
))

library(lavaan)
model = '
genn ~ b*normn
genn ~ c*version + d*face1 + e*face2
normn ~ a*version

IDE := a*b
DE := c
total := c + (a*b)
'

sem(model, df) %>% summary(standardized = TRUE, fit.measures = TRUE)




df %>% glmer(formula = genn ~ gen_face * version + (1|pid), data = ., family = "binomial") -> face_glmer
fixef(face_glmer)
cc <- confint(face_glmer,parm="beta_")  ## slow (~ 11 seconds)
ctab <- cbind(est=fixef(face_glmer),cc)
rtab <- exp(ctab)
print(rtab, digits=3)

df$pid = substr(df$pid, 2, 4)
df$pid %<>% as.numeric()

mixedpower(face_glmer,
           df,
           fixed_effects = c("gen_face", "version"),
           simvar = "pid", 
           steps = c(24, 48, 72, 96), 
           critical_value = 1.96,
           n_sim = 100,
           SESOI = F,
           databased = T)
