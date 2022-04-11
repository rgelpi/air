setwd("~/Downloads")

library(tidyverse)
library(magrittr)

df_face = read_csv("face.csv")
df_face %<>% filter(DistributionChannel != "preview")

# gather the category question values
df_face %<>%
  gather(key = catQ, value = catA, c(19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41))

# gather the face question values
df_face %<>%
  gather(key = face, value = gen, c(19:30))

# eliminate redundant pairs (i.e. only matching scenarios)
df_face %<>%
  filter(catQ %>% str_sub(1, 4) == face %>% str_sub(1, 4))

# rename values to be more human readable
df_face %<>%
  mutate(prior = case_when(
    grepl("Dante", face) ~ "PNV",
    grepl("Dean", face) ~ "PNV",
    grepl("Molly", face) ~ "PNV",
    grepl("Gerald", face) ~ "PNV",
    grepl("Gina", face) ~ "PNV",
    grepl("Corina", face) ~ "PNV",
    grepl("Paul", face) ~ "PV",
    grepl("Judy", face) ~ "PV",
    grepl("Susan", face) ~ "PV",
    grepl("Jeremy", face) ~ "PV",
    grepl("Eileen", face) ~ "PV",
    grepl("Marley", face) ~ "PV",
  ))

df_face %<>%
  mutate(face = case_when(
    grepl("NGen", face) ~ "Neutral",
    grepl("MGen", face) ~ "Angry",
  ))

df_face %<>%
  mutate(catQ = case_when(
    grepl("MAN", catQ) ~ "MAN",
    grepl("NHN", catQ) ~ "NHN",
  ))

mygg = function(dat, f1, f2=NA, f3=NA) {
  
  dat = dat %>% 
    mutate_(grouping = 
              if (is.na(f2)) {
                f1
              } else if (is.na(f3)) {
                interp(~paste(f1,f2, sep=' '), f1=as.name(f1), f2=as.name(f2))
              } else {
                interp(~paste(f1,f2,f3,sep=' '), f1=as.name(f1), f2=as.name(f2), f3=as.name(f3))
              })
  return(dat)
}

df_face <- mygg(df_face, "prior", "face")

# plot generalization by face and prior behaviour
df_face %>%
  ggplot(aes(x = face, y = as.numeric(gen == "Yes"), group = prior, fill = prior, colour = prior)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(0.9))

# plot frequency of generalization by trait inference and facial expressions
df_face %>%
  filter(catQ == "MAN") %>%
  ggplot(aes(x = catA, group = gen, fill = gen)) +
  geom_histogram(stat = "count", position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(vars(grouping), labeller = as_labeller(c(`PNV Angry` = "No prior violation, frown",
                                                      `PNV Neutral` = "No prior violation, neutral",
                                                      `PV Angry` = "Prior violation, frown",
                                                      `PV Neutral` = "Prior violation, neutral"))) +
  ggtitle("Facial expressions are most predictive of generalization",
          subtitle = "Children's predictions of moral violations by face, prior behaviour, and trait inference") +
  scale_fill_brewer(palette = "Set1") +
  bbc_style() +
  theme(legend.title = element_text(family = "Avenir", size = 20)) +
  theme(plot.title = element_text(family = "Avenir Next")) +
  labs(fill = "Response on prediction question",
       x = "Trait vs. emotion inference response")

        