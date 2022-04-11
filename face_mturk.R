setwd("~/Documents/TECL/AIR")

# Load libraries
library(pacman)
p_load(tidyverse, magrittr)

df_mturk = read_csv("face_mturk.csv")
# Only select the ones recruited via MTurk
df_mturk %<>% filter(DistributionChannel == "anonymous")
# Get rid of data values we are not going to use/analyze
df_mturk %<>% select(-ResponseId, 
               -RecipientFirstName, 
               -RecipientLastName, 
               -RecipientEmail, 
               -ExternalReference, 
               -LocationLatitude, 
               -LocationLongitude, 
               -DistributionChannel, 
               -UserLanguage, 
               -X18, 
               -Status, 
               -IPAddress, 
               -Finished, 
               -StartDate, 
               -EndDate, 
               -Progress)

df_mturk %<>% mutate(Dean_accept = coalesce(Dean_V_accept_1, Dean_NV_accept_1),
               Dean_trust = coalesce(`Dean_V_trust _1`, Dean_NV_trust_1),
               Dean_consid = coalesce(Dean_V_consid_1, Dean_NV_consid_1),
               Molly_accept = coalesce(Molly_V_accept_1, Molly_NV_accept_1),
               Molly_trust = coalesce(Molly_V_trust_1, Molly_NV_trust_1),
               Molly_consid = coalesce(`Molly_V_consid _1`, Molly_NV_consid_1),
               Gina_accept = coalesce(Laura_PNV_accept_1, Gina_NV_accept_1),
               Gina_trust = coalesce(Gina_V_trust_1, Gina_NV_accept_1_1),
               Gina_consid = coalesce(Gina_V_consid_1, Gina_NV_consid_1),
               Mike_accept = coalesce(Mike_V_accept_1, Mike_NV_accept_1),
               Mike_trust = coalesce(Mike_V_trust_1, Mike_NV_trust_1),
               Mike_consid = coalesce(`Mike_V_consid _1`, Mike_NV_trust_1_1),
               Judy_accept = coalesce(Judy_V_accept_1, Judy_NV_accept_1),
               Judy_trust = coalesce(Judy_V_trust_1, Judy_NV_trust_1),
               Judy_consid = coalesce(Judy_V_consid_1, Judy_NV_consid_1),
               Paul_accept = coalesce(Paul_V_accept_1, Paul_NV_accept_1),
               Paul_trust = coalesce(Paul_V_trust_1, Paul_NV_trust_1),
               Paul_consid = coalesce(Paul_V_consid_1, `Paul_NV_consid _1`),
               Marco_accept = coalesce(Marco_V_accept_1, Marc_NV_accept_1),
               Marco_trust = coalesce(`Marco_V_trust _1`, Marco_NV_accept_1),
               Marco_consid = coalesce(Marco_V_consid_1, Marco_NV_accept_1_1),
               Vida_accept = coalesce(Vida_V_accept_1, Vida_NV_accept_1),
               Vida_trust = coalesce(Vida_V_trust_1, Vida_NV_trust_1),
               Vida_consid = coalesce(Vida_V_consid_1, Vida_NV_consid_1),
               Jared_accept = coalesce(Jared_V_accept_1, Jared_NV_accept_1),
               Jared_trust = coalesce(Jared_V_trust_1, Jared_NV_trust_1),
               Jared_consid = coalesce(Jared_V_consid_1, Jared_NV_consid_1))

df_mturk %<>% select(-Dean_V_accept_1, 
               -Dean_NV_accept_1,
               -`Dean_V_trust _1`, 
               -Dean_NV_trust_1,
               -Dean_V_consid_1, 
               -Dean_NV_consid_1,
               -Molly_V_accept_1, 
               -Molly_NV_accept_1,
               -Molly_V_trust_1, 
               -Molly_NV_trust_1,
               -`Molly_V_consid _1`, 
               -Molly_NV_consid_1,
               -Laura_PNV_accept_1, 
               -Gina_NV_accept_1,
               -Gina_V_trust_1, 
               -Gina_NV_accept_1_1,
               -Gina_V_consid_1, 
               -Gina_NV_consid_1,
               -Mike_V_accept_1, 
               -Mike_NV_accept_1,
               -Mike_V_trust_1, 
               -Mike_NV_trust_1,
               -`Mike_V_consid _1`, 
               -Mike_NV_trust_1_1,
               -Judy_V_accept_1, 
               -Judy_NV_accept_1,
               -Judy_V_trust_1, 
               -Judy_NV_trust_1,
               -Judy_V_consid_1, 
               -Judy_NV_consid_1,
               -Paul_V_accept_1, 
               -Paul_NV_accept_1,
               -Paul_V_trust_1, 
               -Paul_NV_trust_1,
               -Paul_V_consid_1, 
               -`Paul_NV_consid _1`,
               -Marco_V_accept_1, 
               -Marc_NV_accept_1,
               -`Marco_V_trust _1`, 
               -Marco_NV_accept_1,
               -Marco_V_consid_1, 
               -Marco_NV_accept_1_1,
               -Vida_V_accept_1, 
               -Vida_NV_accept_1,
               -Vida_V_trust_1, 
               -Vida_NV_trust_1,
               -Vida_V_consid_1, 
               -Vida_NV_consid_1,
               -Jared_V_accept_1, 
               -Jared_NV_accept_1,
               -Jared_V_trust_1, 
               -Jared_NV_trust_1,
               -Jared_V_consid_1, 
               -Jared_NV_consid_1)

# Get the face for each scenario
df_mturk %<>% mutate(Gina_face = case_when(
  !is.na(Gina_N_push_1) ~ "N",
  !is.na(Gina_M_push_1) ~ "M",
  !is.na(Gina_H_push_1) ~ "H"),
  Dean_face = case_when(
    !is.na(`Dean_N_push _1`) ~ "N",
    !is.na(`Dean_M_push _1`) ~ "M",
    !is.na(`Dean_H_push _1`) ~ "H"),
  Molly_face = case_when(
    !is.na(Molly_N_push_1) ~ "N",
    !is.na(`Molly_M_push _1`) ~ "M",
    !is.na(`Molly_H_push _1`) ~ "H"),
  Mike_face = case_when(
    !is.na(`Mike_N_push _1`) ~ "N",
    !is.na(Mike_M_push_1) ~ "M",
    !is.na(Mike_H_push_1) ~ "H"),
  Judy_face = case_when(
    !is.na(`Judy_N_push _1`) ~ "N",
    !is.na(Judy_M_push_1) ~ "M",
    !is.na(`Judy_H_push _1`) ~ "H"),
  Paul_face = case_when(
    !is.na(`Paul_N_push _1`) ~ "N",
    !is.na(`Paul_M_push _1`) ~ "M",
    !is.na(`Paul_H_push _1`) ~ "H"),
  Marco_face = case_when(
    !is.na(`Marco_N_push _1`) ~ "N",
    !is.na(`Marco_M_push _1`) ~ "M",
    !is.na(`Marco_H_push _1`) ~ "H"),
  Vida_face = case_when(
    !is.na(`Vida_N_push _1`) ~ "N",
    !is.na(Vida_M_push_1) ~ "M",
    !is.na(`Vida_H_push _1`) ~ "H"),
  Jared_face = case_when(
    !is.na(`Jared_N_push _1`) ~ "N",
    !is.na(`Jared_M_push _1`) ~ "M",
    !is.na(Jared_H_push_1) ~ "H"),
)

# merge columns
df_mturk %<>% mutate(
  Gina_push = coalesce(Gina_N_push_1, Gina_M_push_1, Gina_H_push_1),
  Gina_reason = coalesce(Gina_N_reason, Gina_M_reason, Gina_H_reason),
  Dean_push = coalesce(`Dean_M_push _1`, `Dean_H_push _1`, `Dean_N_push _1`),
  Dean_reason = coalesce(Dean_N_reason, Dean_M_reason, `Gina_H_reason `),
  Molly_push = coalesce(Molly_N_push_1, `Molly_M_push _1`, `Molly_H_push _1`),
  Molly_reason = coalesce(Molly_N_reason, Molly_M_reason, Molly_H_reason),
  Mike_push = coalesce(`Mike_N_push _1`, Mike_M_push_1, Mike_H_push_1),
  Mike_reason = coalesce(`Mike_N_reason `, `Mike_M_reason `, `Mike_H_reason `),
  Judy_push = coalesce(`Judy_N_push _1`, Judy_M_push_1, `Judy_H_push _1`),
  Judy_reason = coalesce(Judy_N_reason, `Judy_M_reason `, Judy_H_reason),
  Paul_push = coalesce(`Paul_N_push _1`, `Paul_M_push _1`, `Paul_H_push _1`),
  Paul_reason = coalesce(Paul_N_reason, Paul_M_reason, Paul_H_reason),
  Marco_push = coalesce(`Marco_N_push _1`, `Marco_M_push _1`, `Marco_H_push _1`),
  Marco_reason = coalesce(Marco_N_reason, Marco_M_reason, Marco_H_reason),
  Vida_push = coalesce(`Vida_N_push _1`, Vida_M_push_1, `Vida_H_push _1`),
  Vida_reason = coalesce(Vida_N_reason, `Vida_M_reason `, Vida_H_reason),
  Jared_push = coalesce(`Jared_N_push _1`, `Jared_M_push _1`, Jared_H_push_1),
  Jared_reason = coalesce(Jared_N_reason, Jared_M_reason, `Jared_H_reason `)
)

# drop extra columns
df_mturk %<>% select(-Gina_N_push_1, 
               -Gina_M_push_1, 
               -Gina_H_push_1,
               -Gina_N_reason,
               -Gina_M_reason, 
               -Gina_H_reason,
               -`Dean_M_push _1`, 
               -`Dean_H_push _1`, 
               -`Dean_N_push _1`,
               -Dean_N_reason, 
               -Dean_M_reason, 
               -`Gina_H_reason `,
               -Molly_N_push_1, 
               -`Molly_M_push _1`, 
               -`Molly_H_push _1`,
               -Molly_N_reason, 
               -Molly_M_reason, 
               -Molly_H_reason,
               -`Mike_N_push _1`, 
               -Mike_M_push_1, 
               -Mike_H_push_1,
               -`Mike_N_reason `, 
               -`Mike_M_reason `, 
               -`Mike_H_reason `,
               -`Judy_N_push _1`, 
               -Judy_M_push_1, 
               -`Judy_H_push _1`,
               -Judy_N_reason, 
               -`Judy_M_reason `, 
               -Judy_H_reason,
               -`Paul_N_push _1`, 
               -`Paul_M_push _1`, 
               -`Paul_H_push _1`,
               -Paul_N_reason, 
               -Paul_M_reason, 
               -Paul_H_reason,
               -`Marco_N_push _1`, 
               -`Marco_M_push _1`, 
               -`Marco_H_push _1`,
               -Marco_N_reason, 
               -Marco_M_reason, 
               -Marco_H_reason,
               -`Vida_N_push _1`, 
               -Vida_M_push_1, 
               -`Vida_H_push _1`,
               -Vida_N_reason, 
               -`Vida_M_reason `, 
               -Vida_H_reason,
               -`Jared_N_push _1`, 
               -`Jared_M_push _1`, 
               -Jared_H_push_1,
               -Jared_N_reason, 
               -Jared_M_reason, 
               -`Jared_H_reason `)

#Long form
df_mturk %<>% gather(key = "scenario", value = "face", Gina_face, Dean_face, Molly_face, Mike_face, Judy_face, Paul_face, Marco_face, Vida_face, Jared_face)
df_mturk$scenario %<>% str_remove("_face")

df_mturk %<>% mutate(
  accept = case_when(
    scenario == "Gina" ~ Gina_accept,
    scenario == "Dean" ~ Dean_accept,
    scenario == "Molly" ~ Molly_accept,
    scenario == "Mike" ~ Mike_accept,
    scenario == "Judy" ~ Judy_accept,
    scenario == "Paul" ~ Paul_accept,
    scenario == "Marco" ~ Marco_accept,
    scenario == "Vida" ~ Vida_accept,
    scenario == "Jared" ~ Jared_accept,
  ),
  trust = case_when(
    scenario == "Gina" ~ Gina_trust,
    scenario == "Dean" ~ Dean_trust,
    scenario == "Molly" ~ Molly_trust,
    scenario == "Mike" ~ Mike_trust,
    scenario == "Judy" ~ Judy_trust,
    scenario == "Paul" ~ Paul_trust,
    scenario == "Marco" ~ Marco_trust,
    scenario == "Vida" ~ Vida_trust,
    scenario == "Jared" ~ Jared_trust,
  ),
  consid = case_when(
    scenario == "Gina" ~ Gina_consid,
    scenario == "Dean" ~ Dean_consid,
    scenario == "Molly" ~ Molly_consid,
    scenario == "Mike" ~ Mike_consid,
    scenario == "Judy" ~ Judy_consid,
    scenario == "Paul" ~ Paul_consid,
    scenario == "Marco" ~ Marco_consid,
    scenario == "Vida" ~ Vida_consid,
    scenario == "Jared" ~ Jared_consid,
  ),
  push = case_when(
    scenario == "Gina" ~ Gina_push,
    scenario == "Dean" ~ Dean_push,
    scenario == "Molly" ~ Molly_push,
    scenario == "Mike" ~ Mike_push,
    scenario == "Judy" ~ Judy_push,
    scenario == "Paul" ~ Paul_push,
    scenario == "Marco" ~ Marco_push,
    scenario == "Vida" ~ Vida_push,
    scenario == "Jared" ~ Jared_push,
  ),
  reason = case_when(
    scenario == "Gina" ~ Gina_reason,
    scenario == "Dean" ~ Dean_reason,
    scenario == "Molly" ~ Molly_reason,
    scenario == "Mike" ~ Mike_reason,
    scenario == "Judy" ~ Judy_reason,
    scenario == "Paul" ~ Paul_reason,
    scenario == "Marco" ~ Marco_reason,
    scenario == "Vida" ~ Vida_reason,
    scenario == "Jared" ~ Jared_reason,
  )
)

# Only keep what we need
df_mturk %<>% select(`Duration (in seconds)`, RecordedDate, `Q) Jumble`, `ID (MTurk ID)`, `Random ID`, CB, PRIOR, scenario, face, accept, trust, consid, push, reason)

# Remove failed jumble
df_mturk %<>% filter(grepl("cake", `Q) Jumble`, ignore.case = TRUE))

# Remove that weird "they was on the crowd" respondent.
df_mturk %<>% filter(!grepl("They was on the crowd.", reason, ignore.case = TRUE))

is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}
df_mturk %<>% 
  mutate_if(is_all_numeric,as.numeric)

library(lme4)
library(lmerTest)

names(df_mturk) = c("duration", "time", "jumble", "mid", "rid", "cb", "prior", "scenario", "face", "accept", "trust", "consid", "push", "reason")
# Reorder these so they look the same for both graphs
df_mturk$face %<>% factor(levels = c("H", "N", "M"))

# Rename the versions for the graph
df_mturk$prior %<>% factor(levels = c("V", "NV"))

df_mturk %>%
  ggplot(aes(x = face, y = push, group = face, fill = face)) +
  facet_grid(.~prior, labeller = as_labeller(c(`V` = "Violation", `NV` = "Non-Violation"))) + 
  geom_bar(stat = "summary", position = position_dodge(0.9)) +
  geom_errorbar(stat = "summary", position = position_dodge(0.9), width = 0.2, color = "black") +
  scale_fill_manual(values = c("#F3C09D", "#F7995C", "#AD5013")) +
  scale_x_discrete(labels = c("Hidden", "Neutral", "Mean")) +
  xlab("Cue presented at prediction trial") +
  ylab("Likelihood of committing a moral violation") +
  ylim(c(0,100)) +
  ggtitle("Inference of moral violation by cue type", 
          subtitle = "Prior moral violation (left) and no prior violation (right)") +
  bbc_style() +
  theme(legend.position = "none")
    

df_mturk %>%
  ggplot(aes(x = trust, y = push)) +
  geom_jitter(size = 1) +
  geom_smooth(method = "lm", color = "black") +
  xlab("Trustworthiness of character") +
  ylab("Likelihood of committing a moral violation") +
  ggtitle("Trust in character predicts generalizations",
          subtitle = "Effect of trust thermometer ratings on inference of future violation") +
  bbc_style()

# Build contrasts
df_mturk %<>% mutate(face1 = case_when(
  face == "M" ~ 0.5,
  face == "N" ~ -0.5,
  face == "H" ~ 0,
))

df_mturk %<>% mutate(face2 = case_when(
  face == "M" ~ 0,
  face == "N" ~ -0.5,
  face == "H" ~ 0.5,
))

library(lavaan)
model = '
push ~ b*trust
push ~ c*prior + d*face1 + e*face2
trust ~ a*prior

IDE := a*b
DE := c
total := c + (a*b)
'

sem(model, df_mturk) %>% summary(standardized = TRUE, fit.measures = TRUE)

sem()
