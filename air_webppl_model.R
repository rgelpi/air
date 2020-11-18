library(pacman)
p_load(tidyverse, rwebppl, magrittr)

indep = webppl("
/* Most basic function: Evidence from prior behaviour and facial cues are 
statistically independent, although facial information is weighted more. */
var genDist = Infer({method: 'enumerate'}, function() {
  var prior = flip()
  var face = flip()
  var gen = (prior && flip(0.3)) || (face && flip(0.7)) || flip(0.1)
  return {prior: prior, face: face, gen: gen}
})

var out = mapN(function(x) {sample(genDist)}, 1000)
out
             ")

brn = webppl("
var genDist_2 = Infer({method: 'enumerate'}, function() {
  var prior = flip()
  var face = flip()
  var gen = (face && flip(0.9)) || flip(0.3)
  return {prior: prior, face: face, gen: gen}
})

var out = mapN(function(x) {sample(genDist_2)}, 1000)
out
             ")

dep = webppl("
var genDist_3 = Infer({method: 'enumerate'}, function() {
  var prior = flip()
  var face = flip()
  var gen = (prior ? (face && flip(0.9)) : (face && flip(0.3))) || flip(0.1)
  return {prior: prior, face: face, gen: gen}
})

var out = mapN(function(x) {sample(genDist_3)}, 1000)
out
             ")


recode_val = function(df) {
  df %<>% mutate(genn = if_else(df$gen, 1, 0))
  df %<>% mutate(gen_face = if_else(df$face, "mean", "neutral"))
  df %<>% mutate(gen_prior = if_else(df$prior, "yes", "no"))
  return(df)
}

brn = recode_val(brn)
indep = recode_val(indep)
dep = recode_val(dep)

brn %>% ggplot(aes(x = gen_face, y = genn, fill = gen_face)) +
  facet_grid(.~gen_prior) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(0.9)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.9)) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Cue presented at generalization trial") +
  ylab("Proportion of generalizations about moral violation")

indep %>% ggplot(aes(x = gen_face, y = genn, fill = gen_face)) +
  facet_grid(.~gen_prior) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(0.9)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.9)) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Cue presented at generalization trial") +
  ylab("Proportion of generalizations about moral violation")

dep %>% ggplot(aes(x = gen_face, y = genn, fill = gen_face)) +
  facet_grid(.~gen_prior) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(0.9)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(0.9)) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Cue presented at generalization trial") +
  ylab("Proportion of generalizations about moral violation")
