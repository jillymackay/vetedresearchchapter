# Variation

library(tidyverse)


set.seed(11)

grades <- tibble(`Normal Population`= rnorm(n = 100, mean = 73, sd = 5),
                 `Normal Population with lots of variation` = rnorm(n = 100, mean = 73, sd = 15)) %>% 
  pivot_longer(cols = c(`Normal Population`, `Normal Population with lots of variation`),
               names_to = "Population",
               values_to = "Grades") %>% 
  group_by(Population) %>% 
  mutate(mean = mean(Grades)) %>% 
  ungroup() %>% 
  mutate(fit = mean-Grades) 

write.csv(grades, "data/Grades.csv")

grades <- read.csv("data/grades.csv")

grades %>% 
  ggplot(aes(x = Grades, fill = Population)) +
  geom_density(alpha = 0.3) +
  facet_wrap(facets = ~ Population, nrow = 3) +
  theme(legend.position = "none") +
  labs(x = "Grade Distribution", y = "Density")


grades %>% 
  ggplot(aes(x = Grades, fill = Population)) +
  geom_histogram(alpha = 0.3) +
  facet_wrap(facets = ~ Population, nrow = 3) +
  labs(x = "Grade Distribution", y = "Density") +
  theme_classic() +
  theme(legend.position = "none")



grades %>% 
  ggplot(aes(x = fit, fill = Population)) +
  geom_histogram(alpha = 0.3) +
  facet_wrap(facets = ~ Population, nrow = 3) +
  labs(x = "Fit Distribution", y = "Density") +
  theme_classic() +
  theme(legend.position = "none")


grades %>% 
  group_by(Population) %>% 
  summarise(min = min(fit),
            max = max(fit),
            sd = sd(Grades),
            mean = mean(Grades))


t.test(subset(grades$Grades,grades$Population == "Normal Population"), mu = 74)
t.test(subset(grades$Grades,grades$Population == "Normal Population with lots of variation"), mu = 74)


big_grades <-  rnorm(n = 10000, mean = 73, sd = 15)
write.csv(big_grades, "data/big_grades.csv")
big_grades <- read.csv("data/big_grades.csv")
mean(big_grades$x)
sd(big_grades$x)
t.test(big_grades, mu = 74)





library(ggstatsplot)
library(easystats)
library(rstanarm)
library(bayestestR)

cool <- read.csv("data/veted_COOLscores.csv")



ggbetweenstats(cool, condition, score)


cool %>% 
  group_by(condition) %>% 
  summarise(median = median(score))

bay_cool <- stan_glm(score ~ condition, data = cool)
pars <- insight::get_parameters(bay_cool)


pars %>% 
  pivot_longer(cols = -`(Intercept)`, names_to = "term", values_to="posterior") %>% 
  ggplot(aes(x = posterior)) +
  geom_density(aes(fill = reorder(term, desc(term)))) +
  scale_fill_manual(values = awtools::spalette) +
  facet_wrap(facets = ~term, nrow = 3) +
  theme_classic() +
  labs(fill = "Model Term", x = "Posterior", y = "Density")

plot(hdi(pars,  ci = c(0.5, 0.75, 0.89, 0.95)))  +
  theme_classic()

describe_posterior(pars)

# What % of data is outside the ROPE
pd_pars <- p_direction(pars)
perc_in_rope5 <- rope(pars, ci = 1)
plot(pd_pars) + theme_classic() 
plot(perc_in_rope5) + theme_classic() 


BF <- bayesfactor_parameters(bay_cool, null = 0)
BF

effectsize::interpret_bf(exp(BF$log_BF[2]), include_value = TRUE, rules = "raftery1995")

       


# Change ROPE


BF2 <- bayesfactor_parameters(bay_cool, null = c(-5, 5))
BF2

effectsize::interpret_bf(exp(BF2$log_BF[2]), include_value = TRUE, rules = "raftery1995")
library(see)
plot(BF)


test_group2_right <- bayesfactor_parameters(bay_cool, direction = ">")
test_group2_right
plot(test_group2_right)
