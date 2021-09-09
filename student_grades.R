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

       