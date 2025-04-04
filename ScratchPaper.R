library(tidyverse)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/AVONETdataset1.csv"

d <- read_csv(f, col_names= TRUE)

d <- d |>
  select(Species1, Family1, Order1, Beak.Length_Culmen, Beak.Width, Beak.Depth, Tarsus.Length, Wing.Length, Tail.Length, Mass, Habitat, Migration, Trophic.Level, Trophic.Niche, Min.Latitude, Max.Latitude, Centroid.Latitude, Primary.Lifestyle, Range.Size)

# Base R function
summary(d)

# Skimr package
library(skimr)
skim(d)

# Which of the variables are categorical and which are numeric?
# categorical = Species1, Family1, Order1, Habitat, Trophic.Level, Trophic.Niche, Primary.Lifestyle
# numeric = Beak.Length_Culmen, Beak.Width, Beak.Depth, Tarsus.Length, Wing.Length, Tail.Length, Mass, Migration, Min.Latitude, Max.Latitude, Centroid.Latitude, Range.Size

# CHALLENGE 1

# STEP 1

d$Migration <- as.factor(d$Migration)

ggplot(data = d |>
         drop_na(Trophic.Level),
       aes(x = Trophic.Level, y = log(Mass))) +
  geom_boxplot()

ggplot(data = d |>
         drop_na(Migration),
       aes(x = Migration, y = log(Mass))) +
  geom_boxplot()

# STEP 2

m1 <- lm(log(Mass) ~ Trophic.Level, data = d)
m2 <- lm(log(Mass) ~ Migration, data = d)

summary(m1)
summary(m2)

# Is log(Mass) associated with either Trophic.Level or Migration category? 
# Yes, in Trophic.Levels ... Herbivore and Scavanger are associated.
# Yes, in Migration... Migration2 & MIgration3 are associated

# That is, in the global test of significance, is the F statistic large enough to reject the null hypothesis of an F value of zero?
# Yes, it is large enough.

# Given the regression coefficients returned for your Migration model, which Migration categor(ies) are different than the reference level? 
# Migration2 & Migration3

# What level is the reference level? 
# 3.77457 - which is Migration1

# Relevel and assess differences among the remaining pair of Migration categories.
levels(d$Migration)

d$Migration <- relevel(d$Migration, ref = "2")
levels(d$Migration)

m3 <- lm(log(Mass) ~ Migration, data = d)
summary(m3)

# Intercept is now Migration2 and all othe other levels have a lower log(Mass)

# STEP 3

pairwise.t.test(log(d$Mass), d$Migration, p.adj = "bonferroni")
m4 <- aov(log(Mass) ~ Migration, data = d) |>
  broom::tidy() |>
  filter(term == "Trophic.Level")

posthoc <- TukeyHSD(m4, which = "Migration", conf.level = 0.95)
plot(posthoc, las = 1)

# STEP 4

library(infer)

obs_F <- d |> 
  mutate(log_Mass = log(Mass)) |>  # Create a transformed column
  specify(response = log_Mass, explanatory = Migration) |> 
  calculate(stat = "F")

null_distribution <- d |> 
  mutate(log_Mass = log(Mass)) |> 
  specify(response = log_Mass, explanatory = Migration) |> 
  hypothesize(null = "independence") |> 
  generate(reps = 1000, type = "permute") |> 
  calculate(stat = "F")

p_value <- null_distribution |> 
  summarise(p_value = mean(stat >= obs_F))

p_value


# CHALLENGE 2

# STEP 1
d <- d |> 
  mutate(log_Mass = log(Mass), log_Beak = log(Beak.Length_Culmen))

beak_m <- lm(log_Beak ~ log_Mass, data = d)

d <- d |> 
  mutate(Relative_Beak_Length = residuals(beak_m))

d <- d |> 
  mutate(log_Tarsus = log(Tarsus.Length))

tarsus_m <- lm(log_Tarsus ~ log_Mass, data = d)

d <- d |> 
  mutate(Relative_Tarsus_Length = residuals(tarsus_m))

# STEP 2

ggplot(d, aes(x = Primary.Lifestyle, y = Relative_Tarsus_Length)) +
  geom_boxplot() +
  labs(title = "Relative Tarsus Length by Primary Lifestyle",
       x = "Primary Lifestyle",
       y = "Relative Tarsus Length") 

ggplot(d, aes(x = Trophic.Niche, y = Relative_Beak_Length)) +
  geom_boxplot() +
  labs(title = "Relative Beak Length by Trophic Niche",
       x = "Trophic Niche",
       y = "Relative Beak Length") 

# STEP 3

migrate_p <- d |> drop_na(Migration)

hist(migrate_p$Range.Size, breaks = 50, main = "Histogram of Range Size", xlab = "Range Size")

qqnorm(migrate_p$Range.Size)
qqline(migrate_p$Range.Size, col = "red")

migrate_z <- migrate_p |>  mutate(log_Range.Size = log(Range.Size))

hist(migrate_z$log_Range.Size, breaks = 50, main = "Histogram of Range Size", xlab = "Log Range Size")

qqnorm(migrate_z$log_Range.Size)
qqline(migrate_z$log_Range.Size, col = "red")

anova_model <- aov(log_Range.Size ~ Migration, data = migrate_z)
summary(anova_model)

ggplot(migrate_z, aes(x = Migration, y = log_Range.Size)) +
  geom_boxplot() +
  labs(title = "Geographic Range Size by Migration Category",
       x = "Migration Category",
       y = "Log(Range Size)") 

# Based on the global model, is range size associated with form of migration? Yes it is.
# How much of the variance in your measure of range size is associated with Migration behavior style? 8.7% is associated
# Given the regression coefficients returned in the output of the model, which Migration categor(ies) are different than the reference level? Migration 2


# What level is the reference level? 2
levels(d$Migration)

d$Migration <- relevel(d$Migration, ref = "1")
levels(d$Migration)

d$Migration <- relevel(d$Migration, ref = "3")
levels(d$Migration)


TukeyHSD(anova_model)



# STEP 4

pass <- d |> 
  filter(Order1 == "Passeriformes")

m1 <- lm(Relative_Beak_Length ~ Primary.Lifestyle, data = pass)
summary(m1)
anova(m1)  

m1 <- aov(Relative_Beak_Length ~ Primary.Lifestyle, data = pass)
summary(m1)

tukey_m1 <- TukeyHSD(aov(Relative_Beak_Length ~ Primary.Lifestyle, data = pass))
print(tukey_m1)

ggplot(pass, aes(x = Primary.Lifestyle, y = Relative_Beak_Length)) +
  geom_boxplot() +
  labs(x = "Primary Lifestyle", y = "Relative Beak Length") 

plot(m1)


m2 <- lm(Relative_Beak_Length ~ Trophic.Level, data = pass)
summary(m2)
anova(m2)  


m2 <- aov(Relative_Beak_Length ~ Trophic.Level, data = pass)
summary(m2)

tukey_m2 <- TukeyHSD(aov(Relative_Beak_Length ~ Trophic.Level, data = pass))
print(tukey_m2)


ggplot(pass, aes(x = Primary.Lifestyle, y = Relative_Beak_Length)) +
  geom_boxplot() +
  labs(x = "Primary Lifestyle", y = "Relative Beak Length") 

plot(m2)


# STEP 5
m3 <- lm(Relative_Beak_Length ~ Primary.Lifestyle * Trophic.Level, data = pass)
summary(m3)
anova(m3)

# Based on the model output, what would you conclude about how relative beak length is related to these two variables?


# STEP 6
m4 <- lm(Relative_Beak_Length ~ Primary.Lifestyle + Trophic.Level + Primary.Lifestyle:Trophic.Level, data = pass)
summary(m4)
anova(m4) 

# Based on the model output, what would you now conclude about how relative beak length is related to these two variables?
# Relative beak length is significantly associated with Primary Lifestyle but not with Trophic Level.
# The Primary Lifestyle categories (such as Generalist, Insessorial, Terrestrial) are statistically significant, meaning they impact the relative beak length of the birds.
# There is no significant interaction between Primary Lifestyle and Trophic Level, suggesting that the effect of one factor does not depend on the level of the other.

# STEP 7

interaction.plot(x.factor = pass$Primary.Lifestyle, 
                 trace.factor = pass$Trophic.Level, 
                 response = pass$Relative_Beak_Length,
                 type = "b", 
                 legend = TRUE,
                 xlab = "Primary Lifestyle", 
                 ylab = "Relative Beak Length", 
                 trace.label = "Trophic Level")

# STEP 8






