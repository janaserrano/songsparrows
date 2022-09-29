#######---
#####  title: "Song sparrow survival through the breeding season in Mandarte Island from 2005 to 2022"
##### author: Janaina Serrano
##### date: August 30, 2022
##### output: pdf_document
#####---

# library(dplyr)
# library(tidyverse)
# library(effects) #for plotting parameter effects
# library(jtools) #for transforming model summaries
# #plotting CI (requires the officer and flextable packages):
# library(officer)
# library(flextable)
# library(ggplot2)
# library("ggstance")

t.cards <- read.csv("C:/Users/janas/OneDrive - McGill University/R/LDP_project/songsparrows/tables/territorycards.csv")
plot(presence_absence ~ year, data=t.cards)
model1 <- glm(presence_absence ~ year, data=t.cards, family=binomial)
summary(model1)
model2 <- lm(presence_absence ~ year, data=t.cards)
summary(model2)
summ(model1)

plot(allEffects(model1))

jtools::plot_summs(model1, scale = TRUE, legend.title = "Year effect")
plot(presence_absence ~ year, data=t.cards, col="red4")
newdat <- data.frame(year=seq(min(t.cards$year), max(t.cards$year),len=13872))
newdat$presence_absence = predict(model1, newdata=newdat, type="response")
lines(presence_absence ~ year, newdat, col="green4", lwd=2)

####or

ggplot(t.cards, aes(x=year, y=presence_absence)) + geom_point() +
  stat_smooth(method="glm", color="green", se=TRUE,
              method.args = list(family=binomial))

t.cards1 <- t.cards %>%
  mutate(presence_absence = if_else(is.na(presence_absence), 1, presence_absence))

ggplot(t.cards1, aes(x=year, y=presence_absence)) + geom_point() +
  stat_smooth(method="glm", color="green", se=TRUE,
              method.args = list(family=binomial))

model2 <- glm(presence_absence ~ year, data=t.cards1, family=binomial)
summary(model2)
