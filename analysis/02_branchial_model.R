# @note: This file is part of a paper on heterotopy in epibranchial organs.
#
# @name: Acacia Ackles
# @contact: acacia.ackles@gmail.com
# @date: 2021
#
# @brief: Builds a model for branchial arches
# Simplified linear model 
#

# required packages
library(lme4)
library(lmerTest)
library(effectsize)
library(dplyr)
library(tidyr)

# files
proj.dir <- "~/Documents/research/Cohen_etal_2021_EBO/" #path to this repo
data.dir <- paste(proj.dir, "data/", sep="")
df.fname <- paste(data.dir, "branchial_df.Rdata", sep="")
model.fname <- paste(data.dir, "model_df.Rdata", sep="")
slopes.fname <- paste(data.dir, "model_slopes.csv", sep="")

# load the required data
ab.data <- readRDS(df.fname)

# Build Models
model <- lmer(data=ab.data, Arch.Length ~ SL*Species*Arch.Type + (0 + SL | Arch.Type:Arch.ID))

# Get Effect Sizes
model.effects <- data.frame(anova(model)) %>%
  mutate(eta2=signif(F_to_eta2(F.value, NumDF, DenDF)$Eta2_partial,2), 
         eta2_lowCI=signif(F_to_eta2(F.value, NumDF, DenDF)$CI_low,2),
         eta2_hiCI=signif(F_to_eta2(F.value, NumDF, DenDF)$CI_high,2))

# add model fit to data
ab.data$fit <- predict(model)
ab.data$fitratio <- ab.data$fit/ab.data$SL

# model slopes (i am so sorry)
ab.slopes <- ab.data %>% 
  group_by(Species, Arch.ID) %>%
  arrange(SL) %>%
  mutate(Slope = 
           signif((last(Arch.Length) - first(Arch.Length))/(last(SL)-first(SL))), digits=3) %>%
  dplyr::select(Species, Arch.ID, Slope) %>%
  distinct() %>%
  spread(Species, Slope)

# Save and export dataframe with model fits attached for visualization 
saveRDS(ab.data, file=model.fname)
write.csv(ab.slopes, file=slopes.fname)
