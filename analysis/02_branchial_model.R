# @note: This file is part of a paper on heterotopy in epibranchial organs.
#
# @name: Acacia Ackles
# @contact: acacia.ackles@gmail.com
# @date: 2021
#
# @brief: Builds a model for branchial arches
# Arch Type is treated as a fixed effect while sub-sets ArchID treated as random
#

# required packages
library(lme4)
library(lmerTest)
library(dplyr)
library(tidyr)

# files
proj.dir <- "~/Documents/research/paper-ebo/" #path to this repo
data.dir <- paste(proj.dir, "data/", sep="")
df.fname <- paste(data.dir, "branchial_df.Rdata", sep="")
model.fname <- paste(data.dir, "model_df.Rdata", sep="")
slopes.fname <- paste(data.dir, "model_slopes.csv")

# load the required data
ab.data <- readRDS(df.fname)

# Build Models
length.model <- lmer(data=ab.data, Arch.Length ~ SL*Species*Arch.Type + (0 + SL | Arch.Type:Arch.ID))
ratio.model <- lmer(data=ab.data, Arch.Ratio ~ SL*Species*Arch.Type + (SL | Arch.Type:Arch.ID))

# add model fit to data
ab.data$fitratio <- predict(ratio.model)
ab.data$fitlength <- predict(length.model)

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
