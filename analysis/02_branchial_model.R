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
library(ggeffects)

# files
proj.dir <- "~/Documents/research/paper-ebo/" #path to this repo
data.dir <- paste(proj.dir, "data/", sep="")
df.fname <- paste(data.dir, "branchial_df.Rdata", sep="")
model.fname <- paste(data.dir, "model_df.Rdata", sep="")


# load the required data
ab.data <- readRDS(df.fname)

# Build Models
length.model <- lmer(data=ab.data, Arch.Length ~ SL*Species*Arch.Type + (0 + SL | Arch.Type:Arch.ID))
ratio.model <- lmer(data=ab.data, Arch.Ratio ~ SL*Species*Arch.Type + (SL | Arch.Type:Arch.ID))

# View the model data
cb.effects.brev <- ggpredict(length.model, terms=c("Arch.ID", "Arch.Type [cerato]"), condition=c(Species="brevoortia"), type="random")
cb.effects.anch <- ggpredict(length.model, terms=c("Arch.ID", "Arch.Type [cerato]"), condition=c(Species="anchoa"), type="random")
eb.effects.brev <- ggpredict(length.model, terms=c("Arch.ID", "Arch.Type [epi]"), condition=c(Species="brevoortia"), type="random")
eb.effects.anch <- ggpredict(length.model, terms=c("Arch.ID", "Arch.Type [epi]"), condition=c(Species="anchoa"), type="random")

# add model fit to data
ab.data$fitratio <- predict(ratio.model)
ab.data$fitlength <- predict(length.model)

# Save and export dataframe with model fits attached for visualization 
saveRDS(ab.data, file=model.fname)