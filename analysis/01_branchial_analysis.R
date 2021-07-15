# @note: This file is part of a paper on heterotopy in epibranchial organs.
#
# @name: Acacia Ackles
# @contact: acacia.ackles@gmail.com
# @date: 2021
#
# @brief: Analyzes branchial arches within species


###############################################
# 0. DEFINITIONS
###############################################

# required packages
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(ggeffects)

# define file names
proj.dir <- "~/Documents/research/paper-ebo/" #path to this repo
data.dir <- paste(proj.dir, "data/", sep="")
df.fname <- paste(data.dir, "branchial_df.Rdata", sep="")

###############################################
# 1. DATA PRE-PROCESSING
################################################

# load branchial arch data
branchial.data <- read.csv((paste(data.dir, "branchial_data.csv", sep="")))

# Identify factors 
facs <- c("Species", "Arch.ID", "Arch.Type")
branchial.data[,facs] <- lapply(branchial.data[,facs], factor)

# Grab the data we actually want: anchoa and brevoortia only
ab.data <- branchial.data %>% 
  filter(Species=="anchoa" | Species=="brevoortia") %>%
  droplevels() %>%
  {.}

# Create "Arch Ratio", our replacement for Arch Length
ab.data$Arch.Ratio <- ab.data$Arch.Length/ab.data$SL


##############################################
# 2. BUILD THE MODELS
#############################################

length.model <- lmer(data=ab.data, Arch.Length ~ SL*Species*Arch.Type + (0 + SL | Arch.Type:Arch.ID))
ratio.model <- lmer(data=ab.data, Arch.Ratio ~ SL*Species*Arch.Type + (SL | Arch.Type:Arch.ID))

# add model fit to data

ab.data$fitratio <- predict(ratio.model)
ab.data$fitlength <- predict(length.model)

# View the model data
cb.effects.brev <- ggpredict(length.model, terms=c("Arch.ID", "Arch.Type [cerato]"), condition=c(Species="brevoortia"), type="random")
cb.effects.anch <- ggpredict(length.model, terms=c("Arch.ID", "Arch.Type [cerato]"), condition=c(Species="anchoa"), type="random")
eb.effects.brev <- ggpredict(length.model, terms=c("Arch.ID", "Arch.Type [epi]"), condition=c(Species="brevoortia"), type="random")
eb.effects.anch <- ggpredict(length.model, terms=c("Arch.ID", "Arch.Type [epi]"), condition=c(Species="anchoa"), type="random")

##########################################
# 3. SAVE DF & MODEL TO EXPORT
##########################################

saveRDS(ab.data, file=df.fname)
