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
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)

# define file names
proj.dir <- "~/Documents/research/paper-ebo/" #path to this repo
data.dir <- paste(proj.dir, "data/", sep="")
fig.dir <- paste (proj.dir, "figs/", sep="")

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


##############################################
# 2. BUILD THE MODEL
#############################################

h.model <- lmer(data=ab.data, Arch.Length ~ SL*Species*Arch.Type + (0 + SL | Arch.Type:Arch.ID))
summ.model <- summary(h.model)
coef.model <- coef(summ.model)

###########################################
# 3. PRETTY PICTURES
##########################################
