# @note: This file is part of a paper on heterotopy in epibranchial organs.
#
# @name: Acacia Ackles
# @contact: acacia.ackles@gmail.com
# @date: 2021
#
# @brief: Cleanup and pre-processing of raw data

# required packages
library(dplyr)

# define file names
proj.dir <- "~/Documents/research/paper-ebo/" #path to this repo
data.dir <- paste(proj.dir, "data/", sep="")
df.fname <- paste(data.dir, "branchial_df.Rdata", sep="")

# load branchial arch data
branchial.data <- read.csv((paste(data.dir, "branchial_data.csv", sep="")))

# Identify factors 
facs <- c("Species", "Arch.ID", "Arch.Type", "Oss.Status")
branchial.data[,facs] <- lapply(branchial.data[,facs], factor)

#re-order oss status labels so they are in the correct order
branchial.data$Oss.Status <- factor(branchial.data$Oss.Status, levels=c("cartilage", "partial", "ossified"))

# Grab the data we actually want: anchoa and brevoortia only
ab.data <- branchial.data %>% 
  filter(Species=="anchoa" | Species=="brevoortia") %>%
  droplevels() %>%
  {.}

# Create "Arch Ratio", our replacement for Arch Length
ab.data$Arch.Ratio <- ab.data$Arch.Length/ab.data$SL

saveRDS(ab.data, file=df.fname)