# @note: This file is part of a paper on heterotopy in epibranchial organs.
#
# @name: Acacia Ackles
# @contact: acacia.ackles@gmail.com
# @date: 2021
#
# @brief: Analyzes branchial arches within species

# required packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)

# define file names
proj.dir <- "~/Documents/researc/paper-ebo/" #path to this repo
data.dir <- paste(proj.dir, "data/", sep="")
fig.dir <- paste (proj.dir, "figs/", sep="")



# load branchial arch data
cb.data <- read.csv((paste(data.dir, "cb_data.csv", sep="")))
eb.data <- read.csv((paste(data.dir, "eb_data.csv", sep="")))