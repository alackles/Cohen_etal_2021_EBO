# EPIBRANCHIAL ARCH DATA ANALYSIS
#
# Acacia Ackles
# alackles@msu.edu
#
# -------------------------------
# INTRO -------------------------
# -------------------------------

# required packages
library(tidyverse)
library(viridis)

# set directories
input.dir <- "~/pCloudDrive/research/ebo/data/"
output.dir <- "~/pCloudDrive/research/ebo/figures/"

# file input and output names
input.cb <-  "~/pCloudDrive/research/ebo/data/cb_data.csv"
input.eb <-   "~/pCloudDrive/research/ebo/data/eb_data.csv"

# load branchial arch data data
cb.data <- read.csv(input.cb)
eb.data <- read.csv(input.eb)

# colorblind palette

cbPalette <- c("#F0E442","#009E73","#E69F00" )


# -------------------------------
# DATA PROCESSING ---------------
# -------------------------------

# re-order ossification status levels to progress logically from cartilaginous to full ossification
eb.data$Oss.Status <- factor(eb.data$Oss.Status, levels = c("cartilage","partial","full"))
cb.data$Oss.Status <- factor(cb.data$Oss.Status, levels = c("cartilage", "partial", "full"))

# create ratio of arch length to standard length 
eb.data$Arch.Ratio <- eb.data$Arch.Length/eb.data$SL
cb.data$Arch.Ratio <- cb.data$Arch.Length/cb.data$SL


# ------------------------------ CERATOBRANCHIALS ---------------------------

# -------------------------------
# PLOTTING ----------------------
# -------------------------------

# Labels
plot.xlab <- "Standard Length (mm)"
plot.ylab <- "Arch Length/Standard Length (mm/mm)"

#for facet wrapping


#cb.subset <- cb.data[cb.data$Species=="anchoa" | cb.data$Species=="brevoortia",]
cb.subset <- cb.data[cb.data$Species!="danio" & cb.data$Species!="molitrix",]
levels(cb.subset$Arch.ID) <- c("cb1" = "CB1", "cb2" = "CB2", "cb3" = "CB3", "cb4" = "CB4", "cb5" = "CB5")
levels(cb.subset$Species)  <- c("anchoa" = "A. mitchilli", "brevoortia" = "B. tyrannus","danio"="", "molitrix"="H. molitrix")

cb.plot <- ggplot(cb.subset, aes(x=SL, y=Arch.Ratio, fill=Arch.ID, color=Arch.ID)) + 
    geom_point(size=4, aes(alpha=Oss.Status, fill=Arch.ID, color=Arch.ID), pch=21) + 
    scale_alpha_discrete(labels=c("Cartilage", "Partial", "Full"), name="Ossification") +
    guides(alpha=guide_legend(override.aes=list(shape=16))) +
    geom_point(pch=1, size=4, aes(color=Arch.ID)) + 
    scale_fill_viridis(discrete=TRUE) +
    scale_colour_viridis(discrete=TRUE) + 
    theme_minimal(base_size = 12) +
    theme(legend.position="none", strip.text.x = element_blank()) +
    facet_wrap(~Species) + 
    geom_smooth(data=cb.subset, aes(fill=Arch.ID), method="lm", fullrange=T) +   
    labs(x="",y="")

# -----------------------------------
# SAVING ----------------------------
# -----------------------------------

# Save Plots
ggsave("cb_plot_sidebyside_legend_side.pdf", plot=cb.plot, device="pdf", path = output.dir, width=10, height=5, dpi=300)


# ------------------------------ EPIBRANCHIALS ---------------------------

# -------------------------------
# PLOTTING ----------------------
# -------------------------------

# Labels
plot.xlab <- "Standard Length (mm)"
plot.ylab <- "Arch Length/Standard Length (mm/mm)"

eb.subset <- eb.data[(eb.data$Arch.ID=="eb1" | eb.data$Arch.ID=="eb4") & (eb.data$Species=="anchoa" | eb.data$Species == "brevoortia"),]
levels(eb.subset$Arch.ID) <- c("eb1" = "EB1", "eb2" = "EB2", "eb3" = "EB3", "eb4" = "EB4")
levels(eb.subset$Species)  <- c("anchoa" = "A. mitchilli", "brevoortia" = "B. tyrannus","danio"="", "molitrix"="")

eb.plot <- ggplot(eb.subset, aes(x=SL, y=Arch.Ratio, fill=Species, color=Species)) + 
    geom_point(size=4, aes(alpha=Oss.Status, fill=Species, color=Species), pch=21) + 
    scale_alpha_discrete(labels=c("Cartilage", "Partial", "Full"), name="Ossification") +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    guides(alpha=guide_legend(override.aes=list(shape=16))) +
    geom_point(pch=1, size=4, aes(color=Species)) + 
    theme_minimal(base_size = 12) +
    theme(legend.position="none", strip.text.x = element_blank()) +
    facet_wrap(~Arch.ID) + 
    geom_smooth(data=eb.subset, aes(fill=Species), method="lm", fullrange=T) +
    #stat_smooth(data=eb.subset, method = "lm", se = F, fullrange=T) + 
    #stat_smooth(data=eb.subset, method = "lm", geom = "ribbon", fill = NA, linetype = "dashed", fullrange=T) +
    labs(x="", y="") 

# SAVING ----------------------------

# Save Plots
ggsave("eb_plot_legend_side.pdf", plot = eb.plot, device="pdf", path=output.dir, width=5, height=5, dpi = 300)

# Silver Carp Plot

# -------------------------------
# PLOTTING ----------------------
# -------------------------------

# Labels
plot.xlab <- "Standard Length (mm)"
plot.ylab <- "Arch Length/Standard Length (mm/mm)"

#for facet wrapping


sc.subset <- cb.data[cb.data$Species!="danio" & cb.data$Arch.ID=="cb4",]
levels(sc.subset$Arch.ID) <- c("cb1" = "CB1", "cb2" = "CB2", "cb3" = "CB3", "cb4" = "CB4", "cb5" = "CB5")
levels(sc.subset$Species)  <- c("anchoa" = "A. mitchilli", "brevoortia" = "B. tyrannus","danio"="", "molitrix"="H. molitrix")

sc.plot <- ggplot(sc.subset, aes(x=SL, y=Arch.Ratio, fill=Species, color=Species)) + 
    geom_point(size=4, aes(alpha=Oss.Status, fill=Species, color=Species), pch=21) + 
    scale_alpha_discrete(labels=c("Cartilage", "Partial", "Full"), name="Ossification") +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    guides(alpha=guide_legend(override.aes=list(shape=16))) +
    geom_point(pch=1, size=4, aes(color=Species)) + 
    theme_minimal(base_size = 22) +
    theme(legend.position="none", strip.text.x = element_blank()) +
    geom_smooth(data=cb.subset, aes(fill=Species), method="lm", fullrange=T) +
    geom_smooth(data=subset(sc.subset, Species=="H. molitrix"), method="lm", fullrange=T, alpha=0.1, linetype="dotted") +
    #stat_smooth(data=eb.subset, method = "lm", se = F, fullrange=T) + 
    #stat_smooth(data=eb.subset, method = "lm", geom = "ribbon", fill = NA, linetype = "dashed", fullrange=T) +
    #stat_smooth(data=subset(sc.subset, Species=="H. molitrix"), method = "lm", se = F, fullrange=T, linetype="dotted") + 
    #stat_smooth(data=subset(sc.subset, Species=="H. molitrix"), method = "lm", geom = "ribbon", fill = NA, linetype = "dotted", fullrange=T) +
    labs(x="",y="")

ggsave("sc_inset_filled.pdf", plot = sc.plot, device="pdf", path=output.dir, width=6, height=6, dpi = 300)
