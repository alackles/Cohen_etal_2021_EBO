# EPIBRANCHIAL ARCH DATA ANALYSIS
#
# Acacia Ackles
# alackles@msu.edu
#
# -------------------------------
# INTRO -------------------------
# -------------------------------

# required packages
library(ggplot2)

# define file names
input <- "~/Documents/hernandez_lab/tidy_eb_data.csv"
output.dir <- "~/Documents/hernandez_lab/"

# load branchial arch data data
eb.data <- read.csv(input)

# -------------------------------
# DATA PROCESSING ---------------
# -------------------------------

# re-order ossification status levels to progress logically from cartilaginous to full ossification
eb.data$Oss.Status <- factor(eb.data$Oss.Status, levels = c("cartilage","partial","full"))

# create ratio of arch length to standard length 
eb.data$Arch.Ratio <- eb.data$Arch.Length/eb.data$SL

# -------------------------------
# ANALYSIS OF VARIANCE ----------
# -------------------------------

# COMPARE DIFFERENT ARCHES WITHIN SPECIES 
aov.anchoa.eb <- aov(data=subset(eb.data, Species=="anchoa"), SL~Arch.Length*Arch.ID)
aov.brevoortia.eb <- aov(data=subset(eb.data, Species=="brevoortia"), SL~Arch.Length*Arch.ID)


# COMPARE SAME ARCHES ACROSS SPECIES 
aov.eb1 <- aov(data=subset(eb.data, Arch.ID=="eb1"), SL~Arch.Length*Species)
aov.eb2 <- aov(data=subset(eb.data, Arch.ID=="eb2"), SL~Arch.Length*Species)
aov.eb3 <- aov(data=subset(eb.data, Arch.ID=="eb3"), SL~Arch.Length*Species)
aov.eb4 <- aov(data=subset(eb.data, Arch.ID=="eb4"), SL~Arch.Length*Species)

# COMPARE RATIOS OF ARCH LENGTH TO BODY LENGTH ACROSS SPECIES

aov.eb1.ratio <- aov(data=subset(eb.data, Arch.ID=="eb1"), SL~Arch.Ratio*Species)
aov.eb2.ratio <- aov(data=subset(eb.data, Arch.ID=="eb2"), SL~Arch.Ratio*Species)
aov.eb3.ratio <- aov(data=subset(eb.data, Arch.ID=="eb3"), SL~Arch.Ratio*Species)
aov.eb4.ratio <- aov(data=subset(eb.data, Arch.ID=="eb4"), SL~Arch.Ratio*Species) 


# PLOTTING ---------------------------

# Set Axis Labels

#for facet wrapping
levels(eb.data$Arch.ID) <- c("eb1" = "EB1", "eb2" = "EB2", "eb3" = "EB3", "eb4" = "EB4")
levels(eb.data$Species)  <- c("anchoa" = "A. mitchilli", "brevoortia" = "B. tyrannus")

#for normal labels
plot.xlab <- "Standard Length (mm)"
plot.ylab <- "Arch Length/Standard Length (mm/mm)"


eb.plot <- ggplot(eb.data, aes(x=SL, y=Arch.Ratio)) + 
  geom_point(size=4, aes(alpha=Oss.Status, fill=Species, color=Species), pch=21) + 
  scale_alpha_discrete(labels=c("Cartilage", "Partial", "Full"), name="Ossification") +
  guides(alpha=guide_legend(override.aes=list(shape=16))) +
  geom_point(pch=1, size=4, aes(color=Species)) + 
  facet_wrap(~Arch.ID) + 
  labs(x=plot.xlab, y=plot.ylab) +
  stat_smooth(data=eb.data, aes(color=Species), method="lm",fullrange = TRUE)


# SAVING ----------------------------

# Save Plots
ggsave("eb_plot.pdf", plot = eb.plot.se, device="pdf", path=output.dir, scale = 1, dpi = 300)

