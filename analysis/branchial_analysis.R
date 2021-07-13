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
library(dplyr)
library(tidyr)
library(broom)

# define file names
input.cb <- "~/pCloudDrive/research/ebo/data/cb_data.csv"
input.eb <- "~/pCloudDrive/research/ebo/data/eb_data.csv"
output.dir <- "~/pCloudDrive/research/ebo/figures/"

# load branchial arch data data
cb.data <- read.csv(input.cb)
eb.data <- subset(read.csv(input.eb), select=-c(ID)) 

# -------------------------------
# DATA PROCESSING ---------------
# -------------------------------

# re-order ossification status levels to progress logically from cartilaginous to full ossification
eb.data$Oss.Status <- factor(eb.data$Oss.Status, levels = c("cartilage","partial","ossified"))
cb.data$Oss.Status <- factor(cb.data$Oss.Status, levels = c("cartilage", "partial", "ossified"))

# create ratio of arch length to standard length 
eb.data$Arch.Ratio <- eb.data$Arch.Length/eb.data$SL
cb.data$Arch.Ratio <- cb.data$Arch.Length/cb.data$SL


# ------------------------------ CERATOBRANCHIALS ---------------------------
# -------------------------------
# LINEAR MODELING ---------------
# -------------------------------

anchoa.cbdata <- drop_na(subset(cb.data,Species=="anchoa")) 
anchoa.cb1 <- lm(formula = SL ~ Arch.Length, data = subset(anchoa.cbdata,Arch.ID=="cb1"))
anchoa.cb2 <- lm(formula = SL ~ Arch.Length, data = subset(anchoa.cbdata,Arch.ID=="cb2"))
anchoa.cb3 <- lm(formula = SL ~ Arch.Length, data = subset(anchoa.cbdata,Arch.ID=="cb3"))
anchoa.cb4 <- lm(formula = SL ~ Arch.Length, data = subset(anchoa.cbdata,Arch.ID=="cb4"))
anchoa.cb5 <- lm(formula = SL ~ Arch.Length, data = subset(anchoa.cbdata,Arch.ID=="cb5"))

anchoa.ebdata <- drop_na(subset(eb.data, Species=="anchoa"))
anchoa.eb1 <- lm(formula = SL ~ Arch.Length, data = subset(anchoa.ebdata,Arch.ID=="eb1"))
anchoa.eb2 <- lm(formula = SL ~ Arch.Length, data = subset(anchoa.ebdata,Arch.ID=="eb2"))
anchoa.eb3 <- lm(formula = SL ~ Arch.Length, data = subset(anchoa.ebdata,Arch.ID=="eb3"))
anchoa.eb4 <- lm(formula = SL ~ Arch.Length, data = subset(anchoa.ebdata,Arch.ID=="eb4"))


brevoortia.cbdata <- drop_na(subset(cb.data,Species=="brevoortia")) 
brevoortia.cb1 <- lm(formula = SL ~ Arch.Length, data = subset(brevoortia.cbdata,Arch.ID=="cb1"))
brevoortia.cb2 <- lm(formula = SL ~ Arch.Length, data = subset(brevoortia.cbdata,Arch.ID=="cb2"))
brevoortia.cb3 <- lm(formula = SL ~ Arch.Length, data = subset(brevoortia.cbdata,Arch.ID=="cb3"))
brevoortia.cb4 <- lm(formula = SL ~ Arch.Length, data = subset(brevoortia.cbdata,Arch.ID=="cb4"))
brevoortia.cb5 <- lm(formula = SL ~ Arch.Length, data = subset(brevoortia.cbdata,Arch.ID=="cb5"))

brevoortia.ebdata <- drop_na(subset(eb.data, Species=="brevoortia"))
brevoortia.eb1 <- lm(formula = SL ~ Arch.Length, data = subset(brevoortia.ebdata,Arch.ID=="eb1"))
brevoortia.eb2 <- lm(formula = SL ~ Arch.Length, data = subset(brevoortia.ebdata,Arch.ID=="eb2"))
brevoortia.eb3 <- lm(formula = SL ~ Arch.Length, data = subset(brevoortia.ebdata,Arch.ID=="eb3"))
brevoortia.eb4 <- lm(formula = SL ~ Arch.Length, data = subset(brevoortia.ebdata,Arch.ID=="eb4"))


# %>% group_by(Species, Arch.ID) %>% do(m1 = lm(SL~Arch.Length, data=.))


# -------------------------------
# ANALYSIS OF VARIANCE ----------
# -------------------------------

# ANCOVA following http://r-eco-evo.blogspot.com/2011/08/comparing-two-regression-slopes-by.html

# COMPARE DIFFERENT ARCHES WITHIN SPECIES ----------------
comparison <- lm(formula = SL ~ Arch.ID + Arch.Length + Arch.ID:Arch.Length, data = anchoa.cbdata)

aov.anchoa.cb <- aov(data=subset(cb.data, Species=="anchoa"), SL~Arch.Length*Arch.ID)
aov.brevoortia.cb <- aov(data=subset(cb.data, Species=="brevoortia"), SL~Arch.Length*Arch.ID)
aov.danio.cb <- aov(data=subset(cb.data, Species=="danio"), SL~Arch.Length*Arch.ID)


# COMPARE SAME ARCHES ACROSS SPECIES -------------------
aov.cb1 <- aov(data=subset(cb.data, Arch.ID=="cb1"), SL~Arch.Length*Species)
aov.cb2 <- aov(data=subset(cb.data, Arch.ID=="cb2"), SL~Arch.Length*Species)
aov.cb3 <- aov(data=subset(cb.data, Arch.ID=="cb3"), SL~Arch.Length*Species)
aov.cb4 <- aov(data=subset(cb.data, Arch.ID=="cb4"), SL~Arch.Length*Species)
aov.cb5 <- aov(data=subset(cb.data, Arch.ID=="cb5"), SL~Arch.Length*Species)

# COMPARE DIFFERENT ARCHES ACROSS SPECIES --------------------------
# to show interaction effect of arch number & species

# Perform ANCOVA on arches across species
aov.cbs <- aov(data=cb.data, Arch.Length~Arch.ID*Species)

# COMPARE ARCH RATIOS WITHIN SPECIES

aov.anchoa.cb.scaled <- aov(data=subset(cb.data, Species=="anchoa"), SL~Arch.Ratio)
aov.brevoortia.cb.scaled <- aov(data=subset(cb.data, Species=="brevoortia"), SL~Arch.Ratio)
aov.danio.cb.scaled <- aov(data=subset(cb.data, Species=="danio"), SL~Arch.Ratio)

# -------------------------------
# PLOTTING ----------------------
# -------------------------------


cb.plot <- ggplot(data=subset(cb.data, Species=="anchoa" | Species=="brevoortia"), aes(x=SL, y=Arch.Ratio, fill=Species, color=Species)) + 
    geom_point(size=2) +
    #labs(x=plot.xlab, y=plot.ylab) +
    theme_minimal() + 
    facet_wrap(~Arch.ID) + 
    stat_smooth(data=cb.data, aes(color=Species), method="lm",fullrange = TRUE) +
    ggtitle("Ceratobranchials")


# Plot anchoa data 
anchoa.plot.cb <- ggplot(data=subset(cb.data, Species=="anchoa"), aes(x=SL, y=Arch.Length, color=Arch.ID, fill=Arch.ID)) + 
    geom_point(size=2) +
    theme_minimal() +
    stat_smooth(data=subset(cb.data, Species=="anchoa"), method="lm", fullrange=TRUE) +
    ggtitle("A. mitchilli")

# Plot brevoortia data
brevoortia.plot.cb <- ggplot(data=subset(cb.data, Species=="brevoortia"), aes(x=SL, y=Arch.Length, color=Arch.ID, fill=Arch.ID)) + 
    geom_point(size=2) +
    theme_minimal() +
    stat_smooth(data=subset(cb.data, Species=="brevoortia"), method="lm", fullrange=TRUE) +
    ggtitle("B. tyrannus")

# Plot danio data
danio.plot.cb <- ggplot(data=subset(cb.data, Species=="danio"), aes(x=SL, y=Arch.Length, color=Arch.ID, fill=Arch.ID)) + 
    geom_point(size=2) +
    theme_minimal() +
    stat_smooth(data=subset(cb.data, Species=="danio"), method="lm", fullrange=TRUE) +
    ggtitle("D. rerio")

# -----------------------------------
# SAVING ----------------------------
# -----------------------------------

# Save Plots
ggsave("cb_all.pdf", plot=cb.plot, device="pdf", path = output.dir, scale = 1, dpi=300)
ggsave("cb_anchoa.pdf", plot = anchoa.plot.cb, device="pdf", path=output.dir, scale = 1, dpi = 300)
ggsave("cb_brevoortia.pdf", plot = brevoortia.plot.cb, device="pdf", path=output.dir, scale = 1, dpi = 300)
ggsave("cb_danio.pdf", plot = danio.plot.cb, device="pdf", path=output.dir, scale = 1, dpi = 300)


# ------------------------------ EPIBRANCHIALS ---------------------------

# -------------------------------
# ANALYSIS OF VARIANCE ----------
# -------------------------------

# COMPARE DIFFERENT ARCHES WITHIN SPECIES ----------------
aov.anchoa.eb <- aov(data=subset(eb.data, Species=="anchoa"), SL~Arch.Length*Arch.ID)
aov.brevoortia.eb <- aov(data=subset(eb.data, Species=="brevoortia"), SL~Arch.Length*Arch.ID)
aov.danio.eb <- aov(data=subset(eb.data, Species=="danio"), SL~Arch.Length*Arch.ID)


# COMPARE SAME ARCHES ACROSS SPECIES -------------------
aov.eb1 <- aov(data=subset(eb.data, Arch.ID=="eb1"), SL~Arch.Length*Species)
aov.eb2 <- aov(data=subset(eb.data, Arch.ID=="eb2"), SL~Arch.Length*Species)
aov.eb3 <- aov(data=subset(eb.data, Arch.ID=="eb3"), SL~Arch.Length*Species)
aov.eb4 <- aov(data=subset(eb.data, Arch.ID=="eb4"), SL~Arch.Length*Species)

# COMPARE DIFFERENT ARCHES ACROSS SPECIES --------------------------
# to show interaction effect of arch number & species

# Perform ANCOVA on arches across species
aov.ebs <- aov(data=eb.data, Arch.Length~Arch.ID*Species)

# COMPARE ARCH RATIOS WITHIN SPECIES

aov.anchoa.eb.scaled <- aov(data=subset(eb.data, Species=="anchoa"), SL~Arch.Ratio)
aov.brevoortia.eb.scaled <- aov(data=subset(eb.data, Species=="brevoortia"), SL~Arch.Ratio)
aov.danio.eb.scaled <- aov(data=subset(eb.data, Species=="danioa"), SL~Arch.Ratio)

# -------------------------------
# PLOTTING ----------------------
# -------------------------------


eb.plot <- ggplot(eb.data, aes(x=SL, y=Arch.Ratio)) + 
    geom_point(size=4, aes(alpha=Oss.Status, fill=Species, color=Species), pch=21) + 
    scale_alpha_discrete(labels=c("Cartilage", "Partial", "Full"), name="Ossification") +
    guides(alpha=guide_legend(override.aes=list(shape=16))) +
    geom_point(pch=1, size=4, aes(color=Species)) + 
    facet_wrap(~Arch.ID) + 
    stat_smooth(data=eb.data, aes(color=Species, fill=Species), method="lm",fullrange = TRUE) +
    theme_minimal() + 
    ggtitle("Epibranchials")


# Plot anchoa data 
anchoa.plot.eb <- ggplot(data=subset(eb.data, Species=="anchoa"), aes(x=SL, y=Arch.Length, color=Arch.ID, fill=Arch.ID)) + 
    geom_point(size=4, aes(alpha=Oss.Status, fill=Arch.ID, color=Arch.ID), pch=21) + 
    scale_alpha_discrete(labels=c("Cartilage", "Partial", "Full"), name="Ossification") +
    theme_minimal() +
    stat_smooth(data=subset(eb.data, Species=="anchoa"), method="lm", fullrange=TRUE) +
    ggtitle("A. mitchilli")

# Plot brevoortia data
brevoortia.plot.eb <- ggplot(data=subset(eb.data, Species=="brevoortia"), aes(x=SL, y=Arch.Length, color=Arch.ID, fill=Arch.ID)) + 
    geom_point(size=4, aes(alpha=Oss.Status, fill=Arch.ID, color=Arch.ID), pch=21) + 
    scale_alpha_discrete(labels=c("Cartilage", "Partial", "Full"), name="Ossification") +
    theme_minimal() +
    stat_smooth(data=subset(eb.data, Species=="anchoa"), method="lm", fullrange=TRUE) +
    ggtitle("B. tyrannus")

# Plot danio data
danio.plot.eb <- ggplot(data=subset(eb.data, Species=="danio"), aes(x=SL, y=Arch.Length, color=Arch.ID, fill=Arch.ID)) + 
    geom_point(size=4, aes(alpha=Oss.Status, fill=Arch.ID, color=Arch.ID), pch=21) + 
    scale_alpha_discrete(labels=c("Cartilage", "Partial", "Full"), name="Ossification") +
    theme_minimal() +
    stat_smooth(data=subset(eb.data, Species=="danio"), method="lm", fullrange=TRUE) +
    ggtitle("D.rerio")


# SAVING ----------------------------

# Save Plots
ggsave("eb_all.pdf", plot = eb.plot, device="pdf", path=output.dir, scale = 1, dpi = 300)
ggsave("eb_anchoa.pdf", plot = anchoa.plot.eb, device="pdf", path=output.dir, scale = 1, dpi = 300)
ggsave("eb_brevoortia.pdf", plot = brevoortia.plot.eb, device="pdf", path=output.dir, scale = 1, dpi = 300)
ggsave("eb_danio.pdf", plot = danio.plot.eb, device="pdf", path=output.dir, scale = 1, dpi = 300)