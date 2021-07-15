# @note: This file is part of a paper on heterotopy in epibranchial organs.
#
# @name: Acacia Ackles
# @contact: acacia.ackles@gmail.com
# @date: 2021
#
# @brief: Creates visualizations based on models and data from analysis


###############################################
# 0. DEFINITIONS
###############################################

# required packages
library(ggplot2)
library(viridis)


# files
proj.dir <- "~/Documents/research/paper-ebo/" #path to this repo
data.dir <- paste(proj.dir, "data/", sep="")
df.fname <- paste(data.dir, "branchial_df.Rdata", sep="")
fig.dir <- paste (proj.dir, "figs/", sep="")


# load the required data
ab.data <- readRDS(df.fname)

##########################################
# 1. PRETTY PICTURES
##########################################

#############
# These plots are for ARCH RATIO

plot.cb.ratio <- ggplot(subset(ab.data, Arch.Type=="cerato"), aes(x=SL, y=Arch.Ratio, color=Arch.ID, fill=Arch.ID)) +
  facet_wrap(~Species, scales="free") +
  geom_point(size=4, aes(alpha=Oss.Status, fill=Arch.ID), pch=21) +
  geom_point(pch=1, size=4, aes(color=Arch.ID)) + 
  geom_line(aes(y=fitratio), size=1.5) +
  #geom_ribbon(aes(y=fit, ymin=lwr, ymax=upr), alpha=0.2, color=NA) +
  scale_fill_viridis(discrete=TRUE) +
  scale_colour_viridis(discrete=TRUE) + 
  scale_alpha_discrete(labels=c("Cartilage", "Partial", "Ossified"), name="Ossification") +
  guides(alpha=guide_legend(override.aes=list(shape=16))) +
  theme_minimal(base_size = 12) +
  theme(legend.position="none", strip.text.x = element_blank()) +
  labs(x="",y="") +
  NULL

ggsave("cb_ratio.pdf", plot=plot.cb.ratio, device="pdf", path = fig.dir, width=10, height=5, dpi=300)


plot.eb.ratio <- ggplot(subset(ab.data, Arch.Type=="epi"), aes(x=SL, y=Arch.Ratio, color=Arch.ID, fill=Arch.ID)) +
  facet_wrap(~Species, scales="free") +
  geom_point(size=4, aes(alpha=Oss.Status, fill=Arch.ID), pch=21) +
  geom_point(pch=1, size=4, aes(color=Arch.ID)) + 
  geom_line(aes(y=fitratio), size=1.5) +
  #geom_ribbon(aes(y=fit, ymin=lwr, ymax=upr), alpha=0.2, color=NA) +
  scale_fill_viridis(discrete=TRUE) +
  scale_colour_viridis(discrete=TRUE) + 
  scale_alpha_discrete(labels=c("Cartilage", "Partial", "Ossified"), name="Ossification") +
  guides(alpha=guide_legend(override.aes=list(shape=16))) +
  theme_minimal(base_size = 12) +
  theme(legend.position="none", strip.text.x = element_blank()) +
  labs(x="",y="") +
  NULL

ggsave("eb_ratio.pdf", plot=plot.eb.ratio, device="pdf", path = fig.dir, width=10, height=5, dpi=300)

#############
# These plots are for ARCH LENGTH

plot.cb.length <- ggplot(subset(ab.data, Arch.Type=="cerato"), aes(x=SL, y=Arch.Length, color=Arch.ID, fill=Arch.ID)) +
  facet_wrap(~Species, scales="free") +
  geom_point(size=4, aes(alpha=Oss.Status, fill=Arch.ID), pch=21) +
  geom_point(pch=1, size=4, aes(color=Arch.ID)) + 
  geom_line(aes(y=fitlength), size=1.5) +
  #geom_ribbon(aes(y=fit, ymin=lwr, ymax=upr), alpha=0.2, color=NA) +
  scale_fill_viridis(discrete=TRUE) +
  scale_colour_viridis(discrete=TRUE) + 
  scale_alpha_discrete(labels=c("Cartilage", "Partial", "Ossified"), name="Ossification") +
  guides(alpha=guide_legend(override.aes=list(shape=16))) +
  theme_minimal(base_size = 12) +
  theme(legend.position="none", strip.text.x = element_blank()) +
  labs(x="",y="") +
  NULL

ggsave("cb_length_ci.pdf", plot=plot.cb.length, device="pdf", path = fig.dir, width=10, height=5, dpi=300)


plot.eb.length <- ggplot(subset(ab.data, Arch.Type=="epi"), aes(x=SL, y=Arch.Length, color=Arch.ID, fill=Arch.ID)) +
  facet_wrap(~Species, scales="free") +
  geom_point(size=4, aes(alpha=Oss.Status, fill=Arch.ID), pch=21) +
  geom_point(pch=1, size=4, aes(color=Arch.ID)) + 
  geom_line(aes(y=fitlength), size=1.5) +
  #geom_ribbon(aes(y=fit, ymin=lwr, ymax=upr), alpha=0.2, color=NA) +
  scale_fill_viridis(discrete=TRUE) +
  scale_colour_viridis(discrete=TRUE) + 
  scale_alpha_discrete(labels=c("Cartilage", "Partial", "Ossified"), name="Ossification") +
  guides(alpha=guide_legend(override.aes=list(shape=16))) +
  theme_minimal(base_size = 12) +
  theme(legend.position="none", strip.text.x = element_blank()) +
  labs(x="",y="") +
  NULL

ggsave("eb_length.pdf", plot=plot.eb.length, device="pdf", path = fig.dir, width=10, height=5, dpi=300)