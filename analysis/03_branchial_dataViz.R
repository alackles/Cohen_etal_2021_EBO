# @note: This file is part of a paper on heterotopy in epibranchial organs.
#
# @name: Acacia Ackles
# @contact: acacia.ackles@gmail.com
# @date: 2021
#
# @brief: Creates visualizations based on models and data from analysis

# required packages
library(dplyr)
library(ggplot2)
library(viridis)
library(svglite)


# files
proj.dir <- "~/Documents/research/Cohen_etal_2021_EBO/" #path to this repo
data.dir <- paste(proj.dir, "data/", sep="")
model.fname <- paste(data.dir, "model_df.Rdata", sep="")
fig.dir <- paste (proj.dir, "figs/", sep="")


# load the required data
model.data <- readRDS(model.fname)

# add spelled-out names...
legend.labels <- list(
  "Ceratobranchial 1" = "cb1",
  "Ceratobranchial 2" = "cb2",
  "Ceratobranchial 3" = "cb3",
  "Ceratobranchial 4" = "cb4",
  "Ceratobranchial 5" = "cb5",
  "Epibranchial 1" = "eb1",
  "Epibranchial 2" = "eb2",
  "Epibranchial 3" = "eb3",
  "Epibranchial 4" = "eb4"
)

model.data$Arch <- model.data$Arch.ID
levels(model.data$Arch) <- legend.labels

# find first ossification point
oss.thresholds <- model.data %>% 
  group_by(Species, Arch.Type) %>%
  mutate(xmin=min(SL)) %>%
  filter(Oss.Status=="ossified") %>%
  transmute(xmin = xmin-1, xmax = min(SL), ymin=-Inf, ymax=Inf) %>%
  distinct()

plot_ebo <- function(model.df, y="length", legend="off") {
  
  plt <- ggplot(model.df)
  
  # Determine y-axis
  if (y == "length") {
    plt <- plt + aes(x=SL, y=Arch.Length) + geom_line(aes(y=fit), size=1.2)
  } else if (y == "ratio") {
    plt <- plt + aes(x=SL, y=Arch.Ratio) + geom_line(aes(y=fitratio), size=1.2)
  } else {
    print("ERROR: Invalid y. Check spelling?")
    return(-1)
  }
  
  # Basic plot features
  plt <- plt + aes(color=Arch, fill=Arch) + 
    facet_wrap(Arch.Type ~ Species, scales="free") +
    geom_point(size=4, aes(fill=Arch), pch=21) +
    geom_point(pch=1, size=4, aes(color=Arch)) + 
    geom_rect(data=oss.thresholds, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes=FALSE) +
    geom_vline(data=oss.thresholds, aes(xintercept=xmax), linetype="dotted") +
    scale_fill_viridis(discrete=TRUE) +
    scale_colour_viridis(discrete=TRUE) + 
    theme_minimal(base_size = 12)
  
  # show or hide legend?
  if (legend=="off") {
    plt <- plt + theme(legend.position="none", strip.text.x = element_blank()) +
      labs(x="\n",y="\n")
  } else {
    plt <- plt + guides(fill=guide_legend(override.aes=list(shape=16))) 
  }
  
  return(plt)
  
}

# programmatically generate figures

legend.opts <- c("on", "off")
y.opts <- c("ratio", "length")

for (leg in legend.opts) {
  for (y in y.opts) {
    filetype <- "svg"
    figname <- paste(y, "_", "legend_", leg, ".", filetype, sep="")
    ggsave(figname,
           plot=plot_ebo(model.data, legend=leg, y=y),
           device=filetype,
           path=fig.dir,
           width=16, height=10, dpi=300
           )
  }
}
