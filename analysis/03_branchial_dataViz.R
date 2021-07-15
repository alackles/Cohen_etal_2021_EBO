# @note: This file is part of a paper on heterotopy in epibranchial organs.
#
# @name: Acacia Ackles
# @contact: acacia.ackles@gmail.com
# @date: 2021
#
# @brief: Creates visualizations based on models and data from analysis

# required packages
library(ggplot2)
library(viridis)


# files
proj.dir <- "~/Documents/research/paper-ebo/" #path to this repo
data.dir <- paste(proj.dir, "data/", sep="")
model.fname <- paste(data.dir, "model_df.Rdata", sep="")
fig.dir <- paste (proj.dir, "figs/", sep="")


# load the required data
model.data <- readRDS(model.fname)

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
  plt <- plt + aes(color=Arch.ID, fill=Arch.ID) + 
    facet_wrap(~Arch.Type*Species, scales="free") +
    geom_point(size=4, aes(alpha=Oss.Status, fill=Arch.ID), pch=21) +
    geom_point(pch=1, size=4, aes(color=Arch.ID)) + 
    scale_fill_viridis(discrete=TRUE) +
    scale_colour_viridis(discrete=TRUE) + 
    theme_minimal(base_size = 12)
  
  # show or hide legend?
  if (legend=="off") {
    plt <- plt + theme(legend.position="none", strip.text.x = element_blank()) +
      labs(x="",y="")
  } else {
    plt <- plt + guides(alpha=guide_legend(override.aes=list(shape=16)))
  }
  
  return(plt)
  
}

ggsave("length_blank.pdf", 
       plot= plot_ebo(model.data),
       device="pdf", 
       path = fig.dir, 
       width=12, height=9, dpi=300)

ggsave("length_legend.pdf", 
       plot= plot_ebo(model.data, legend="on"),
       device="pdf", 
       path = fig.dir, 
       width=12, height=9, dpi=300)

ggsave("ratio_blank.pdf",
       plot= plot_ebo(model.data, y="ratio"),
       device="pdf", 
       path = fig.dir, 
       width=12, height=9, dpi=300)

ggsave("ratio_legend.pdf",
       plot= plot_ebo(model.data, y="ratio", legend="on"),
       device="pdf", 
       path = fig.dir, 
       width=12, height=9, dpi=300)
