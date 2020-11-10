# Written by Manuel, 25th March 2019

# Libraries we will use 

library(dplyr)
library(tidyr) 
library(ggplot2)
library(gridExtra)
library(stringr)
library(purrr)

# Data we will use
setwd("~/Desktop/R studio/PONDR with MoRFs")
input_files = list.files(pattern="*.csv")  # To work with all csv files from the folder
PONDR_files = lapply(input_files, read.csv, stringsAsFactors = F) # read all csv files
names(PONDR_files) <- str_replace(input_files, "_pondr.csv", "") # Get XXXX name from the file

# Plots

pondr_plot <- function(df, name) { # inputs: PONDR data from the website, name of the XXX
  if (str_detect(name, "1")) { # Since XXX1 doesn't have long IDR stretches, we make a plain plot
    rectangle <- data.frame(x1 = c(0), x2 = c(which(df$Domain == 1)), y1 = c(-Inf), y2= c(+Inf))
    ggplot(data = df, mapping = aes(x = Num, y = VLXT)) +
      geom_line() +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
      geom_rect(data=rectangle, 
                mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = "grey", alpha=0.2, inherit.aes=FALSE,
                show.legend = F) +
      theme(panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5),
            aspect.ratio = 0.15) +
      ylab("PONDR score") +
      xlab(NULL) +
      ggtitle(paste0("xXXX",str_replace(name, "XXX", ""))) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
  } else { # For the other 3 we highlight the stretches
  rectangle <- data.frame(x1 = c(0), x2 = c(which(df$Domain == 1)), y1 = c(-Inf), y2= c(+Inf))
  rectangle2 <- data.frame(x1 = c(which(df$Stretch == 1)), x2 = c(which(df$Stretch == 2)), y1 = c(0.52), y2= c(0.48))
  rectangle3 <- data.frame(x1 = c(which(df$Stretch == 3)), x2 = c(which(df$Stretch == 4)), y1 = c(0.52), y2= c(0.48))
  ggplot(data = df, mapping = aes(x = Num, y = VLXT)) +
    geom_line() +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    geom_rect(data=rectangle, 
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = "grey", alpha=0.2, inherit.aes=FALSE,
              show.legend = F) +
    geom_rect(data=rectangle2, 
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = "black", inherit.aes=FALSE,
              show.legend = F) +
    geom_rect(data=rectangle3, 
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = "black", inherit.aes=FALSE,
              show.legend = F) +
    theme(panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          aspect.ratio = 0.15) +
    ylab("PONDR score") +
    xlab(NULL) +
    ggtitle(paste0("xXXX",str_replace(name, "XXX", ""))) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
  }
}

PONDR_plots <- map2(PONDR_files, names(PONDR_files), pondr_plot) # Apply function pondr_plot to all XXXs

grid_plots <- grid.arrange(PONDR_plots$XXX1, PONDR_plots$XXX2, PONDR_plots$XXX3, PONDR_plots$XXX4, ncol = 1) # Make a single plot

ggsave(filename = "PONDR.plots.tiff", grid_plots, dpi = 200) 



