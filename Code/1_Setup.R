## CODE FOR MACROINVERTEBRATE COMMUNITY COMPOSITION IN SUB-ALPINE STREAMS ACROSS KAMIKOCHI NATIONAL PARK... ## 

# CODED CREATED BY F. M. WINDSOR
# EXPLORATORY ANALYSES HAVE BEEN EXCLUDED FOR SIMPLICITY/PARSIMONY
# FOR ENQUIRIES PLEASE CONTACT fmwindsor@gmail.com


## 1 - Setup


#### Setup ####

## Get the workspace ready for the following analysis 

# Clear workspace 
rm(list=ls())

# Set working directory
setwd("")

# Load packages
library(ggplot2); library(lme4); library(tidyverse); library(bipartite); library(grid); library(gridExtra)
library(mvabund); library(scales); library(ade4); library(ggrepel); library(Hmisc)



#### Functions ####

## Create the functions that we need for latter analyses

# A function to prepare fuzzy coded data
prep.fuzzy.df<-function (traits, col.blocks){
  if (!is.data.frame(traits)) 
    stop("Data.frame expected")
  if (sum(col.blocks) != ncol(traits)) {
    stop("Non convenient data in col.blocks")
  }
  if (is.null(names(col.blocks))) {
    names(col.blocks) <- paste("FV", as.character(1:length(col.blocks)), sep = "")
  }
  f1 <- function(x) {
    a <- sum(x)
    if (is.na(a)) 
      return(rep(0, length(x)))
    if (a == 0) 
      return(rep(0, length(x)))
    return(x/a)
  }
  k2 <- 0
  col.w <- rep(1, ncol(traits))
  
  for (k in 1:(length(col.blocks))) {
    k1 <- k2 + 1
    if (col.blocks[k]==1) k2<-k1 else k2 <- k2 + col.blocks[k]
    X <- as.matrix(traits[, k1:k2])
    if (col.blocks[k]==1) X[which(X[,1]>0),]<-1 else X <- t(apply(X, 1, f1))
    X.marge <- apply(X, 1, sum)
    X.marge <- X.marge
    X.marge <- X.marge/sum(X.marge)
    X.mean <- apply(X * X.marge, 2, sum)
    nr <- sum(X.marge == 0)
    cat(nr, "missing data found in block", k, "\n")
    traits[, k1:k2] <- X
    col.w[k1:k2] <- X.mean
  }
  attr(traits, "col.blocks") <- col.blocks
  attr(traits, "col.freq") <- col.w
  col.num <- factor(rep((1:length(col.blocks)), col.blocks))
  attr(traits, "col.num") <- col.num
  return(traits)
}



#### Data input ####

## Read in the dataset and get it ready for metric calculation and analysis

# Read in and manipulate macroinvertebrate data
dframe1 <- read.csv("Kamikochi_inverts.csv") # macroinvertebrate data
dframe1[is.na(dframe1)] <- 0 # set all NAs to zero
dframe1 <- data.frame(dframe1[,1:10], empty(dframe1[,-c(1:10)])) # remove empty rows and columns (keep meta-data)

dframe2 <- read.csv("Kamikochi_physicos.csv") # water chemistry variables

dframe3 <- read.csv("chlorophylla.csv") # chlorophyll a concentrations

dframe4 <- read.csv("silica.csv") # silica concentrations (conservative tracer)

dframe5 <- read.csv("precipitation.csv") # precipitation for Kamikochi region

dframe6 <- read.csv("Kamikochi_traits_1.csv", row.names = 1) # trait data for macroinvertebrates

dframe7 <- read.csv("Kamikochi_environment.csv", row.names = 1) # environmental data for individual sites

dframe8 <- read.csv("Azusa_discharge.csv")

dframe9 <- read.csv("Kamikochi_organics.csv")



#### Metric calculation #### 

## Basic community metrics (abundance, species richness, diversity)

# Calculate the specific community metrics for each replicate
dframe1$total_abundance <- rowSums(dframe1[,-c(1:10)])
dframe1$total_abundance_m2 <- dframe1$total_abundance * 10.63
dframe1$species_richness <- specnumber(dframe1[,-c(1:10)]) 
dframe1$shannon_diversity <- vegan::diversity(dframe1[,-c(1:10)])
