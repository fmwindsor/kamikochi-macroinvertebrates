## CODE FOR MACROINVERTEBRATE COMMUNITY COMPOSITION IN SUB-ALPINE STREAMS ACROSS KAMIKOCHI NATIONAL PARK... ## 

# CODED CREATED BY F. M. WINDSOR
# EXPLORATORY ANALYSES HAVE BEEN EXCLUDED FOR SIMPLICITY/PARSIMONY
# FOR ENQUIRIES PLEASE CONTACT fmwindsor@gmail.com


## 3 - Macroinvertebrate trait analysis


#### Setup ####

# Clear environment
rm(list = ls())

# Set working directory
setwd("")

# Load the previous data preparation script
source("Code/1_Setup.R")



#### Hypothesis 2 - Functional characteristics of macroinvertebrate communities in streams #### 

apply(dframe6,2,sum)>0 -> sel.t # which traits have no data 
traits.s <- dframe6[,sel.t] # remove the traits that have no data
traits.blo <- c(2,5,2,6,3,6,4,5,6)
names(traits.blo) <- c("Size", "Form", "Generations", "Feeding", "Aerial.dispersal", "Attachment", "Pupation", "Flow", "Habitat")
traits.s <- prep.fuzzy.df(traits.s, traits.blo) # generate proportion data (e.g. standardise across trait groups)

# Create the environmental variables dataset
env.s <- dframe7[c(3,5:9)]

# Trim the abundance data to only taxa with trait data available
tax.mat.s <- dframe1[,-c(1:10,58:61)]
tax.mat.s <- tax.mat.s[,rownames(traits.s)]

# RLQ analysis to link traits to communities to sites
afcl.test <- dudi.coa(tax.mat.s, scannf = FALSE)
acpR.test <- dudi.hillsmith(env.s[,-c(1:3)], row.w = afcl.test$lw, scannf = FALSE)
acpQ.test <- dudi.pca(traits.s, row.w = afcl.test$cw, scannf = FALSE)
rlq.test <- rlq(acpR.test, afcl.test, acpQ.test, scannf = FALSE, nf = 2)

plot(rlq.test)
summary(rlq.test)
randtest(rlq.test)

# Taxa scores (Q row scores)
Qca <- rlq.test$lQ
Qca$Taxa <- rownames(Qca)
Qca$Taxa <- gsub("_", " ", Qca$Taxa)

Ax1 <- quantile(Qca$AxcQ1, probs = c(0.1, 0.9)) # Calculate upper and lower 10% of records for axis 1
Ax2 <- quantile(Qca$AxcQ2, probs = c(0.1, 0.9)) # Calculate upper and lower 10% of records for axis 2
Qca_sub <- distinct(rbind(subset(Qca, AxcQ1 < Ax1[1]), subset(Qca, AxcQ1 > Ax1[2]),
                          subset(Qca, AxcQ2 < Ax2[1]), subset(Qca, AxcQ2 > Ax2[2]))) # Subset a dataframe to label plots

# Site scores (R row scores)
Rca <- rlq.test$lR
Rca$Site <- dframe1$Site
Rca$Site <- factor(Rca$Site, levels(Rca$Site)[c(1:2,5,3:4,6)])
levels(Rca$Site) <- c("Shimizugawa","Minamisawa","Bentenzawa", "Dakesawa", "Shirasawa", "Tokusawa")

# Site characteristics (R canonical weights)
Rcw <- rlq.test$l1
Rcw$Env <- rownames(Rcw)
Rcw$xmin <- rep(0, nrow(Rcw))
Rcw$ymin <- rep(0, nrow(Rcw))

# Trait characteristics (Q canonical weights)
Qcw <- rlq.test$co
Qcw$Trait <- rownames(Qcw)
Qcw$Trait <- gsub("_", " ", Qcw$Trait)
Qcw$xmin <- rep(0, nrow(Qcw))
Qcw$ymin <- rep(0, nrow(Qcw))

Qc1 <- quantile(Qcw$Comp1, probs = c(0.1, 0.9)) # Calculate upper and lower 10% of records for axis 1 
Qc2 <- quantile(Qcw$Comp2, probs = c(0.1, 0.9)) # Calculate upper and lower 10% of records for axis 2
Qcw_sub <- distinct(rbind(subset(Qcw, Comp1 < Qc1[1]), subset(Qcw, Comp1 > Qc1[2]),
                          subset(Qcw, Comp2 < Qc2[1]), subset(Qcw, Comp2 > Qc2[2]))) # Subset a dataframe to label plots

# Plotting the R row scores for sites
plota <- ggplot(aes(x = AxcR1, y = AxcR2), data = Rca) + 
  geom_point(aes(fill = Site), pch = 21, size = 5) + 
  theme_bw() +
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), legend.position = c(0.64,0.09), 
        legend.background = element_rect(colour = "black"), legend.direction = "horizontal") +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.title = element_blank()) + 
  ylab("R score (Axis 2)") + 
  xlab("R score (Axis 1)") + 
  ggtitle("A")
plota

# Plotting the Q row scores for taxa 
plotb <- ggplot(aes(x = AxcQ1, y = AxcQ2), data = Qca) + 
  geom_point() + 
  geom_text_repel(aes(x = AxcQ1, y = AxcQ2, label = Taxa), data = Qca_sub) + 
  theme_bw() +
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), legend.position = c(0.3,0.805), 
        legend.background = element_rect(colour = "black"), legend.direction = "horizontal") +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
  ylab("Q score (Axis 2)") + 
  xlab("Q score (Axis 1)") + 
  ggtitle("C")
plotb

# Plotting the canonical weights for taxa (traits)
plotc <- ggplot(aes(x = Comp1, y = Comp2), data = Qcw) + 
  #geom_point() + 
  geom_text_repel(aes(x = Comp1, y = Comp2, label = Trait), size = 4, data = Q_arrows) + 
  geom_segment(aes(x = xmin, xend = Comp1, y = ymin, yend = Comp2), arrow = arrow(type = "open", length = unit(3, "mm")), data = Q_arrows, alpha = 0.3) + 
  theme_bw() +
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12)) +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
  ylab("Q canonical weight (Axis 2)") + 
  xlab("Q canonical weight (Axis 1)") + 
  ggtitle("D")
plotc

# Need to adjust the labels for a clear representation 
R_arrows <- Rcw
R_arrows$RS1 <- R_arrows$RS1 + c(0,0,0,-0.06,0.05,-0.18,-0.22,0.12,0.212,0)
R_arrows$RS2 <- R_arrows$RS2 + c(0.04,-0.03,0.04,-0.05,-0.1,0.06,0.06,0.06,0.08,0.06)

# Plotting the canonical weights for sites (environmental variables)
plotd <- ggplot(aes(x = RS1, y = RS2), data = Rcw) + 
  geom_text(aes(label = c("Season autumn", "Season spring", "Season summer", "Stream groundwater",
                          "Stream snowmelt", "Regime intermittent", "Regime permanent", "Mean water temperature", 
                          "Min water temperature", "Max water temperature"), size = 4, x = RS1, y = RS2), data = R_arrows) + 
  geom_segment(aes(x = xmin, xend = RS1, y = ymin, yend = RS2), arrow = arrow(type = "open", length = unit(3, "mm")), alpha = 0.3) + 
  theme_bw() +
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), legend.position = "NA") +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
  ylab("R canonical weight (Axis 2)") + 
  xlab("R canonical weight (Axis 1)") + 
  ggtitle("B")
plotd

grid.arrange(plota, plotd, plotb, plotc, nrow = 2)

# Fourth corner analysis for the trait data
four.test <- fourthcorner.rlq(rlq.test)
four.test
