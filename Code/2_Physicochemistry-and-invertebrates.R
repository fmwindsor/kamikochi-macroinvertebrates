## CODE FOR MACROINVERTEBRATE COMMUNITY COMPOSITION IN SUB-ALPINE STREAMS ACROSS KAMIKOCHI NATIONAL PARK... ## 

# CODED CREATED BY F. M. WINDSOR
# EXPLORATORY ANALYSES HAVE BEEN EXCLUDED FOR SIMPLICITY/PARSIMONY
# FOR ENQUIRIES PLEASE CONTACT fmwindsor@gmail.com


## 2 - Analysis of physicochemistry


#### Setup ####

# Clear environment
rm(list = ls())

# Set working directory
setwd("")

# Load the previous data preparation script
source("Code/1_Setup.R")



#### Hypothesis 1 - Groundwater streams have higher abundance, species richness and diversity ####

## Hypothesis 1a - Physicochemical variation between streams

# Stream discharge CV differences between stream types
modela <- glm(CV_discharge ~ Regime, data = dframe2)

# Validation of the GLM
plot(modela)

# Summarisation of the results of the GLM
anova(modela, test = 'Chisq')
summary.lm(modela, test = 'Chisq')
drop1(modela)

# Water temperature Cv differences between stream types
modelb <- glm(CV_water_temperature ~ Regime, data = dframe2)

# Validation of the GLM
plot(modelb)

# Summarisation of the results of the GLM
anova(modelb, test = 'Chisq')
summary.lm(modelb, test = 'Chisq')
drop1(modelb)

# Summarise the silica data for Table 1
summary <- do.call(data.frame, aggregate(.~Site, data = dframe4, function(x) c(mean = mean(x), min = min(x), max = max(x))))

# Physicochemical data to look at flow seasonality 
dframe5$Date <- as.Date(dframe5$Date, "%Y/%m/%d")
dframe8$Date <- as.Date(dframe8$Year.Month.Date, "%d/%m/%Y")

# Plot precipiation over time
plot1a <- ggplot(aes(x=Date, y=Precipitation_mm_day), data = dframe5[1:245,]) + 
  geom_rect(xmin = as.Date("2017-07-20"), xmax = as.Date("2017-08-06"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) + 
  annotate(x = as.Date("2017-09-07"), y = 170, geom = "text", label = "Period of flow cessation") + 
  geom_line(colour = "darkblue", size = 1) + 
  theme_bw() +
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), legend.position = "NA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background = element_rect(colour = "NA", fill = "white")) +
  theme(panel.grid = element_blank()) + 
  ylab(expression(paste("Precipitation (mm",~day^-1,")"))) + 
  xlab("") + 
  scale_x_date(date_breaks = "1 month", minor_breaks = NULL, labels = date_format("%d-%m")) +
  scale_y_continuous(breaks = c(0,25,50,75,100,125,150,175), limits = c(0,175)) + 
  ggtitle("A")
plot1a

# Plot discharge over time
plot1b <- ggplot(aes(x=Date, y=Discharge), data = dframe8[2161:8040,]) + 
  geom_rect(xmin = as.Date("2017-07-20"), xmax = as.Date("2017-08-06"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) + 
  geom_line(colour = "skyblue", size = 1) + 
  theme_bw() +
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), legend.position = "NA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background = element_rect(colour = "NA", fill = "white")) +
  theme(panel.grid = element_blank()) + 
  ylab(expression(paste("Discharge (",m^3~s^-1,")"))) + 
  xlab("Date") + 
  scale_x_date(date_breaks = "1 month", minor_breaks = NULL, labels = date_format("%d-%m")) +
  scale_y_continuous(breaks = c(0,50,100,150,200,250), limits = c(0,250)) + 
  ggtitle("B")
plot1b


## Hypothesis 1b - Univariate metrics

# Generalised Linear Model (GLM) for total abundance of macroinvertebrates 
model1 <- lmer(total_abundance ~ Regime + Season + Regime:Season + (1|Site), data = dframe1)
model0 <- lmer(total_abundance ~ 1 + (1|Site), data = dframe1)

# Validation of the GLM
plot(model1)

# Summarisation of the results of the GLM
anova(model1)
summary(model1)
drop1(model1, test = 'Chisq')
r.squaredGLMM(model1)
mcp.fnc(model1)

# Get information on difference from null model
anova(model0, model1, test = "F")

# Barplot of the results of the GLM
plot2 <- ggplot(aes(y = total_abundance_m2, x = Regime), data = dframe1) + 
  stat_summary(aes(fill = Regime), geom = "bar", fun.y = mean, colour = "black", position = position_dodge(1)) + 
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, position = position_dodge(1), width = 0.2) +
  theme_bw() + 
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), legend.position = "NA") +
  theme(panel.grid = element_blank()) + 
  scale_fill_manual(values = c("grey30", "grey60")) + 
  xlab("") + 
  ylab(expression(paste("Mean total abundance (n ",~m^-2,")"))) + 
  coord_cartesian(ylim = c(0,2000)) + 
  #scale_y_continuous(breaks = c(0,25,50,75,100,125,150,175)) + 
  ggtitle("A")
plot2

# Generalised Linear Model (GLM) for species richness of macroinvertebrates 
model2 <- glm(species_richness ~ Regime + Season + Regime:Season, family = poisson (link = "log"), data = dframe1)

# Validation of the GLM
plot(model2)

# Summarisation of the results of the GLM
anova(model2, test = 'Chisq')
summary.lm(model2, test = "Chisq")
drop1(model2)

# Barplot of the results of the GLM
plot3 <- ggplot(aes(y = species_richness, x = Regime), data = dframe1) + 
  stat_summary(aes(fill = Regime), geom = "bar", fun.y = mean, colour = "black", position = position_dodge(1)) + 
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, position = position_dodge(1), width = 0.2) +
  theme_bw() + 
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), legend.position = "NA") +
  theme(panel.grid = element_blank()) + 
  scale_fill_manual(values = c("grey30", "grey60")) + 
  xlab("Water source") + 
  ylab("Species richness (n)") + 
  coord_cartesian(ylim = c(0,11.5)) + 
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12)) +
  ggtitle("B")
plot3

# Generalised Linear Model (GLM) for shannon diversity of macroinvertebrates 
model3 <- glm(shannon_diversity ~ Regime + Season + Regime:Season, family = gaussian (link = "log"), data = dframe1)

# Validation of the GLM
plot(model3)

# Summarisation of the results of the GLM
anova(model3, test = "F")
summary.lm(model3)
drop1(model3)

# Barplot of the results of the GLM
plot4 <- ggplot(aes(y = shannon_diversity, x = Regime), data = dframe1) +
  stat_summary(aes(fill = Regime), geom = "bar", fun.y = mean, colour = "black", position = position_dodge(1)) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, position = position_dodge(1), width = 0.2) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), legend.position = "NA") +
  scale_fill_manual(values = c("grey30", "grey60")) +
  xlab("") +
  ylab("Shannon's diversity index (1/D)") +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8)) +
  ggtitle("C")
plot4

# Composite plot for Hypothesis 1a and b
grid.arrange(plot2, plot3, plot4, nrow=1)


## Hypothesis 1c - Multivariate analyses 

# Non-metric multidimensional scaling to assess community composition 
nmds <- metaMDS(dframe1[,-c(1:10,58:61)], trymax = 200, distance = "bray", k = 3)

# Collate the information for plotting the results
data.scores <- as.data.frame(scores(nmds)$sites) 
data.scores$Site <- dframe1$Site
data.scores$Regime <- dframe1$Regime

# Plotting the results of the NMDS 
plot5a <- ggplot(aes(x = NMDS1, y = NMDS2), data = data.scores) + 
  geom_point(aes(fill = Regime), size = 5, pch = 21) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), legend.position = c(0.15,0.85),
        legend.background = element_rect(colour = "black")) +
  theme(panel.grid = element_blank()) + 
  scale_fill_manual(values = c("grey30", "grey90")) +  
  coord_cartesian(xlim = c(-1.5,1.5), ylim = c(-1.5,1.5)) +   
  scale_y_continuous(breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5)) + 
  scale_x_continuous(breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5)) + 
  ggtitle("A")
plot5a

data.scores$Site <- factor(data.scores$Site, levels(data.scores$Site)[c(1:2,5,3:4,6)])
levels(data.scores$Site) <- c("Shimizugawa","Minamisawa","Bentenzawa", "Dakesawa", "Shirasawa", "Tokusawa")

plot5b <- ggplot(aes(x = NMDS1, y = NMDS2), data = data.scores) + 
  geom_point(aes(fill = Site), size = 5, pch = 21) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), legend.position = "NA",
        legend.background = element_rect(colour = "black")) +
  theme(strip.text =  element_text(size = 12, hjust = 0), strip.background = element_rect(colour = "NA", fill = "white")) +
  coord_cartesian(xlim = c(-1.5,1.5), ylim = c(-1.5,1.5)) +   
  scale_y_continuous(breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5)) + 
  scale_x_continuous(breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5)) + 
  theme(panel.grid = element_blank()) + 
  facet_wrap(~ Site) + 
  ggtitle("B")
plot5b


# Manipulate the data before modelling 
spp <- mvabund(dframe1[,-c(1:10,58:61)])
Regime <- dframe1$Regime
Season <- dframe1$Season

# Multivariate-GLM (mvabund) for statistical support of the NMDS
model4 <- manyglm(spp ~ Regime + Season, family = "negative_binomial")

# Validation of the M-GLM
residuals.manyglm(model4)
plot(model4)

# Results of the M-GLM 
mvabund.aov <- anova(model4, p.uni = "adjusted")
print(mvabund.aov)
print.manyglm(model4)
summary(model4, p.uni = "unadjusted")
predict.manyglm(model4, p.uni = "adjusted")
best.r.sq(spp ~ Regime + Season)




## Hypothesis 1d - Chlorophyll a data and organic carbon

# Generalised Linear Model (GLM) for shannon diversity of macroinvertebrates 
model5 <- glm((chla_ug_cm2 + 0.01) ~ Regime + Month + Regime:Month, family = Gamma (link = "sqrt"), data = dframe3)

# Validation of the GLM
plot(model5)

# Summarisation of the results of the GLM
anova(model5, test = "F")
summary.lm(model5)
drop1(model5)

# Plot chlorophyll a between the streams 
dframe3$Site <- factor(dframe3$Site, levels(dframe3$Site)[c(1:2,5,3:4,6)])
levels(dframe3$Site) <- c("Shimizugawa","Minamisawa","Bentenzawa", "Dakesawa", "Shirasawa", "Tokusawa")

plot6 <- ggplot(aes(y = (chla_ug_cm2 + 0.01), x = Site, group = Date), data = dframe3) + 
  stat_summary(geom = "bar", fun.y = mean, aes(fill = Site), position = position_dodge(1)) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width = 0.25, position = position_dodge(1)) +
  theme_bw() +
  ylab(expression(paste("Chlorophyll a (",~mu*g~cm^-2,")"))) +
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), legend.position = "NA") +
  theme(panel.grid = element_blank()) + 
  geom_text(aes(label = Date, y = mean(chla_ug_cm2 + 0.01), angle = 90, hjust = 1.5), position = position_dodge(1), check_overlap = T) + 
  coord_cartesian(ylim = c(-1.1,6.5)) +
  geom_vline(linetype = "dashed", xintercept = 1.5) +
  geom_vline(linetype = "dashed", xintercept = 2.5) +
  geom_vline(linetype = "dashed", xintercept = 3.5) +
  geom_vline(linetype = "dashed", xintercept = 4.5) +
  geom_vline(linetype = "dashed", xintercept = 5.5)
plot6

# Generalised Linear Model (GLM) for organic carbon 
model5b <- glm((Dry_weight_mg + 0.01) ~ Regime + Month + Regime:Month + Site, data = dframe9)

# Validation of the GLM
plot(model5b)

# Summarisation of the results of the GLM
anova(model5b, test = "F")
summary.lm(model5b, test = "LRT")
drop1(model5b)
mcp.fnc(model5b)
