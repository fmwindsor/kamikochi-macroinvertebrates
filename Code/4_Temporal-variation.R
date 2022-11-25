# CODE FOR MACROINVERTEBRATE COMMUNITY COMPOSITION IN SUB-ALPINE STREAMS ACROSS KAMIKOCHI NATIONAL PARK... ## 

# CODED CREATED BY F. M. WINDSOR
# EXPLORATORY ANALYSES HAVE BEEN EXCLUDED FOR SIMPLICITY/PARSIMONY
# FOR ENQUIRIES PLEASE CONTACT fmwindsor@gmail.com


## 4 - Temporal variation in macroinvertebrates


#### Setup ####

# Clear environment
rm(list = ls())

# Set working directory
setwd("")

# Load the previous data preparation script
source("Code/1_Setup.R")



#### Hypothesis 3 - Seasonal variation is lesser in groundwater streams ####

## 

# Arrange the fields to represent the macroinvertebrate data 
dframe1$Site <- factor(dframe1$Site, levels(dframe1$Site)[c(1:2,5,3:4,6)])
levels(dframe1$Site) <- c("Shimizugawa","Minamisawa","Bentenzawa", "Dakesawa", "Shirasawa", "Tokusawa")
dframe1$Date <- as.Date(dframe1$Date, "%d/%m/%Y")
intermittent <- subset(dframe1, Intermittence == "Intermittent")

# For statistical results look at the models presented in Hypothesis 1

# Plot the time series data for each stream (facet wrap)
plot7 <- ggplot(aes(y = total_abundance_m2, x = Date), data = dframe1) + 
  geom_rect(xmin = as.Date("2017-07-20"), xmax = as.Date("2017-08-06"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5, data = intermittent) + 
  stat_summary(geom = "bar", fun.y = mean, width = 10, aes(y = total_abundance_m2, x = Date), data = dframe1) + 
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width = 5, aes(y = total_abundance_m2, x = Date), data = dframe1) + 
  theme_bw() +
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), legend.position = "NA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background = element_rect(colour = "NA", fill = "white")) +
  theme(strip.text.x = element_text(hjust = 0, size = 12), panel.grid.major.x = element_blank()) + 
  theme(panel.grid = element_blank()) + 
  facet_wrap(~Site) + 
  scale_x_date(date_breaks = "1 month", minor_breaks = NULL, labels = date_format("%d-%m"), limits = c(as.Date("2017-04-01"),as.Date("2017-12-01"))) + 
  ylab(expression(paste("Mean total abundance (n ",~m^-2,")"))) + 
  xlab("Date") +
  coord_cartesian(ylim = c(0,5000))
plot7

# Assess temporal variation in the abundance of macroinvertebrates across the intermittent streams 
model6 <- glm(total_abundance ~ Site + Date + Site:Date, data = intermittent)

# Validation of the GLM
plot(model6)

# Summarisation of the results of the GLM
anova(model6, test = "F")
summary.lm(model6)
drop1(model6)

# Assess temporal variation in the abundance of macroinvertebrates across the intermittent streams 
model7 <- glm(species_richness ~ Site + Date + Site:Date, data = intermittent)

# Validation of the GLM
plot(model7)

# Summarisation of the results of the GLM
anova(model7, test = "F")
summary.lm(model7)
drop1(model7)

# Assess temporal variation in the abundance of macroinvertebrates across the intermittent streams 
model7 <- glm(shannon_diversity ~ Site + Date + Site:Date, data = intermittent)

# Validation of the GLM
plot(model8)

# Summarisation of the results of the GLM
anova(model8, test = "F")
summary.lm(model8)
drop1(model8)
