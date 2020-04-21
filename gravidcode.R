my_data <- read.csv('gravid.txt', header=T, sep="\t")
head(my_data)
my_data=data.frame(my_data)
levels(my_data$Group)
library(dplyr)
group_by(my_data, Group)%>%
  summarise(
    count = n(),
    mean = mean(Heartbeat_counts, na.rm = TRUE),
    sd = sd(Heartbeat_counts, na.rm = TRUE)
  )


###Visualize your data
install.packages("ggpubr")
# Box plots
# ++++++++++++++++++++
# Plot Heartrate by Group and color by Group
library("ggpubr")
ggboxplot(my_data, x = "Group", y = "Heartrate", 
          color = "Group", palette = c("#00AFBB", "#E7B800"),
          order = c("Nongravid", "Gravid"),
          ylab = "Heartrate", xlab = "Group")

# Mean plots
# ++++++++++++++++++++
# Plot Heartrate by Group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(my_data, x = "Group", y = "Heartrate", 
       add = c("mean_se", "jitter"), 
       order = c("Nongravid", "Gravid"),
       ylab = "Heartrate", xlab = "Group")



##Outliers checking
# libraries we'll use
library(tidyverse) # handy utility functions
library(outliers) # library for identifying outliers


# look for outliers for each different group
ggplot(my_data, aes(Group, Heartbeat_counts)) +
    geom_boxplot() +
    coord_flip() # flip our plot so the labels are easier to read



# get the z-scores for each value in refunt_value
outlier_scores <- scores(my_data$Heartbeat_counts)
# create a dataframe with only outliers
my_data_outliers <- my_data[outlier_scores > 3| outlier_scores < -3, ]
# take a peek
head(my_data_outliers)








##Normal QQ Plot
library(ggplot2)
# Change point shapes by groups
p<-qplot(sample = Heartbeat_counts, data = my_data, shape=Group, color=Group)+
labs(title=" Normal Q-Q plot of heart beat counts",
       y = "Heart beat counts of Gravid and Nongravid per minute")
p





##Compute one-way ANOVA test

# Compute the analysis of variance
res.aov <- aov(Heartbeat_counts ~ Group, data = my_data)
# Summary of the analysis
summary(res.aov)

# 1. Homogeneity of variances
plot(res.aov, 1)

# 2. Normality
plot(res.aov, 2)


library(car)
leveneTest(Heartbeat_counts ~ Group, data = my_data)
fligner.test(Heartbeat_counts ~ Group, data = my_data)

# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
##data  is normally distributed


