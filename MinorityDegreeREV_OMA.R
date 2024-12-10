library(olsrr)
library(tidycensus)
library(tidyverse)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggcorrplot)

# Load data
setwd("C:/Users/hanna/Onedrive/Desktop/projfile") 
revenue <- read_csv("tuition.csv")

# Filter missing data
revenue <- na.omit(revenue)
ls(revenue)
revenue <- revenue %>% select(-c("Institution_Name","UnitID"))

#new variables
blkdegree <- revenue$Black_Degree/revenue$totdegree *100
hisdegree <- revenue$hispanicdegrees/revenue$totdegree *100
year <- revenue$Year
totrev <- revenue$totrev

#create smaller data
blkhisrev <- revenue %>% select(-c("instate_tuition", "Year","totdegree"))
ls(blkhisrev)
View(blkhisrev)

#scatterplot
pairs(blkhisrev, main = "Black and Hispanic Enrollment and Public Institution Revenues", upper.panel = NULL)
ls(blkhisrev)

#correlation
# Example data: replace 'your_data' with your actual dataset
# Compute correlation matrix
cor_matrixmod2 <- cor(blkhisrev)

# Rename the row and column names in the correlation matrix
colnames(cor_matrixmod2) <- c("Revenue", "Black", "Hispanic", "Total Students", "Student Aid", "Federal Funding")  # New names
rownames(cor_matrixmod2) <- c("Revenue", "Black", "Hispanic", "Total Students", "Student Aid", "Federal Funding")

corrplot(cor_matrixmod2,
         method = "circle",     # Type of visualization ('ellipse', 'number', 'shade', etc.)e
         tl.col = "black",      # Text label color
         tl.srt = 45,           # Text label rotation
         col = colorRampPalette(c("orange3", "pink", "#5548e4"))(200) # Gradient color scale
       )   # Adjust margins

#correlation
mtrx <- cor(blkhisrev)
corrplot(mtrx)

#multivariate regression
multi_revs <- lm(blkhisrev$totrev ~ blkhisrev$Black_Degree + blkhisrev$hispanicdegrees, data=blkhisrev)
summary(multi_revs)

#model 1
ls(revenue)
revenue_mod1 <- revenue %>% select(-c("FederalFunding", "FederalFunding", "StudentFedAid_Percent","Year","instate_tuition", "Total_Students", "totdegree"))
ls(revenue_mod1)

#scatterplot
pairs(revenue_mod1, main = "Black and Hispanic Degree Attainment and Public Institution Revenues", upper.panel = NULL)

pairs(revenue_mod1,
      main = "Minority Degree Attainment and Public Institution Revenues",
      labels = c("Total Revenue", "Black Degree", "Hispanic Degree"),  # Custom labels for the plot
      cex.labels = 1.5,  # Increase label size
      gap = 0.5,
      upper.panel = NULL)  # Reduce gap between panels

#correlation
# Example data: replace 'your_data' with your actual dataset
# Compute correlation matrix
cor_matrix <- cor(revenue_mod1)

# Rename the row and column names in the correlation matrix
colnames(cor_matrix) <- c("Total Revenue", "Black Degree", "Hispanic Degree")  # New names
rownames(cor_matrix) <- c("Total Revenue", "Black Degree", "Hispanic Degree")


corrplot(cor_matrix, 
         main = "Model 1: Black and Hispanic Enrollment and Public Institution Revenues",
         method = "circle",     # Type of visualization ('ellipse', 'number', 'shade', etc.)
         type = "lower",        # Show only lower triangle
         tl.col = "black",      # Text label color
         tl.srt = 45,           # Text label rotation
         addCoef.col = "black", # Add correlation coefficients
         col = colorRampPalette(c("magenta", "#edeced", "#5548e4"))(200), # Gradient color scale
         title = "Correlation Matrix",
         mar = c(0, 0, 1, 0))   # Adjust margins


#HBCU vs PWI
setwd("C:/Users/hanna/Onedrive/Desktop/projfile") 
hbcu <- read.csv("HBCU.csv")
View(hbcu)
hbcu <-na.omit(hbcu)

hbcurev <- hbcu$HBCUrevstudent

#descriptive statistics hbcu revenue per student
mean(hbcurev)
median(hbcurev)
sd(hbcurev)
var(hbcurev)

setwd("C:/Users/hanna/Onedrive/Desktop/projfile") 
pwi <- read.csv("AVGstudentrev.csv")
pwi <- na.omit(pwi)
View(pwi)

pwirev <- pwi$RevenueperStudent

#descriptive statistics pwi revenue per student
mean(pwirev)
median(pwirev)
sd(pwirev)
var(pwirev)

hist(hbcurev,
     main = "HBCU Revenues",
     xlab = "Revenue Per Student",
     breaks = 12)

ggplot(hbcu, aes(x = hbcurev)) +
  geom_histogram(fill = "sienna3", color = "white") +
  labs(title = "HBCU Revenue per Student", x = "Revenue per Student", y = "Frequency")

# Create the histogram
ggplot(pwi, aes(x = pwirev)) +
  geom_histogram(binwidth = 8000, fill = "#5548e4", color = "white", alpha = 0.9) +
  labs(title = "HBCU Revenue per Student", x = "Revenue per Student", y = "Frequency") +
  scale_x_continuous(limits = c(40000, 200000), labels = scales::comma) +
  scale_y_continuous(limits = c(0, 60)) +
  labs(title = "PWI Revenues", 
       x = "Revenues per Student", 
       y = "Frequency")
