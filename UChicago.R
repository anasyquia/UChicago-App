# Using Chi-squared tests to evalute fungal presence on caged, bagged, and exposed stigmas

# Import library
library(ggplot2)

# Load data set
# This data set was a remaining file I had from a prior Biology class
fungalData <- read.csv(file.path("/Users/anasyquia/Downloads/analysis_data_fungal_presence.csv"))

# Create a factor for the categorical independent variable 
# Exposed, Caged, or Bagged stigmas
fungalData$TreatmentFactor <- factor(fungalData$Treatment, levels = c("Exposed", "Caged", "Bagged"))

# Create a factor for the categorical dependent variable 
# Fungal presence or no fungal presence
fungalData$StigmaFactor <- fungalData$Fungal_CFU_per_uL > 0

# Vector of ones to count each row
count <- rep(1, nrow(fungalData))

# Generate a table that counts the number of rows (flowers) 
# that have open or closed stigmas in each treatment category
data <- data.frame(count, fungalData$TreatmentFactor, fungalData$StigmaFactor)

# Examine table
data 

# create stacked bar chart
ggplot(data, aes(fill=fungalData$StigmaFactor,  x = fungalData$TreatmentFactor)) +
  geom_bar() +
  guides(fill = guide_legend(title = "Presence of Fungus")) +
  labs(title = "Evaluating Fungal Presence by Flower Treatment", x = "Treatment", y = "Total flowers examined")         

# Perform a Chi-square test on a subset of the data--exposed vs bagged flowers
# Create a function for this
perform_chi_square <- function(data, treatment1, treatment2) {
  subsetData <- subset(data, data$Treatment==treatment1 | data$Treatment==treatment2) #subset
  subsetData$TreatmentFactor <- factor(subsetData$Treatment, levels = c(treatment1, treatment2)) #re-create factors since "Caged" was dropped
  count_table <- table(subsetData$TreatmentFactor, subsetData$StigmaFactor) #create table
  return(chisq.test(count_table))
}

perform_chi_square(fungalData,"Bagged", "Exposed")
