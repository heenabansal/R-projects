library(MASS)
library(tidyr)
library(faraway)
barley <- immer
head(barley)
str(barley)
colnames(barley) <- c("Location","Barley_Type","1931","1932")

barley2 <- gather(barley,"Year","Yield",c(3,4))

suicide_data <- suicide
head(suicide_data)

suicide_wide <- spread(suicide_data,sex,y)
colnames(suicide_wide) = c("Cause","Age_group","Male_Count","Female_Count")

suicide2 = gather(suicide_wide,"Gender","Count",c(3,4))


hours <- round(runif(50, min = 0, max = 5), 0)
minutes <- round(runif(50, min = 0, max = 60), 0)
seconds <- round(runif(50, min = 0, max = 60), 0)

# Put hours, minutes and seconds together as a data frame:
sample <- data.frame(Hours = hours, Minutes = minutes, Seconds = seconds)

# A peek at the data:
head(sample)

sample$Hours <- ifelse(sample$Hours < 10, paste0(0, sample$Hours), as.character(sample$Hours))
sample$Minutes <- ifelse(sample$Minutes < 10, paste0(0, sample$Minutes), as.character(sample$Minutes))
sample$Seconds <- ifelse(sample$Seconds < 10, paste0(0, sample$Seconds), as.character(sample$Seconds))

head(sample)

united <- unite(sample, Time , Hours, Minutes, Seconds, sep = ":")

# One last look:
head(united)


treat_gender <- c("A_1", "A_2", "B_1", "B_2")
Count <- c(3, 8 , 10, 6)

sample <- data.frame(treat_gender, Count)
sample

separate(sample,treat_gender, into = c("Treat", "Gender"), sep = "_")
