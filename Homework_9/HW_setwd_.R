#Preliminary

library(devtools)
library(usethis)
library(upscaler)
library(tidyverse)
library(dplyr)
library(ggplot2)


#Question 2**
# Using for loops, iterate through each year’s folders to gather the file names of these “countdata” .csv files. 

#setwd("~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/Homework_9")

filesearch <- list.files("~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/Homework_9/NEON_count-landbird")

filenames <- c()

for(i in 1:10) {
  setwd(paste0("~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/Homework_9/NEON_count-landbird","/", filesearch[i]))
  filenames[i] <- list.files(pattern = "countdata")
}

# for (i in 1:10) {
#   path <- paste0("~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/NEON_count-landbird/", filesearch[i])
#   print(path)
#   if (dir.exists(path)) {
#     setwd(path)
#     filenames[i] <- list.files(pattern = "countdata")
#   } else {
#     warning(paste("Directory does not exist:", path))
#   }
# }
filenames



# **Question 3**
#  Pseudocode to generate functions.


#2015
setwd("~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/Homework_9/NEON_count-landbird/NEON.D01.BART.DP1.10003.001.2015-06.basic.20250129T000730Z.RELEASE-2025")
data2015 <-read.csv(file= "NEON.D01.BART.DP1.10003.001.brd_countdata.2015-06.basic.20241118T065914Z.csv")

#2016
setwd("~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/Homework_9/NEON_count-landbird/NEON.D01.BART.DP1.10003.001.2016-06.basic.20250129T000730Z.RELEASE-2025")
data2016 <-read.csv(file="NEON.D01.BART.DP1.10003.001.brd_countdata.2016-06.basic.20241118T142515Z.csv")

#2017
setwd("~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/Homework_9/NEON_count-landbird/NEON.D01.BART.DP1.10003.001.2017-06.basic.20250129T000730Z.RELEASE-2025")
data2017 <-read.csv(file="NEON.D01.BART.DP1.10003.001.brd_countdata.2017-06.basic.20241118T043125Z.csv")

#2018
setwd("~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/Homework_9/NEON_count-landbird/NEON.D01.BART.DP1.10003.001.2018-06.basic.20250129T000730Z.RELEASE-2025")
data2018 <-read.csv(file="NEON.D01.BART.DP1.10003.001.brd_countdata.2018-06.basic.20241118T105926Z.csv")

#2019
setwd("~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/Homework_9/NEON_count-landbird/NEON.D01.BART.DP1.10003.001.2019-06.basic.20250129T000730Z.RELEASE-2025")
data2019 <-read.csv(file="NEON.D01.BART.DP1.10003.001.brd_countdata.2019-06.basic.20241118T064156Z.csv")

#2020
setwd("~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/Homework_9/NEON_count-landbird/NEON.D01.BART.DP1.10003.001.2020-06.basic.20250129T000730Z.RELEASE-2025")
data2020a <-read.csv(file="NEON.D01.BART.DP1.10003.001.brd_countdata.2020-06.basic.20241118T184512Z.csv")

setwd("~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/Homework_9/NEON_count-landbird/NEON.D01.BART.DP1.10003.001.2020-07.basic.20250129T000730Z.RELEASE-2025")
data2020b <-read.csv(file="NEON.D01.BART.DP1.10003.001.brd_countdata.2020-07.basic.20241118T010504Z.csv")

#2021
setwd("~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/Homework_9/NEON_count-landbird/NEON.D01.BART.DP1.10003.001.2021-06.basic.20250129T000730Z.RELEASE-2025")
data2021 <-read.csv(file="NEON.D01.BART.DP1.10003.001.brd_countdata.2021-06.basic.20241118T105538Z.csv")

#2022
setwd("~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/Homework_9/NEON_count-landbird/NEON.D01.BART.DP1.10003.001.2022-06.basic.20250129T000730Z.RELEASE-2025")
data2022 <-read.csv(file="NEON.D01.BART.DP1.10003.001.brd_countdata.2022-06.basic.20241118T033934Z.csv")

#2023
setwd("~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/Homework_9/NEON_count-landbird/NEON.D01.BART.DP1.10003.001.2023-06.basic.20250129T000730Z.RELEASE-2025")
data2023 <-read.csv(file="NEON.D01.BART.DP1.10003.001.brd_countdata.2023-06.basic.20241118T091043Z.csv")



# Cleaning the data for any empty/missing cases
remove_missing_rows <- function(filename){
  cleaned_data <- na.omit(filename)
  return(cleaned_data)
}

# Calculate Abundance for each year (Total number of individuals found)
calculate_abundance <- function(filenames) {
  abundance <- nrow(filenames)
  return(abundance)
}

calculate_richness <- function(filenames) {
  richness <- n_distinct(filenames$scientificName)
  return(richness)
}


# Run a simple regression model for Species Richness (S) vs. Abundance for every year
data <- data.frame()

#2015
remove_missing_rows(data2015)
a <- calculate_abundance(data2015)
A <- calculate_richness(data2015)
aa <- c(2015, a,A)

#2016
remove_missing_rows(data2016)
b <- calculate_abundance(data2016)
B <- calculate_richness(data2016)
bb <- c(2016,b,B)

#2017
remove_missing_rows(data2017)
c <- calculate_abundance(data2017)
C <- calculate_richness(data2017)
cc <- c(2017,c,C)

#2018
remove_missing_rows(data2018)
d <- calculate_abundance(data2018)
D <- calculate_richness(data2018)
dd <- c(2018,d, D)

#2019
remove_missing_rows(data2019)
e <- calculate_abundance(data2019)
E <- calculate_richness(data2019)
ee <- c(2019,e,E)

#2020a
remove_missing_rows(data2020a)
f <- calculate_abundance(data2020a)
f0 <- calculate_richness(data2020a)
ff <- c(2020,f,f0)

#2020b
remove_missing_rows(data2020b)
g <- calculate_abundance(data2020b)
G <- calculate_richness(data2020b)
gg <- c(2020,g,G)

#2021
remove_missing_rows(data2021)
h <- calculate_abundance(data2021)
H <- calculate_richness(data2021)
hh <- c(2021,h,H)

#2022
remove_missing_rows(data2022)
i <- calculate_abundance(data2022)
I <- calculate_richness(data2022)
ii <- c(2022,i,I)

#2023
remove_missing_rows(data2023)
j <- calculate_abundance(data2023)
J <- calculate_richness(data2023)
jj <- c(2023,j,J)

data1 <- rbind(aa,bb,cc,dd,ee,ff,gg,hh,ii,jj)
colnames(data1) <- c("year","abundance", "richness")
data1

write.csv(data1, "~/Desktop/UVM/CompBio Spring2025/CompBio/Week 2/test2025/Homework_9/Raw_data.csv")
