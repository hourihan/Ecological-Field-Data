##LPI analysis##

#set working directory. Important - this is where txt files generated below will go
setwd("C:/Users/Erin.Hourihan/OneDrive - USDA/9-WOD/DSP")

install.packages("dplyr")
library("dplyr")
install.packages("tidyverse")
library("tidyverse")

#read in data from csv into dataframe
LPI_raw <- read.csv("C:/Users/Erin.Hourihan/OneDrive - USDA/9-WOD/DSP/LPI_S2021OK007005_csv.csv")
view(LPI_raw)

#replace blank (empty strings) with NA in all columns. try na_if() ?
LPI_raw[LPI_raw ==""]<-NA

#find N in top_layer and replace with none. (r recognizes N as sample size)
###str_replace(S2021OK007001$top_layer, "N", "none") < this replaces all strings with N% not just N alone
LPI_raw$top_layer[LPI_raw$top_layer=="N"]<-"none"

#put data into a data frame
#S2021OK007001 <- data.frame(S2021OK007001)

#explore the data
#view column names
colnames(LPI_raw)
#dimensions of the data frame
dim(LPI_raw)
#descriptive stats
summary(LPI_raw)


#__________________________________#
####GROUND COVER#### 
#__________________________________#
#count unique hits in the top layer and surface hits with nothing in the canopy 
top <- data.frame(LPI_raw %>% group_by(date) %>% count(top_layer))
#remove rows with none from top. ground cover hits with no canopy will be included below
top <- top %>% filter(!grepl('none', top_layer))
#soil surface hits with nothing in the top layer
surface <- data.frame(LPI_raw %>% group_by(date, surface) %>% summarize(count =sum(top_layer=='none')))

# keep only litter and soil hits with no canopy for GROUND COVER. 
#(remove basal hits on plants, by definition they have top canopy)
#use ^ and $ to specify beginning and end of strings to get exact match
surface <- surface %>% filter(grepl('^L$|^S$', surface))

#rename columns so they can be combined
colnames(surface) #view column names
colnames(top) #view column names
names(top)[names(top)=="top_layer"] <- "ground cover"
names(top)[names(top)=="n"]<- "count"
names(surface)[names(surface)=="surface"] <- "ground cover"
#combine ground cover hits from top canopy and surface. 
#rbind stacks dataframes on top of each other. 
ground.cover <- dplyr::bind_rows(top,surface)
#arrange row by date column
ground.cover <- arrange(ground.cover,date) 
##check data. total points = # of LPI hits
total.points <- ground.cover %>% group_by(date) %>% summarise(total.points=sum(count))

#calculate % cover and add as column pct
ground.cover$pct <- (ground.cover$count/total.points$total.points)*100

#export ground cover as txt file to working directory
write.table(ground.cover, file = "ground.cover.txt", sep ='\t', quote = FALSE, row.names = F)

#____________________________________#
####FOLIAR COVER####
#count total number of plant hits in all canopy layers
#____________________________________#

#create dataframe of distinct species names across all columns
top.names <- LPI_raw %>% group_by(date) %>% count(top_layer)  
names(top.names)[names(top.names)=="top_layer"] <- "foliar"
lyr_1 <- LPI_raw %>% group_by(date) %>% count(layer_1)
names(lyr_1)[names(lyr_1)=="layer_1"] <- "foliar"
lyr_2 <- LPI_raw %>% group_by(date) %>% count(layer_2) #try sapply() or lapply() for strings
names(lyr_2)[names(lyr_2)=="layer_2"] <- "foliar"
#combine top, lyr_1 and lyr_2
foliar1 <- rbind(top.names,lyr_1,lyr_2) %>% as.data.frame()
#aggregate counts across all columns https://r-coder.com/aggregate-r/#Aggregate_by_multiple_columns_in_R
foliar <- aggregate(foliar1$n, list(foliar1$date, foliar1$foliar), FUN = sum)
#rename columns for output
names(foliar)[names(foliar)=="Group.1"] <- "date"
names(foliar)[names(foliar)=="Group.2"] <- "species"
names(foliar)[names(foliar)=="x"] <- "foliar.hits"

foliar <- arrange(foliar,date) #arrange rows by date column
foliar <- foliar %>% filter(!grepl('none', species)) #remove rows with none
#foliar %>% filter(!grepl('NA', species)) #remove rows with NA

#calculate % foliar cover and add as column pct
foliar$pct <- (foliar$foliar.hits/total.points$total.points)*100

#export foliar cover as txt file to working directory
write.table(foliar, file = "foliar.cover.txt", sep ='\t', quote = FALSE, row.names = F)

#________________________________#
###BASAL COVER### 
#________________________________#
#total number of plant hits in soil surface column.
basal.cover <- data.frame(LPI_raw %>% group_by(date) %>% count(surface))
basal.cover <- basal.cover %>% filter(!grepl('^L$|^S$', surface)) ##<remove rows with L and S

#calculate % cover and add as column pct
basal.cover$pct <- (basal.cover$n/total.points$total.points)*100

write.table(basal.cover, file = "basal.cover.txt", sep ='\t', quote = FALSE, row.names = F)
# group by date, calculate %
####________________________________#########

          
###GROUND (soil) SURFACE COVER###
###___________________________###
#count number of unique hits in the soil surface column
ground.surface.cover <- data.frame(LPI_raw %>% group_by(date) %>% count(surface))

#calculate % cover and add as column pct
ground.surface.cover$pct <- (ground.surface.cover$n/total.points$total.points)*100
write.table(ground.surface.cover, file = "ground.surface.cover.txt", sep ='\t', quote = FALSE, row.names = F)

##______________________________###

#try formatting tables 
#install.packages("knitr")#<<for formatting tables
#install.packages("insight")
#install.packages("gt")
#library("knitr") 
#library("insight")
#library("gt")

#____________________________________#
#things to try
#rename soil to bare ground

dim(ground.cover) #dimension of df
#check numbers. sum count and pct
sum(lpi_2022$count)
sum(lpi_2022$pct)

#calculate % cover using group_by date 
pct <- ground.cover %>%
  group_by(date) %>%
  summarize(count)

#add row 
new.row <- list(count, pct)
#add row to sum across all columns
ground.cover2 <- rbind(ground.cover, new.row)

#barplot of ground cover
barplot(ground.cover$count, las=1, xlab = "ground cover", ylab= "count", names.arg = ground.cover$`ground cover`)
