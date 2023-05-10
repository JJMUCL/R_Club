## R Club - Getting Started in R for Epidemiology ##

# Chapter 1: Getting started, bringing in data & exploring your data
# Chapter 2: Visualising further: Summarising your data before or after cleaning
# Chapter 3: Data Cleaning
# Chapter 4: Basic Visualisation / Normality checks 
# Chapter 5: Modularizing' a repetitive cleaning task 
# Chapter 6: Quickstart Table 1 & Extracting values for table-building 
#
#



##----- [Chapter 1]: Getting started, bringing in Data & exploring your data -----##

# Install & Loading packages
library(haven)
install.packages("tidytuesdayR")
library(tidyverse, tidytuesdayR, dplyr)

# Set working directory
setwd("/Users/admin/Documents/7. UCL E:S:PA/R Study Group")

# Import dataset (careful of your filetype)
#olympics <- read.csv()  
#Stata files:  olympics <- read_dta()
#.tab files:   olympics <- read.table()
tuesdata <- tidytuesdayR::tt_load('2021-07-27')
olympics <- tuesdata$olympics
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')


# Exploring the dataset
head(olympics)
tail(olympics)
nrow(olympics)
dim(olympics)
describe(olympics)
str(olympics)

library(skimr)
skim_without_charts(olympics)

#Visualising your variables 
#hist(x)
#plot(x)
#plot(x,y)
ggplot(olympics, aes(x=age)) + geom_histogram()

# Summarize categorical variable 
table(olympics$sex, useNA = "always") 
# Crosstabulate
table(olympics$sex, olympics$age)
prop.table((table(olympics$sex)))

olympics %>%
  count(sex) %>%
  mutate(percent = prop.table(n) * 100) %>%
  rename(n = "n", percent = "percent") 


#Create a function with two arguments (data, variable) to save repeating this process for multiple variables..#
sum_catpercent <- function(data, variable) {
  data %>%
    count({{variable}}) %>%
    mutate(percent = prop.table(n) * 100) %>%
    rename(n = "n", percent = "percent")
}

# Call this new table function with the dataset and variable specified
sum_catpercent(olympics, sex)
sum_catpercent(olympics, medal)


# Summarizing your continuous variable 
summary(olympics$age)
mean(olympics$age, na.rm = TRUE)
sd(olympics$age, na.rm = TRUE)


# Summarize continuous variable by categorical variable (assuming "gender" is categorical variable and "age" is continuous variable)
aggregate(olympics$age, by = list(sex = olympics$sex), FUN = summary)

#----------------------------- [END OF CHAPTER 1] -----------------------------------------##



##----- [Chapter 2]: Visualising further: Summarising your data before or after cleaning ----##

regions <- tt_data$regions
tt_data <- tt_load("2021-07-27")
df <- tt_data$olympics
regions <- tt_data$regions

####Summary Tools Package#####
install.packages("summarytools")
library(summarytools)
##NB: summarytable functions can handle sampling weights!##

##Neat tables, NA's and Cumulative/non-cumulative Proportions
freq(df$medal) #with NAs
freq(df$medal,report.nas = FALSE) #no NAs
freq(df$medal,totals = FALSE, cumul = FALSE) #no totals and cumulative %
freq(df$medal,totals = FALSE, cumul = FALSE, headings = FALSE) #headings not working
freq(df$medal,totals = FALSE, cumul = FALSE, style = "rmarkdown") 

#to order frequencies
freq(df$medal, order = "freq")
freq(df$medal, order = "-freq") 

#stby to group by other variable
with(df, stby(medal, sex, freq)) 
with(df, stby(medal, sex, freq, totals = FALSE, cumul = FALSE, style = "rmarkdown")) 

#subsetting by groups of interest
freq(df$sport, order = "freq", rows = 1:3)
freq(df$sport, order = "freq", rows = 1:5)

#write your table of results to a text file to show to your supervisor!
freq_sport <- freq(df$sport, order = "freq", rows = 1:5)
print(freq_sport, method = "pander", file = "freq_sporttext.txt")




#####2. Cross tabulation##### 
?ctable
#for categorical variables
ctable (x = df$medal, y = df$sex) #shows row proportions by default
ctable (x = df$medal, y = df$sex, prop = "c") #columns proportions
ctable (x = df$medal, y = df$sex, prop = "n") #no prop
ctable (x = df$medal, y = df$sex, prop = "t") #total

ctable (x = df$medal, y = df$sex, useNA = "no") #no NA (can change for "always" or "ifany")
ctable (x = df$medal, y = df$sex, prop = "c", totals = FALSE, justify = "c", style = "rmarkdown")


##Descriptive Statistics from Tables
#Chi-square, OR and RR
library(magrittr)
df %$% #$ works like with
  ctable (x = df$medal, y = df$sex,
          chisq = TRUE,
          totals = FALSE,
          prop = "c"
  )

# OR and RR only work with 2x2 tables
freq(df$age)
df$age_bin <- as.factor(ifelse(df$age < 18, 1, 0))

df %$% 
  ctable (x = df$age_bin, y = df$sex,
          chisq = TRUE,
          OR = TRUE,
          RR = TRUE,
          totals = FALSE,
          prop = "c"
  )

#statistics for categories of one variable
with(df, 
     stby(data    = list(x = sex, y = medal), 
          INDICES = age_bin, 
          FUN     = ctable,
          chisq = TRUE
     ))

##Descriptive (Univariate) Statistics## 
#generate common central tendency statistics and measures of dispersion for numerical data
?descr()

desc_df <- descr(df, style = "rmarkdown")
desc_df[1]

#ignores non-numerical variables (strings and factors)
descr(df,
      var = age, #for 1 var
      stats = c("mean", "sd"), #which stats to show
      transpose = TRUE #stats as columns, vars as rows
)

#statistics for categories of one variable
with(df,
     stby(data = height,
          INDICES = age_bin,
          FUN = descr,
          stats = c("mean", "sd", "min", "max")
     )
)

#tables can be transformed into tibbles
df_tib <- df %>%
  descr(stats = "common") %>%
  tb()

#to customise the functions you can ammend the summary table default options (st_options)
?st_options

st_options(
  freq.cumul = FALSE,
  ctable.prop = "c"
)


#----------------------------- [END OF CHAPTER 2] -----------------------------------------##


##----- [Chapter 3]: Data Cleaning -----##

library(skimr)
library(psych)
library(describedata)
library(tidyverse)
library(haven)
library(VGAMdata)
library(boot) 
library(ggplot2)
library(psych)

data(oly12)

##Summarising using pipes##
oly12 %>% select(-c('DOB', 'Name', 'Country')) %>% describe()
oly12 %>% select(c('Age', 'Weight', 'Sex')) %>% map(table)

##Remove Missing Vars, creating complete cases, or otherwise subsetting using indexing##
CompleteDOB <- oly12[!is.na(oly12$DOB), ]
First5Cols <- oly12[ , 1:5]
First5Rows <- oly12[1:5,]
NameAge <- oly12[ , c("Age", "DOB")]
Missing <- oly12[c(is.na(oly12$DOB) | is.na(oly12$Height) | is.na(oly12$Weight)), ]
##Missingness Visualisation using Naniar package
gg_miss_var(Missing)

CompleteObs <- oly12[complete.cases(oly12), ]
CompleteObs_subset <- oly12[complete.cases(oly12[, c('DOB', 'Name')]), ]

Temp_Cols <- subset(oly12, select=c("Name", "Age", "Weight"))
oly12 %>% subset(is.na(DOB)) %>% count()

##Aligning ID names especially before a merge using dplyr rename or 'names' 
oly12 <- dplyr::rename(oly12, "ID" = "Name")
CompleteDOB <- dplyr::rename(CompleteDOB, "ID" = "Name")
#quick but names requires exhaustive specification of column names
names(Temp_Cols) <- c("ID", "Years")


##Merging
mergeolycols <- merge(CompleteDOB, Temp_Cols, by='ID', all=F)
mergeolycols <- merge(CompleteDOB, Temp_Cols, by='ID', all.y = T)
mergeolycols <- merge(CompleteDOB, Temp_Cols, by='ID', all.x = T)
##logical; if TRUE, then extra rows will be added to the output, one for each row in x that has no matching row in y.##


##1 Subset, 2clean, 3merge  cleaning sequence ##
#1 subset (preserve the raw data from cleaning)
medals <- subset(oly12, select=c("ID", "Gold", "Silver", "Bronze"))
map(medals[,2:4], table)

#2 clean in subset df
##Create a summary/composite variable##
medals$doublemedalists <- as.numeric(NA)

for (i in 1:length(medals$ID)) {
  if ((medals$Gold[i]>1) &        (!is.na(medals$Gold[i]))) {
    medals$doublemedalists[i] <- 3 }
  else if (( (medals$Gold[i] + medals$Silver[i] + medals$Bronze[i])>=2) &                           (!is.na(medals$Gold[i]) & !is.na(medals$Silver[i]) & !is.na(medals$Bronze[i]))) {
    medals$doublemedalists[i] <- 2 }
  else if (( (medals$Gold[i] + medals$Silver[i] + medals$Bronze[i])>=1) &                           (!is.na(medals$Gold[i]) & !is.na(medals$Silver[i]) & !is.na(medals$Bronze[i]))) {
    medals$doublemedalists[i] <- 1 }
  else { medals$doublemedalists[i] <- 0 }
}

table(medals$doublemedalists, useNA="always")
table(medals$Gold)

#3 Send back to your old or new DF #(unless the df rows differs in which case merge)##
oly12$doublemedals <- medals$doublemedalists


##Collapse with recode or mutate##
##Recode requires exhaustive specification of vars##
oly12$Weight_Collapse <- dplyr::recode(oly12$weight_cats, "4"=2, "3"=2, "2"=1, "1"=1) 
table(oly12$Weight_Collapse, useNA = "always")

##Standardised scores - 
oly12$weight_z <- scale(oly12$Weight)

##Calculating a new variable with pipes (even when there's missingness in your column)
oly12$BMI <- NA
oly12 <- oly12 %>% mutate(BMI = ifelse(!is.na(Weight) & !is.na(Height), Weight/(Height)^2, NA))

##Formatting your final variables before modelling using baseR (e.g. Factor Vars with levels), else 'as.numeric()', 'as.character()'.
oly12$doublemedals <- factor(oly12$doublemedals, order = TRUE, levels = c("0", "1", "2", "3"))

##Create unique numeric ID's rather than text ID's can be important for some model packages## 
oly12$NumID <-  with(oly12, match(ID, unique(ID)))

##Pivot Wide/Long##
oly12_long <- oly12 %>% mutate(Gold16 = Gold)
oly12_long <- oly12_long  %>% pivot_longer(cols=c('Gold', 'Gold16'),
                                           names_to='Year',
                                           values_to='Win')



#----------------------------- [END OF CHAPTER 3] -----------------------------------------##


##----- [Chapter 4]: Basic Visualisation / Normality checks -----##


##Visualise## - Distributions and missingness before more processing 
##assumptions checking & model building##
#install.packages("naniar")

library(naniar)
vis_miss(cleandf)
gg_miss_var(cleandf)

ggplot(cleandf, aes(x=Weight)) + 
  geom_histogram(binwidth=1)

plot(oly12$Weight, oly12$Height)

##Assess normality
library(ggpubr)
ggqqplot(oly12$Weight)
qqPlot(oly12$Weight)
ggdensity(oly12$Weight)
barplot(table(cleandf$doublemedals, useNA="always"))

#install.packages("moments")
library(moments)
skewness(oly12$Weight, na.rm=T)
#1 - expected, +ve left skew; -ve right skew
kurtosis(oly12$Weight, na.rm=T)
#1 - expected, +ve kurtosis (pointy) ; -ve kurtosis (chubby)


##Overlaid plots - StackOverflow
x2 <- seq(min(oly12$Age), max(oly12$Age), length = 40)
# Normal curve
fun <- dnorm(x2, mean = mean(oly12$Age), sd = sd(oly12$Age))
# Histogram
hist(oly12$Age, prob = TRUE, col = "white",
     ylim = c(0, max(fun)),
     main = "Histogram with normal curve")
lines(x2, fun, col = 2, lwd = 2)

##Transformation Plot Visualization in case your variable isn't normal## 
hist(oly12$Weight)
gladder(oly12$Weight)



#----------------------------- [END OF CHAPTER 4] -----------------------------------------##

##----- [Chapter 5]: Modularising a repetitive cleaning function -----##
#"Modular programming is a software design technique that emphasizes separating 
#the functionality of a program into independent, interchangeable modules, 
#such that each contains everything necessary to execute only one aspect 
#of the desired functionality."

##Functions allow us to repeat repetitive tasks (See tablebuilder function in chapter 5), 
#and can also be used to 'zip-up' a completed task like cleaning so it can be run in the
#background. This is helpful when using multiple datasets or waves with their own cleaning requirements.

##Imagine my only cleaning task is to re-categorize weight according to the quartiles:
data(oly12)
summary(oly12$Weight)
#I can zip this task into the Cleaner_1 function:
Cleaner_1 <- function(df){
  df$weight_cats <- as.numeric(NA)
  for (i in 1:length(df$Name)) {
    if ((df$Weight[i]>81) & (!is.na(df$Weight[i]))) {
      df$weight_cats[i] <- 1 }
    else if  ((df$Weight[i]>70) &  (!is.na(df$Weight[i]))) {
      df$weight_cats[i] <- 2 }
    else if  ((df$Weight[i]>61) &  (!is.na(df$Weight[i]))) {
      df$weight_cats[i] <- 3 }
    else if  ((df$Weight[i]<=61) &  (!is.na(df$Weight[i]))) {
      df$weight_cats[i] <- 4 }
    else { df$weight_cats[i] <- NA_real_ }
  }
  
df$Weight_Collapse <- dplyr::recode(df$weight_cats, "4"=2, "3"=2, "2"=1, "1"=1) 

#Return my clean dataset
return(as.data.frame(df)) } 

##In another sheet I can 'source' my cleaner file from my computer files 
#to have the function ready in the background: 
source("C:/Users/JohnM/Documents/Cleaner_1_file.R")
##and then call my function with oly12 data.
df_clean <- Cleaner_1(oly12)
##You can now modularise whole scripts so they'll run cleanly in the background of a 'main' script#
##If you have a problem- don't worry, just fix  one module, you needn't run the whole script from A-Z. 

#----------------------------- [END OF CHAPTER 5] -----------------------------------------##

##----- [Chapter 6]: Table building & Quickstart Table 1 -----##

#Quickstart Table 1 using 'furniture' package:
tuesdata <- tidytuesdayR::tt_load('2021-07-27')
olympics <- tuesdata$olympics

# Table 1: descriptive by gender 
install.packages("furniture")
library(furniture) 

table1(olympics, 
       age="Age name", height, weight, season, sex, medal)


table1(olympics, 
       age, height, weight, season, medal,
       splitby = ~sex)
table1(olympics, 
       age, height, weight, season, medal,
       splitby = ~sex, 
       row_wise = TRUE)

table1(olympics, 
       age, height, weight, season, medal,
       splitby = ~sex, 
       row_wise = TRUE,
       test=TRUE)

# table1(olympics[,c(2:5)])

table1(olympics, 
       age, height, weight, season, medal,
       splitby = ~sex, 
       row_wise = TRUE,
       test=TRUE,
       format_number = TRUE)

t1<-table1(olympics, 
           age, height, weight, season, medal,
           splitby = ~sex, 
           row_wise = TRUE,
           test=TRUE,
           format_number = TRUE,
           na.rm = FALSE)
install.packages("openxlsx")
library(openxlsx)
openxlsx::write.xlsx(t1, "Test_table_24March23.xlsx", overwrite = TRUE)



## Table builder Function (for extracting components of the regression output)
#You could simply 'source' this at the top of your script to be an available function in the background#
Model_output <- function(model) {  
  Conf <- confint(model)
  m25 <-  Conf[ , 1]
  m95 <-  Conf[ , 2]
  return(cbind(model$coefficients, m25, m95, summary(model)$coefficients[,4])) }


Regression_output <- lm(Age ~ Total + as.factor(Sex), data=oly12)
summary(Regression_output)

Table_for_export <-  Model_output(Regression_output)
Table_for_export

#write to excel:
write.csv(Table_for_export, "C:/Users/JohnM/Downloads/Table_for_export.csv")
#write to .tab file:
write.table(Table_for_export, "C:/Users/JohnM/Downloads/Table_for_export.tab")

#----------------------------- [END OF CHAPTER 6] -----------------------------------------##
