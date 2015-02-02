Epidemiology Using R: Chapter 3 problem set

getwd()
ls()
rm(list=ls())

## 3.1 ## ==============================================
# Using RStudio and the data from Table 3.1 on page 
# 107 Create the following data frame:

Status <- c("Dead", "Survived", "Dead", "Survived", "Dead", "Survived", "Dead", "Survived") 
Treatment <- c("Tolbutamide", "Tolbutamide", "Placebo", "Placebo", "Tolbutamide", "Tolbutamide", "Placebo", "Placebo") 
Agegrp <- c("<55", "<55", "<55", "<55", "55+", "55+", "55+", "55+")
Freq <- c(8, 98, 5, 115, 22, 76, 16, 69)
dat <- data.frame(Status, Treatment, Agegrp, Freq); dat

## 3.2 ## ==============================================
# Select 3 to 5 classmates and collect data on first name,
# last name, affil- iation, two email addresses, and today’s 
# date. Using a text editor, create a data frame with this data.

First.name <- c("John", "Lucy", "Sandra")
Last.name <- c("Smith", "Bone", "Key")
Affiliation <- c("Duke", "UNC", "Duke")
Email <- c("NA", "lb123@unc.edu", "sk345@duke.edu")
Date <- Sys.Date()

dat2 <- data.frame(First.name, Last.name, Affiliation, Email, Date); dat2


## 3.3 ## ==============================================
# Review the United States data on AIDS cases by year available 
# at http: //www.medepi.net/data/aids.txt. Read this data into a 
# data frame. Graph a calendar time series of AIDS cases.

aids <- read.table("http://www.medepi.net/data/aids.txt", header = TRUE)
aids
class(aids)
plot(aids$year, aids$cases, type = "l", xlab = "Cases", lwd =2, ylab = "Year", main = "Cases of AIDS by Year")

## 3.4 ## ==============================================
# Review the United States data on measles cases by year available at 
# http://www.medepi.net/data/measles.txt. Read this data into a data frame. 
# Graph a calendar time series of measle cases using an arithmetic and 
# semi-logarithmic scale.

measles <- read.table("http://www.medepi.net/data/measles.txt", header = TRUE)
measles

# Arithmetic Plot
plot(measles$year, measles$cases/1000, type = "l", 
     xlab = "Year", ylab = "Measles Cases (x1000)", 
     main = "Measles cases reported in the US, 1950-2001",
     axes = FALSE, xlim = c(1950, 2001), ylim = c(0,1000),
     xaxs = "i", yaxs = "i")
axis(1, at = measles$year, labels = FALSE, tick = TRUE)
axis(1, at = seq(1950, 2000, 5), labels = seq(1950, 2000, 5),
     tick = TRUE, tcl = -1)
axis(2, at = seq(0, 1000, 100), labels = seq(0, 1000, 100),
     las = 2, tick = TRUE)
        
# Semilogarithmic plot
plot(measles$year, log(measles$cases), type = "l", 
     xlab = "Year", ylab = "Measles Cases (log)", 
     main = "Measles cases reported in the US, 1950-2001")
     axes = FALSE, xlim = c(1950, 2001), ylim = c(0,1000), 
     xaxs = "i", yaxs = "i")
axis(1, at = measles$year, labels = FALSE, tick = TRUE)
axis(1, at = seq(1950, 2000, 5), labels = seq(1950, 2000, 5),
     tick = TRUE, tcl = -1)
axis(2, at = c(seq(1, 10, 1), seq(10, 100, 10),
               seq(100, 1000, 100), seq(1000, 10000, 1000),
               seq(10000, 100000, 10000), seq(100000, 1000000, 100000)),
     labels = FALSE, tick = TRUE)

## 3.5 ## ==============================================


## 3.6 ## ==============================================
# Review the United States data on hepatitis B cases by year available at 
# http://www.medepi.net/data/hepb.txt. Read this data into a data frame. 
# Using the R code below, plot a times series of AIDS and hepatitis B cases.
library(dplyr)
hepb <- read.csv("http://www.medepi.net/data/hepb.txt", header = TRUE)
hepb <- tbl_df(hepb)
aids <- tbl_df(aids)
years <- cbind(hepb$year, aids$year)
cases <- (hepb, aids)


matplot(, cbind(hepb$cases, aids$cases), type = "l",
        xlab = "Year", ylab = "Cases",
        main = "Reported cases of Hepatitis B and AIDS,
        United States, 1980-2003", xlim = c(1980, 2003))
legend(x = "topleft", legend = c("Hepatitis B", "AIDS"), 
       lwd = 2, lty = 1:2, col = 1:2, cex = 0.7)
axis(2, at = seq(0, 10000, 1000), labels = seq(0, 10000, 1000), las = 2, tick = TRUE)

## 3.6 ## ==============================================
# Review data from the Evans cohort study in which 609 white males 
# were followed for 7 years, with coronary heart disease as the 
# outcome of interest (http://www.medepi.net/data/evans.txt). 
# The data dictionary is provided in Table 3.10.

chd <- read.table("http://www.medepi.net/data/evans.txt", header = TRUE)
chd
summary(chd)

# a Recode the binary variables (0, 1) into factors with 2 levels.
chd$chd <- ifelse(chd$chd == 0, "no", "yes")
chd$cat <- ifelse(chd$cat == 0, "normal", "high")
chd$smk <- ifelse(chd$smk == 0, "never", "ever")
chd$ecg <- ifelse(chd$ecg == 0, "no abnormality", "abnormality")
chd$hpt <- ifelse(chd$hpt == 0, "no", "yes")
chd

# b Discretized age into a factor with more than 2 levels.

agelab <- c("30-40", "40-50", "50-60", "60-70", ">70")
agegrp <- cut(chd$age, breaks = c(30, 40, 50, 60, 70, 80), 
              right = FALSE, labels = agelab)
table(agegrp)

# c Create a new hyptertension categorical variable based on the current
# classification scheme17:
#   Normal: SBP< 120 and DBP< 80;
#   Prehypertension: SBP=[120, 140) or DBP=[80, 90); 
#   Hypertension-Stage 1: SBP=[140, 160) or DBP=[90, 100); 
#   Hypertension-Stage 2: SBP≥ 160 or DBP≥ 100.

hyplab <- c("Normal", "Prehypertension", "Hypertension-Stage 1",
            "Hypertension-Stage 2")
hyp <- c(chd$dbp & chd$sbp)
hyp[chd$dbp < 80 & chd$sbp < 120] <- "Normal"
hyp[(chd$dbp >= 80 & chd$dbp <= 90) | (chd$sbp >= 120 & chd$sbp <= 140)] <- "Prehypertension"
hyp[(chd$dbp >= 90 & chd$dbp <= 100) | (chd$sbp >= 140 & chd$sbp <= 160)] <- "Hypertension-Stage 1"
hyp[chd$dbp >= 100 | chd$sbp >= 160] <- "Hypertension-Stage 2"
table(hyp)

# d Using R, construct a contigency table comparing the old and new hyper- tension variables.

hyp <- factor(hyp, levels = hyplab, ordered = T)
hpt <- ifelse(chd$hpt == "no", "Not Hypertensive", "Hypertensive")
hpt.vs.hyp <- table(hpt, hyp)
hpt.vs.hyp

## 3.7 ## ==============================================
# Review the California 2004 surveillance data on human West Nile virus cases 
# available at http://www.medepi.net/data/wnv/wnv2004raw. txt. Read in the data, 
# taking into account missing values. Convert the calendar dates into the 
# international standard format. Using the write.table function export the 
# data as an ASCII text file.

# Read data in and account for missing values
wnv <- read.csv("http://www.medepi.net/data/wnv/wnv2004raw.txt", header = TRUE); wnv
class(wnv) 
wnv[wnv == "." | wnv == "Unknown"] <- NA
summary(wnv)

# convert calendar dates
wnv$date.onset <- as.Date(wnv$date.onset, "%m/%d/%Y")
wnv$date.tested <- as.Date(wnv$date.tested, "%m/%d/%Y")
wnv

#Export to ASCII text file (used csv because I find csv's easier to work with)
write.csv(wnv, "wnv.csv")

## 3.8 ## ==============================================

oswego <- read.table("http://www.medepi.net/data/oswego/oswego.txt", header = TRUE)
oswego

# a. Using RStudio plot the cases by time of onset of illness (include 
#   appropriate labels and title). What does this graph tell you? 
#   (Hint: Process the text data and then use the hist function.)

onset.date <- paste(paste(oswego$onset.date, "/1940", sep = "",
                          oswego$onset.time), sep = " ")
onset.dt <- strptime(onset.date, "%m/%d/%Y%I:%M %p")
onset.dt
class(onset.dt)
onset <- onset.dt[!is.na(onset.dt)]
summary(onset)
min(onset)


hist(onset, breaks = 20, xlab = "Time of Onset", ylab = "Number of Cases", 
     main = "Cases by Time of Onset, Oswego 1940",
     xaxt = "n")

axis.POSIXct(1, at = seq(min(onset), max(onset), by = "hour"), 
     labels = seq(as.POSIXct(min(onset)), as.POSIXct(max(onset)), by = "hour"), 
     format = "%H:%M", cex.lab = 1)


                 
# b. Are there any cases for which the times of onset are inconsistent with 
#   the general experience? How might they be explained?


# c. How could the data be sorted by illness status and illness onset times?

library(dplyr)
oswego.df <- tbl_df(oswego)
ill.onset <- select(oswego.df, onset.date, onset.time)
ill.onset <- arrange(oswego.df, desc(ill), onset.date, onset.time)
ill.onset #sorted by illness, date, and time of onset. 


# d. Where possible, calculate incubation periods and illustrate their 
#   distribu-tion with an appropriate graph. Use the truehist function in 
#   the MASS package. Determine the mean, median, and range of the incubation period.

help(package = MASS)
library(MASS)


# Converting meal timem to standard time.
meal.dt <- paste("4/18/1940", oswego$meal.time, sep = " ")
meal.dt <- strptime(meal.dt, "%m/%d/%Y%I:%M %p")
meal.dt

# Finding the incubation period, and its mean, median, range.
inc.period <- onset.dt - meal.dt
mean.inc.period <- mean(inc.period, na.rm = TRUE)
median.inc.period <- median(as.numeric(inc.period[!is.na(inc.period)]))
range.inc.period <- range(as.numeric(inc.period[!is.na(inc.period)]))
inc.period


mean.inc.period
median.inc.period
range.inc.period  

# Creating a histogram with truehist from the MASS package

truehist(as.numeric(inc.period), nbins = 10, xlab = "Incubation Period (hours)",
         ylab = "Proportion of Cases", main = "Range of Incubation Periods, Oswego 1940",
         xlim = c(3, 8), ylim = c(0, 1), labels = FALSE, xaxs = "i", yaxs = "i")

axis(2, at = seq(0, 10, 0.1), labels = seq(0, 10, 0.1), cex.lab = 0.5,
     las = 2)
axis(1, at = seq(3, 8, 0.5), labels = seq(3, 8, 0.5), cex.lab = 0.5)

  
