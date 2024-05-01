# R for PSYC 306
# Developing a novel measure of perceived control over sociopolitical events
install.packages("BiocManager")
BiocManager::install("Rgraphviz")
# Load the libraries from these two packages. This needs to happen every session.
renv::activate(project = "PSYC_306_env")
renv::install("RcmdrMisc")
renv::install("reshape2")
renv::install("GPArotation")
renv::status()
library("Rgraphviz")
library("RcmdrMisc")
library("psych")
library("here")
library("nFactors")

setwd("C:/Users/This PC/Desktop/Oberlin 20-21/Course Work/Spring 2023/PSYC 306/Projec")

# Read in data file to a data frame. Make sure the file is in the same folder in your computer as your R Script!
dat = read.csv("./data/Study2_ClassData_04.19.23.csv")

# R is a case-sensitive language, so making all names lowercase will decrease the likelihood that we make a typo.
names(dat) <- tolower(names(dat))

#Check out the names of the columns in our data frame.
  # SOME PCSE ITEMS ARE MISSING BECAUSE WE DELETED THEM! Others were modified and have an "m" at the end.
names(dat)

# Check out the first 6 rows in the data frame. Does this look right?
head(dat)
tail(dat)

# Check out the dimensions (rows x columns) of our data frame. Does this look right?
dim(dat)

# View the whole data frame below. Because the View() function opens another tab, keep this commented out when not using it.
#View(dat)

### Perceived Measure of Sociopolitical Control Items
### 1 = STRONGLY DISAGREE, 2 = MODERATELY DISAGREE, 3 = NEITHER AGREE NOR DISAGREE
### 4 = MODERATELY AGREE, 5 = STRONGLY AGREE

# Item 1 (modified) - I believe that my actions can make a difference in the economy.
# Item 2 - Only those in positions of power can influence the sociopolitical landscape of the United States. (Reverse-scored) 
# Item 3 (modified) - I can meaningfully impact politics with my actions. 
# Item 5 - Each vote is important for determining who will be in office.
# Item 7 - I think my participation in social movements can help affect change.
# Item 8 - I feel powerless against the sociopolitical events happening around me. (Reverse-scored)
# Item 9 - I can help abolish injustice in the United States.
# Item 10 - I do not think an individual vote matters in elections. (Reverse-scored)
# Item 11 (modified) - When I work with other people, we can make nation-wide change. 
# Item 12 - I feel like I can influence the opinions of others when it comes to sociopolitical events in the United States.
# Item 15 - I have little influence over the sociopolitical decisions that affect me. (Reverse-scored)
# Item 16 - I often feel frustrated that my political representatives are not expressing opinions similar to my own. (Reverse-scored)
# Item 17 - As a consumer, how I choose to spend my money affects the economy.
# Item 18 - It is important to me to keep up to date about political issues in the United States.
# Item 19 (modified) - Individual civic actions, like volunteering, add up to large scale changes in the United States. 
# Item 20 - I feel like my voice is heard and taken into consideration by those in power.
# Item 21 - I believe that economic issues like inflation and unemployment are largely determined by factors outside of my control. (Reverse-scored)
# Item 22 - I often feel as though my opinions about sociopolitical events are not taken seriously. (Reverse-scored)
# Item 23 - Once an American politician is elected, it is possible to sway what they do in office.
# Item 24 - It is the job of individuals to create sociopolitical change in the United States.
# Item 25 - I cannot change other people's opinions on politics. (Reverse-scored)
# Item 26 - I prefer to focus my energy on things other than solving sociopolitical problems in the United States. (Reverse-scored)
# Item 27 - I feel like I am just a passive observer of sociopolitical events in the United States. (Reverse-scored)
# Item 28 (modified) - If I post political content on social media, I can effectively influence other people. 

# Descriptive statistics
psych::describe(dat$pcseitem_01m)
  hist(dat$pcseitem_28m)
psych::describe(dat$pcseitem_02)
psych::describe(dat$pcseitem_03m)
psych::describe(dat$pcseitem_05)
psych::describe(dat$pcseitem_07)
psych::describe(dat$pcseitem_08)
psych::describe(dat$pcseitem_09)
psych::describe(dat$pcseitem_10)
psych::describe(dat$pcseitem_11m)
psych::describe(dat$pcseitem_12)
psych::describe(dat$pcseitem_15)
psych::describe(dat$pcseitem_16)
psych::describe(dat$pcseitem_17)
psych::describe(dat$pcseitem_19m)
psych::describe(dat$pcseitem_20)
psych::describe(dat$pcseitem_21)
psych::describe(dat$pcseitem_22)
psych::describe(dat$pcseitem_23)
psych::describe(dat$pcseitem_24)
psych::describe(dat$pcseitem_25)
psych::describe(dat$pcseitem_26)
psych::describe(dat$pcseitem_27)
psych::describe(dat$pcseitem_28m)

# Duration to complete survey
psych::describe(dat$duration..in.seconds.)
psych::describe(dat$duration..in.seconds./60)

consented <- subset(dat, dat$consentquestion == 1)
psych::describe(consented$duration..in.seconds.)
hist(consented$duration..in.seconds.)
psych::describe(consented$duration..in.seconds./60)
hist(consented$duration..in.seconds./60)

consented_notoutliers <- subset(consented, consented$duration..in.seconds. < 3000)
psych::describe(consented_notoutliers$duration..in.seconds./60)
hist(consented_notoutliers$duration..in.seconds./60, breaks = 20)

# Reverse-score 
dat$pcseitem_02r <- 6-dat$pcseitem_02
dat$pcseitem_08r <- 6-dat$pcseitem_08
dat$pcseitem_10r <- 6-dat$pcseitem_10
dat$pcseitem_15r <- 6-dat$pcseitem_15
dat$pcseitem_16r <- 6-dat$pcseitem_16
dat$pcseitem_21r <- 6-dat$pcseitem_21
dat$pcseitem_22r <- 6-dat$pcseitem_22
dat$pcseitem_25r <- 6-dat$pcseitem_25
dat$pcseitem_26r <- 6-dat$pcseitem_26
dat$pcseitem_27r <- 6-dat$pcseitem_27

# Create comprehensive data frame
pcse <- dat[c("pcseitem_01m", "pcseitem_02r","pcseitem_03m",
              "pcseitem_05", "pcseitem_07", "pcseitem_08r",
              "pcseitem_09","pcseitem_10r", "pcseitem_11m", 
              "pcseitem_12", "pcseitem_15r", "pcseitem_16r",
              "pcseitem_17", "pcseitem_19m","pcseitem_20", 
              "pcseitem_21r","pcseitem_22r","pcseitem_23", 
              "pcseitem_24","pcseitem_25r","pcseitem_26r", 
              "pcseitem_27r", "pcseitem_28m")]
dat$pcse <- rowMeans(pcse, na.rm=TRUE)
dat$pcse

#Reliability
psych::alpha(pcse)

#Qualitative feedback
dat$itemfeedback

#Attention check
dat$attentioncheck
sum(dat$attentioncheck==1, na.rm=TRUE)
which(dat$attentioncheck!=1)

pcse <- na.omit(pcse)

psych::omega(pcse, 3, rotate="promax")
psych::omega(pcse, 2, rotate="promax")

correlationTable <- cor(pcse, use = "complete.obs")
# ===> Find no items strongly correlate with others

write.csv(correlationTable, file="./data/correlation.csv")

KMO(pcse)
# testing the Bartlett's test for sphericity
cortest.bartlett(pcse)
# ===> reject null hypothesis, therefore we can use EFA


# using 3 factor
scree(pcse, pc=FALSE)

parrallelTest3 <- fa.parallel(pcse, fa="fa")
# choose number of factor of 3

# Fit the data oblique 3
fit3 <- factanal(pcse, 3, rotation="promax")
loads3 <- fit3$loadings
print(loads3)

fa.diagram(loads3)

# identify weak loadings
which(pmax(abs(loads3[, 1]), abs(loads3[, 2]),abs(loads3[, 3])) < 0.3)

write.csv(loads3[1: 23, ], file="./data/oblique3Loads.csv")

lowOblique3 <- c("pcseitem_09", "pcseitem_17", "pcseitem_28m")
# Item 9 - I can help abolish injustice in the United States.
# Item 17 - As a consumer, how I choose to spend my money affects the economy.
# Item 28 (modified) - If I post political content on social media, I can effectively influence other people. 

#load Data
pcse3 <- pcse[, c("pcseitem_01m", "pcseitem_02r", "pcseitem_08r", "pcseitem_11m", 
        "pcseitem_12", "pcseitem_15r", "pcseitem_16r", "pcseitem_20", 
        "pcseitem_21r", "pcseitem_22r", "pcseitem_23", "pcseitem_25r")]
pc23 <- pcse[, c("pcseitem_03m", "pcseitem_05", "pcseitem_10r", 
              "pcseitem_19m", "pcseitem_24")]
pc33 <- pcse[, c("pcseitem_07", "pcseitem_26r", "pcseitem_27r")]

#check alpha (not really good in this case!!!)
psych::alpha(pcse3)
psych::alpha(pc23)
psych::alpha(pc33)

# using 2 factor
# Fit the data in oblique rotation
fit2 <- factanal(pcse, 2, rotation="promax")
loads2 <- fit2$loadings
print(loads2)

fa.diagram(loads2)

# identify weak loadings
which(pmax(abs(loads2[, 1]), abs(loads2[, 2])) < 0.3)

write.csv(loads2[1: 23, ], file="./data/oblique2Loads.csv")

lowVar <- c("pcseitem_09", "pcseitem_17", "pcseitem_24", "pcseitem_26r", "pcseitem_28m")

#load Data
pcse2 <- pcse[, c("pcseitem_01m", "pcseitem_02r", "pcseitem_08r", "pcseitem_11m", 
               "pcseitem_12", "pcseitem_15r", "pcseitem_16r", "pcseitem_20", 
               "pcseitem_21r", "pcseitem_22r", "pcseitem_23", "pcseitem_25r")]
pc12 <- pcse[, c("pcseitem_03m", "pcseitem_05", "pcseitem_07", "pcseitem_10r", 
               "pcseitem_19m", "pcseitem_24", "pcseitem_26r", "pcseitem_27r")]

#check alpha (really good in this case!!!)
psych::alpha(pcse2)
psych::alpha(pc12)

# use optimism and conscientiousness in the data
pcseoc <- dat[c("pcseitem_01m", "pcseitem_02r","pcseitem_03m",
              "pcseitem_05", "pcseitem_07", "pcseitem_08r",
              "pcseitem_09","pcseitem_10r", "pcseitem_11m", 
              "pcseitem_12", "pcseitem_15r", "pcseitem_16r",
              "pcseitem_17", "pcseitem_19m","pcseitem_20", 
              "pcseitem_21r","pcseitem_22r","pcseitem_23", 
              "pcseitem_24","pcseitem_25r","pcseitem_26r", 
              "pcseitem_27r", "pcseitem_28m", "optimism_1",
              "optimism_2", "optimism_3", "optimism_4",
              "optimism_5", "optimism_6", "conscientiousness_1",
              "conscientiousness_2","conscientiousness_3",
              "conscientiousness_4","conscientiousness_5",
              "conscientiousness_6")]

correlationOC <- cor(pcseoc, use = "complete.obs")

pcseoc <- na.omit(pcseoc)

# check alpha first
psych::alpha(pcseoc, check.keys=TRUE)

cortest.bartlett(pcseoc)

scree(pcseoc, pc=FALSE)

fa.parallel(pcseoc, fa="fa")

fitoc4 <- factanal(pcseoc, 4, rotation="promax")
loadsoc4 <- fitoc4$loading
print(loadsoc4)

fa.diagram(loadsoc4)
psych::omega(pcseoc, 4)

which(pmax(abs(loadsoc4[, 1]), abs(loadsoc4[, 2]), abs(loadsoc4[, 3]), abs(loadsoc4[, 4])) < 0.3)

consciencious4 <- pcseoc[, c("conscientiousness_1", "conscientiousness_2", "conscientiousness_3", 
                         "conscientiousness_4", "conscientiousness_5", "conscientiousness_6")]

optimism24 <- pcseoc[, c("optimism_1", "optimism_2", "optimism_3", 
                         "optimism_4", "optimism_5", "optimism_6")]

pcse4 <- pcseoc[, c("pcseitem_01m", "pcseitem_02r", "pcseitem_08r", "pcseitem_11m", 
                         "pcseitem_12", "pcseitem_15r", "pcseitem_16r", "pcseitem_20", 
                         "pcseitem_21r", "pcseitem_22r", "pcseitem_23", "pcseitem_25r")]

pc44 <- pcse[, c("pcseitem_03m", "pcseitem_05", "pcseitem_07", "pcseitem_10r", 
                       "pcseitem_19m", "pcseitem_27r")]

psych::alpha(foblique14, check.keys=TRUE)
psych::alpha(foblique24, check.keys=TRUE)
psych::alpha(foblique34, check.keys=TRUE)
psych::alpha(foblique44, check.keys=TRUE)

fitoc5 <- factanal(pcseoc, 5, rotation="promax")
loadsoc5 <- fitoc5$loading
print(loadsoc5)

fa.diagram(loadsoc5)
psych::omega(pcseoc, 5, rotate = "promax")

which(pmax(abs(loadsoc5[, 1]), abs(loadsoc5[, 2]), abs(loadsoc5[, 3]), abs(loadsoc5[, 4]), abs(loadsoc5[, 5])) < 0.3)

consciencious5 <- pcseoc[, c("conscientiousness_1", "conscientiousness_2", "conscientiousness_3", 
                         "conscientiousness_4", "conscientiousness_5", "conscientiousness_6")]

optimism5 <- pcseoc[, c("optimism_1", "optimism_2", "optimism_3", 
                         "optimism_4", "optimism_5", "optimism_6")]

pcse5 <- pcseoc[, c("pcseitem_01m", "pcseitem_02r", "pcseitem_08r", "pcseitem_11m", 
                         "pcseitem_12", "pcseitem_15r", "pcseitem_16r", "pcseitem_20", 
                         "pcseitem_21r", "pcseitem_22r", "pcseitem_23", "pcseitem_25r")]

pc45 <- pcseoc[, c("pcseitem_03m", "pcseitem_05", "pcseitem_07", "pcseitem_10r", 
                       "pcseitem_19m", "pcseitem_24")]

pc55 <- pcseoc[, c("pcseitem_03m", "pcseitem_07", "pcseitem_09","pcseitem_26r", "pcseitem_27r")]

# we can see that optimism and conscienciousness does not jiggle with pcse
psych::alpha(foblique55)

# conclude: bad items are pcseitem_9 pcseitem_17 pcseitem_28m 
# Item 9 - I can help abolish injustice in the United States.
# Item 17 - As a consumer, how I choose to spend my money affects the economy.
# Item 28 (modified) - If I post political content on social media, I can effectively influence other people. 

hist(pcse$pcseitem_03m)
hist(pcse$pcseitem_09) # negatively skew
hist(pcse$pcseitem_17) # negatively skew 
hist(pcse$pcseitem_28m) # positively skew

psych::alpha(pcse[, Reduce(intersect, list(names(pcse2),names(pcse3),names(pcse4), names(pcse5)))])


save.image("C:/Users/This PC/Desktop/Oberlin 20-21/Course Work/Spring 2023/PSYC 306/Projec/.RData")

renv::snapshot(project = "PSYC_306_env")
