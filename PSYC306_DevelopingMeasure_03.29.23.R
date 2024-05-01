# R for PSYC 306
# Developing a novel measure of perceived control over sociopolitical events
library("renv")

renv::init(project = "PSYC_306_env")
renv::activate()
renv::install("nFactors")
renv::install("sets")
renv::install("psych")
renv::install("here")

# Load the libraries from these two packages. This needs to happen every session.
library("psych")
library("here")
library("sets")

setwd("C:/Users/This PC/Desktop/Oberlin 20-21/Course Work/Spring 2023/PSYC 306/Projec")

# Read in data file to a data frame. Make sure the file is in the same folder in your computer as your R Script!
dat = read.csv("./data/ClassPilotData_03.29.23.csv")

# R is a case-sensitive language, so making all names lowercase will decrease the likelihood that we make a typo.
names(dat) <- tolower(names(dat))

#Check out the names of the columns in our data frame.
names(dat)

# Check out the first 6 rows in the data frame. Does this look right?
head(dat)

# Check out the dimensions (rows x columns) of our data frame. Does this look right?
dim(dat)

columns <- as.set(names(dat)[grepl("pcse", names(dat))])

# View the whole data frame below. Because the View() function opens another tab, keep this commented out when not using it.
# View(dat)

### Perceived Measure of Sociopolitical Control Items
### 1 = STRONGLY DISAGREE, 2 = MODERATELY DISAGREE, 3 = NEITHER AGREE NOR DISAGREE
### 4 = MODERATELY AGREE, 5 = STRONGLY AGREE

# Item 1 - I believe that my actions can make a difference in politics and the economy.
# Item 2 - Only those in positions of power can influence the sociopolitical landscape of the United States. (Reverse-scored) 
# Item 3 - I can meaningfully impact political groups with my actions. 
# Item 4 - I cannot control how the U.S. government is handling the topic of climate change. (Reverse-scored)
# Item 5 - Each vote is important for determining who will be in office.
# Item 6 - Forces outside of my control dictate the state of the nation. (Reverse-scored)
# Item 7 - I think my participation in social movements can help affect change.
# Item 8 - I feel powerless against the sociopolitical events happening around me. (Reverse-scored)
# Item 9 - I can help abolish injustice in the United States.
# Item 10 - I do not think an individual vote matters in elections. (Reverse-scored)
# Item 11 - When people work together, we can make nation-wide change.
# Item 12 - I feel like I can influence the opinions of others when it comes to sociopolitical events in the United States.
# Item 13 - If I pay attention to the news, I often feel a sense of helplessness. (Reverse-scored)
# Item 14 - I think social justice movements can make a difference in American society.
# Item 15 - I have little influence over the sociopolitical decisions that affect me. (Reverse-scored)
# Item 16 - I often feel frustrated that my political representatives are not expressing opinions similar to my own. (Reverse-scored)
# Item 17 - As a consumer, how I choose to spend my money affects the economy.
# Item 18 - It is important to me to keep up to date about political issues in the United States.
# Item 19 - Individual civic actions, like recycling and volunteering, add up to large scale changes in the United States.
# Item 20 - I feel like my voice is heard and taken into consideration by those in power.
# Item 21 - I believe that economic issues like inflation and unemployment are largely determined by factors outside of my control. (Reverse-scored)
# Item 22 - I often feel as though my opinions about sociopolitical events are not taken seriously. (Reverse-scored)
# Item 22 - Once an American politician is elected, it is possible to sway what they do in office.
# Item 24 - It is the job of individuals to create sociopolitical change in the United States.
# Item 25 - I cannot change other people’s opinions on politics. (Reverse-scored)
# Item 26 - I prefer to focus my energy on things other than solving sociopolitical problems in the United States. (Reverse-scored)
# Item 27 - I feel like I am just a passive observer of sociopolitical events in the United States. (Reverse-scored)
# Item 28 - Posting political content on social media is an effective way to influence other people.

# Descriptive statistics
psych::describe(dat$pcseitem_01)
  hist(dat$pcseitem_01)
psych::describe(dat$pcseitem_02)
psych::describe(dat$pcseitem_03)
psych::describe(dat$pcseitem_04)
psych::describe(dat$pcseitem_05)
psych::describe(dat$pcseitem_06)
psych::describe(dat$pcseitem_07)
psych::describe(dat$pcseitem_08)
psych::describe(dat$pcseitem_09)
psych::describe(dat$pcseitem_10)
psych::describe(dat$pcseitem_11)
psych::describe(dat$pcseitem_12)
psych::describe(dat$pcseitem_13)
psych::describe(dat$pcseitem_14)
psych::describe(dat$pcseitem_15)
psych::describe(dat$pcseitem_16)
psych::describe(dat$pcseitem_17)
psych::describe(dat$pcseitem_18)
psych::describe(dat$pcseitem_19)
psych::describe(dat$pcseitem_20)
psych::describe(dat$pcseitem_21)
psych::describe(dat$pcseitem_22)
psych::describe(dat$pcseitem_23)
psych::describe(dat$pcseitem_24)
psych::describe(dat$pcseitem_25)
psych::describe(dat$pcseitem_26)
psych::describe(dat$pcseitem_27)
psych::describe(dat$pcseitem_28)

# Duration to complete survey
psych::describe(dat$duration..in.seconds.)
psych::describe(dat$duration..in.seconds./60)

consented <- subset(dat, dat$consentquestion == 1)
psych::describe(consented$duration..in.seconds.)
psych::describe(consented$duration..in.seconds./60)
hist(consented$duration..in.seconds./60)

# Reverse-score 
reverse_list <- c("pcseitem_02",
                  "pcseitem_04",
                  "pcseitem_06",
                  "pcseitem_08",
                  "pcseitem_10",
                  "pcseitem_13",
                  "pcseitem_15",
                  "pcseitem_16",
                  "pcseitem_21",
                  "pcseitem_22",
                  "pcseitem_25",
                  "pcseitem_26",
                  "pcseitem_27")

keep_list <- columns - as.set(reverse_list)

reverse_function <- function(item) 6 - dat[item]

reverse_data <- do.call(cbind, Map(reverse_function, reverse_list))

names(reverse_data) <- Map(function(item) paste(item, "r", sep=""), reverse_list)

# Create comprehensive data frame
pcse <- cbind(dat[unlist(keep_list)], reverse_data)

dat$pcse <- rowMeans(pcse, na.rm=TRUE)
dat$pcse

#Reliability
psych::alpha(pcse)

#Qualitative feedback
dat$itemfeedback

renv::snapshot(project = "PSYC_306_env")



