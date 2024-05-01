# Introduction to R for PSYC 306
# Using class data on rating the Rosenberg Self-Esteem Scale
library("renv")
renv::init(project = "PSYC_306_env")
renv::install("requiRements")
# First, install the following two packages. They only need to be installed once.
requiRements::install(path_to_requirements = "./requirement.txt")

# Then, load the libraries from these two packages. 
# This needs to happen every session.
library("psych")
library("here")

data_url = "../data/RateRSE_Data.csv"

# Read in data file to a data frame. 
# Make sure the file is in the same folder in your computer as your R Script!
dat = read.csv(data_url)

# R is a case-sensitive language, 
# so making all names lowercase will decrease the likelihood that we make a typo.
names(dat) <- tolower(names(dat))

#Check out the names of the columns in our data frame.
names(dat)

# Check out the first 6 rows in the data frame. Does this look right?
head(dat)

# Check out the dimensions (rows x columns) of our data frame. 
# Does this look right?
dim(dat)

# View the whole data frame below. 
# Because the View() function opens another tab, 
# keep this commented out when not using it.
View(dat)

### Rosenberg Self-Esteem Scale

# Item 1 - On the whole, I am satisfied with myself.
# Item 2 - At times I think I am no good at all. 
# Item 3 - I feel that I have a number of good qualities.
# Item 4 - I am able to do things as well as most other people.
# Item 5 - I feel I do not have much to be proud of.
# Item 6 - I certainly feel useless at times.
# Item 7 - I feel that I am a person of worth, 
#          at least on an equal plane with others.
# Item 8 - I wish I could have more respect for myself.
# Item 9 - All in all, I am inclined to think that I am a failure.
# Item 10 - I take a positive attitude toward myself.

# Outdated
psych::describe(dat$raterse_outdated01) # mean = 1.38
  hist(dat$raterse_outdated01)
psych::describe(dat$raterse_outdated02) # mean = 1.85
psych::describe(dat$raterse_outdated03) # mean = 1.23
psych::describe(dat$raterse_outdated04) # mean = 1.31
psych::describe(dat$raterse_outdated05) # mean = 1.08
psych::describe(dat$raterse_outdated06) # mean = 2.08
psych::describe(dat$raterse_outdated07) # mean = 1.54
psych::describe(dat$raterse_outdated08) # mean = 1.31
psych::describe(dat$raterse_outdated09) # mean = 1.54
psych::describe(dat$raterse_outdated10) # mean = 1.38

# Socially desirable
psych::describe(dat$raterse_socdes01) # mean = 4.69
psych::describe(dat$raterse_socdes02) # mean = 1.23
psych::describe(dat$raterse_socdes03) # mean = 4.54
psych::describe(dat$raterse_socdes04) # mean = 4.54
psych::describe(dat$raterse_socdes05) # mean = 1.69
psych::describe(dat$raterse_socdes06) # mean = 1.23
psych::describe(dat$raterse_socdes07) # mean = 4.46
psych::describe(dat$raterse_socdes08) # mean = 3.38
psych::describe(dat$raterse_socdes09) # mean = 1.15
psych::describe(dat$raterse_socdes10) # mean = 4.62

# Clear wording
psych::describe(dat$raterse_clear01) # mean = 3.08
psych::describe(dat$raterse_clear02) # mean = 2.85
psych::describe(dat$raterse_clear03) # mean = 3.23
psych::describe(dat$raterse_clear04) # mean = 2.00
psych::describe(dat$raterse_clear05) # mean = 3.00
psych::describe(dat$raterse_clear06) # mean = 3.15
psych::describe(dat$raterse_clear07) # mean = 2.62
psych::describe(dat$raterse_clear08) # mean = 2.85
psych::describe(dat$raterse_clear09) # mean = 3.08
psych::describe(dat$raterse_clear10) # mean = 3.31

# Related to self-esteem
psych::describe(dat$raterse_relate01) # mean = 3.38
psych::describe(dat$raterse_relate02) # mean = 3.77
psych::describe(dat$raterse_relate03) # mean = 3.08
psych::describe(dat$raterse_relate04) # mean = 2.31
psych::describe(dat$raterse_relate05) # mean = 3.15
psych::describe(dat$raterse_relate06) # mean = 3.15
psych::describe(dat$raterse_relate07) # mean = 3.62
psych::describe(dat$raterse_relate08) # mean = 2.23
psych::describe(dat$raterse_relate09) # mean = 3.46
psych::describe(dat$raterse_relate10) # mean = 3.08

