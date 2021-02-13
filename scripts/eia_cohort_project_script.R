##-----##
# Oeconomica's Entrepreneurship, Innovation & Antitrust Cohort, 2020–2021
##-----##

# Everything you need to run this code (data and instructions) is here:
# https://github.com/gelkouh/eia-oeconomica

# This snippet of code is a little loop that makes my code work on your computer
root <- getwd()
while(basename(root) != "eia-oeconomica") { # this is the name of your project directory you want to use
  root <- dirname(root)
}

# This line runs the script in your data.R file so that each person can have
# their data in a different place because everyone's file structure will be 
# a little different
source(file.path(root, "data.R"))

# This is the specific folder we want to access data from
ddir <- file.path(ddir, 'eia cohort project')

# Loading the packages we want
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
library(readxl)
if (!require(scales)) install.packages('scales')
library(scales)
if (!require(reshape2)) install.packages('reshape2')
library(reshape2)
if (!require(plm)) install.packages('plm')
library(plm) # masks lag from dplyr (so we need to specify dplyr::lag)
if (!require(stargazer)) install.packages('stargazer')
library(stargazer) 

##-----##
# Exploratory Data Analysis (make a new script with this code)
##-----##

loadRData <- function(fileName){
  # Loads RData file and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# Panel Study of Entrepreneurial Dynamics, PSED II, United States, 2005-2011 (ICPSR 37202)
# http://www.psed.isr.umich.edu/psed/home
df_icpsr_screeners <- loadRData(file.path(ddir, 'ICPSR_37202', 'DS0001', '37202-0001-Data.rda'))
df_icpsr_followups <- loadRData(file.path(ddir, 'ICPSR_37202', 'DS0003', '37202-0003-Data.rda'))

# Connecting Outcome Measures in Entrepreneurship, Technology, and Science (COMETS) database
# https://www.kauffman.org/entrepreneurship/research/comets/
# Note: read_csv works with zipped files
df_test <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Patent CSV', 'patent_citations_v2.csv.zip'))

# Subsetting
subset_df <- function(df, n) {
  set.seed(60637)
  df[sample(1:nrow(df), n, replace=FALSE),]
}

df_test_subset <- subset_df(df_test, 10000)

# Additional datasets
# Include: https://www.nber.org/research/data/nber-ces-manufacturing-industry-database
# Consider: https://wayback.archive-it.org/5902/20181003221605/https://www.nsf.gov/statistics/iris/
# Consider: https://www.nsf.gov/statistics/srvyberd/#tabs-1

# What we can do with this data
# characteristics of high-patent industries (what are the incentives)
# see how r&d dollar per firm affects investment/desire of people to be entrepreneurs 
# r&d and startup survival rate
# are people more encouraged to be entrepreneurs in industries with greater r&d spending
# how about industries where there are lots of patents? 
# are rewards up front greater than delayed gratification? 
# does greater r&d/new entrepreneurship in an industry increase likelihood patent will be cited?
# might need to do some background about patent citations -- this could be the lit review for proposal
# example: https://www.law.northwestern.edu/research-faculty/clbe/events/innovation/documents/AbramsSampatDrugCites060917.pdf

# as subsidies increase, innovation decreases 
# (so potentially as those go up in an industry, which would only happen in more concentrated industries,
# innovation/patents might fall OR patents per researcher)

# For next week
# look through codebooks and find variables you want to explore