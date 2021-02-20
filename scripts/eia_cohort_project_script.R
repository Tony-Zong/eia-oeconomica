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

# ICPSR data cut down to columns of interest
df_icpsr_selected_cols <- df_icpsr_followups %>%
  select(c('CDIVISON', 'METRO', 'QS7', 'INCOME', 'REGIONA', 'IWYEAR', 'AA1', 'AA5A', 'AE15',
           'AB1', 'AF1', 'AF9', 'AF10', 'AQ10_1', 'AS5'))

# Connecting Outcome Measures in Entrepreneurship, Technology, and Science (COMETS) database
# https://www.kauffman.org/entrepreneurship/research/comets/
# Note: read_csv works with zipped files
df_comets_patents <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Patent CSV', 'patents_v2.csv.zip')) %>%
  select(c('patent_id', 'grant_date', 'patent_title'))
df_comets_patent_cite_counts <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Patent CSV', 'patent_cite_counts_v2.csv.zip'))
df_comets_patent_us_classes <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Patent CSV', 'patent_us_classes_v2.csv.zip')) %>%
  select(c('patent_id', 'us_class'))
# This failed for some reason: 
# df_comets_patent_assignees <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Patent CSV', 'patent_assignees_v2.csv.zip')) %>%
#   select(c('patent_id', 'org_type'))
df_comets_patent_zd_cats <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Patent CSV', 'patent_zd_cats_v2.csv.zip')) %>%
  rename(patent_id = patent) %>%
  select(c('patent_id', 'zd'))

# Note: amount is NA for all values (probably to anonymize the data)
df_comets_grants <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Grant CSV', 'grants_v2.csv.zip')) %>%
  select(c('grant_num', 'grant_agency', 'start_date', 'end_date', 'amount'))
df_comets_grantee_orgs <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Grant CSV', 'grantee_orgs_v2.csv.zip')) %>%
  select(c('grant_num', 'grant_agency', 'org_type'))
df_comets_grant_zd_cats <- read_csv(file.path(ddir, 'COMETS', 'All CSV', 'Grant CSV', 'grant_zd_cats_v2.csv.zip')) %>%
  select(c('grant_num', 'grant_agency', 'zd'))

# Subsetting
subset_df <- function(df, n) {
  set.seed(60637)
  df[sample(1:nrow(df), n, replace=FALSE),]
}

df_comets_patents_subset <- subset_df(df_comets_patents, 10000)
df_comets_patent_subset <- df_comets_patents_subset %>%
  left_join(df_comets_patent_cite_counts, by = 'patent_id') %>%
  left_join(df_comets_patent_us_classes, by = 'patent_id') %>%
  left_join(df_comets_patent_zd_cats, by = 'patent_id')

df_comets_grants_subset <- subset_df(df_comets_grants, 10000)
df_comets_grant_subset <- df_comets_grants_subset %>%
  left_join(df_comets_grantee_orgs, by = 'grant_num') %>%
  left_join(df_comets_grant_zd_cats, by = 'grant_num')

# NBER Manufacturing 
# https://www.nber.org/research/data/nber-ces-manufacturing-industry-database
df_nber_sic5811 <- read_csv(file.path(ddir, 'NBER Manufacturing', 'sic5811.csv')) %>%
  select(c('sic', 'year', 'emp', 'pay', 'prode', 'prodw', 'vadd', 
           'invest', 'dtfp5'))

# Here are the three datasets we will use
# We will "unsubset" at the end to run our regressions on the whole dataset
View(df_comets_patent_subset)
View(df_comets_grant_subset)
View(df_nber_sic5811)
  
##

# From the COMETS dataset, it seems there isn’t enough substantial data to base a whole paper on, 
# but we could generally explore which industries receive the most funding from grants for patents 
# and use this information as one point in a larger claim.  For the NBER data, we could examine how 
# growth rates are related to capital expenditures in different industries or discuss how worker wages 
# are proportionally adjusted as a company grows. In the last dataset, there is a lot we could analyze. 
# We could look at how different industries promote entrepreneurial action by looking at categorical 
# questions about attitudes towards a particular market or just generally the percentage of entrants 
# and how much they invest in the start-up. (Arjun)

# For COMETS, we can see whether different organization types receive different amounts in grant 
# funding / support for patents. For NBER, we can see whether additional capital is related to increased 
# value-added, and whether worker wages are affected. For PSED, we can look at geographic location, 
# market competition, and motives for startup companies. I would expect larger investments in cities. (Chris)

##

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