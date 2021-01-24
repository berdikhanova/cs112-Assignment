######### RUN THE CODE BELOW IN R. R-STUDIO IS THE RECOMMENDED IDE. BOTH R AND R-STUDIO ARE FREE.
######### QUESTIONS SHOULD BE POSTED TO PIAZZA
######### THE ACTUAL ASSIGNMENT BEGINS ON LINE 71 (where it says "ASSIGNMENT 1...")


### Multilateral Development Institution Data
foo <- read.csv("https://tinyurl.com/yb4phxx8") # read in the data

# column names
names(foo)

# dimensions of the data set
dim(foo)

# quick look at the data structure
head(foo)

# one thing to be very careful with (in this data set) is the use of dates. 8 columns involve dates.

# take note of the columns representing calendar dates
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)

# these columns need some tweaking--I want to address missing values, calling the blank (empty) 
# elements "NA" instead of leaving them blank, and I wish to tell R these are "Date" objects.

for(i in date.columns)  # this "for loop" only loops through the "date.columns" -- no other columns.
  
{
  
  # identify which values are missing in the "i"th column of the foo data set
  which_values_are_missing <- which(as.character(foo[, i]) == "")
  
  # those values that are missing (blank) in the "i"th column are replaced by <NA>
  # because R knows how to handle "NA" -- NA means something special in R--blanks are handled 
  # more unpredictably (which is bad).
  foo[which_values_are_missing, i] <- NA
  
  # last step--replace each of these columns (which is structured as a column of "factor" values)
  # as a column of dates--i.e., convert them to an object of "class" = Date. They are dates, after all.
  # And if you convert them to the Date class, R will know they are dates and you can manipulate 
  # dates in a simple, straightforward way. Otherwise, you won't be able to easily manipulate them
  # arithmetically.  E.g., for simple Date operations, see lines 48-58 below...
  # **By the way, if you don't understand what a "factor" is in R, you should Google it.** 
  foo[, i] <- as.Date(as.character(foo[, i]))
  
}

# Now R knows that these columns are comprised of dates
# for example...  Replicate this yourself...

# foo[3,12]
# [1] "1968-03-13"

# foo[4,12]
# [1] "1968-07-03"

# foo[3,12] - foo[4,12]
# Time difference of -112 days

# Also, one additional helpful hint... How to eliminate rows with NAs...
# The "is.na" function--for more info, Google it or type ?is.na at the R command prompt in the console.
which.have.NAs <- which(is.na(foo$Rating == TRUE)) # for which rows is the claim "is.na" a TRUE claim?

# Then, if you wanted to, e.g., remove all those rows, retaining only the rows with ratings...
new_foo <- foo[-which.have.NAs, ]
# Notice I called this tweaked data set "new_foo" instead of rewriting over the original data set...
# It's a bit safer to do this, in case I decide I want to quickly revert back to the original data set.

###########################################################################

### ASSIGNMENT 1 -- You may want to read ALL the questions before you begin. 
### NOTE: FOR ALL QUESTIONS BELOW, ONLY CONSIDER PROJECTS WITH 
### non-missing "Circulation.Date" >= 2008-01-01. 
### EXCLUDE ALL OTHER PROJECTS FROM YOUR ANALYSIS.

# Finding the rows where CirculationDate had a "NA" value

which.have.NAs_date <- which(is.na(foo$CirculationDate == TRUE))

# Removing those rows from the data set 

new_foo_date <- foo[-which.have.NAs_date, ] 

# Finding and saving the rows with a CirculationDate later or equal to 2008-01-01, per instructions
cs112_df <- new_foo_date[new_foo_date$CirculationDate >= "2008-01-01",]

### YOU MUST provide a link to your R code. ------ DON'T FORGET TO DO THIS!!!!!!!!!!!!


# Take note of the column names: i.e., you can type: names(foo)
# fyi: the column called "Rating" is the success rating at completion. 0 = lowest, 3 = highest.

# (1) When projects are approved, they are approved for a certain period of time (until the time of
# "original completion date"). While projects are active, this "original" completion date is 
# often pushed out (extended), and then there is a "revised" completion date.

# Removing rows with "NA" values for OriginalCompletionDate, the ApprovalDate had no "NA" values
cs112_q1 <- cs112_df[-which(is.na(cs112_df$OriginalCompletionDate == TRUE)),]

# Finding the durations of projects 
difference_predicted <- cs112_q1$OriginalCompletionDate - cs112_q1$ApprovalDate
# Calculating the mean of the durations
mean_duration_predicted <- mean(difference_predicted)
print ("Mean of durations:")
mean_duration_predicted
# Presenting the mean in months
mean_duration_predicted / 30

# You have been told that project duration at approval is generally about 
# 1.5 years (18 months). In other words, (purportedly) when projects are approved, the difference 
# between the original project completion date and the the approval date is (supposedly) 
# approximately 18 months. 

# (a) Is this claim true? Explain. (Remember, for this ENTIRE assignment, only consider 
# projects with Circulation.Date >= 2008-01-01.)

## No, this is false, because on average the project duration at approval was generally about 21 months
## or close to 1,7 years. 

# Has project duration at approval changed over time (consider projects circulated earlier vs.
# and circulated later). You can interpret 'earlier' as 'first 5 years' and 'later' as 'last 5 years'.
# Be sure to discuss mean durations, median durations, and the
# interquartile range of durations (using the "quantile" function). 
# Approximate suggested length: 3-5 sentences

# Dividing the dataset into early (CirculationDate in the first 5 years) and late projects 
# CirculationDate in the last 5 years. I am not sure about this formula, but it gives approximately
# the range we need.
early_projects <- cs112_q1[cs112_q1$CirculationDate <= min(cs112_q1$CirculationDate) + 365*5,]
late_projects <- cs112_q1[cs112_q1$CirculationDate >= max(cs112_q1$CirculationDate) - 365*5,]

difference_early <- early_projects$OriginalCompletionDate - early_projects$ApprovalDate
mean_duration_early <- mean(difference_early)
print ("Mean of early projects durations:")
mean_duration_early
median_duration_early <- median(difference_early)
print ("Median of early projects durations:")
median_duration_early
quantile_early <- quantile(difference_early)
print ("Interquartile range of early project durations:")
quantile_early

difference_late <- late_projects$OriginalCompletionDate - late_projects$ApprovalDate
mean_duration_late <- mean(difference_late)
print ("Mean of late project durations:")
mean_duration_late
median_duration_late <- median(difference_late)
print ("Median of late project durations:")
median_duration_late
quantile_late <- quantile(difference_late)
print ("Interquartile range of late project durations:")
quantile_late

## The numerbs show a small change in the numbers: the mean duration has increased from 604 to 679,
## implying a longer duration of the project on average. The median duration is also higher for the
## later projects. The general pattern of distribution of durations corresponding to the given probabilities
## is similar with only slight differences. Overall, the duration has increased over time. 

# (b) How does original planned project duration differ from actual duration (if actual duration is 
# measured as the duration between "ApprovalDate" and "RevisedCompletionDate"?)  Once again, use
# means, medians, and interquartile ranges to explain your results by:
# comparing the means, medians, and IQRs of planned durations vs. same metrics for actual durations
# AND estimating the means, medians, and IQRs of the project-level discrepancies btw planned and actual durations.
# Approximate suggested length: 3-8 sentences

difference_predicted <- cs112_q1$OriginalCompletionDate - cs112_q1$ApprovalDate
mean_duration_predicted <- mean(difference_predicted)
print ("Mean of predicted durations:")
mean_duration_predicted

median_duration_predicted <- median(difference_predicted)
print ("Median of predicted durations:")
median_duration_predicted

iqr_duration_predicted <- quantile(difference_predicted)
print ("IQR of predicted durations:")
iqr_duration_predicted

difference_actual <- cs112_q1$RevisedCompletionDate - cs112_q1$ApprovalDate
mean_duration_actual <- mean(difference_actual) 
print ("Mean of actual durations:")
mean_duration_actual

median_duration_actual <- median(difference_actual)
print ("Median of actual durations:")
median_duration_actual

iqr_duration_actual <- quantile(difference_actual)
print ("IQE of actual durations:")
iqr_duration_actual 

## The numbers show a significant difference between the actual duration of the project vs. the predicted.
## While the mean value of the predicted duration is 644 days, the actual average duration is 1218 days,
## implying that on average the actual project durations took twice as much as was predicted. 
## The same pattern is observed according to median values. Interquartile range provides a deeper insight
## into the variability of data and follows the same pattern: the actual duration is twice the values 
## of the predicted durations. 

# (2) What % of projects that have ratings were rated 0?
# What % were rated 1? What % were rated 2? What % were rated 3? Answer these questions using a table
# or a figure. Provide a title AND an explanatory sentence or two that provides the numerical % results
# rounded to the nearest percentage-point. 

cs112_q1 %>% 
  group_by(cs112_q1$Rating) %>% 
  summarise(n = n()) %>%
  ungroup %>% 
  mutate(total = sum(n), rel.freq = n / total)

## About 3% of projects that have ratings were rated 0.
## About 16% of projects that have ratings weere rated 1. 
## About 68% of projects that have ratings were rated 2.
## About 13% of projects that have ratings were rated 3.


# (3) Repeat problem 2, but this time exclude all PPTA projects. PPTA projects are more prone to 
# negative ratings, because after a certain point in time only the low-rated PPTA projects required
# ratings.  PPTA stands for "Project Preparatory Technical Assistance" and it is basically a project
# intended to set up a loan (often a very large multi-million-dollar loan). Only PPTAs that fail to 
# "eventuate" to a loan are rated, which is why they are usually rated negatively.

# Removing the rows with values "PPTA" in the column "Type"
cs112_q1_final <- cs112_q1[cs112_q1$Type != "PPTA", ]
cs112_q1_final %>% 
  group_by(cs112_q1_final$Rating) %>% 
  summarise(n = n()) %>%
  ungroup %>% 
  mutate(total = sum(n), rel.freq = n / total)
## About 2% of projects that have ratings were rated 0.
## About 14% of projects that have ratings weere rated 1. 
## About 70% of projects that have ratings were rated 2.
## About 14% of projects that have ratings were rated 3.

## After the removal of PPTA projects, the fractions of the higher-ranked projects (2,3) have increased. 

# (4) Identify the top 25% of projects by "Revised.Amount" and the bottom 25% of projects by 
# "RevisedAmount". ("RevisedAmount" shows the final project budget.)
# Compare the ratings of these projects. Can you draw a causal conclusion about the effect of 
# budget size on ratings? Why or why not? 
# Hint: Compare the characteristics of the two project groupings,
# e.g., "Dept", "Division", "Cluster", "Country"
# Approximate suggested length: 3-5 sentences.

# Identifying the top 25% of projects 
cs112_q1[cs112_q1$RevisedAmount > quantile(cs112_q1$RevisedAmount,prob=0.75),]
# Identifying the botth 25% of projects 
cs112_q1[cs112_q1$RevisedAmount > quantile(cs112_q1$RevisedAmount,prob=0.25),]

lm1 <- lm(cs112_q1$Rating ~ cs112_q1$RevisedAmount, cs112_q1)
lm1
summary(lm1)
plot(lm1)

## Although from looking at the ratings in the top 25% of projects, which are generally higher,
## than in the bottop 25%, one can try to infer causation or at least correlation. However,
## the relationship between these two values needs to be evaluated further, using linear regression
## model or other tools. Then, one also needs to remember about other variables, and analyse 
## how interconnected they are with the revised amount.  

# (5) Imagine your manager asks you to apply Jeremy Howard's drivetrain model to the 
# problem of optimal budget-setting to maximize project success (i.e., "Rating"). 
# In such a situation, what would be the:
# (a) decision problem or objective?
# (b) lever or levers?
# (c) ideal RCT design?
# (d) dependent variable(s) and independent variable(s) in the modeler
# (e) And---Why would running RCTs and modeling/optimizing over RCT results be preferable 
# to using (observational, non-RCT) "foo" data?
# Approximate suggested length: 1-3 sentences for each sub-question.

## Based on this data and external experience,
## Objective: Maximize project success (receive higher "Rating", better KPI)
## Levers (potential): Cost (the budget, the debt), Time (how much does it take to actually complete project), 
## Scope (the type of project, the industry of the project, the market, the country,how many people are working on it).
## Ideal RCT design: Have a control and an experimental groups of projects, assigned randomly to
## mitigate potential biases. The experimental group could be receving a higher than intended budget,
## while the control group would be receving a normal budget, given everything else stays equal. 
## Then we would compare the results of the averages across teams based on a certain criteria ("Rating").

## Running RCT and using that data is preferrable because it allows us to minimize the "noise", 
## control the counfounding variables and see the direct impact of a certain variable to imply causation,
## which is almost impossible to do with observational data because of the counfouding variables. 
## Although RCT results would also create space for bias and confounding variables, which can 
## impact our results, RCT are effective in mitigating the biases and the externalities. 
