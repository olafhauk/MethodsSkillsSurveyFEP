# setwd('U:/Methods/Methods_Skills_Survey_2015/R4')
# source('Analyze_SurveyMonkeyData.R')

rm(list = ls()) # clear workspace

library(MASS)

library('readxl') # for reading data from Excel file
# library(nnet) # for multinomial regression (multinom)
library(VGAM) # for multinomial regression (vglm)
library(ggplot2) # plotting bar graphs


datain_filename <- 'U:/Methods/Methods_Skills_Survey_2015/Sheet_1_converted_180319.xls'

# directory for bar plots etc.
fig_outdir <- "U:/Methods/Methods_Skills_Survey_2015/R4/Figures/"
print( sprintf("Writing figures to %s", fig_outdir))

source('Survey_functions.R') # function that gets Results (counts, fractions) from data frame data

graphics.off() # close all open plot windows

#######################
# Read and prepare data
#######################

# READ EXCEL spreadsheet (prepared with Convert_SurveyMonkeyData_4R.m)
# first row contains column names
cat('\n')
print( sprintf("Reading data from %s", datain_filename))
data_ori <- read_excel(datain_filename, col_names=T)

names(data_ori)[3] <- "Age" # remove the silly "?"
names(data_ori)[4] <- "Nationality"

# get row with correct responses
correct <- data_ori[1,]

q_yesno <- c(13:32) # indices for "real" knowledge questions
q_names <- names( data_ori )[q_yesno] # names of the real knowledge questions

q_meth_yesno <- c(15:32) # indices for methods-related questions
q_meth_names <- names( data_ori )[q_meth_yesno] # names of the methods-related questions

# remove row with correct response options from data frame
# note: row numbering now starts at 2
data_all <- data_ori[2:nrow(data_ori),]

# PRUNE respondents with insufficient responses etc.
cat('\n')
print("Prune data.")
# returns list with "data" and "data_skipped"
data_dummy <- prune_respondents(data_all, qnames)

data_all <- data_dummy$data

# data for those who skipped all methods questions
data_skipped <- data_dummy$data_skipped

# get indices for various SUB-GROUPS of respondents
cat('\n')
print("Get group indices.")

# returns list with "groups" and new "data", only incl. subjects
# with classifiable undergraduate degrees
get_it <- get_groups(data_all)
groups_all <- get_it$groups
data_all <- get_it$data

get_it <- get_groups(data_skipped)
groups_skipped <- get_it$groups

###
######### Analysis ###########
###
cat('\n')
print("Computing demographics.")
print("Valid respondents.")
demos <- get_demographics(groups_all, data_all)

print("Respondents with too many skips.")
demos_skipped <- get_demographics(groups_skipped, data_skipped)

cat('\n')
print("Plotting demographic data.")

pdf_name <- sprintf("%s/Demographics.pdf", fig_outdir) # avoid some problems with long filenames
plot_demographics(demos, pdf_name)

# problem with "skipped": respondents also skipped many demographic questions
pdf_name <- sprintf("%s/Demographics_skipped.pdf", fig_outdir) # avoid some problems with long filenames
plot_demographics(demos_skipped, pdf_name)

cat('\n')
print("Get results list.")
Results <- get_all_Results(data_all, q_meth_names, correct, groups_all)
# Results$sex$males$frac$Ohm contains values for Cor/Err/NoI/Skp

# the following produced warning message about missing values and removed rows
# for Researcher Type (12 rows)
# for Training Needs (Undgrad All, PhD All, Postdoc All) 5 rows
# for Undgrad All/males/females... 20 rows

cat('\n')
print(sprintf("Plot results to %s.", fig_outdir))
plot_Results(Results, fig_outdir)

cat('\n')
print("Computing logistic regression.")
stat_list <- get_Stats(Results, groups_all)
stat_list$Derivative$Err # contains stats output from glm/polr

# Done
