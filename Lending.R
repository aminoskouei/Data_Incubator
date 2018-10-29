# for data manipulation
library(dplyr)
# for data visualization
library(ggplot2)

setwd("E:/Lending/lending-club-loan-data")

#The first row and last two rows of the csv file are irrelevant but screws up the headers 
#which display on the second row. We use read.csv(.., skip = 1) to ignore first row.
#Then further remove the last two rows.

data <- read.csv('LoanStats_2018Q2.csv', stringsAsFactors = F, skip = 1)
data <- head(data, -2)

names(data)
# the data have 145 Variable and 130772 observation



# before that we should make sure all variable are numeric, lets put important variable in a data-frame


dataimportant <- select(data, 
                 grade, sub_grade, loan_status, funded_amnt, term, int_rate, installment, 
                 annual_inc, 
                 dti, 
                 earliest_cr_line, revol_util, inq_last_12m, total_bal_ex_mort,
                 purpose, emp_title, emp_length, addr_state)
# this variable related with Annual income , FICO score , and DTI


dataimportant$term <- as.numeric(substr(dataimportant$term, 1,3))
dataimportant$emp_length <- as.numeric(substr(dataimportant$emp_length, 1,2))
dataimportant$int_rate <- as.numeric(gsub("%", "", dataimportant$int_rate)) / 100
dataimportant$revol_util <- as.numeric(gsub("%", "", dataimportant$revol_util)) / 100
dataimportant$earliest_cr_line <- as.numeric(difftime(Sys.Date(), as.Date(paste("01-",dataimportant$earliest_cr_line,sep=''), format = "%d-%b-%Y")),units = 'days')/365



# let's start with Annual Income.Do high income borrowers tend to get funded more?

p1 <- ggplot( dataimportant, aes(annual_inc, funded_amnt)) + 
  geom_point(aes(colour = grade)) +
  labs(title = 'annual income vs. funded amnt') +
  geom_smooth()


p1

max(dataimportant$annual_inc)


# get rid of High Income people:

dataimportant <- filter(dataimportant, annual_inc < 100000)

# replot:

p2 <- ggplot(dataimportant, aes(annual_inc, funded_amnt)) +
  geom_point(aes(colour = grade)) +
  labs(title = 'annual income vs. funded amnt') +
  geom_smooth()
p2


#sth not correct here with very low income we see the higher loan for <25000 Income and after that
#we see the linear relationship of income and loan.

# lets have another plot :

p2 + xlim(0,100000) + facet_grid(. ~ grade) + geom_smooth()


#why ?!!





