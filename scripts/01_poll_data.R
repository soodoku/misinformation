#
# Descriptive data on polls
#

# Set directory
setwd(dropboxdir)

# Load libs 
library(stringi)
# devtools::install_github("soodoku/goji")
library(goji)

# Load data
polls <- read.csv("misinfo_misinfo/data/comm_polls/misinformation_question_wording_plus (misinfo items only).csv")

# Only misinformation polls
polls <- subset(polls, misinfo == "Y")

# Only commercial polls/Not for now
polls  <- subset(polls, academic_comm == 0)

# Explicit DK/Not sure 
round(mean(grepl("Y", polls$explicit_dk)), 3)
round(mean(grepl("Y", polls$explicit_ns)), 3)

# Matter of Opinion
round(table(polls$prompt_type)/nrow(polls), 3)
sum(round(table(polls$prompt_type)/nrow(polls), 3)) - (.167 + .400 + .117)

# Number of Options
# Where it is 0 --- open ended
round(table(polls$n_options)/nrow(polls), 3)
round(table(polls$n_options > 4)/nrow(polls), 3)

# Incorrect Response Encouraging
round(mean(grepl("Y", polls$ire)), 3)

# Mode
round(table(polls$mod)/nrow(polls), 3)

# Valence
round(table(polls$correct_valence)/nrow(polls), 3)

# Basic Descriptive Stats to be added in the document
table(polls$topic)
table(polls$polling_firm)
range(as.numeric(stri_sub(polls$date, -2, -1)), na.rm = T)

# Regression
# N Options don't work with the current model as incorrect increases as number of alternatives increase
# Compound variables
polls$explit_dk_ns <- (polls$explicit_dk == "Y" | polls$explicit_ns=="Y")

with(polls, summary(lm(incorrect ~ polls$explit_dk_ns + as.factor(n_options) + ire + topic)))

# Let's get a summary of the comm. polls
poll_firm_table <- table(polls$polling_firm)
write.csv(poll_firm_table, file = "misinfo_misinfo/res/append_a_poll_firm_table.csv", row.names = F)
