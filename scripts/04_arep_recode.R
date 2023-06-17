# AREP Recode 

# Things that need to be done in this R script: 
# 1. The pk function does not work correctly. Currently it does not recode incorrect answers to whatever the rule
# then is.  Are incorrect answers 0 like NAs or something else? My assumption is 0 but I am not sure.  
# 2. Before processing the file there need to be fixes made in the arep csv.  \303\254 Increases the Medicare payroll tax for upper\303\244\303\263\302\220income
# Americans Replaces Medicare with a \303\244\303\263\303\254public option Couldn \302\273t Don \302\273t 3.
# There are a couple of other questions that I have. I have tagged the #TODO, which should make the script searchable for you

# Set dir
setwd(basedir)
setwd("hidden")

# Load libraries
library(goji)
library(car)
library(dplyr)

# Load functions
source("scripts/00_common_func.R")

# Read in the data
arep        <- read.csv("data/arep/arep.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors=FALSE)

# Subset AREP
# -------------------

# 1. Limit to user_ids with only FY prefix as those data belong to real people not experimenters
arep <- subset(arep, grepl("FY", arep$userid))
# 2. Investigate this later --- just one duplicate but investigate which row to keep
arep <- subset(arep, !duplicated(arep$userid))
# 3. country is mispelled. Subset to users in the US --- maybe.  
names(arep)[names(arep)=="county"] <- "country"
#arep <- subset(arep, arep$country == "United States")
# 4. Only completes --- 1 person didn't complete
arep <- subset(arep, !is.na(arep$photo.nonphoto))  

# Add Demographic/Panel Data
# ----------------------------

dem <- read.csv("data/arep/arep_dem.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors=FALSE)
# Keep userids with FY upfront
dem <- subset(dem, grepl("FY", dem$arep.id))
# Nuke duplicates
dem <- subset(dem, !duplicated(dem$arep.id))
# Merge 
arep <- merge(arep, dem, by.x = "userid", by.y = "arep.id", all.x=T, all.y = F)

# Recoding 
# -------------------

# 1. Political Knowledge: photo.nonphoto.pk
# --------------------------------------------

arep$hrc  <- grepl("senate|sente|senator|harry", arep$hr, ignore.case = TRUE) & 
             grepl("leader|reid|chief|leder|head|president|chair|whip", arep$hr, ignore.case = TRUE)

arep$nsc  <- grepl("france|french", arep$ns, ignore.case = TRUE) & 
             grepl("president|leader|head|top dog|pres|chancellor|prez|pm|premier|prime minister", arep$ns, ignore.case = TRUE)

arep$jnc  <- grepl("homeland|security|dhs", arep$jn, ignore.case = TRUE) &  
             grepl("director|sec|head", arep$jn, ignore.case = TRUE)

arep$hr2c <- grepl("senate|sente|senator|harry|herry", arep$hr2, ignore.case = TRUE) & 
             grepl("reid|reed|leader|chief|leder|head|president|chair|whip", arep$hr2, ignore.case = TRUE)

arep$jn2c <- grepl("homeland|security|dhs|janet", arep$jn2, ignore.case = TRUE) &  
             grepl("napolitano|napoletano|napalitano|director|sec|head", arep$jn2, ignore.case = TRUE)

arep$ns2c <- grepl("france|french", arep$ns2, ignore.case = TRUE) &  
             grepl("president|leader|head|top dog|pres|chancellor|prez|pm|premier|prime minister|prince minister|prseident", arep$ns2, ignore.case = TRUE)

# TODO Here we have a problem. There are still a couple of correct and incorrect
# codings that do not get coded 0 or 1 by the pk function.  I made some
# adjustments by including terms in the combine vectors and was able to get all
# the correct versions coded but the incorrect ones remained in the data frame.
# Temporary solution is a manual coding fix but this needs to be checked again.
# Currently, I am unclear what a good solution would look like. Are you OK with a
# manual coding of the remaining correct answers? Alternatively, I can collect
# the series of term combinations that we are currently lacking and include them
# in the vectors.

# TODO On coding of jn: a lot of respondents identified person as cabinet member
# of Obama adminstration. Currently coded as incorrect but is that not
# 'partially' correct?  Given that hidden information is being tested you might
# want to include both a strict and more lenient coding to address Lupia (2006)
# and DeBell (2013).

# Open followed by Closed (Still Photo Versus Text) Text Q
arep$am1c  <- grepl("german|greman|germay", arep$am1, ignore.case = TRUE) &  
              grepl("leader|pres|chancellor|top dog|chanceller|chancelor|chancelier|kansler|pm|premier|prime minister|head", arep$am1, ignore.case = TRUE)

arep$am11c <- nona(arep$am11 == "Chancellor of Germany")

arep$mm1c  <- grepl("senate|sente|senator", arep$mm1, ignore.case = TRUE) & 
              grepl("minority leader|head of repubs|monority|gop-ky|kentucky|republican head|republican leader|head republican", arep$mm1, ignore.case = TRUE)

arep$mm11c <- nona(arep$mm11 == "Senate Minority Leader")

# Photo Q
arep$am2c  <- grepl("german|greman|germay", arep$am2, ignore.case = TRUE) &  
              grepl("leader|pres|chancellor|top dog|chanceller|chancelor|chancelier|kansler|pm|premier|prime minister|head", arep$am2, ignore.case = TRUE)

arep$am21c <- nona(arep$am21 == "Chancellor of Germany")

arep$mm2c  <- grepl("senate|sente|senator", arep$mm2, ignore.case = TRUE) &  
              grepl("minority leader|head of repubs|monority|gop-ky|kentucky|republican head|republican leader|head republican",  arep$mm2, ignore.case = TRUE)

arep$mm21c <- nona(arep$mm21 == "Senate Minority Leader")

arep$pknphoto <- with(arep, rowMeans(cbind(nsc, jnc, hrc, (am1c | am11c), (mm1c | mm11c))))
arep$pkphoto  <- with(arep, rowMeans(cbind(ns2c, jn2c, hr2c, (am2c | am21c), (mm2c | mm21c))))

# Open Closed all work
arep$ww2.cas.c    <- nona(arep$ww2.cas == "[Erstwhile] Soviet Union")
arep$fem.death.c  <- nona(arep$fem.death == "Heart Disease")
arep$ww2.cas2.c   <- grepl("russia|ussr|soviet", arep$ww2.cas2, ignore.case = TRUE)
arep$fem.death2.c <- grepl("heart|card|hypertension", arep$fem.death2, ignore.case = TRUE)
arep$fem.death.m  <- nona(arep$fem.death == "Breast Cancer")
arep$fem.death2.m <- grepl("breast", arep$fem.death2, ignore.case = TRUE)
# TODO The function above does not work. It does not code the incorrect answers
# to 0.

# Confidence Versus Closed Ended
# ------------------------------------

## Closed-Ended
arep$rgc <- nona(arep$rg == "Robert Gates")
arep$sbc <- nona(arep$sb == "Stephen Breyer")

# TODO The functions below do not work because the variables are still string
# variables.  Initially I decided to code them in a certain way but I reversed
# that. Do you remember the initial coding rules?
arep$future.inc.c <- nona(arep$healthbill=="Limits future increases in payments to Medicare providers")
arep$upper.class.c <- nona(arep$healthcare.pk1=="Increases the Medicare payroll tax for upperâ€income Americans")

arep$mammograms.c 	<- grepl("mammograms", arep$healthcare.pk1, ignore.case = TRUE)
arep$illegal.c <- grepl("illegal", arep$healthcare.pk1, ignore.case = TRUE)
arep$single.payer.c  <- grepl("single", arep$healthcare.pk1, ignore.case = TRUE)

arep$death.panel.c  <- grepl("panel", arep$healthbill, ignore.case = TRUE)
arep$medicare.c  <- grepl("replaces medicare", arep$healthbill, ignore.case = TRUE)
arep$cuts.benefits.c <- grepl("cuts benefits", arep$healthbill, ignore.case = TRUE)

arep$death.panel.c  <- arep$healthbill

# Confidence Measures - Preprocessing - greater than 10 is divided by 10 and NA
# is converted to 0

# Robert Gates
arep$rg.rg <- conf.adj(arep$rg.rg)
arep$rg.ad <- conf.adj(arep$rg.ad)
arep$rg.jc <- conf.adj(arep$rg.jc)
arep$rg.ak <- conf.adj(arep$rg.ak)

# Stephen Breyer
arep$sb.sb <- conf.adj(arep$sb.sb)
arep$sb.jb <- conf.adj(arep$sb.jb)
arep$sb.ks <- conf.adj(arep$sb.ks)
arep$sb.tv <- conf.adj(arep$sb.tv)

# Healthcare 1
arep$future.increase <- conf.adj(arep$future.increase)
arep$death.panel     <- conf.adj(arep$death.panel)
arep$medicare        <- conf.adj(arep$medicare)
arep$cuts.benefits   <- conf.adj(arep$cuts.benefits)

# Healthcare 2
arep$upper.class     <- conf.adj(arep$upper.class)
arep$illegal         <- conf.adj(arep$illegal)
arep$single.payer    <- conf.adj(arep$single.payer)
arep$mammograms      <- conf.adj(arep$mammograms)

# Confidence scoring (type 2)
arep$rg.cf          <- with(arep, confy(rg.rg, rg.ad, rg.jc, rg.ak))
arep$sb.cf          <- with(arep, confy(sb.sb, sb.jb, sb.ks, sb.tv))
arep$future.inc.cf  <- with(arep, confy(future.increase, death.panel, medicare, cuts.benefits))
arep$upper.class.cf <- with(arep, confy(upper.class, illegal, single.payer, mammograms))

# Misinformed CF
arep$death.panel.cf   <- with(arep, confy(death.panel, future.increase, medicare, cuts.benefits))
arep$medicare.cf      <- with(arep, confy(medicare, future.increase, death.panel, cuts.benefits))
arep$cuts.benefits.cf <- with(arep, confy(cuts.benefits, future.increase, death.panel, medicare))

arep$illegal.cf       <- with(arep, confy(illegal, upper.class, single.payer, mammograms))
arep$single.payer.cf  <- with(arep, confy(single.payer, upper.class, illegal, mammograms))
arep$mammograms.cf    <- with(arep, confy(mammograms, upper.class, illegal, single.payer))


# Media -------------------------------------------- Conservative Media news.wsj,
# cable.fox, radio.ms, radio.rl

arep$con.media <- with(arep, rowMeans(cbind(!is.na(news.wsj), !is.na(cable.fox), !is.na(radio.ms), !is.na(radio.rl))))

# news.nyt, news.wp, news.usa, news.other, news.other.s tv.cbs, tv.fox, tv.pbs,
# tv.nbc, tv.other, tv.abc, tv.others cable.fox, cable.msnbc, cable.cspan,
# cable.bbc, cable.other, cable.cnn, cable.others radio.npr, radio.rl, radio.ms,
# radio.other, radio.others web.huff, web.drudge, web.portal, web.other,
# web.other.s tv.days attention

arep$attention.r <- recode(arep$attention, 'A great deal'=1, 'A lot'=.75, 'A moderate amount'=.50, 'A little'=.25, 'Not at all'=0)

## Misinformation
# Closed-Ended
# Steagall Act, Closed-Ended
arep$steagall.c   <- nona(arep$steagall == "Bill Clinton")
arep$steagall.rm  <- nona(arep$steagall == "Ronald Reagan" | arep$steagall == "George W. Bush" |  arep$steagall == "George H. Bush")

# Budget Related
arep$reagan.m     <- nona(arep$reagan == "Decreased" | arep$reagan == "Remained the Same")
arep$clinton.m    <- nona(arep$clinton == "Increased" | arep$clinton == "Remained the Same")
arep$wbush.m      <- nona(arep$wbush == "Decreased" | arep$wbush == "Remained the Same")
arep$obama.m    <- nona(arep$obama=="Decreased" | arep$obama=="Remained the Same")

arep$reagan2.m    <- nona(arep$reagan2 == "Decreased" | arep$reagan2 == "Remained the Same")
arep$clinton2.m   <- nona(arep$clinton2 == "Increased" | arep$clinton2 == "Remained the Same")
arep$wbush2.m     <- nona(arep$wbush2 == "Decreased" | arep$wbush2 == "Remained the Same")
arep$obama2.m     <- nona(arep$obama2 == "Decreased" | arep$obama2 == "Remained the Same")

arep$alrgn <- combiner(arep$reagan2, arep$reagan, arep$dkencouraging.dk.discouraging)
arep$alctn <- combiner(arep$clinton2, arep$clinton, arep$dkencouraging.dk.discouraging)
arep$albsh <- combiner(arep$wbush2, arep$wbush, arep$dkencouraging.dk.discouraging)
arep$alobm <- combiner(arep$obama2, NA, arep$dkencouraging.dk.discouraging)

arep$rgn   <- combiner(arep$reagan2 == "Increased", arep$reagan == "Increased", arep$dkencouraging.dk.discouraging)
arep$ctn   <- combiner(arep$clinton2 == "Decreased", arep$clinton == "Decreased", arep$dkencouraging.dk.discouraging)
arep$bsh   <- combiner(arep$wbush2 == "Increased", arep$wbush == "Increased", arep$dkencouraging.dk.discouraging)
arep$obm   <- combiner(arep$obama2 == "Increased", NA, arep$dkencouraging.dk.discouraging)

arep$rgn.m <- combiner(arep$reagan2.m, arep$reagan.m, arep$dkencouraging.dk.discouraging)
arep$ctn.m <- combiner(arep$clinton2.m, arep$clinton.m, arep$dkencouraging.dk.discouraging)
arep$bsh.m <- combiner(arep$wbush2.m, arep$wbush.m, arep$dkencouraging.dk.discouraging)
arep$obm.m <- combiner(arep$obama2.m, NA, arep$dkencouraging.dk.discouraging)

## Immigration Related Deport under Obama The response options are messed up TODO:
## What does the above comment mean for the data/coding?

# TODO Here the deport variable is again a string variable. Do you remember the
# initial coding or do you have a version with out the labels?
arep$deport.c <- nona(arep$deport == "Increased") #Increased
arep$deport.m <- nona(arep$deport == "Remained about the same" | arep$deport == "Decreased")

# AZ Immigration Law
arep$imm.c <- nona(arep$imm == "Can ask people they suspect of being illegal immigrants for their papers only when stopping them for other reasons") 
arep$imm.m <-  nona(arep$imm== "Can  ask anyone they suspect of being an illegal immigrant for their papers")

# Misinformed and Sure
arep$deport.ms <- arep$deport.m * nona(arep$deport.sure > 5)
arep$imm.ms    <- arep$imm.m * nona(arep$imm.sure > 5)

## Open-ended
arep$tax.100k.r <- as.numeric(sub("%", "", arep$tax.100k))
# Roughly what percentage of the U.S. federal budget would you say goes toward
# welfare?
arep$welfare.r <- as.numeric(sub("%", "", arep$welfare))
# Roughly what percentage of Blacks in the U.S. would you say are on welfare?
arep$blk.welfare.r <- as.numeric(sub("%", "", arep$blk.welfare))

# Political Variables
# --------------------------------
arep$libcon.surv <- recode(arep$ideology, "Very liberal"=1, "Liberal"=2, "Slightly liberal"=3, "Moderate"=4,
                           "Slightly conservative"=5, "Conservative"=6, "Very Conservative"=7)

# PID
arep$pid <- NA
temp <- with(arep, paste(pid1, pid2.r, pid2.d, pid.other, pid2.other))
arep$pid[temp == "Democrat NA Strong NA NA"] <- 1
arep$pid[temp == "Democrat NA Not very strong NA NA NA"] <- 2
arep$pid[temp == "Independent NA NA NA Closer to Democratic Party"] <- 3
arep$pid[temp == "Independent NA NA NA Equally close to both"] <- 4
arep$pid[temp == "Independent NA NA NA Closer to Republican Party"] <- 5
arep$pid[temp == "Republican Not very strong NA NA NA"] <- 6
arep$pid[temp == "Republican Strong NA NA NA"] <- 7

arep$rd <- recode(arep$pid, "1" = "dem", "2" = "dem", "3" = "dem",
                  "5" = "rep", "6" = "rep", "7" = "rep", .default = NA_character_)

# Political Interest
arep$pol.interest <- zero1(arep$pol.interest)

# TODO Should the variables below also coded with the zero1 function?

# vote.2010 contribute1 contact attend def.party def.other

## Policy Attitudes
# TODO Still returns DK
arep$healthcare.supp <- combiner(arep$healthcare.law, arep$healthcare.law2, arep$policy.placement)
arep$imm.supp <- combiner(arep$imm.law, arep$imm.law2, arep$policy.placement)

# Thermometer Scales
#-------------------------

arep$therm.br  <- zero1(num(arep$therm.b), 0, 100)
arep$therm.wr  <- zero1(num(arep$therm.w), 0, 100)
arep$therm.rr  <- zero1(num(arep$therm.r), 0, 100)
arep$therm.dr  <- zero1(num(arep$therm.d), 0, 100)
arep$therm.hr  <- zero1(num(arep$therm.h), 0, 100)
arep$therm.lr  <- zero1(num(arep$therm.l), 0, 100)
arep$therm.ilr <- zero1(num(arep$therm.il), 0, 100)

# Sociodem TODO Arep does not have a gender variable

arep$female <- as.numeric(arep$gender == "Female")

# Personality
#-------------------------

re7 <- "7=1;6=2;5=3;4=4;3=5;2=6;1=7"
# arep$structure arep$certain arep$oneside arep$changeview arep$confident
arep$curi.r <- zero1(arep$curiosity, 1, 7)
arep$cons.r <- zero1(arep$considerate, 1, 7)
arep$inde.r <- zero1(arep$independence, 1, 7)
arep$obed.r <- zero1(recode(arep$obedience, re7), 1, 7)
cor(cbind(arep$curi.r, arep$cons.r, arep$inde.r, arep$obed.r), use = "na.or.complete")
arep$auth <- rowMeans(cbind(arep$curi.r, arep$cons.r, arep$inde.r, arep$obed.r), na.rm = T)

