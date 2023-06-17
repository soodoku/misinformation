##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##   											
##		Selective Exposure
##		Main Survey: Recode
##		Last Edited: 4.09.13   	
##		Gaurav Sood	
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

# Source functions
	source(paste0(basedir, "func.R"))    

# Load data
	#load("SelectiveExposurePolarization/data/selex.recode.rdata")
	selex <- foreign::read.spss("SelectiveExposurePolarization/data/MainSurvey/STAN0053_OUTPUT.sav",to.data.frame=T)

# PID
	selex$pid3 <- car::recode(as.numeric(selex$pid7),"1:3='Democrat';5:7='Republican';else='Independent'",as.factor=T)
	selex$pid2 <- car::recode(as.numeric(selex$pid7),"1:3='Democrat';5:7='Republican';else=NA",as.factor=T)
	
	selex$pidfolded		<- zero1(selex$pid7-4)
	selex$strongpartisan <- car::recode(as.numeric(selex$pid7),"1=1;7=1;else=0")
	
# Sociodem.
	#selex$gender
	selex$age		<- zero1(2011-selex$birthyr)	
	selex$south		<- car::recode(selex$inputstate,"c('Texas','Oklahoma','Arkansas','Louisiana','Mississippi','Alabama','Georgia','Tennessee','Kentucky','West Virginia','Virginia','Maryland','Delaware','Kentucky','South Carolina','North Carolina','Florida')=1;else=0")
 	selex$single		<- car::recode(selex$marstat,"'Single'=1;else=0")
	selex$educnum	<- car::recode(as.numeric(selex$educ),"1:2='Hs or Less';3:4='Some College';5:6='College+'",as.factor=T)
	selex$race		<- car::recode(as.numeric(selex$race),"1='White';else='Non-White'")
	
# Partisan Feelings
	##Traditional Affective Polarization Feeling Thermometer
	selex$therm_rp[selex$therm_rp>100]=NA
	selex$therm_dp[selex$therm_dp>100]=NA
  
  	selex$outfeelings <- NA
	selex$outfeelings[which(selex$pid2=='Republican')] <- selex$therm_dp[which(selex$pid2=='Republican')]
	selex$outfeelings[which(selex$pid2=='Democrat')]	 <- selex$therm_rp[which(selex$pid2=='Democrat')]
	selex$infeelings <- NA
	selex$infeelings[which(selex$pid2=='Democrat')] <- selex$therm_dp[which(selex$pid2=='Democrat')]
	selex$infeelings[which(selex$pid2=='Republican')]	 <- selex$therm_rp[which(selex$pid2=='Republican')]
	selex$affpol_ft <- zero1(selex$infeelings - selex$outfeelings)
	
  ##Relative feelings about party supporters

	selex$likescale_sdem <- car::recode(selex$likescale_sdem,"997=NA")
	selex$likescale_srep <- car::recode(selex$likescale_srep,"997=NA")
	
	selex$likesin <- NA
	selex$likesin[which(selex$pid2=='Democrat')] <- selex$likescale_sdem[which(selex$pid2=='Democrat')]
	selex$likesin[which(selex$pid2=='Republican')] <- selex$likescale_srep[which(selex$pid2=='Republican')]
	
	selex$likesout <- NA
	selex$likesout[which(selex$pid2=='Republican')] <- selex$likescale_sdem[which(selex$pid2=='Republican')]
	selex$likesout[which(selex$pid2=='Democrat')] <- selex$likescale_srep[which(selex$pid2=='Democrat')]
	selex$personpolarization <- zero1(selex$likesin - selex$likesout)
	
#   ##split ticket
# selex$happycongress		<- zero1(abs((as.numeric(selex$angry_dem)+as.numeric(selex$happy_rep))-(as.numeric(selex$angry_rep)+as.numeric(selex$happy_dem))))
	
  ##Perceived Polarization, difference in difference
	selex$rep_likescale_srep <- car::recode(selex$rep_likescale_srep,"997=NA")
	selex$rep_likescale_sdem <- car::recode(selex$rep_likescale_sdem,"997=NA")
	selex$dem_likescale_sdem <- car::recode(selex$dem_likescale_sdem,"997=NA")
	selex$dem_likescale_srep <- car::recode(selex$dem_likescale_srep,"997=NA")
  
 selex$perceivedpolarization <-  zero1(with(data, abs(rep_likescale_srep-rep_likescale_sdem)-(dem_likescale_sdem-dem_likescale_srep)))

  ##
	selex$socialdems <- as.numeric(selex$feeling1)+as.numeric(selex$feeling3)
	selex$socialreps <- as.numeric(selex$feeling2)+as.numeric(selex$feeling4)
	selex$socialdistancepolarization <- NA
	selex$socialdistancepolarization[which(selex$pid2=='Democrat')] <- zero1(selex$socialdems[which(selex$pid2=='Democrat')]-selex$socialreps[which(selex$pid2=='Democrat')])
	selex$socialdistancepolarization[which(selex$pid2=='Republican')] <- zero1(selex$socialreps[which(selex$pid2=='Republican')]-selex$socialreps[which(selex$pid2=='Republican')])
	  
	
  psych::alpha(with(data,data.frame(personpolarization,affpol_ft,socialdistancepolarization)))
  
  selex$inparty_signs  <- ifelse((selex$pid2=='Democrat' & selex$sign_insert==levels(selex$sign_insert)[2])| 
								(selex$pid2=='Republican' & selex$sign_insert==levels(selex$sign_insert)[1]),1,0)

  
	#psych::alpha(with(data,data.frame(affpol_ft,polarizationsd,happycongress,personpolarization)))
	selex$affectivepolarization	<- rowMeans(with(data,data.frame(affpol_ft,socialdistancepolarization,personpolarization)),na.rm=T)
	
# Political Activism
	selex$eventsinterest <- car::recode(as.numeric(selex$eventsinterest),"5=NA")
	selex$newsint		<- car::recode(as.numeric(selex$newsint),"5=NA")
    selex$registered <- car::recode(as.numeric(selex$votereg),"3=2")
	selex$activist		<- abs(zero1(rowMeans(cbind(selex$registered,selex$eventsinterest, selex$newsint),na.rm=T))-1)
	  
  selex$pidfolded <-   as.numeric(selex$pid7)
	selex$pidfolded[selex$pid7==8]=4
  selex$pidfolded <- abs(selex$pidfolded-4)
  # Political Knowledge
	#source("pk.recode.R")
	load("Data/abilityscores.Rdata")
	selex$pk <- zero1(z1$F1)
# Selective Exposure
	source("./Rcode/selfselectionvariable.R")
	selex$ssindex <- rowMeans(with(data,data.frame(ss1,ss2,ss3,ss4),na.rm=F))
	source("./Rcode/hsvariable.R")
	selex$hsindex <- rowMeans(with(data,data.frame(hs1=="Soft",hs2=="Soft",hs3=="Soft",hs4=="Soft"),na.rm=F))
	#polycor::hetcor(data.frame(dr,as.factor(selex$voter)))
	selex$snewsk <- rowMeans(data.frame(selex$snews1r,selex$snews2r))
###Ratio of how close R follows soft news to how closely R follows hard news
	selex$snewsg1 <- abs(zero1(as.numeric(selex$snewsg1))-1)
	selex$snewsg2 <- abs(zero1(as.numeric(selex$snewsg2))-1)
	selex$snewsg3 <- abs(zero1(as.numeric(selex$snewsg3))-1)
	selex$snewsg4 <- abs(zero1(as.numeric(selex$snewsg4))-1)
	selex$snewsg5 <- abs(zero1(as.numeric(selex$snewsg5))-1)
	selex$snewsg6 <- abs(zero1(as.numeric(selex$snewsg6))-1)
	selex$snewsg7 <- abs(zero1(as.numeric(selex$snewsg7))-1)
	
	selex$softnews <- zero1((with(data, rowMeans(data.frame(snewsg2,snewsg3,snewsg7)))+1)/(with(data, rowMeans(data.frame(snewsg4, snewsg5,snewsg6)))+1))

###Ratio of how close R follows soft news to how closely R follows hard news
	
  
	selex$congruentindex <- rowMeans(data.frame(selex$tss1=="Congruent",selex$tss2=="Congruent",selex$tss3=="Congruent",selex$tss4=="Congruent"))
	  
##  product of (not interaction term) prop. hard news and prop. congenial
selex$hsindexssindex <- selex$hsindex * selex$ssindex
  
## Proportion of  times R selected hard and congruent
selex$hardsel <- rowMeans(data.frame(selex$hs1=='Hard' & selex$tss1=="Congruent",selex$hs2=='Hard' & selex$tss2=="Congruent",selex$hs3=='Hard' & selex$tss3=="Congruent",selex$hs4=='Hard' & selex$tss4=="Congruent"))
	
  datal$tss <- as.factor(datal$tss1)
	datal$tss <- relevel(datal$tss,ref="Incongruent")
	datal$trial <- as.factor(datal$time)

# Save data
	save(data,file="Data/coded.RData")
	
# Reshape data
	datal <- reshape(data, idvar='caseid', varying=list(c("tss1","tss2","tss3","tss4"),c("hs1","hs2","hs3","hs4")) ,direction='long')
	save(datal,file="Data/codedlong.RData")
	
  
  