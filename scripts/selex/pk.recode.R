##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##   											##
##      Selective Exposure (Recode)
##		YouGov/Polimetrix 
##		Last Edited: 4.09.12   	
##   	Yph Lelkes and Gaurav Sood				##		
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

# Set Working dir.
	setwd(basedir)

# Sourcing Common Functions
	source("func/func.R")
	source("func/match.R")
	source("func/mes.func.R")
	
# Read Data
	selex <- foreign::read.spss("SelectiveExposurePolarization/Data/MainSurvey/STAN0053_OUTPUT.sav", to.data.frame=T)

# PID
	selex$pid3 <- car::recode(as.numeric(selex$pid7),"1:3='Democrat';5:7='Republican';else='Independent'",as.factor=T)
	selex$pid2 <- car::recode(as.numeric(selex$pid7),"1:3='Democrat';5:7='Republican';else=NA",as.factor=T)
	
	selex$pidfolded		 <- zero1(as.numeric(selex$pid7)-4)
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

# Political Activism
	selex$eventsinterest <- car::recode(as.numeric(selex$eventsinterest),"5=NA")
	selex$newsint		<- car::recode(as.numeric(selex$newsint),"5=NA")
	selex$registered <- car::recode(as.numeric(selex$votereg),"3=2")
	selex$activist		<- abs(zero1(rowMeans(cbind(selex$registered,selex$eventsinterest, selex$newsint),na.rm=T))-1)
	
# Political Knowledge
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	# Two things going on: 
		# Picture or no picture (qpk1_treat, qpk2_treat)
		# Those who mark Don't Know are asked to guess
	
	# Coding: Assign Missing to 0
			
	# John Boehner
		selex$boehner   <- as.numeric(car::recode(selex$qpk1, "'Speaker of the U.S. House of Representatives'=1; else=0")==1)
		selex$boehner2  <- as.numeric(car::recode(selex$qpk1_force, "'Speaker of the U.S. House of Representatives'=1; else=0")==1)
		selex$boehner2  <- ifelse(!is.na(selex$qpk1) & as.numeric(as.factor(selex$qpk1))==5, selex$boehner2, selex$boehner)

		# People do better with text: with(selex, xtabs(~ qpk1_treat + boehner2))
		# with(selex, chisq.test(boehner, qpk1_treat))

	# Stephen Breyer
		selex$breyer   <- as.numeric(car::recode(selex$qpk2, "'United States Supreme Court Justice'=1; else=0")==1)
		selex$breyer2  <- as.numeric(car::recode(selex$qpk2_force, "'United States Supreme Court Justice'=1; else=0")==1)
		selex$breyer2  <- ifelse(as.numeric(as.factor(selex$qpk2))==5, selex$breyer2, selex$breyer)
		
		# No Diff. here: with(selex, xtabs(~ qpk1_treat + breyer))
		# with(selex, chisq.test(breyer, qpk1_treat))
	
	# No Photo Manipulation
	# Ben Bernanke
		selex$bernanke   <- as.numeric(car::recode(selex$qpk3, "'Chairman of the Federal Reserve'=1; else=0")==1)
		selex$bernanke2  <- as.numeric(car::recode(selex$qpk3_force, "'Chairman of the Federal Reserve'=1; else=0")==1)
		selex$bernanke2  <- ifelse(as.numeric(as.factor(selex$qpk3))==5, selex$bernanke2, selex$bernanke)
	
	# Partisan Cues (qpk7_insert)
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		# Unemployment
			selex$unemp    <- as.numeric(car::recode(selex$qpk7, "'Gone Down'=1; else=0")==1)
		
		# Budget Deficit
			selex$deficit   <- as.numeric(car::recode(selex$qpk8, "'Gone Up'=1; else=0")==1)
			
# PK Open-ended
# ~~~~~~~~~~~~~
	# Phillip Phillips (Lots of people identify him as Zuckerberg); Prop. Correct = .1
	# Coded as correct if people were able to identify his first/last name (and misspelled versions), place him on american idol
		selex$snews1r <- nona(pk(clean(selex$snews1), c("phil", "america", "idol", "phlips", "phiphs"), c(""))) 
	
	# Lady Gaga
	# Error for 1 respondent or more: "thereisnopictureonthispage"; code as Missing
	# Partial Correct can be given to those who think she is a singer
	# Lots of people think she is Nikki Minaj
		selex$snews2r <- nona(pk(clean(selex$snews2), c("gaga", "gagga", "lady", "gaugau"), c(""))) 

	# Harry Reid
	
		# Correct: Anybody who identifies him as a senator; gets part of his name correct
		temp 		 <- pk(clean(selex$qpk4), c("sen", "sebate", "harry", "sean", "hreed", "rei"), "")
		
		# Epithets Coded as correct
		temp2        <- pk(temp, c("blowhard",  "senateidiot", "chiefidiot", "leaderofthehouseandchiefjackass", "leaderoftheidiots", "biggestassholeinthesenate", 
									"turd", "aholelicker", "heshouldberemovedfromoffice", "topbunghole", "traitor", "notanintelligentone", "bigmouth"), c(""))
		# Majority or Minority Leader, Dem. Leader, etc.
		temp3        <- pk(temp2, c("maj", "manority", "marjoity", "mayority", "mrgority", "minority", "dem", "house", "congress"), c("lead",  "topdog", "speaker", "spaker", "head", "chair", "pres", "whip", "whp"))
		# Misc.
		selex$qpk4r <- nona(pk(temp3, c("majorityle", "headifse", "repofnevada", "speakerh"), c("")))
				
		
	# Current U.S. Secretary of Defense
		selex$qpk5r   <- nona(pk(clean(selex$qpk5), c("pen", "pane", "pann", "pina", "pine", "pana", "pone", "leo", "pideta", "baneta", "enetta", "patella", "planneta"), c(""))) 
	
	# President of France (Election held in late April)
		# After Hollande, Lots of people pick Sarkozy. A good question for much attention people are paying currently to politics
		# Coding: Hollande, Sarkozy, Else
		temp   		  <- pk(clean(selex$qpk6), c("hol", "frank", "francois" , "social", "socilist", "headoftheirliberalparty"), c(""), "hollande") 
		temp2	  	  <- pk(temp, c("sarko", "sar", "zar"), c(""), "sarkozy") 
		selex$qpk6r	  <- ifelse(temp2 %in% c("hollande", "sarkozy"), temp2, 'else')
		selex$qpk6rc  <- car::recode(selex$qpk6r,"'sarkozy'=1;else=0")
		
		
	# Barack Obama's Religion
		# Coding: Muslim, Christian, (Communist, Marxist), Atheist or Agnostic, All Other (jewish, mormon, jehovah, hindu, buddhist, druad, heath), Liberation Theology, Who Knows, Don't Know
		# Some using 'not' christian
		# Tangible proportion respond saying: Does it matter?

		temp1		<- pk(clean(selex$qpk9), c("dont", "donot", "dunno", "donoknow", "nosure", "notsure", "noclue", "unknown", "hhjjj", "yhjyjum", "iamnotentirely", 
												"dfbdfbsdfgb", "hfjhjkhgg", "asdfghjuk", "unkown", "cantsay", "icantdecide", "goodquestion", "nothing", "noidea", "whocares",
												"ihaventaclue", "unaware", "cantremember", "cantmakemymind", "dna", 
												"canttell", "kontknow", "notsuer", "dk", "forget", "whateve"), c(""), "don't know")
		temp2   	<- pk(temp1, c("communis", "comu", "marx", "socialism"), c(""), "communist")
		temp3   	<- pk(temp2, c("athei", "athi", "aithest", "atiest", "noreligion", "doeshehaveone", "agnostic", "doesnthaveone", 
									"doesenthaveonechurchofobama", "hehasnone", "doesntseemtoreallyhaveone", "none"), c(""), "atheist")
		temp4   	<- pk(temp3, c("theology", "liberation", "black", "african", "racism", "revwright"), c(""), "black theology")
		temp5   	<- pk(temp4, c("whoreallyknows",  "whoknowsreally", "hardtotell", "nobodyknows", "hedosnotknowhimself", "weallwouldliketoknowthatone", "whknows", "whoknow", "youtellme", 
									"whoknowsforsure", "whoknowsdoeshaveone", "havenoideaitsabigsecretapparently", "undetermined", "whoknowsheisallovertheplace","youtellmeplease",
									"whoknowsheliessomuchnooneknowsthetru", "questionable", "hewonttellus", "nooneknowsthetrueanswer","whocanbesureofsuchapolitician",
									"wellthatisthequestionisntitithinkhisreligionishimself",
									"doesntsay", "hedoesntknow"), c(""), "who knows") 
		temp6   	<- pk(temp5, c("non", "not", "anti", "faux"), c("christ"), "All Other") 
		temp7   	<- pk(temp6, c("jew", "heath", "druad", "pagan", "jeohvah", "mythicalevan", "budda", "buddisht", "budhist", "johova", "mormon", "hindu"), c(""), "All Other") 
		temp8   	<- pk(temp7, c("irrelevant", "doesntmatter", "itdoesnotmatter", "whydoesthatmatter", "thatshisbusiness", "notrelevant"), c(""), "Does it Matter") 
		temp9       <- pk(temp8, c("musl", "muselim", "museum", "muli", "mosl", "musi", "isam", "morman", "unslim", "mulsum", "mulsom", "mussl", "molsen", "musem", "musulim", "islam", "isalm", "islom", "isloum"), c(""), "Muslim") 
		temp10	  	<- pk(temp9, c("chris", "cris", "chri", "chir", "chis", "xian", "chiris", "chtis", "chistin", "cristain",  "creshtion",
									"lutheran", "presbyterian", "metodist","unitarian", "ucc",
									"proestant", "protestan", "prodestant", "prodistant","prodesde",
									"catolic", "cath", "chatlic", 
									"babtist", "bapist", "baptist",  "baptised", "bapit", 
									"episcop", "episca", "methodist"), "", "Christian")
		selex$qpk9r <- ifelse(temp10 %in% c("don't know", "communist", "atheist", "black theology", "who knows", "all other", "does it matter", "muslim", "Christian"), temp10, "don't know")
		selex$qpk9rc<- car::recode(selex$qpk9r,"'Christian'=1;else=0") 
		
		#	table(clean(selex$qpk9[selex$qpk9r=='muslim']))
				
	# Mitt Romney's Religion
		# Coding: Mormon, (Protestant, Catholic, Christian), All Other
		selex$qpk10r   <- nona(pk(clean(selex$qpk10), c("mpr", "mor", "mom", "mol", "mon",  "7day", "seventh", "lds", "laterday",  "churchofl", "latter", "moorman", "norm", "utahsbaserelig"), c("")))

		# Coding Ideological Placement of Politicians and Parties
		# Dem. and Republicans
		selex$ideo_dem <-car::recode(as.numeric(selex$ideo_dem),"8=NA")
		selex$ideo_rep <-car::recode(as.numeric(selex$ideo_rep),"8=NA")
		
		# 
		selex$ideo_ows <-car::recode(as.numeric(selex$ideo_ows),"8=NA")
		selex$ideo_tp  <-car::recode(as.numeric(selex$ideo_tp),"8=NA")
		
		# Romney and Obama
		selex$ideo_mr <-car::recode(as.numeric(selex$ideo_mr),"8=NA")
		selex$ideo_bo <-car::recode(as.numeric(selex$ideo_bo),"8=NA")
		
		# Relative scoring of ideology
		selex$ideopk1 <- with(selex, ifelse(ideo_dem < ideo_rep|is.na(ideo_dem)|is.na(ideo_rep),1,0))
		selex$ideopk2 <- with(selex, ifelse(ideo_ows < ideo_tp |is.na(ideo_ows)|is.na(ideo_tp),1,0))
		selex$ideopk3 <- with(selex, ifelse(ideo_bo  < ideo_mr |is.na(ideo_bo)|is.na(ideo_mr),1,0))
		
		forirt 		  <- with(selex, data.frame(boehner2,breyer2,bernanke2, qpk4r,qpk5r,qpk6rc,deficit,unemp,qpk9rc,qpk10r,ideopk1,ideopk2,ideopk3))
		
		require(ltm)
		ltm.ltm		<- ltm(forirt~z1)
		plot(ltm.ltm)
		ltm.scores	<- factor.scores(ltm.ltm)
		forirt$join <- do.call(paste0, forirt)
		newdf		<- ltm.scores$score.dat
		newdf$join 	<- do.call(paste0, newdf[,1:13])
		ll 			<- merge(forirt, newdf, by='join', all.x=T, all.y=T)
		politicalknowledge <- with(selex, rowMeans(data.frame(boehner2,breyer2,bernanke2, qpk4r,qpk5r,qpk6rc,deficit,unemp,qpk9rc,qpk10r,ideopk1,ideopk2,ideopk3),na.rm=T))
		
		# Something bad is going on here - Please explore
		#################################################
		cor(ll$z1, politicalknowledge, use='pairwise.complete.obs')
		
	# Save Data
		save(selex, file="misinformation/data/selex.recode.rdata")