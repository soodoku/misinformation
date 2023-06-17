#
# Analysis of AREP and STEP
#

# Set Working dir 
setwd(basedir)
setwd("hidden")

# Load libaries
library(broom)

# Sourcing Common Functions
source("scripts/00_common_func.R")

# Source Recode Files
source("scripts/01_arep_recode.R")
source("scripts/02_srep_recode.R")

## Randomizations policy.placement open.closed photo.nonphoto.pk correct.or.conf
## dkencouraging.dk.discouraging

# Table 1: Closed- vs Open-ended versions of the same item
tt_arep_ww2  <- with(arep, tidy(t.test(ww2.cas.c ~ open.closed)))
tt_srep_ww2  <- with(srep, tidy(t.test(ww2.cas.c ~ open.closed)))
tt_arep_fd   <- with(arep, tidy(t.test(fem.death.c ~ open.closed)))
tt_srep_fd   <- with(srep, tidy(t.test(fem.death.c ~ open.closed)))

tab_1 <- rbind(tt_arep_ww2, tt_arep_fd, tt_srep_ww2, tt_srep_fd)
tab_1 <- cbind(labs = c("Alumni WW2", "Staff FD", "SREP WW2", "SREP FD"), tab_1)

write.csv(tab_1, file="arep_srep_closed_open.csv", row.names=F)

# Confidence Assessment 
# --------------------------------
tab.conf <- data.frame(arep.qno = NA, arep.cls = NA, arep.08 = NA, arep.06 = NA,
    arep.00 = NA, srep.cls = NA, srep.08 = NA, srep.06 = NA, srep.00 = NA)

arep$death.panel.c <- arep$death.panel.c == table(arep$death.panel.c)[1]

tab.conf[1, 1:5] <- c("Death Panel", with(arep, par3(death.panel > 0.8, death.panel > 0.5 & death.panel <= 0.8, death.panel <= 0.5, death.panel.c, correct.or.conf)))
tab.conf[1, 6:9] <- with(srep, par3(death.panel > 0.8, death.panel > 0.5 & death.panel <=
    0.8, death.panel <= 0.5, death.panel.c, correct.or.conf))

tab.conf[2, 1:5] <- c("Medicare", with(arep, par3(medicare > 0.8, medicare > 0.5 &
    medicare <= 0.8, medicare <= 0.5, medicare.c, correct.or.conf)))
tab.conf[2, 6:9] <- with(srep, par3(medicare > 0.8, medicare > 0.5, medicare <= 0.5 &
    medicare <= 0.8, medicare.c, correct.or.conf))

tab.conf[3, 1:5] <- c("Cuts Benefits", with(arep, par3(cuts.benefits > 0.8, cuts.benefits >
    0.5 & cuts.benefits <= 0.8, cuts.benefits <= 0.5, cuts.benefits.c, correct.or.conf)))
tab.conf[3, 6:9] <- with(srep, par3(cuts.benefits > 0.8, cuts.benefits > 0.5, cuts.benefits <=
    0.5 & cuts.benefits <= 0.8, cuts.benefits.c, correct.or.conf))

tab.conf[4, 1:5] <- c("Illegals", with(arep, par3(illegal > 0.8, illegal > 0.5 &
    illegal <= 0.8, illegal <= 0.5, illegal.c, correct.or.conf)))
tab.conf[4, 6:9] <- with(srep, par3(illegal > 0.8, illegal > 0.5, illegal <= 0.5 &
    illegal <= 0.8, illegal.c, correct.or.conf))

tab.conf[5, 1:5] <- c("Single Payer", with(arep, par3(single.payer > 0.8, single.payer >
    0.5 & single.payer <= 0.8, single.payer <= 0.5, single.payer.c, correct.or.conf)))
tab.conf[5, 6:9] <- with(srep, par3(single.payer > 0.8, single.payer > 0.5 & single.payer <=
    0.8, single.payer <= 0.5, single.payer.c, correct.or.conf))

tab.conf[6, 1:5] <- c("Mammograms", with(arep, par3(mammograms > 0.8, mammograms >
    0.5 & mammograms <= 0.8, mammograms <= 0.5, mammograms.c, correct.or.conf)))
tab.conf[6, 6:9] <- with(srep, par3(mammograms > 0.8, mammograms > 0.5 & mammograms <=
    0.8, mammograms <= 0.5, mammograms.c, correct.or.conf))

write.csv(tab.conf, file = "res/tab5.asrep.csv")


tab.conf <- data.frame(arep.qno = NA, arep.cls = NA, arep.08 = NA, arep.06 = NA,
    arep.00 = NA, srep.cls = NA, srep.08 = NA, srep.06 = NA, srep.00 = NA)

tab.conf[1, 1:5] <- c("Obama -Deport", with(arep, par3(deport.m > 0.8, deport.m > 0.5 & deport.m <= 0.8, deport.m <= 0.5, deport.m)))
tab.conf[1, 6:9] <- with(srep, par3(deport.m > 0.8, deport.m > 0.5 & deport.m <= 0.8, deport.m <= 0.5, deport.m))

tab.conf[2, 1:5] <- c("AZ - Imm.: Miranda", with(arep, par3(imm.m > 0.8, imm.m >
    0.5 & imm.m <= 0.8, imm.m <= 0.5, imm.m)))
tab.conf[2, 6:9] <- with(srep, par3(imm.m > 0.8, imm.m > 0.5 & medicare <= 0.8, imm.m <=
    0.5, imm.m))

tab.conf[2, 1:5] <- c("AZ - Imm.: Papers", with(arep, par3(imm.m > 0.8, imm.m > 0.5 &
    imm.m <= 0.8, imm.m <= 0.5, imm.m)))
tab.conf[2, 6:9] <- with(srep, par3(imm.m > 0.8, imm.m > 0.5 & medicare <= 0.8, imm.m <=
    0.5, imm.m))

tab.conf[2, 1:5] <- c("AZ - Imm.: Escort", with(arep, par3(imm.m > 0.8, imm.m > 0.5 &
    imm.m <= 0.8, imm.m <= 0.5, imm.m)))
tab.conf[2, 6:9] <- with(srep, par3(imm.m > 0.8, imm.m > 0.5 & medicare <= 0.8, imm.m <=
    0.5, imm.m))

tab.conf[2, 1:5] <- c("AZ - Imm.", with(arep, par3(imm.m > 0.8, imm.m > 0.5 & imm.m <=
    0.8, imm.m <= 0.5, imm.m)))
tab.conf[2, 6:9] <- with(srep, par3(imm.m > 0.8, imm.m > 0.5 & medicare <= 0.8, imm.m <=
    0.5, imm.m))


write.csv(tab.conf, file = "res/tab4.asrep.csv")

tab.mis[7, ] <- c("Obama -Deport", rmean(arep$deport.m), with(arep, unpaired(deport.m,
    rd)), rmean(srep$deport.m), with(srep, unpaired(deport.m, rd)))
tab.mis[8, ] <- c("Obama -Deport Sure", rmean(arep$deport.ms), with(arep, unpaired(deport.ms,
    rd)), rep("", 5))
tab.mis[9, ] <- c("AZ - Imm.", rmean(arep$imm.m), with(arep, unpaired(imm.m, rd)),
    rmean(srep$imm.m), with(srep, unpaired(imm.m, rd)))
tab.mis[10, ] <- c("AZ - Imm. -Sure", rmean(arep$imm.ms), with(arep, unpaired(imm.ms,
    rd)), rmean(srep$imm.ms), with(srep, unpaired(imm.ms, rd)))



# Table 1: Photo Vs. Textual Identification 
# ------------------------------------------------

arep$nsc_pnp     <-  with(arep, combiner(nsc, ns2c, photo.nonphoto.pk))
arep$jnc_pnp     <-  with(arep, combiner(jnc, jn2c, photo.nonphoto.pk))
arep$hrc_pnp     <-  with(arep, combiner(hrc, hr2c, photo.nonphoto.pk))
arep$am1c_pnp    <-  with(arep, combiner(am1c, am2c, photo.nonphoto.pk))
#with(subset(arep, is.na(arep$am2) & is.na(arep$am1)), combiner(am21c, am11c, photo.nonphoto.pk), photo.nonphoto.pk) # Closed 
arep$am111c_pnp  <-  with(arep, combiner((am1c | am11c), (am2c | am21c), photo.nonphoto.pk)) # Open plus closed
arep$mm1c_pnp    <-  with(arep, combiner(mm1c, mm2c, photo.nonphoto.pk))
#with(subset(arep, is.na(arep$mm2) & is.na(arep$mm1)), combiner(mm11c, mm21c, photo.nonphoto.pk), photo.nonphoto.pk)
arep$mm111c_pnp  <-  with(arep, combiner((mm1c | mm11c), (mm2c | mm21c), photo.nonphoto.pk), photo.nonphoto.pk)

srep$nsc_pnp     <-  with(srep, combiner(nsc, ns2c, photo.nonphoto.pk))
srep$jnc_pnp     <-  with(srep, combiner(jnc, jn2c, photo.nonphoto.pk))
srep$hrc_pnp     <-  with(srep, combiner(hrc, hr2c, photo.nonphoto.pk))
srep$am1c_pnp    <-  with(srep, combiner(am1c, am2c, photo.nonphoto.pk))
srep$am111c_pnp  <-  with(srep, combiner((am1c | am11c), (am2c | am21c), photo.nonphoto.pk)) # Open plus closed
srep$mm1c_pnp    <-  with(srep, combiner(mm1c, mm2c, photo.nonphoto.pk))
srep$mm111c_pnp  <-  with(srep, combiner((mm1c | mm11c), (mm2c | mm21c), photo.nonphoto.pk), photo.nonphoto.pk)

# Create a vector of names of knowledge questions (col names)
know_qs   <- paste0(c("nsc", "jnc", "hrc", "am1c", "am111c", "mm1c", "mm111c"), "_pnp")
know_labs <- c("Nicholas Sarkozy", "Janet Napolitano", "Harry Reid", "Angela Merkel --- Open", "Angela Merkel --- Open and Closed", "Mitch McConnell --- Open", "Mitch McConnell --- Open and Closed")

list_res_arep  <- lapply(arep[,know_qs], function(x) tidy(t.test(x ~ arep$photo.nonphoto.pk=='photo')))
res_pnp_arep   <- cbind(survey = "AREP", know_labs,  do.call(rbind, list_res_arep))

list_res_srep  <- lapply(srep[,know_qs], function(x) tidy(t.test(x ~ srep$photo.nonphoto.pk=='photo')))
res_pnp_srep   <- cbind(survey = "SREP", know_labs, do.call(rbind, list_res_srep))

res_pnp        <- rbind(res_pnp_arep, res_pnp_srep)

# Write out to CSV 
write.csv(res_pnp, file="res/arep_srep_vis_txt.csv", row.names=F)

# Correlation with TV Days
arep_nophoto <- subset(arep, photo.nonphoto.pk == "nonphoto")
arep_photo   <- subset(arep, photo.nonphoto.pk == "photo")
srep_nophoto <- subset(srep, photo.nonphoto.pk == "nonphoto")
srep_photo   <- subset(srep, photo.nonphoto.pk == "photo")

with(arep_nophoto,  cor(tv.days, pknphoto, use = "complete.obs"))
with(arep_photo,    cor(tv.days, pkphoto, use = "complete.obs"))
with(arep_nophoto,  cor(tv.days * attention.r, pknphoto, use = "complete.obs"))
with(arep_photo,    cor(tv.days * attention.r, pkphoto, use = "complete.obs"))
with(srep_nophoto,  cor(tv.days, pknphoto, use = "complete.obs"))
with(srep_photo,    cor(tv.days, pkphoto, use = "complete.obs"))
with(srep_nophoto,  cor(tv.days * attention.r, pknphoto, use = "complete.obs"))
with(srep_photo,    cor(tv.days * attention.r, pkphoto, use = "complete.obs"))
with(arep_nophoto, summary(lm(pknphoto ~ pol.interest)))
with(arep, summary(lm(I(combiner(pknphoto, pkphoto, photo.nonphoto.pk)) ~ photo.nonphoto.pk * pol.interest)))
with(srep, summary(lm(I(combiner(pknphoto, pkphoto, photo.nonphoto.pk)) ~ photo.nonphoto.pk * pol.interest)))

# Table 2: Partial Knowledge in Open 
# Open-Closed and Open Followed by close 
# ----------------------------------------------------

## Open Followed by close
mean(arep_nophoto$am11c[is.na(arep$am1)])
mean(arep_photo$am21c[is.na(arep$am2)])
mean(arep_nophoto$mm11c[is.na(arep$mm1)])
mean(arep_photo$mm21c[is.na(arep$mm2)])

mean(srep_nophoto$am11c[is.na(srep$am1)])
mean(srep_photo$am21c[is.na(srep$am2)])
mean(srep_nophoto$mm11c[is.na(srep$mm1)])
mean(srep_photo$mm21c[is.na(srep$mm2) & srep$photo.nonphoto.pk == "photo"])

tab.partial <- data.frame(arep.qno = NA, arep.open = NA, arep.closed = NA, arep.diff = NA,
    arep.n = NA, srep.open = NA, srep.closed = NA, srep.diff = NA, srep.n = NA)

tab.partial[1, 1:5] <- c("WW 2 Casualties - USSR", with(arep, unpaired(combiner(ww2.cas.c,
    ww2.cas2.c, open.closed), open.closed)))
tab.partial[1, 6:9] <- with(srep, unpaired(combiner(ww2.cas2.c, ww2.cas.c, open.closed),
    open.closed))

tab.partial[2, 1:5] <- c("WW 2 Casualties - Germany", with(arep, unpaired(combiner(nona(ww2.cas ==
    "Germany"), nona(pk(ww2.cas2, c("german"), c(""))), open.closed), open.closed)))
tab.partial[2, 6:9] <- with(srep, unpaired(combiner(nona(pk(ww2.cas2, c("german"),
    c(""))), nona(ww2.cas == "Germany"), open.closed), open.closed))

tab.partial[3, 1:5] <- c("Female Mortality - Heart", with(arep, unpaired(combiner(fem.death.c,
    fem.death2.c, open.closed), open.closed)))
tab.partial[3, 6:9] <- with(srep, unpaired(combiner(fem.death2.c, fem.death.c, open.closed),
    open.closed))

tab.partial[4, 1:5] <- c("Female Mortality - Breast of Lung Cancer", with(arep, unpaired(combiner(nona(fem.death ==
    "Breast Cancer"), nona(pk(fem.death2, c("breast"), c(""))), open.closed), open.closed)))
tab.partial[4, 6:9] <- with(srep, unpaired(combiner(nona(pk(fem.death2, c("breast"),
    c(""))), nona(fem.death == "Breast Cancer"), open.closed), open.closed))

write.csv(tab.partial, file = "res/tab.partial.csv")

## ***********************************************## Table 3: Confidence
## (Men/Women) ## ***********************************************##

tab.men <- data.frame(arep.qno = NA, arep.m = NA, arep.fm = NA, arep.diff = NA, arep.n = NA,
    srep.m = NA, srep.fm = NA, srep.diff = NA, srep.n = NA)

tab.men[1, 1:5] <- c("Robert Gates", with(arep[arep$correct.or.conf == "percent",
    ], unpaired(rg.rg, female)))
tab.men[2, 1:5] <- c("Robert Gates - Regular", with(arep[arep$correct.or.conf ==
    "regular", ], unpaired(rgc, female)))
tab.men[3, 1:5] <- c("Robert Gates - Conf.", with(arep[arep$correct.or.conf == "percent",
    ], unpaired(rg.cf, female)))

tab.men[1, 6:9] <- with(srep[srep$correct.or.conf == "percent", ], unpaired(rg.rg,
    female))
tab.men[2, 6:9] <- with(srep[arep$correct.or.conf == "regular", ], unpaired(rgc,
    female))
tab.men[3, 6:9] <- with(srep[arep$correct.or.conf == "percent", ], unpaired(rg.cf,
    female))

tab.men[4, 1:5] <- c("Stephen Breyer", with(arep[arep$correct.or.conf == "percent",
    ], unpaired(sb.sb, female)))
tab.men[5, 1:5] <- c("Stephen Breyer - Regular", with(arep[arep$correct.or.conf ==
    "regular", ], unpaired(sbc, female)))
tab.men[6, 1:5] <- c("Stephen Breyer - Conf.", with(arep[arep$correct.or.conf ==
    "percent", ], unpaired(sb.cf, female)))

tab.men[4, 6:9] <- with(srep[srep$correct.or.conf == "percent", ], unpaired(sb.sb,
    female))
tab.men[5, 6:9] <- with(srep[srep$correct.or.conf == "regular", ], unpaired(sbc,
    female))
tab.men[6, 6:9] <- with(srep[srep$correct.or.conf == "percent", ], unpaired(sb.cf,
    female))

tab.men[7, 1:5] <- c("Healthcare - Future Increase", with(arep[arep$correct.or.conf ==
    "percent", ], unpaired(future.increase, female)))
tab.men[8, 1:5] <- c("Healthcare - Future Increase -Regular", with(arep[arep$correct.or.conf ==
    "regular", ], unpaired(future.inc.c, female)))
tab.men[9, 1:5] <- c("Healthcare - Future Increase -Conf.", with(arep[arep$correct.or.conf ==
    "percent", ], unpaired(future.inc.cf, female)))

tab.men[7, 6:9] <- with(srep[srep$correct.or.conf == "percent", ], unpaired(future.increase,
    female))
tab.men[8, 6:9] <- with(srep[srep$correct.or.conf == "regular", ], unpaired(future.inc.c,
    female))
tab.men[9, 6:9] <- with(srep[srep$correct.or.conf == "percent", ], unpaired(future.inc.cf,
    female))

tab.men[10, 1:5] <- c("Healthcare - Upperclass tax", with(arep[arep$correct.or.conf ==
    "percent", ], unpaired(upper.class, female)))
tab.men[11, 1:5] <- c("Healthcare - Upperclass tax -Regular", with(arep[arep$correct.or.conf ==
    "regular", ], unpaired(upper.class.c, female)))
tab.men[12, 1:5] <- c("Healthcare - Upperclass tax -Conf.", with(arep[arep$correct.or.conf ==
    "percent", ], unpaired(upper.class.cf, female)))

tab.men[10, 6:9] <- with(srep[srep$correct.or.conf == "percent", ], unpaired(upper.class,
    female))
tab.men[11, 6:9] <- with(srep[srep$correct.or.conf == "regular", ], unpaired(upper.class.c,
    female))
tab.men[12, 6:9] <- with(srep[srep$correct.or.conf == "percent", ], unpaired(upper.class.cf,
    female))

write.csv(tab.men, file = "res/tab.men.csv")

## *************************************************## Table 4: Confidence versus
## closed-ended ## *************************************************##

par3 <- function(x11, x12, x13, y, grp) {
    mu1 <- rmean(x11[grp == "percent"])
    mu2 <- rmean(x12[grp == "percent"])
    mu3 <- rmean(x13[grp == "percent"])
    mu4 <- rmean(y[grp == "regular"])
    c(mu4, mu1, mu2, mu3)
}

tab.conf <- data.frame(arep.qno = NA, arep.cls = NA, arep.10 = NA, arep.5 = NA, arep.cf = NA,
    srep.cls = NA, srep.10 = NA, srep.5 = NA, srep.cf = NA)

tab.conf[1, 1:5] <- c("Robert Gates", with(arep, par3(rg.rg == 1, rg.rg > 0.5, rg.cf,
    rgc, correct.or.conf)))
tab.conf[1, 6:9] <- with(srep, par3(rg.rg == 1, rg.rg > 0.5, rg.cf, rgc, correct.or.conf))

tab.conf[2, 1:5] <- c("Stephen Breyer", with(arep, par3(sb.sb == 1, sb.sb > 0.5,
    sb.cf, sbc, correct.or.conf)))
tab.conf[2, 6:9] <- with(srep, par3(sb.sb == 1, sb.sb > 0.5, sb.cf, sbc, correct.or.conf))

tab.conf[3, 1:5] <- c("Future Increase", with(arep, par3(future.increase == 1, future.increase >
    0.5, future.inc.cf, future.inc.c, correct.or.conf)))
tab.conf[3, 6:9] <- with(srep, par3(future.increase == 1, future.increase > 0.5,
    future.inc.cf, future.inc.c, correct.or.conf))

tab.conf[4, 1:5] <- c("Upperclass tax", with(arep, par3(upper.class == 1, upper.class >
    0.5, upper.class.cf, upper.class.c, correct.or.conf)))
tab.conf[4, 6:9] <- with(srep, par3(upper.class == 1, upper.class > 0.5, upper.class.cf,
    upper.class.c, correct.or.conf))

tab.conf[5, 1:5] <- c("Death Panel", with(arep, par3(death.panel == 1, death.panel >
    0.5, death.panel.cf, death.panel.c, correct.or.conf)))
tab.conf[5, 6:9] <- with(srep, par3(death.panel == 1, death.panel > 0.5, death.panel.cf,
    death.panel.c, correct.or.conf))

tab.conf[6, 1:5] <- c("Medicare", with(arep, par3(medicare == 1, medicare > 0.5,
    medicare.cf, medicare.c, correct.or.conf)))
tab.conf[6, 6:9] <- with(srep, par3(medicare == 1, medicare > 0.5, medicare.cf, medicare.c,
    correct.or.conf))

tab.conf[7, 1:5] <- c("Cuts Benefits", with(arep, par3(cuts.benefits == 1, cuts.benefits >
    0.5, cuts.benefits.cf, cuts.benefits.c, correct.or.conf)))
tab.conf[7, 6:9] <- with(srep, par3(cuts.benefits == 1, cuts.benefits > 0.5, cuts.benefits.cf,
    cuts.benefits.c, correct.or.conf))

tab.conf[8, 1:5] <- c("Illegals", with(arep, par3(illegal == 1, illegal > 0.5, illegal.cf,
    illegal.c, correct.or.conf)))
tab.conf[8, 6:9] <- with(srep, par3(illegal == 1, illegal > 0.5, illegal.cf, illegal.c,
    correct.or.conf))

tab.conf[9, 1:5] <- c("Single Payer", with(arep, par3(single.payer == 1, single.payer >
    0.5, single.payer.cf, single.payer.c, correct.or.conf)))
tab.conf[9, 6:9] <- with(srep, par3(single.payer == 1, single.payer > 0.5, single.payer.cf,
    single.payer.c, correct.or.conf))

tab.conf[10, 1:5] <- c("Mammograms", with(arep, par3(mammograms == 1, mammograms >
    0.5, mammograms.cf, mammograms.c, correct.or.conf)))
tab.conf[10, 6:9] <- with(srep, par3(mammograms == 1, mammograms > 0.5, mammograms.cf,
    mammograms.c, correct.or.conf))


write.csv(tab.conf, file = "res/tab.conf.csv")

## *************************************************## Table 5: Misinformation ##
## *************************************************##

tab.mis <- data.frame(arep.qno = NA, arep.mean = NA, arep.dem = NA, arep.rep = NA,
    arep.diff = NA, arep.n = NA, srep.mean = NA, srep.dem = NA, srep.rep = NA, srep.diff = NA,
    srep.n = NA)
tab.mis[1, ] <- c("Glass Steagall", rmean(arep$steagall.c), with(arep, unpaired(steagall.c, rd)), rmean(srep$steagall.c), with(srep, unpaired(steagall.c, rd)))
tab.mis[2, ] <- c("Glass Steagall Mis.", rmean(arep$steagall.rm), with(arep, unpaired(steagall.rm, rd)), rmean(srep$steagall.rm), with(srep, unpaired(steagall.rm, rd)))
tab.mis[3, ] <- c("Reagan Budget", rmean(arep$rgn.m), with(arep, unpaired(rgn.m, rd)), rmean(srep$rgn.m), with(srep, unpaired(rgn.m, rd)))
tab.mis[4, ] <- c("Clinton Budget", rmean(arep$ctn.m), with(arep, unpaired(ctn.m,
    rd)), rmean(arep$ctn.m), with(srep, unpaired(ctn.m, rd)))
tab.mis[5, ] <- c("Bush Budget", rmean(arep$bsh.m), with(arep, unpaired(bsh.m, rd)),
    rmean(srep$bsh.m), with(srep, unpaired(bsh.m, rd)))
tab.mis[6, ] <- c("Obama Budget", rmean(arep$obm.m), with(arep, unpaired(obm.m, rd)),
    rmean(srep$obm.m), with(srep, unpaired(obm.m, rd)))
tab.mis[7, ] <- c("Obama -Deport", rmean(arep$deport.m), with(arep, unpaired(deport.m,
    rd)), rmean(srep$deport.m), with(srep, unpaired(deport.m, rd)))
tab.mis[8, ] <- c("Obama -Deport Sure", rmean(arep$deport.ms), with(arep, unpaired(deport.ms,
    rd)), rep("", 5))
tab.mis[9, ] <- c("AZ - Imm.", rmean(arep$imm.m), with(arep, unpaired(imm.m, rd)),
    rmean(srep$imm.m), with(srep, unpaired(imm.m, rd)))
tab.mis[10, ] <- c("AZ - Imm. -Sure", rmean(arep$imm.ms), with(arep, unpaired(imm.ms,
    rd)), rmean(srep$imm.ms), with(srep, unpaired(imm.ms, rd)))

tab.mis[11, ] <- c("Tax 100k", rmean(arep$tax.100k.r), with(arep, unpaired(tax.100k.r,
    rd)), rmean(srep$tax.100k.r), with(srep, unpaired(tax.100k.r, rd)))
tab.mis[12, ] <- c("Welfare %", rmean(arep$welfare.r), with(arep, unpaired(welfare.r,
    rd)), rmean(srep$welfare.r), with(srep, unpaired(welfare.r, rd)))
tab.mis[13, ] <- c("Black Welfare", rmean(arep$blk.welfare.r), with(arep, unpaired(blk.welfare.r,
    rd)), rmean(srep$blk.welfare.r), with(srep, unpaired(blk.welfare.r, rd)))

ars <- subset(arep, arep$correct.or.conf == "percent")
srs <- subset(srep, srep$correct.or.conf == "percent")

tab.mis[14, ] <- c("Death Panel", rmean(ars$death.panel), with(ars, unpaired(death.panel,
    rd)), rmean(srs$death.panel), with(srs, unpaired(death.panel, rd)))
tab.mis[15, ] <- c("Medicare", rmean(ars$medicare), with(ars, unpaired(medicare,
    rd)), rmean(srs$medicare), with(srs, unpaired(medicare, rd)))
tab.mis[16, ] <- c("Limit Future Inc.", rmean(ars$future.increase), with(ars, unpaired(future.increase,
    rd)), rmean(srs$future.increase), with(srs, unpaired(future.increase, rd)))
tab.mis[17, ] <- c("Cuts Medicare", rmean(ars$cuts.benefits), with(ars, unpaired(cuts.benefits,
    rd)), rmean(srs$cuts.benefits), with(srs, unpaired(cuts.benefits, rd)))
tab.mis[18, ] <- c("Covers Illegals", rmean(ars$illegal), with(ars, unpaired(illegal,
    rd)), rmean(srs$illegal), with(srs, unpaired(illegal, rd)))
tab.mis[19, ] <- c("Single Payer", rmean(ars$single.payer), with(ars, unpaired(single.payer,
    rd)), rmean(srs$single.payer), with(srs, unpaired(single.payer, rd)))
tab.mis[20, ] <- c("Upper class Pay.", rmean(ars$upper.class), with(ars, unpaired(upper.class,
    rd)), rmean(srs$upper.class), with(srs, unpaired(upper.class, rd)))
tab.mis[21, ] <- c("Mammogram", rmean(ars$mammograms), with(ars, unpaired(mammograms,
    rd)), rmean(srs$mammograms), with(srs, unpaired(mammograms, rd)))

write.csv(tab.mis, file = "res/tab.mis.csv")

## *************************************************## Table 6: Don't Know means
## Don't Know ## *************************************************##

tab6.mis <- data.frame(arep.qno = NA, arep.enc = NA, arep.dsc = NA, arep.diff = NA,
    arep.n = NA, srep.enc = NA, srep.dsc = NA, srep.diff = NA, srep.n = NA)
tab6.mis[1, ] <- c("Reagan Budget", with(arep, unpaired(rgn.m, dkencouraging.dk.discouraging)),
    with(srep, unpaired(rgn.m, dkencouraging.dk.discouraging)))
tab6.mis[2, ] <- c("Clinton Budget", with(arep, unpaired(ctn.m, dkencouraging.dk.discouraging)),
    with(srep, unpaired(ctn.m, dkencouraging.dk.discouraging)))
tab6.mis[3, ] <- c("Bush Budget", with(arep, unpaired(bsh.m, dkencouraging.dk.discouraging)),
    with(srep, unpaired(bsh.m, dkencouraging.dk.discouraging)))
tab6.mis[4, ] <- c("Obama Budget", "", "", "", "", with(srep, unpaired(obm.m, dkencouraging.dk.discouraging)))

tab6.mis[5, ] <- c("Reagan Budget", with(arep[arep$rd == "rep", ], unpaired(rgn.m,
    dkencouraging.dk.discouraging)), with(srep[srep$rd == "rep", ], unpaired(rgn.m,
    dkencouraging.dk.discouraging)))
tab6.mis[6, ] <- c("Clinton Budget", with(arep[arep$rd == "rep", ], unpaired(ctn.m,
    dkencouraging.dk.discouraging)), with(srep[srep$rd == "rep", ], unpaired(ctn.m,
    dkencouraging.dk.discouraging)))
tab6.mis[7, ] <- c("Bush Budget", with(arep[arep$rd == "rep", ], unpaired(bsh.m,
    dkencouraging.dk.discouraging)), with(srep[srep$rd == "rep", ], unpaired(bsh.m,
    dkencouraging.dk.discouraging)))
tab6.mis[8, ] <- c("Obama Budget", "", "", "", "", with(srep[srep$rd == "rep", ],
    unpaired(obm.m, dkencouraging.dk.discouraging)))

tab6.mis[9, ] <- c("Reagan Budget", with(arep[arep$rd == "dem", ], unpaired(rgn.m,
    dkencouraging.dk.discouraging)), with(srep[srep$rd == "dem", ], unpaired(rgn.m,
    dkencouraging.dk.discouraging)))
tab6.mis[10, ] <- c("Clinton Budget", with(arep[arep$rd == "dem", ], unpaired(ctn.m,
    dkencouraging.dk.discouraging)), with(srep[srep$rd == "dem", ], unpaired(ctn.m,
    dkencouraging.dk.discouraging)))
tab6.mis[11, ] <- c("Bush Budget", with(arep[arep$rd == "dem", ], unpaired(bsh.m,
    dkencouraging.dk.discouraging)), with(srep[srep$rd == "dem", ], unpaired(bsh.m,
    dkencouraging.dk.discouraging)))
tab6.mis[12, ] <- c("Obama Budget", "", "", "", "", with(srep[srep$rd == "dem", ],
    unpaired(obm.m, dkencouraging.dk.discouraging)))

# Branch A is DK encouraging

tab6.mis[1, ] <- c("Reagan Budget", with(arep, unpaired(rgn, dkencouraging.dk.discouraging)),
    with(srep, unpaired(rgn, dkencouraging.dk.discouraging)))
tab6.mis[2, ] <- c("Clinton Budget", with(arep, unpaired(ctn, dkencouraging.dk.discouraging)),
    with(srep, unpaired(ctn, dkencouraging.dk.discouraging)))
tab6.mis[3, ] <- c("Bush Budget", with(arep, unpaired(bsh, dkencouraging.dk.discouraging)),
    with(srep, unpaired(bsh, dkencouraging.dk.discouraging)))
tab6.mis[4, ] <- c("Obama Budget", "", "", "", "", with(srep, unpaired(obm, dkencouraging.dk.discouraging)))

tab6.mis[5, ] <- c("Reagan Budget", with(arep[arep$female == 1 & !is.na(arep$female),
    ], unpaired(rgn, dkencouraging.dk.discouraging)), with(srep[srep$female == 1 &
    !is.na(srep$female), ], unpaired(rgn, dkencouraging.dk.discouraging)))
tab6.mis[6, ] <- c("Clinton Budget", with(arep[arep$female == 1 & !is.na(arep$female),
    ], unpaired(ctn, dkencouraging.dk.discouraging)), with(srep[srep$female == 1 &
    !is.na(srep$female), ], unpaired(ctn, dkencouraging.dk.discouraging)))
tab6.mis[7, ] <- c("Bush Budget", with(arep[arep$female == 1 & !is.na(arep$female),
    ], unpaired(bsh, dkencouraging.dk.discouraging)), with(srep[srep$female == 1 &
    !is.na(srep$female), ], unpaired(bsh, dkencouraging.dk.discouraging)))
tab6.mis[8, ] <- c("Obama Budget", "", "", "", "", with(srep[srep$female == 1 & !is.na(srep$female),
    ], unpaired(obm, dkencouraging.dk.discouraging)))

tab6.mis[9, ] <- c("Reagan Budget", with(arep[arep$female == 0 & !is.na(arep$female),
    ], unpaired(rgn, dkencouraging.dk.discouraging)), with(srep[srep$female == 0 &
    !is.na(srep$female), ], unpaired(rgn, dkencouraging.dk.discouraging)))
tab6.mis[10, ] <- c("Clinton Budget", with(arep[arep$female == 0 & !is.na(arep$female),
    ], unpaired(ctn, dkencouraging.dk.discouraging)), with(srep[srep$female == 0 &
    !is.na(srep$female), ], unpaired(ctn, dkencouraging.dk.discouraging)))
tab6.mis[11, ] <- c("Bush Budget", with(arep[arep$female == 0 & !is.na(arep$female),
    ], unpaired(bsh, dkencouraging.dk.discouraging)), with(srep[srep$female == 0 &
    !is.na(srep$female), ], unpaired(bsh, dkencouraging.dk.discouraging)))
tab6.mis[12, ] <- c("Obama Budget", "", "", "", "", with(srep[srep$female == 0 &
    !is.na(srep$female), ], unpaired(obm, dkencouraging.dk.discouraging)))


tab6.mis[5, ] <- c("Reagan Budget", with(arep[arep$rd == "rep" & !is.na(arep$rd),
    ], unpaired(rgn, dkencouraging.dk.discouraging)), with(srep[srep$rd == "rep" &
    !is.na(srep$rd), ], unpaired(rgn, dkencouraging.dk.discouraging)))
tab6.mis[6, ] <- c("Clinton Budget", with(arep[arep$rd == "rep" & !is.na(arep$rd),
    ], unpaired(ctn, dkencouraging.dk.discouraging)), with(srep[srep$rd == "rep" &
    !is.na(srep$rd), ], unpaired(ctn, dkencouraging.dk.discouraging)))
tab6.mis[7, ] <- c("Bush Budget", with(arep[arep$rd == "rep" & !is.na(arep$rd), ],
    unpaired(bsh, dkencouraging.dk.discouraging)), with(srep[srep$rd == "rep" & !is.na(srep$rd),
    ], unpaired(bsh, dkencouraging.dk.discouraging)))
tab6.mis[8, ] <- c("Obama Budget", "", "", "", "", with(srep[srep$rd == "rep" & !is.na(srep$rd),
    ], unpaired(obm, dkencouraging.dk.discouraging)))

tab6.mis[9, ] <- c("Reagan Budget", with(arep[arep$rd == "dem" & !is.na(arep$rd),
    ], unpaired(rgn, dkencouraging.dk.discouraging)), with(srep[srep$rd == "dem" &
    !is.na(srep$rd), ], unpaired(rgn, dkencouraging.dk.discouraging)))
tab6.mis[10, ] <- c("Clinton Budget", with(arep[arep$rd == "dem" & !is.na(arep$rd),
    ], unpaired(ctn, dkencouraging.dk.discouraging)), with(srep[srep$rd == "dem" &
    !is.na(srep$rd), ], unpaired(ctn, dkencouraging.dk.discouraging)))
tab6.mis[11, ] <- c("Bush Budget", with(arep[arep$rd == "dem" & !is.na(arep$rd),
    ], unpaired(bsh, dkencouraging.dk.discouraging)), with(srep[srep$rd == "dem" &
    !is.na(srep$rd), ], unpaired(bsh, dkencouraging.dk.discouraging)))
tab6.mis[12, ] <- c("Obama Budget", "", "", "", "", with(srep[srep$rd == "dem" &
    !is.na(srep$rd), ], unpaired(obm, dkencouraging.dk.discouraging)))


write.csv(tab6.mis, file = "res/tab6.mis.csv")


# Correlation Between Thermometer Ratings and Estimated Blacks on Welfare
with(subset(srep, srep$white == "White"), cor((therm.br - therm.wr), blk.welfare.r,
    use = "na.or.complete"))
with(subset(srep, is.na(srep$white)), cor((therm.br), blk.welfare.r, use = "complete.obs"))

# Correlation Between Thermometer Ratings and Estimated Blacks on Welfare
with(subset(arep, arep$ethnic == 7), cor((therm.br - therm.wr), blk.welfare.r, use = "na.or.complete"))
with(subset(arep, arep$ethnic != 7), cor((therm.br), blk.welfare.r, use = "complete.obs"))
