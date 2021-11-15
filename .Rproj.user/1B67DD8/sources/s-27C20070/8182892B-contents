#############################################################
#  Rcode Compendium for CoMSAfrica Geneva meetings
#############################################################

#######################
# R session information
#######################

sessionInfo()

#R version 3.6.1 (2019-07-05)
#Platform: x86_64-apple-darwin15.6.0 (64-bit)
#Running under: macOS Mojave 10.14.6

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

###############################
#Print list of package versions
###############################

ip <- as.data.frame(installed.packages()[,c(1,3:4)])
rownames(ip) <- NULL
ip <- ip[is.na(ip$Priority),1:2,drop=FALSE]
print(ip, row.names=FALSE)

################
# Notes on package versions
# We use the scale_color_virdis addition to ggplot
# Also, the recode function in the 'car' package overwrites 'dplyr's'
# recode function (the one we use). To stop this from happening
# make sure that dplyr loads after car
################

################
# Detach packages and load libraries
################

detachAllPackages <- function() {
      
      basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
      
      package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
      
      package.list <- setdiff(package.list,basic.packages)
      
      if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
      
}

detachAllPackages()

# Create package list to check for installed packages

library(dplyr)
library(rptR)
library(krippendorffsalpha)
library(tidyverse)
library(irr) 

############################## 
# Set working directory and load datafile
##############################

# Set to source directory with relevant .csv datafile provided
# through the Open Science repository
getwd()

## Add datasets

comsafrica_data<-read.csv("comsafrica_complete_adjusted.csv") %>%
   subset(!flake_id == "NONR3495") #filter out this non numbered flake

# change column names to lower case
colnames(comsafrica_data) <- tolower(colnames(comsafrica_data)) 

####################################
# exploring data to identify and rectify potential numbering mistakes
##################################

attach(comsafrica_data)
cols <- c("assemblage_code", "analyst_id", "flake_id", "completeness", "damage",
          "dorsal_cortex", "Dorsal_Cortex_Location", "dorsal_cortex_location_other", 
          "platform_cortex", "dorsal_scar_count", "directionality", "Proximal_Scars",
          "Left_Scars", "Distal_Scars", "Right_Scars", "PLATFMORPH", "PLATFLIPP", "BULB",
          "SHATTBULB", "INITIATION", "VENTR_PLANE_FORM", "SECTION", "LATEDGETYPE", "FLAKETERM",
          "DISTPLANFORM", "KOMBEWA", "FLK_TYPE", "RED_SYST", "FLK_FORM")
data1[cols] <- lapply(comsafrica_data[cols], factor) #not clear where data1 is here?


table1 <- table(analyst_id, flake_id) #identify errors in flake numbering. A number should not appear more than twice.
#And should appear equally among all analysts
# e.g., analyst 8240a has 3 no 23; 3 no 60;  d764f has 3 no 86
table1
#write.csv(table1, file = "Tables/table1.csv")

#errors corrected:
#99 condition B --> 94
#8 condition B removed from the analysis --> too broken for too many analysts
#nonr corresponds to 34 and 95 for two other analysts. The others may not have included it (total between 99 and 100)
#For 8240a, a number of duplicates were identified (analysis took place in two areas if I am not mistaken)
#Some pieces were misnumbered (e.g. 17 for 19 or 14, etc)
#I cross-checked the number of artefacts per analyst and comparisons with all artefacts from all analysts
#when in a doubt, I checked with the actual artefacts to see if the confusion in the numbers made sense or if there were any notes in the bags
#in total, around 25 pieces were reassigned. Other numbering mistakes may appear and in case of too strong inter-analyst discrepancies, this is a factor that should be considered

detach()

############################## 
# Trim/tidy data and subset data for analyses
##############################

## subset

comsafrica_data_complete<-comsafrica_data %>%
      select(c(assemblage_code,analyst_id,flake_id,proximal_scars,left_scars,distal_scars,right_scars,
               dorsal_cortex,mass,maximumdimension,maximumwidth,maximumthickness,techlength,techmaxwidth,techmaxthickness,
               techwidthprox,techwidthmes,techwidthdist,techthickprox,techthickmes,techthickdist,
               platfwidth,platfthickimpact,platfthickmid,platfthickmax,edgeplatf))

comsafrica_data_complete_condb<-comsafrica_data_complete %>%
      subset(assemblage_code=="chert_condition_B")

comsafrica_data_complete_conda<-comsafrica_data_complete %>%
      subset(assemblage_code=="chert_condition_A") 

##### Inter rater visualizations and data analyses

### repeatability coefficients for continuous (Gaussian) data ####
## Condition b ####
# cortex-NOT SURE WHAT TO DO WITH CORTEX-STRANGE DISTRIBUTION?
hist(log(comsafrica_data_complete_condb$dorsal_cortex))
set.seed(50)
comsafrica_cortex_boot<-rpt(dorsal_cortex ~ (1 | flake_id), 
                            grname = "flake_id", 
                            data = comsafrica_data_complete_condb, 
                            datatype = "Gaussian", 
                            nboot = 1000, npermut = 100)
summary(comsafrica_cortex_boot)
summary(comsafrica_cortex_boot$mod)
plot(comsafrica_cortex_boot, type = "boot", cex.main = 0.8,main="maximum dimension")
plot(comsafrica_cortex_boot, type = "permut", cex.main = 1)

# maxdim
hist(comsafrica_data_complete_condb$maximumdimension)
set.seed(50)
comsafrica_maxdim_boot<-rpt(maximumdimension ~ (1 | flake_id), 
                  grname = "flake_id", 
                  data = comsafrica_data_complete_condb, 
                  datatype = "Gaussian", 
                  nboot = 1000, npermut = 100)
summary(comsafrica_maxdim_boot)
summary(comsafrica_maxdim_boot$mod)
plot(comsafrica_maxdim_boot, type = "boot", cex.main = 0.8,main="maximum dimension")
plot(comsafrica_maxdim_boot, type = "permut", cex.main = 1)

#mass
hist(log(comsafrica_data_complete_condb$mass))
set.seed(50)
comsafrica_mass<-rpt(log(mass) ~ (1 | flake_id), 
                       grname = "flake_id", 
                       data = filter(comsafrica_data_complete_condb,mass>0), 
                       datatype = "Gaussian", 
                       nboot = 1000, npermut = 100)
summary(comsafrica_mass)
plot(comsafrica_mass, type = "boot", cex.main = 0.8,main="mass")

#flake width
set.seed(50)
comsafrica_maxwidth<-rpt(maximumwidth ~ (1 | flake_id), 
                     grname = "flake_id", 
                     data = comsafrica_data_complete_condb, 
                     datatype = "Gaussian", 
                     nboot = 1000, npermut = 100)
summary(comsafrica_maxwidth)
plot(comsafrica_maxwidth, type = "boot", cex.main = 0.8,main="max width")

#flake max thickness
set.seed(50)
comsafrica_maxthick<-rpt(maximumthickness ~ (1 | flake_id), 
                         grname = "flake_id", 
                         data = comsafrica_data_complete_condb, 
                         datatype = "Gaussian", 
                         nboot = 1000, npermut = 100)
summary(comsafrica_maxthick)
plot(comsafrica_maxthick, type = "boot", cex.main = 0.8,main="max thickness")

#flake tech length
set.seed(50)
comsafrica_techlength<-rpt(techlength ~ (1 | flake_id), 
                         grname = "flake_id", 
                         data = comsafrica_data_complete_condb, 
                         datatype = "Gaussian", 
                         nboot = 1000, npermut = 100)
summary(comsafrica_techlength)
plot(comsafrica_techlength, type = "boot", cex.main = 0.8,main="tech length")

#flake tech max width
set.seed(50)
comsafrica_techmaxwidth<-rpt(techmaxwidth ~ (1 | flake_id), 
                           grname = "flake_id", 
                           data = comsafrica_data_complete_condb, 
                           datatype = "Gaussian", 
                           nboot = 1000, npermut = 100)
summary(comsafrica_techmaxwidth)
plot(comsafrica_techmaxwidth, type = "boot", cex.main = 0.8,main="tech max width")

#flake tech max thickness
set.seed(50)
comsafrica_techmaxthick<-rpt(techmaxthickness ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_condb, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 100)
summary(comsafrica_techmaxthick)
plot(comsafrica_techmaxthick, type = "boot", cex.main = 0.8,main="tech max thickness")

#flake tech width prox
set.seed(50)
comsafrica_techwidthprox<-rpt(techwidthprox ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_condb, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 100)
summary(comsafrica_techwidthprox)
plot(comsafrica_techwidthprox, type = "boot", cex.main = 0.8,main="tech width proximal")

#flake tech width mes
set.seed(50)
comsafrica_techwidthmes<-rpt(techwidthmes ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_condb, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 100)
summary(comsafrica_techwidthmes)
plot(comsafrica_techwidthmes, type = "boot", cex.main = 0.8,main="tech width medial")

#flake tech width dist
set.seed(50)
comsafrica_techwidthdist<-rpt(techwidthdist ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_condb, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 100)
summary(comsafrica_techwidthdist)
plot(comsafrica_techwidthdist, type = "boot", cex.main = 0.8,main="tech width dist")

#flake tech thick prox
set.seed(50)
comsafrica_techtechthickprox<-rpt(techthickprox ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_condb, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 100)
summary(comsafrica_techtechthickprox)
plot(comsafrica_techtechthickprox, type = "boot", cex.main = 0.8,main="tech thick prox")

#flake tech thick med
set.seed(50)
comsafrica_techtechthickmes<-rpt(techthickmes ~ (1 | flake_id), 
                                  grname = "flake_id", 
                                  data = comsafrica_data_complete_condb, 
                                  datatype = "Gaussian", 
                                  nboot = 1000, npermut = 100)
summary(comsafrica_techtechthickmes)
plot(comsafrica_techtechthickmes, type = "boot", cex.main = 0.8,main="tech thick medial")

#flake tech thick dist
set.seed(50)
comsafrica_techthickdist<-rpt(techthickdist ~ (1 | flake_id), 
                                 grname = "flake_id", 
                                 data = comsafrica_data_complete_condb, 
                                 datatype = "Gaussian", 
                                 nboot = 1000, npermut = 100)
summary(comsafrica_techthickdist)
plot(comsafrica_techthickdist, type = "boot", cex.main = 0.8,main="tech thick dist")

#flake platform width
set.seed(50)
comsafrica_platfwidth<-rpt(platfwidth ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_condb, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 100)
summary(comsafrica_platfwidth)
plot(comsafrica_platfwidth, type = "boot", cex.main = 0.8,main="platform width")

#flake platform thickness
set.seed(50)
comsafrica_platfthickmax<-rpt(platfthickmax ~ (1 | flake_id), 
                           grname = "flake_id", 
                           data = comsafrica_data_complete_condb, 
                           datatype = "Gaussian", 
                           nboot = 1000, npermut = 100)
summary(comsafrica_platfthickmax)
plot(comsafrica_platfthickmax, type = "boot", cex.main = 0.8,main="platform thickness")

#flake EPA
set.seed(50)
comsafrica_edgeplatf<-rpt(edgeplatf ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_condb, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 100)
summary(comsafrica_edgeplatf)
plot(comsafrica_edgeplatf, type = "boot", cex.main = 0.8,main="EPA")

## Condition a ####

# cortex-NOT SURE WHAT TO DO WITH CORTEX-STRANGE DISTRIBUTION?
hist(log(comsafrica_data_complete_conda$dorsal_cortex))
set.seed(50)
comsafrica_cortex_boot<-rpt(dorsal_cortex ~ (1 | flake_id), 
                            grname = "flake_id", 
                            data = comsafrica_data_complete_conda, 
                            datatype = "Gaussian", 
                            nboot = 1000, npermut = 100)
summary(comsafrica_cortex_boot)
summary(comsafrica_cortex_boot$mod)
plot(comsafrica_cortex_boot, type = "boot", cex.main = 0.8,main="maximum dimension")
plot(comsafrica_cortex_boot, type = "permut", cex.main = 1)

# maxdim
set.seed(50)
comsafrica_maxdim_boot<-rpt(maximumdimension ~ (1 | flake_id), 
                            grname = "flake_id", 
                            data = comsafrica_data_complete_conda, 
                            datatype = "Gaussian", 
                            nboot = 1000, npermut = 100)
summary(comsafrica_maxdim_boot)
plot(comsafrica_maxdim_boot, type = "boot", cex.main = 0.8,main="maximum dimension")

#mass
hist(log(comsafrica_data_complete_conda$mass))
set.seed(50)
comsafrica_mass<-rpt(log(mass) ~ (1 | flake_id), 
                     grname = "flake_id", 
                     data = filter(comsafrica_data_complete_conda, mass>0), 
                     datatype = "Gaussian", 
                     nboot = 1000, npermut = 100)
summary(comsafrica_mass)
plot(comsafrica_mass, type = "boot", cex.main = 0.8,main="mass")

#flake width
set.seed(50)
comsafrica_maxwidth<-rpt(maximumwidth ~ (1 | flake_id), 
                         grname = "flake_id", 
                         data = comsafrica_data_complete_conda, 
                         datatype = "Gaussian", 
                         nboot = 1000, npermut = 100)
summary(comsafrica_maxwidth)
plot(comsafrica_maxwidth, type = "boot", cex.main = 0.8,main="max width")

#flake max thickness
set.seed(50)
comsafrica_maxthick<-rpt(maximumthickness ~ (1 | flake_id), 
                         grname = "flake_id", 
                         data = comsafrica_data_complete_conda, 
                         datatype = "Gaussian", 
                         nboot = 1000, npermut = 100)
summary(comsafrica_maxthick)
plot(comsafrica_maxthick, type = "boot", cex.main = 0.8,main="max thickness")

#flake tech length
set.seed(50)
comsafrica_techlength<-rpt(techlength ~ (1 | flake_id), 
                           grname = "flake_id", 
                           data = comsafrica_data_complete_conda, 
                           datatype = "Gaussian", 
                           nboot = 1000, npermut = 100)
summary(comsafrica_techlength)
plot(comsafrica_techlength, type = "boot", cex.main = 0.8,main="tech length")

#flake tech max width
set.seed(50)
comsafrica_techmaxwidth<-rpt(techmaxwidth ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_conda, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 100)
summary(comsafrica_techmaxwidth)
plot(comsafrica_techmaxwidth, type = "boot", cex.main = 0.8,main="tech max width")

#flake tech max thickness
set.seed(50)
comsafrica_techmaxthick<-rpt(techmaxthickness ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_conda, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 0)
summary(comsafrica_techmaxthick)
plot(comsafrica_techmaxthick, type = "boot", cex.main = 0.8,main="tech max thickness")

#flake tech width prox
set.seed(50)
comsafrica_techwidthprox<-rpt(techwidthprox ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_conda, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
summary(comsafrica_techwidthprox)
plot(comsafrica_techwidthprox, type = "boot", cex.main = 0.8,main="tech width proximal")

#flake tech width mes
set.seed(50)
comsafrica_techwidthmes<-rpt(techwidthmes ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_conda, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 0)
summary(comsafrica_techwidthmes)
plot(comsafrica_techwidthmes, type = "boot", cex.main = 0.8,main="tech width medial")

#flake tech width dist
set.seed(50)
comsafrica_techwidthdist<-rpt(techwidthdist ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_conda, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
summary(comsafrica_techwidthdist)
plot(comsafrica_techwidthdist, type = "boot", cex.main = 0.8,main="tech width dist")

#flake tech thick prox
set.seed(50)
comsafrica_techtechthickprox<-rpt(techthickprox ~ (1 | flake_id), 
                                  grname = "flake_id", 
                                  data = comsafrica_data_complete_conda, 
                                  datatype = "Gaussian", 
                                  nboot = 1000, npermut = 0)
summary(comsafrica_techtechthickprox)
plot(comsafrica_techtechthickprox, type = "boot", cex.main = 0.8,main="tech thick prox")

#flake tech thick med
set.seed(50)
comsafrica_techtechthickmes<-rpt(techthickmes ~ (1 | flake_id), 
                                 grname = "flake_id", 
                                 data = comsafrica_data_complete_conda, 
                                 datatype = "Gaussian", 
                                 nboot = 1000, npermut = 0)
summary(comsafrica_techtechthickmes)
plot(comsafrica_techtechthickmes, type = "boot", cex.main = 0.8,main="tech thick medial")

#flake tech thick dist
set.seed(50)
comsafrica_techthickdist<-rpt(techthickdist ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_conda, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
summary(comsafrica_techthickdist)
plot(comsafrica_techthickdist, type = "boot", cex.main = 0.8,main="tech thick dist")

#flake platform width
set.seed(50)
comsafrica_platfwidth<-rpt(platfwidth ~ (1 | flake_id), 
                           grname = "flake_id", 
                           data = comsafrica_data_complete_conda, 
                           datatype = "Gaussian", 
                           nboot = 1000, npermut = 0)
summary(comsafrica_platfwidth)
plot(comsafrica_platfwidth, type = "boot", cex.main = 0.8,main="platform width")

#flake platform thickness
set.seed(50)
comsafrica_platfthickmax<-rpt(platfthickmax ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_conda, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
summary(comsafrica_platfthickmax)
plot(comsafrica_platfthickmax, type = "boot", cex.main = 0.8,main="platform thickness")

#flake EPA
hist(log10(comsafrica_data_complete_conda$edgeplatf))
set.seed(50)
comsafrica_edgeplatf<-rpt(log(edgeplatf) ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = filter(comsafrica_data_complete_conda,edgeplatf>0), 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
summary(comsafrica_edgeplatf)
plot(comsafrica_edgeplatf, type = "boot", cex.main = 0.8,main="EPA")

### repeatability coefficients for Count data ####

comsafrica_data_count_data<-comsafrica_data %>%
   select(c(assemblage_code,analyst_id,flake_id,proximal_scars,left_scars,distal_scars,right_scars,
            dorsal_scar_count))

comsafrica_data_count_data_condb<-comsafrica_data_count_data %>%
   subset(assemblage_code=="chert_condition_B")

comsafrica_data_count_data_conda<-comsafrica_data_count_data %>%
   subset(assemblage_code=="chert_condition_A") 

## Condition a####

# proximal_scars
hist(comsafrica_data_count_data_conda$proximal_scars)
set.seed(50)
comsafrica_proximal_scars_boot<-rpt(proximal_scars ~ (1 | flake_id), 
                                    grname = "flake_id", 
                                    data = comsafrica_data_count_data_conda, 
                                    datatype = "Poisson", 
                                    nboot = 100, npermut = 100)
summary(comsafrica_proximal_scars_boot)
summary(comsafrica_proximal_scars_boot$mod)
plot(comsafrica_proximal_scars_boot, type = "boot", cex.main = 0.8,main="")
plot(comsafrica_proximal_scars_boot, type = "permut", cex.main = 1)

# left_scars
hist(comsafrica_data_count_data_conda$left_scars)
set.seed(50)
comsafrica_left_scars_boot<-rpt(left_scars ~ (1 | flake_id), 
                                grname = "flake_id", 
                                data = comsafrica_data_count_data_conda, 
                                datatype = "Poisson", 
                                nboot = 100, npermut = 100)
summary(comsafrica_left_scars_boot)
summary(comsafrica_left_scars_boot$mod)
plot(comsafrica_left_scars_boot, type = "boot", cex.main = 0.8,main="")
plot(comsafrica_left_scars_boot, type = "permut", cex.main = 1)

# distal_scars
hist(comsafrica_data_count_data_conda$distal_scars)
set.seed(50)
comsafrica_distal_scars_boot<-rpt(distal_scars ~ (1 | flake_id), 
                                  grname = "flake_id", 
                                  data = comsafrica_data_count_data_conda, 
                                  datatype = "Poisson", 
                                  nboot = 100, npermut = 100)
summary(comsafrica_distal_scars_boot)
summary(comsafrica_distal_scars_boot$mod)
plot(comsafrica_distal_scars_boot, type = "boot", cex.main = 0.8,main="")
plot(comsafrica_distal_scars_boot, type = "permut", cex.main = 1)

# right_scars
hist(comsafrica_data_count_data_conda$right_scars)
set.seed(50)
comsafrica_right_scars_boot<-rpt(right_scars ~ (1 | flake_id), 
                                 grname = "flake_id", 
                                 data = comsafrica_data_count_data_conda, 
                                 datatype = "Poisson", 
                                 nboot = 100, npermut = 100)
summary(comsafrica_right_scars_boot)
summary(comsafrica_right_scars_boot$mod)
plot(comsafrica_right_scars_boot, type = "boot", cex.main = 0.8,main="")
plot(comsafrica_right_scars_boot, type = "permut", cex.main = 1)

# dorsal scar count
hist(comsafrica_data_count_data_conda$dorsal_scar_count)
set.seed(50)
comsafrica_dorsal_scar_count_boot<-rpt(dorsal_scar_count ~ (1 | flake_id), 
                                       grname = "flake_id", 
                                       data = comsafrica_data_count_data_conda, 
                                       datatype = "Poisson", 
                                       nboot = 100, npermut = 100)
summary(comsafrica_dorsal_scar_count_boot)
summary(comsafrica_dorsal_scar_count_boot$mod)
plot(comsafrica_dorsal_scar_count_boot, type = "boot", cex.main = 0.8,main="")
plot(comsafrica_dorsal_scar_count_boot, type = "permut", cex.main = 1)

## Condition b####

# proximal_scars
hist(comsafrica_data_count_data_condb$proximal_scars)
comsafrica_proximal_scars_boot<-rpt(proximal_scars ~ (1 | flake_id), 
                            grname = "flake_id", 
                            data = comsafrica_data_count_data_condb, 
                            datatype = "Poisson", 
                            nboot = 100, npermut = 100)
summary(comsafrica_proximal_scars_boot)
summary(comsafrica_proximal_scars_boot$mod)
plot(comsafrica_proximal_scars_boot, type = "boot", cex.main = 0.8,main="")
plot(comsafrica_proximal_scars_boot, type = "permut", cex.main = 1)

# left_scars
hist(comsafrica_data_count_data_condb$left_scars)
comsafrica_left_scars_boot<-rpt(left_scars ~ (1 | flake_id), 
                                    grname = "flake_id", 
                                    data = comsafrica_data_count_data_condb, 
                                    datatype = "Poisson", 
                                    nboot = 100, npermut = 100)
summary(comsafrica_left_scars_boot)
summary(comsafrica_left_scars_boot$mod)
plot(comsafrica_left_scars_boot, type = "boot", cex.main = 0.8,main="")
plot(comsafrica_left_scars_boot, type = "permut", cex.main = 1)

# distal_scars
hist(comsafrica_data_count_data_condb$distal_scars)
comsafrica_distal_scars_boot<-rpt(distal_scars ~ (1 | flake_id), 
                                grname = "flake_id", 
                                data = comsafrica_data_count_data_condb, 
                                datatype = "Poisson", 
                                nboot = 100, npermut = 100)
summary(comsafrica_distal_scars_boot)
summary(comsafrica_distal_scars_boot$mod)
plot(comsafrica_distal_scars_boot, type = "boot", cex.main = 0.8,main="")
plot(comsafrica_distal_scars_boot, type = "permut", cex.main = 1)

# right_scars
hist(comsafrica_data_count_data_condb$right_scars)
comsafrica_right_scars_boot<-rpt(right_scars ~ (1 | flake_id), 
                                  grname = "flake_id", 
                                  data = comsafrica_data_count_data_condb, 
                                  datatype = "Poisson", 
                                  nboot = 100, npermut = 100)
summary(comsafrica_right_scars_boot)
summary(comsafrica_right_scars_boot$mod)
plot(comsafrica_right_scars_boot, type = "boot", cex.main = 0.8,main="")
plot(comsafrica_right_scars_boot, type = "permut", cex.main = 1)

# dorsal scar count
hist(comsafrica_data_count_data_condb$dorsal_scar_count)
comsafrica_dorsal_scar_count_boot<-rpt(dorsal_scar_count ~ (1 | flake_id), 
                                 grname = "flake_id", 
                                 data = comsafrica_data_count_data_condb, 
                                 datatype = "Poisson", 
                                 nboot = 100, npermut = 100)
summary(comsafrica_dorsal_scar_count_boot)
summary(comsafrica_dorsal_scar_count_boot$mod)
plot(comsafrica_dorsal_scar_count_boot, type = "boot", cex.main = 0.8,main="")
plot(comsafrica_dorsal_scar_count_boot, type = "permut", cex.main = 1)

### repeatability coefficients for categorical data ####

comsafrica_data_cat_data<-comsafrica_data %>%
   select(c(assemblage_code,analyst_id,flake_id,completeness,platform_cortex,directionality,platfmorph,
            platflipp,bulb,shattbulb,initiation,ventr_plane_form,section,latedgetype,flaketerm,
            distplanform,kombewa,red_syst,flk_form))

comsafrica_data_cat_data_condb<-comsafrica_data_cat_data %>%
   subset(assemblage_code=="chert_condition_B")

comsafrica_data_cat_data_conda<-comsafrica_data_cat_data %>%
   subset(assemblage_code=="chert_condition_A") 

## Condition a ####

## Reduction system

red_syst_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,red_syst) %>% 
   mutate(red_syst=as.factor(red_syst),
          red_syst=recode(red_syst,
                          Discoidal = "Discoid",
                          idnet = "Indet",
                          inde = "Indet",
                          Indeterminate = "Indet",
                          other = "Indet",
                          Ind = "Indet",
                          indet = "Indet",
                          indeterminate = "Indet",
                          laminar = "Laminar",
                          'Lev or Disc' = "Indet",
                          'Levallois indet' = "Levallois",
                          'Levallois non-Nubian' = "Levallois",
                          'LEVALLOIS/LEVALLOIS-RELATED' = "Levallois",
                          'Other (informal)' = "Indet",
                          'potential levallois' = "Levallois",
                          levallois = "Levallois",
                          LEVALLOIS = "Levallois",
                          'Levallois non Nubian' = "Levallois",
                          'Levallois?' = "Levallois",
                          'potential Lev' = "Levallois",
                          'potential Levallois' = "Levallois",
                          CENTRIPETAL = "Discoid",
                          discoid = "Discoid",
                          DISCOID = "Discoid",
                          FLAKE = "Flake")) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

# delete flake #'s with < 4 observations
red_syst_data_a<-red_syst_data_a[as.numeric(ave(red_syst_data_a$flake_id, 
                                                red_syst_data_a$flake_id, 
                                                    FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
red_syst_data_a_krip<- red_syst_data_a %>%
   mutate(red_sys_dummy=unclass(red_syst)) %>% 
   select(-red_syst) %>%
   spread(analyst_id, red_sys_dummy) %>%
   select(-flake_id) %>% 
   as.matrix() 

set.seed(42)
fit.full_red_syst<-krippendorffs.alpha(red_syst_data_a_krip, 
                                       level = "nominal", 
                                       control = list(parallel = FALSE,bootit=100), 
                                       verbose = TRUE)
summary(fit.full_red_syst)

plot(fit.full_red_syst, xlim = c(0, 0.9), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
# requires actual categorical variables not reclasssed values
red_sys_data_a_fleiss<- red_syst_data_a %>%
   mutate(red_syst=as.factor(red_syst)) %>%
   spread(analyst_id, red_syst) %>%
   select(-flake_id) 

kappam.fleiss(red_sys_data_a_fleiss, detail = T)

## flake form

flake_form_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,flk_form) %>% 
   mutate(flk_form=recode(comsafrica_data_cat_data_conda$flk_form, 
                          BLADE = "Blade",
                          CONVFLAKE = "Convflake", 
                          ELONG = "Blade",
                          Elong = "Blade",
                          flake = "Flake",
                          FLAKE = "Flake")) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

# delete flake #'s with < 4 observations
flake_form_data_a<-flake_form_data_a[as.numeric(ave(flake_form_data_a$flake_id, 
                                                    flake_form_data_a$flake_id, 
                                                        FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
flake_form_data_a_krip<- flake_form_data_a %>%
   mutate(flk_form=as.factor(flk_form),
          flk_form_dummy=unclass(flk_form)) %>% 
   select(-flk_form) %>%
   spread(analyst_id, flk_form_dummy) %>%
   select(-flake_id) %>% 
   as.matrix() 

set.seed(42)
fit.full_flk_form<-krippendorffs.alpha(flake_form_data_a_krip, 
                                           level = "nominal", 
                                           control = list(parallel = FALSE,bootit=100), 
                                           verbose = TRUE)
summary(fit.full_flk_form)

plot(fit.full_flk_form, xlim = c(0, 0.9), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
# requires actual categorical variables not reclasssed values
flake_form_data_a_fleiss<- flake_form_data_a %>%
   mutate(flk_form=as.factor(flk_form)) %>%
   spread(analyst_id, flk_form) %>%
   select(-flake_id) 

kappam.fleiss(flake_form_data_a_fleiss, detail = T)

## completeness

completeness_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,completeness) %>% 
   na_if("") %>% #delete coding episodes with no data
   na.omit

# delete flake #'s with < 4 observations
completeness_data_a<-completeness_data_a[as.numeric(ave(completeness_data_a$flake_id, 
                                                        completeness_data_a$flake_id, 
                                                        FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
completeness_data_a_krip<- completeness_data_a %>%
   mutate(completeness=as.factor(completeness),
          completeness_dummy=unclass(completeness)) %>% 
   select(-completeness) %>%
   spread(analyst_id, completeness_dummy) %>%
   select(-flake_id) %>% 
   as.matrix() 

set.seed(42)
fit.full_completeness<-krippendorffs.alpha(completeness_data_a_krip, 
                                           level = "nominal", 
                                           control = list(parallel = FALSE,bootit=100), 
                                           verbose = TRUE)
summary(fit.full_completeness)

plot(fit.full_completeness, xlim = c(0, 0.9), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
# requires actual categorical variables not reclasssed values
completeness_data_a_fleiss<- completeness_data_a %>%
   mutate(completeness=as.factor(completeness)) %>%
   spread(analyst_id, completeness) %>%
   select(-flake_id) 

kappam.fleiss(completeness_data_a_fleiss, detail = T)

## platform cortex
platform_cortex_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,platform_cortex) %>%
   mutate(platform_cortex=recode(comsafrica_data_cat_data_conda$platform_cortex, INDET = "Indeterminate",
                                 complete = "Complete", absent = "Absent"),
          platform_cortex=as.factor(platform_cortex),
          platform_cortex_dummy=unclass(platform_cortex)) %>% 
   na_if("") %>% #delete coding episodes with no data
   na.omit 

platform_cortex_data_a<-platform_cortex_data_a[as.numeric(ave(platform_cortex_data_a$flake_id, 
                                                              platform_cortex_data_a$flake_id, 
                                                              FUN=length)) >= 6, ]

platform_cortex_data_a_krip<- platform_cortex_data_a %>%
   mutate(platform_cortex=as.factor(platform_cortex),
          platform_cortex_dummy=unclass(platform_cortex)) %>% 
   select(-platform_cortex) %>%
   spread(analyst_id, platform_cortex_dummy) %>%
   select(-flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_cortex<-krippendorffs.alpha(platform_cortex_data_a_krip, 
                                     level = "nominal", 
                                     control = list(parallel = FALSE,bootit=100), 
                                     verbose = TRUE)
summary(fit.full_cortex)

plot(fit.full_cortex, xlim = c(0, 1), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
platform_cortex_data_a_fleiss<-platform_cortex_data_a %>%
   select(flake_id,analyst_id,platform_cortex) %>%
   spread(analyst_id,platform_cortex) %>%
   select(-flake_id)

kappam.fleiss(platform_cortex_data_a_fleiss, detail = T)

## scar directions

directionality_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,directionality) %>% 
   na_if("") %>% #delete coding episodes with no data
   na.omit %>%
   mutate(directionality=as.factor(directionality),
          directionality=recode(directionality, 
                                centripetal = "Centripetal",
                                Other = "Indeterminate"))

directionality_data_a<-directionality_data_a[as.numeric(ave(directionality_data_a$flake_id, 
                                                            directionality_data_a$flake_id, 
                                                            FUN=length)) >= 6, ]

directionality_data_a_krip<- directionality_data_a %>%
   mutate(directionality_dummy=unclass(directionality)) %>% 
   select(-directionality) %>%
   spread(analyst_id, directionality_dummy) %>%
   select(-flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_directions<-krippendorffs.alpha(directionality_data_a_krip, 
                                         level = "nominal", 
                                         control = list(parallel = FALSE,bootit=100), 
                                         verbose = TRUE)
summary(fit.full_directions)

plot(fit.full_directions, xlim = c(0, 0.4), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
directionality_data_a_fleiss<- directionality_data_a %>%
   spread(analyst_id, directionality) %>%
   select(-flake_id)

kappam.fleiss(directionality_data_a_fleiss, detail = T)

## platf morph

plat_morph_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,platfmorph) %>% 
   na_if("") %>% #delete coding episodes with no data
   na.omit %>%
   mutate(platfmorph=recode(platfmorph, 
                            'Chapeau de Gendarme' = "ChapeauDeGendarme",
                            linear = "Linear",
                            Diherdral = "Dihedral",
                            Other = "Indeterminate",
                            facetted = "Facetted"),
          platfmorph=as.factor(platfmorph))

plat_morph_data_a<-plat_morph_data_a[as.numeric(ave(plat_morph_data_a$flake_id, 
                                                    plat_morph_data_a$flake_id, 
                                                    FUN=length)) >= 6, ]

plat_morph_data_a_krip<- plat_morph_data_a %>%
   mutate(plat_morph_dummy=unclass(platfmorph)) %>% 
   select(-platfmorph) %>%
   spread(analyst_id, plat_morph_dummy) %>%
   select(-flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_platmorphs<-krippendorffs.alpha(plat_morph_data_a_krip, 
                                         level = "nominal", 
                                         control = list(parallel = FALSE,bootit=100), 
                                         verbose = TRUE)
summary(fit.full_platmorphs)

plot(fit.full_platmorphs, xlim = c(0, 0.7), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
plat_morph_data_a_fleiss<- plat_morph_data_a %>%
   spread(analyst_id, platfmorph) %>%
   select(-flake_id)

kappam.fleiss(plat_morph_data_a_fleiss, detail = T)

## platf lip

plat_lip_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,platflipp) %>% 
   na_if("") %>% #delete coding episodes with no data
   na.omit %>%
   mutate(platflipp=as.factor(platflipp),
          platflipp=recode_factor(platflipp, YES = "yes", Yes='yes',
                                  NO = "no", No="no", 'NOT APPLICABLE' = "Indeterminate"),
          platflipp=as.factor(platflipp))

plat_lip_data_a<-plat_lip_data_a[as.numeric(ave(plat_lip_data_a$flake_id, 
                                                plat_lip_data_a$flake_id, 
                                                FUN=length)) >=6, ]

plat_lip_data_a_krip<- plat_lip_data_a %>%
   mutate(plat_lip_dummy=unclass(platflipp)) %>% 
   select(-platflipp) %>%
   spread(analyst_id, plat_lip_dummy) %>%
   select(-flake_id) %>% 
   as.matrix()

set.seed(42)
fit.full_platlip<-krippendorffs.alpha(plat_lip_data_a_krip, 
                                      level = "nominal", 
                                      control = list(parallel = FALSE,bootit=100), 
                                      verbose = TRUE)

summary(fit.full_platlip)

plot(fit.full_platlip, xlim = c(0, 0.5), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
plat_lip_data_a_fleiss<- plat_lip_data_a %>%
   spread(analyst_id, platflipp) %>%
   select(-flake_id)

kappam.fleiss(plat_lip_data_a_fleiss, detail = T)

## bulb

plat_bulb_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,bulb) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit %>%
   mutate(bulb=recode(bulb, YES = "yes", Yes="yes",
                      NO = "no",No="no"),
          bulb=as.factor(bulb)) 


plat_bulb_data_a<-plat_bulb_data_a[as.numeric(ave(plat_bulb_data_a$flake_id, 
                                                  plat_bulb_data_a$flake_id, 
                                                  FUN=length)) >=6, ]

plat_bulb_data_a_krip<- plat_bulb_data_a %>%
   mutate(plat_bulb_dummy=unclass(bulb)) %>% 
   select(-bulb) %>%
   spread(analyst_id, plat_bulb_dummy) %>%
   select(-flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_bulb<-krippendorffs.alpha(plat_bulb_data_a_krip, 
                                   level = "nominal", 
                                   control = list(parallel = FALSE,bootit=100), 
                                   verbose = TRUE)

summary(fit.full_bulb)

plot(fit.full_bulb, xlim = c(0, 1), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
bulb_data_a_fleiss<- plat_bulb_data_a %>%
   spread(analyst_id, bulb) %>%
   select(-flake_id)

kappam.fleiss(bulb_data_a_fleiss, detail = T)

## Shattbulb

plat_shattbulb_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,shattbulb) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit %>%
   mutate(shattbulb=recode(shattbulb, 
                           Indet = "Indeterminate",
                           Indeterminateerminate = "Indeterminate",
                           NO = "No",
                           no="No",
                           YES = "Yes"),
          shattbulb=as.factor(shattbulb))

plat_shattbulb_data_a<-plat_shattbulb_data_a[as.numeric(ave(plat_shattbulb_data_a$flake_id, 
                                                            plat_shattbulb_data_a$flake_id, 
                                                            FUN=length)) >=6, ]

plat_shattbulb_data_a_krip<- plat_shattbulb_data_a %>%
   mutate(plat_shattbulb_dummy=unclass(shattbulb)) %>% 
   select(-shattbulb) %>%
   spread(analyst_id, plat_shattbulb_dummy) %>%
   select(-flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_shattbulb<-krippendorffs.alpha(plat_shattbulb_data_a_krip, 
                                        level = "nominal", 
                                        control = list(parallel = FALSE,bootit=100), 
                                        verbose = TRUE)

summary(fit.full_shattbulb)

plot(fit.full_shattbulb, xlim = c(0, 0.6), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
shattbulb_data_a_fleiss<- plat_shattbulb_data_a %>%
   spread(analyst_id, shattbulb) %>%
   select(-flake_id)

kappam.fleiss(shattbulb_data_a_fleiss, detail = T)

## initiation

plat_initiation_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,initiation) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit %>%
   mutate(initiation=recode(initiation, 
                            BENDING = "Bending",
                            HERTZIAN = "Hertzian",
                            WEDGING = "Wedging"),
          initiation=as.factor(initiation))

plat_initiation_data_a<-plat_initiation_data_a[as.numeric(ave(plat_initiation_data_a$flake_id, 
                                                              plat_initiation_data_a$flake_id, 
                                                              FUN=length)) >=6, ]

plat_initiation_data_a_krip<- plat_initiation_data_a %>%
   mutate(initiation_dummy=unclass(initiation)) %>% 
   select(-initiation) %>%
   spread(analyst_id, initiation_dummy) %>%
   select(-flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_initiation<-krippendorffs.alpha(plat_initiation_data_a_krip, 
                                         level = "nominal", 
                                         control = list(parallel = FALSE,bootit=100), 
                                         verbose = TRUE)

summary(fit.full_initiation)

plot(fit.full_initiation, xlim = c(0, 1), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
initiation_data_a_fleiss<- plat_initiation_data_a %>%
   spread(analyst_id, initiation) %>%
   select(-flake_id)

kappam.fleiss(initiation_data_a_fleiss, detail = T)

## ventr_plane_form

ventr_plane_form_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,ventr_plane_form) %>%
   mutate(ventr_plane_form=as.factor((ventr_plane_form)),
          ventr_plane_form=recode(ventr_plane_form, 
                                  very_concave = "Very concave",
                                  Very_concave = "Very concave",
                                  BULBAR = "Bulbar",
                                  CONCAVE = "Concave",
                                  FLAT = "Flat",
                                  TWISTED = "Twisted",
                                  'VERY CONCAVE' = "Very concave",
                                  VERY_CONCAVE = "Very concave")) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

ventr_plane_form_data_a<-ventr_plane_form_data_a[as.numeric(ave(ventr_plane_form_data_a$flake_id, 
                                                                ventr_plane_form_data_a$flake_id, 
                                                                FUN=length)) >=6, ]

ventr_plane_form_data_a_krip<- ventr_plane_form_data_a %>%
   mutate(ventr_plane_form_dummy=unclass(ventr_plane_form)) %>% 
   select(-ventr_plane_form) %>%
   spread(analyst_id, ventr_plane_form_dummy) %>%
   select(-flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_ventr_plane_form<-krippendorffs.alpha(ventr_plane_form_data_a_krip, 
                                               level = "nominal", 
                                               control = list(parallel = FALSE,bootit=100), 
                                               verbose = TRUE)

summary(fit.full_ventr_plane_form)

plot(fit.full_ventr_plane_form, xlim = c(0, 0.3), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
ventr_plane_form_fleiss<- ventr_plane_form_data_a %>%
   spread(analyst_id, ventr_plane_form) %>%
   select(-flake_id)

kappam.fleiss(ventr_plane_form_fleiss, detail = T)

## Section

section_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,section) %>%
   mutate(section=as.factor((section)),
          section=recode(section, 
                         DOMED = "Domed",
                         INDET = "Indeterminate",
                         LENTIC = "Lenticular",
                         LENTICULAR = "Lenticular",
                         RIGHTTRI = "Righttriangle",
                         RIGHTTRIANGLE = "Righttriangle",
                         TRAP = "Trapezoidal",
                         TRAPEZOIDAL = "Trapezoidal",
                         TRI = "Triangular",
                         TRIANGULAR = "Triangular")) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

section_data_a<-section_data_a[as.numeric(ave(section_data_a$flake_id, 
                                              section_data_a$flake_id, 
                                              FUN=length)) >=6, ]

section_data_a_krip<- section_data_a %>%
   mutate(section_dummy=unclass(section)) %>% 
   select(-section) %>%
   spread(analyst_id, section_dummy) %>%
   select(-flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_section<-krippendorffs.alpha(section_data_a_krip, 
                                      level = "nominal", 
                                      control = list(parallel = FALSE,bootit=100), 
                                      verbose = TRUE)

summary(fit.full_section)

plot(fit.full_section, xlim = c(0.2, 0.5), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
section_fleiss<- section_data_a %>%
   spread(analyst_id, section) %>%
   select(-flake_id)

kappam.fleiss(section_fleiss, detail = T)

## Lateral edge type

latedge_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,latedgetype) %>%
   mutate(latedgetype=as.factor(latedgetype),
          latedgetype=recode(latedgetype, 
                             AMORPH = "Amorphous",
                             CONV = "Convergent",
                             CONVERGENT = "Convergent",
                             DIAMOND = "Diamond",
                             DIV = "Divergent",
                             DIVERGENT = "Divergent",
                             INDET = "Indeterminate",
                             na = "Indeterminate",
                             OVAL = "Ovoid",
                             OVOID = "Ovoid",
                             PARALLEL = "Parallel")) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

latedge_data_a<-latedge_data_a[as.numeric(ave(latedge_data_a$flake_id, 
                                              latedge_data_a$flake_id, 
                                              FUN=length)) >=6, ]

latedge_data_a_krip<- latedge_data_a %>%
   mutate(latedge_dummy=unclass(latedgetype)) %>% 
   select(-latedgetype) %>%
   spread(analyst_id, latedge_dummy) %>%
   select(-flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_latedge<-krippendorffs.alpha(latedge_data_a_krip, 
                                      level = "nominal", 
                                      control = list(parallel = FALSE,bootit=100), 
                                      verbose = TRUE)

summary(fit.full_latedge)

plot(fit.full_latedge, xlim = c(0.2, 0.5), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
latedge_fleiss<- latedge_data_a %>%
   spread(analyst_id, latedgetype) %>%
   select(-flake_id)

kappam.fleiss(latedge_fleiss, detail = T)

## Flake termination

flaketerm_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,flaketerm) %>%
   mutate(flaketerm=as.factor(flaketerm),
          flaketerm=recode(flaketerm, 
                           FEATHER = "Feather",
                           HINGE = "Hinge",
                           INDET = "Indeterminate",
                           OVERSHOT = "Overshot",
                           AXIAL= "Axial")) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

flaketerm_data_a<-flaketerm_data_a[as.numeric(ave(flaketerm_data_a$flake_id, 
                                                  flaketerm_data_a$flake_id, 
                                                  FUN=length)) >=6, ]

flaketerm_data_a_krip<- flaketerm_data_a %>%
   mutate(flaketerm_dummy=unclass(flaketerm)) %>% 
   select(-flaketerm) %>%
   spread(analyst_id, flaketerm_dummy) %>%
   select(-flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_flaketerm<-krippendorffs.alpha(flaketerm_data_a_krip, 
                                        level = "nominal", 
                                        control = list(parallel = FALSE,bootit=100), 
                                        verbose = TRUE)

summary(fit.full_flaketerm)

plot(fit.full_flaketerm, xlim = c(0, 0.6), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
flaketerm_fleiss<- flaketerm_data_a %>%
   spread(analyst_id, flaketerm) %>%
   select(-flake_id)

kappam.fleiss(flaketerm_fleiss, detail = T)

## Kombewa

kombewa_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,kombewa) %>%
   mutate(kombewa=as.factor(kombewa),
          kombewa=recode(kombewa, 
                         NO = "No",
                         YES = "Yes",
                         '0.84'="Indet")) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

kombewa_data_a<-kombewa_data_a[as.numeric(ave(kombewa_data_a$flake_id, 
                                              kombewa_data_a$flake_id, 
                                              FUN=length)) > 4, ]

kombewa_data_a_krip<- kombewa_data_a %>%
   mutate(kombewa_dummy=unclass(kombewa)) %>% 
   select(-kombewa) %>%
   spread(analyst_id, kombewa_dummy) %>%
   select(-flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_kombewa<-krippendorffs.alpha(kombewa_data_a_krip, 
                                      level = "nominal", 
                                      control = list(parallel = FALSE,bootit=100), 
                                      verbose = TRUE)

summary(fit.full_kombewa)

plot(fit.full_kombewa, xlim = c(0, 1), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
kombewa_fleiss<- kombewa_data_a %>%
   spread(analyst_id, kombewa) %>%
   select(-flake_id)

kappam.fleiss(kombewa_fleiss, detail = T)

## Distal plan form

distplanform_data_a<-comsafrica_data_cat_data_conda %>%
   select(flake_id,analyst_id,distplanform) %>%
   mutate(distplanform=as.factor(distplanform),
          distplanform=recode(distplanform, 
                              FLAT = "Flat",
                              INDET = "Indeterminate",
                              INDETERMINATE = "Indeterminate",
                              IRR = "Irregular",
                              Irreg = "Irregular",
                              POINTED = "Pointed",
                              rounded = "Rounded",
                              ROUNDED = "Rounded")) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

distplanform_data_a<-distplanform_data_a[as.numeric(ave(distplanform_data_a$flake_id, 
                                                        distplanform_data_a$flake_id, 
                                                        FUN=length)) > 4, ]

distplanform_data_a_krip<- distplanform_data_a %>%
   mutate(distplanform_dummy=unclass(distplanform)) %>% 
   select(-distplanform) %>%
   spread(analyst_id, distplanform_dummy) %>%
   select(-flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_distplanform<-krippendorffs.alpha(distplanform_data_a_krip, 
                                           level = "nominal", 
                                           control = list(parallel = FALSE,bootit=100), 
                                           verbose = TRUE)

summary(fit.full_distplanform)

plot(fit.full_distplanform, xlim = c(0.1, 0.5), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
distplanform_fleiss<- distplanform_data_a %>%
   spread(analyst_id, distplanform) %>%
   select(-flake_id)

kappam.fleiss(distplanform_fleiss, detail = T)

## Condition b ####

## Reduction system

red_syst_data_b<-comsafrica_data_cat_data_condb %>%
   select(flake_id,analyst_id,red_syst) %>% 
   mutate(red_syst=as.factor(red_syst),
          red_syst=recode(red_syst,
                          Discoidal = "Discoid",
                          idnet = "Indet",
                          inde = "Indet",
                          Indeterminate = "Indet",
                          other = "Indet",
                          Ind = "Indet",
                          indet = "Indet",
                          indeterminate = "Indet",
                          INDET = "Indet",
                          'indeterminate (broken)' = "Indet",
                          'Indeterminate (broken)' = "Indet",
                          laminar = "Laminar",
                          'Lev or Disc' = "Indet",
                          'Levallois indet' = "Levallois",
                          'Levallois non-Nubian' = "Levallois",
                          'levallois non nubian' = "Levallois",
                          'LEVALLOIS OR DISCOID' = "Indet",
                          na = "Indet",
                          none = "Indet",
                          'Platform (laminar?)' = "Platform",
                          'Platform / Laminar' = "Platform",
                          'possible Levallois non-Nubian' = "Levallois",
                          'LEVALLOIS/LEVALLOIS-RELATED' = "Levallois",
                          'Levallois (pref)' = "Levallois",
                          'Other (informal)' = "Indet",
                          'potential levallois' = "Levallois",
                          levallois = "Levallois",
                          LEVALLOIS = "Levallois",
                          'Levallois non Nubian' = "Levallois",
                          'Levallois?' = "Levallois",
                          'potential Lev' = "Levallois",
                          'potential Levallois' = "Levallois",
                          CENTRIPETAL = "Discoid",
                          discoid = "Discoid",
                          DISCOID = "Discoid",
                          'Discoid?' = "Discoid",
                          FLAKE = "Flake",
                          multidirectional = "Platform",
                          'NON-LEVALLOIS; ORTHOGONAL VOLUME EXPLOITATION' = "Platform")) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

levels(red_syst_data_b$red_syst)

# delete flake #'s with < 4 observations
red_syst_data_b<-red_syst_data_b[as.numeric(ave(red_syst_data_b$flake_id, 
                                                red_syst_data_b$flake_id, 
                                                FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
red_syst_data_b_krip<- red_syst_data_b %>%
   mutate(red_sys_dummy=unclass(red_syst)) %>% 
   select(-red_syst) %>%
   spread(analyst_id, red_sys_dummy) %>%
   select(-flake_id) %>% 
   as.matrix() 

set.seed(42)
fit.full_red_syst<-krippendorffs.alpha(red_syst_data_b_krip, 
                                       level = "nominal", 
                                       control = list(parallel = FALSE,bootit=100), 
                                       verbose = TRUE)
summary(fit.full_red_syst)

plot(fit.full_red_syst, xlim = c(0, 0.9), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
# requires actual categorical variables not reclasssed values
red_sys_data_b_fleiss<- red_syst_data_b %>%
   mutate(red_syst=as.factor(red_syst)) %>%
   spread(analyst_id, red_syst) %>%
   select(-flake_id) 

kappam.fleiss(red_sys_data_b_fleiss, detail = T)

## flake form

flake_form_data_b<-comsafrica_data_cat_data_condb %>%
   select(flake_id,analyst_id,flk_form) %>% 
   mutate(flk_form=recode(comsafrica_data_cat_data_condb$flk_form, 
                          BLADE = "Blade",
                          CONVFLAKE = "Convflake", 
                          ELONG = "Blade",
                          Elong = "Blade",
                          flake = "Flake",
                          FLAKE = "Flake",
                          indet = "Indet",
                          INDET = "Indet")) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

# delete flake #'s with < 4 observations
flake_form_data_b<-flake_form_data_b[as.numeric(ave(flake_form_data_b$flake_id, 
                                                    flake_form_data_b$flake_id, 
                                                    FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
flake_form_data_b_krip<- flake_form_data_b %>%
   mutate(flk_form=as.factor(flk_form),
          flk_form_dummy=unclass(flk_form)) %>% 
   select(-flk_form) %>%
   spread(analyst_id, flk_form_dummy) %>%
   select(-flake_id) %>% 
   as.matrix() 

set.seed(42)
fit.full_flk_form<-krippendorffs.alpha(flake_form_data_b_krip, 
                                       level = "nominal", 
                                       control = list(parallel = FALSE,bootit=100), 
                                       verbose = TRUE)
summary(fit.full_flk_form)

plot(fit.full_flk_form, xlim = c(0, 0.9), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa

flake_form_data_b_fleiss<- flake_form_data_b %>%
   mutate(flk_form=as.factor(flk_form)) %>%
   spread(analyst_id, flk_form) %>%
   select(-flake_id) 

kappam.fleiss(flake_form_data_b_fleiss, detail = T)

## completeness

completeness_data_b<-comsafrica_data_cat_data_condb %>%
   select(flake_id,analyst_id,completeness) %>% 
   na_if("") %>% #delete coding episodes with no data
   na.omit

# delete flake #'s with < 4 observations
completeness_data_b<-completeness_data_b[as.numeric(ave(completeness_data_b$flake_id, 
                                                        completeness_data_b$flake_id, 
                                                            FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
completeness_data_b_krip<- completeness_data_b %>%
   mutate(completeness=as.factor(completeness),
          completeness_dummy=unclass(completeness)) %>% 
   select(-completeness) %>%
   spread(analyst_id, completeness_dummy) %>%
   select(-flake_id) %>% 
   as.matrix() 

set.seed(42)
fit.full_completeness<-krippendorffs.alpha(completeness_data_b_krip, 
                                         level = "nominal", 
                                         control = list(parallel = FALSE,bootit=100), 
                                         verbose = TRUE)
summary(fit.full_completeness)

plot(fit.full_completeness, xlim = c(0, 0.9), 
     xlab = "Bootstrap Estimates", 
     main = "Nominal Data",
     density = FALSE)

# Compute kapa-allows us to see which categories are performing better
# requires actual categorical variables not reclasssed values
completeness_data_b_fleiss<- completeness_data_b %>%
   mutate(completeness=as.factor(completeness)) %>%
   spread(analyst_id, completeness) %>%
   select(-flake_id) 

kappam.fleiss(completeness_data_b_fleiss, detail = T)

## platform cortex
 platform_cortex_data_b<-comsafrica_data_cat_data_condb %>%
    select(flake_id,analyst_id,platform_cortex) %>%
    mutate(platform_cortex=recode(comsafrica_data_cat_data_condb$platform_cortex, INDET = "Indeterminate",
           complete = "Complete", absent = "Absent"),
           platform_cortex=as.factor(platform_cortex),
           platform_cortex_dummy=unclass(platform_cortex)) %>% 
    na_if("") %>% #delete coding episodes with no data
    na.omit 
 
 platform_cortex_data_b<-platform_cortex_data_b[as.numeric(ave(platform_cortex_data_b$flake_id, 
                                                               platform_cortex_data_b$flake_id, 
                                                         FUN=length)) >= 6, ]
 
 platform_cortex_data_b_krip<- platform_cortex_data_b %>%
    mutate(platform_cortex=as.factor(platform_cortex),
           platform_cortex_dummy=unclass(platform_cortex)) %>% 
    select(-platform_cortex) %>%
    spread(analyst_id, platform_cortex_dummy) %>%
    select(-flake_id) %>%
    as.matrix()
 
 set.seed(42)
 fit.full_cortex<-krippendorffs.alpha(platform_cortex_data_b_krip, 
                               level = "nominal", 
                               control = list(parallel = FALSE,bootit=100), 
                               verbose = TRUE)
 summary(fit.full_cortex)
 
 plot(fit.full_cortex, xlim = c(0, 1), 
      xlab = "Bootstrap Estimates", 
      main = "Nominal Data",
      density = FALSE)
 
 # Compute kapa-allows us to see which categories are performing better
 platform_cortex_data_b_fleiss<-platform_cortex_data_b %>%
    select(flake_id,analyst_id,platform_cortex) %>%
    spread(analyst_id,platform_cortex) %>%
    select(-flake_id)
 
 kappam.fleiss(platform_cortex_data_b_fleiss, detail = T)
 
## scar directions
 
 directionality_data_b<-comsafrica_data_cat_data_condb %>%
    select(flake_id,analyst_id,directionality) %>% 
    na_if("") %>% #delete coding episodes with no data
    na.omit %>%
    mutate(directionality=as.factor(directionality),
           directionality=recode(directionality, 
                                 centripetal = "Centripetal",
                                 Other = "Indeterminate"))
 
 directionality_data_b<-directionality_data_b[as.numeric(ave(directionality_data_b$flake_id, 
                                      directionality_data_b$flake_id, 
                                      FUN=length)) >= 6, ]
 
 directionality_data_b_krip<- directionality_data_b %>%
      mutate(directionality_dummy=unclass(directionality)) %>% 
    select(-directionality) %>%
    spread(analyst_id, directionality_dummy) %>%
    select(-flake_id) %>%
    as.matrix()
 
 set.seed(42)
 fit.full_directions<-krippendorffs.alpha(directionality_data_b_krip, 
                                      level = "nominal", 
                                      control = list(parallel = FALSE,bootit=100), 
                                      verbose = TRUE)
 summary(fit.full_directions)
 
 plot(fit.full_directions, xlim = c(0, 0.4), 
      xlab = "Bootstrap Estimates", 
      main = "Nominal Data",
      density = FALSE)
 
 # Compute kapa-allows us to see which categories are performing better
 directionality_data_b_fleiss<- directionality_data_b %>%
    spread(analyst_id, directionality) %>%
    select(-flake_id)
 
 kappam.fleiss(directionality_data_b_fleiss, detail = T)
 
## platf morph
 
 plat_morph_data_b<-comsafrica_data_cat_data_condb %>%
    select(flake_id,analyst_id,platfmorph) %>% 
    na_if("") %>% #delete coding episodes with no data
    na.omit %>%
    mutate(platfmorph=recode(plat_morph_data_b$platfmorph, 
                             'Chapeau de Gendarme' = "ChapeauDeGendarme",
                             linear = "Linear",
                             Diherdral = "Dihedral",
                             Other = "Indeterminate"),
           platfmorph=as.factor(platfmorph))
 
 plat_morph_data_b<-plat_morph_data_b[as.numeric(ave(plat_morph_data_b$flake_id, 
                                                     plat_morph_data_b$flake_id, 
                                                             FUN=length)) >= 6, ]
 
 plat_morph_data_b_krip<- plat_morph_data_b %>%
    mutate(plat_morph_dummy=unclass(platfmorph)) %>% 
    select(-platfmorph) %>%
    spread(analyst_id, plat_morph_dummy) %>%
    select(-flake_id) %>%
    as.matrix()
 
 set.seed(42)
 fit.full_platmorphs<-krippendorffs.alpha(plat_morph_data_b_krip, 
                                          level = "nominal", 
                                          control = list(parallel = FALSE,bootit=100), 
                                          verbose = TRUE)
 summary(fit.full_platmorphs)
 
 plot(fit.full_platmorphs, xlim = c(0, 0.7), 
      xlab = "Bootstrap Estimates", 
      main = "Nominal Data",
      density = FALSE)
 
 # Compute kapa-allows us to see which categories are performing better
 plat_morph_data_b_fleiss<- plat_morph_data_b %>%
    spread(analyst_id, platfmorph) %>%
    select(-flake_id)
 
 kappam.fleiss(plat_morph_data_b_fleiss, detail = T)
 
## platf lip
 
 plat_lip_data_b<-comsafrica_data_cat_data_condb %>%
    select(flake_id,analyst_id,platflipp) %>% 
    na_if("") %>% #delete coding episodes with no data
    na.omit %>%
    mutate(platflipp=as.factor(platflipp),
           platflipp=recode_factor(platflipp, YES = "yes", Yes='yes',
                                   NO = "no", No="no", 'NOT APPLICABLE' = "Indeterminate"),
           platflipp=as.factor(platflipp))
 
 plat_lip_data_b<-plat_lip_data_b[as.numeric(ave(plat_lip_data_b$flake_id, 
                                                 plat_lip_data_b$flake_id, 
                                                     FUN=length)) >=6, ]
 
 plat_lip_data_b_krip<- plat_lip_data_b %>%
    mutate(plat_lip_dummy=unclass(platflipp)) %>% 
    select(-platflipp) %>%
    spread(analyst_id, plat_lip_dummy) %>%
    select(-flake_id) %>% 
    as.matrix()
 
 set.seed(42)
 fit.full_platlip<-krippendorffs.alpha(plat_lip_data_b_krip, 
                                          level = "nominal", 
                                          control = list(parallel = FALSE,bootit=100), 
                                          verbose = TRUE)
 
 summary(fit.full_platlip)
 
 plot(fit.full_platlip, xlim = c(0, 0.5), 
      xlab = "Bootstrap Estimates", 
      main = "Nominal Data",
      density = FALSE)
 
 # Compute kapa-allows us to see which categories are performing better
 plat_lip_data_b_fleiss<- plat_lip_data_b %>%
    spread(analyst_id, platflipp) %>%
    select(-flake_id)
 
 kappam.fleiss(plat_lip_data_b_fleiss, detail = T)
 
## bulb
 
plat_bulb_data_b<-comsafrica_data_cat_data_condb %>%
    select(flake_id,analyst_id,bulb) %>%
    na_if("") %>% #delete coding episodes with no data
    na.omit %>%
    mutate(bulb=recode(plat_bulb_data_b$bulb, YES = "yes", Yes="yes",
                            NO = "no",No="no"),
           bulb=as.factor(bulb)) 
 
 
 plat_bulb_data_b<-plat_bulb_data_b[as.numeric(ave(plat_bulb_data_b$flake_id, 
                                                   plat_bulb_data_b$flake_id, 
                                                 FUN=length)) >=6, ]
 
 plat_bulb_data_b_krip<- plat_bulb_data_b %>%
    mutate(plat_bulb_dummy=unclass(bulb)) %>% 
    select(-bulb) %>%
    spread(analyst_id, plat_bulb_dummy) %>%
    select(-flake_id) %>%
    as.matrix()
 
 set.seed(42)
 fit.full_bulb<-krippendorffs.alpha(plat_bulb_data_b_krip, 
                                       level = "nominal", 
                                       control = list(parallel = FALSE,bootit=100), 
                                       verbose = TRUE)
 
 summary(fit.full_bulb)
 
 plot(fit.full_bulb, xlim = c(0, 1), 
      xlab = "Bootstrap Estimates", 
      main = "Nominal Data",
      density = FALSE)
 
 # Compute kapa-allows us to see which categories are performing better
 bulb_data_b_fleiss<- plat_bulb_data_b %>%
    spread(analyst_id, bulb) %>%
    select(-flake_id)
 
 kappam.fleiss(bulb_data_b_fleiss, detail = T)
 
## Shattbulb
 
 plat_shattbulb_data_b<-comsafrica_data_cat_data_condb %>%
    select(flake_id,analyst_id,shattbulb) %>%
    na_if("") %>% #delete coding episodes with no data
    na.omit %>%
    mutate(shattbulb=recode(plat_shattbulb_data_b$shattbulb, 
                            Indet = "Indeterminate",
                            Indeterminateerminate = "Indeterminate",
                            NO = "No",
                            YES = "Yes"),
           shattbulb=as.factor(shattbulb))
 
 plat_shattbulb_data_b<-plat_shattbulb_data_b[as.numeric(ave(plat_shattbulb_data_b$flake_id, 
                                                    plat_shattbulb_data_b$flake_id, 
                                                   FUN=length)) >=6, ]
 
 plat_shattbulb_data_b_krip<- plat_shattbulb_data_b %>%
    mutate(plat_shattbulb_dummy=unclass(shattbulb)) %>% 
    select(-shattbulb) %>%
    spread(analyst_id, plat_shattbulb_dummy) %>%
    select(-flake_id) %>%
    as.matrix()
 
 set.seed(42)
 fit.full_shattbulb<-krippendorffs.alpha(plat_shattbulb_data_b_krip, 
                                    level = "nominal", 
                                    control = list(parallel = FALSE,bootit=100), 
                                    verbose = TRUE)
 
 summary(fit.full_shattbulb)
 
 plot(fit.full_shattbulb, xlim = c(0, 0.6), 
      xlab = "Bootstrap Estimates", 
      main = "Nominal Data",
      density = FALSE)
 
 # Compute kapa-allows us to see which categories are performing better
 shattbulb_data_b_fleiss<- plat_shattbulb_data_b %>%
    spread(analyst_id, shattbulb) %>%
    select(-flake_id)
 
 kappam.fleiss(shattbulb_data_b_fleiss, detail = T)
 
## initiation
 
 plat_initiation_data_b<-comsafrica_data_cat_data_condb %>%
    select(flake_id,analyst_id,initiation) %>%
    na_if("") %>% #delete coding episodes with no data
    na.omit %>%
    mutate(initiation=recode(plat_initiation_data_b$initiation, 
                             BENDING = "Bending",
                             HERTZIAN = "Hertzian"),
           initiation=as.factor(initiation))
 
 plat_initiation_data_b<-plat_initiation_data_b[as.numeric(ave(plat_initiation_data_b$flake_id, 
                                                               plat_initiation_data_b$flake_id, 
                                                             FUN=length)) >=6, ]
 
 plat_initiation_data_b_krip<- plat_initiation_data_b %>%
    mutate(initiation_dummy=unclass(initiation)) %>% 
    select(-initiation) %>%
    spread(analyst_id, initiation_dummy) %>%
    select(-flake_id) %>%
    as.matrix()
 
 set.seed(42)
 fit.full_initiation<-krippendorffs.alpha(plat_initiation_data_b_krip, 
                                         level = "nominal", 
                                         control = list(parallel = FALSE,bootit=100), 
                                         verbose = TRUE)
 
 summary(fit.full_initiation)
 
 plot(fit.full_initiation, xlim = c(0, 1), 
      xlab = "Bootstrap Estimates", 
      main = "Nominal Data",
      density = FALSE)
 
 # Compute kapa-allows us to see which categories are performing better
 initiation_data_b_fleiss<- plat_initiation_data_b %>%
    spread(analyst_id, initiation) %>%
    select(-flake_id)
 
 kappam.fleiss(initiation_data_b_fleiss, detail = T)
 
## ventr_plane_form
 
 ventr_plane_form_data_b<-comsafrica_data_cat_data_condb %>%
    select(flake_id,analyst_id,ventr_plane_form) %>%
    mutate(ventr_plane_form=as.factor((ventr_plane_form)),
           ventr_plane_form=recode(ventr_plane_form, 
                                   very_concave = "Very concave",
                                   Very_concave = "Very concave",
                                   BULBAR = "Bulbar",
                                   CONCAVE = "Concave",
                                   FLAT = "Flat",
                                   TWISTED = "Twisted",
                                   'VERY CONCAVE' = "Very concave",
                                   VERY_CONCAVE = "Very concave")) %>%
    na_if("") %>% #delete coding episodes with no data
    na.omit
 
 ventr_plane_form_data_b<-ventr_plane_form_data_b[as.numeric(ave(ventr_plane_form_data_b$flake_id, 
                                                                 ventr_plane_form_data_b$flake_id, 
                                                                  FUN=length)) >=6, ]
 
 ventr_plane_form_data_b_krip<- ventr_plane_form_data_b %>%
    mutate(ventr_plane_form_dummy=unclass(ventr_plane_form)) %>% 
    select(-ventr_plane_form) %>%
    spread(analyst_id, ventr_plane_form_dummy) %>%
    select(-flake_id) %>%
    as.matrix()
 
 set.seed(42)
 fit.full_ventr_plane_form<-krippendorffs.alpha(ventr_plane_form_data_b_krip, 
                                          level = "nominal", 
                                          control = list(parallel = FALSE,bootit=100), 
                                          verbose = TRUE)
 
 summary(fit.full_ventr_plane_form)
 
 plot(fit.full_ventr_plane_form, xlim = c(0, 0.3), 
      xlab = "Bootstrap Estimates", 
      main = "Nominal Data",
      density = FALSE)
 
 # Compute kapa-allows us to see which categories are performing better
 ventr_plane_form_fleiss<- ventr_plane_form_data_b %>%
    spread(analyst_id, ventr_plane_form) %>%
    select(-flake_id)
 
 kappam.fleiss(ventr_plane_form_fleiss, detail = T)
 
## Section
 
 section_data_b<-comsafrica_data_cat_data_condb %>%
    select(flake_id,analyst_id,section) %>%
    mutate(section=as.factor((section)),
           section=recode(section, 
                                   DOMED = "Domed",
                                   INDET = "Indeterminate",
                                   LENTIC = "Lenticular",
                                   LENTICULAR = "Lenticular",
                                   RIGHTTRI = "Righttriangle",
                                   RIGHTTRIANGLE = "Righttriangle",
                                   TRAP = "Trapezoidal",
                                   TRAPEZOIDAL = "Trapezoidal",
                                   TRI = "Triangular",
                                   TRIANGULAR = "Triangular")) %>%
    na_if("") %>% #delete coding episodes with no data
    na.omit
 
 section_data_b<-section_data_b[as.numeric(ave(section_data_b$flake_id, 
                                               section_data_b$flake_id, 
                                               FUN=length)) >=6, ]
 
 section_data_b_krip<- section_data_b %>%
    mutate(section_dummy=unclass(section)) %>% 
    select(-section) %>%
    spread(analyst_id, section_dummy) %>%
    select(-flake_id) %>%
    as.matrix()
 
 set.seed(42)
 fit.full_section<-krippendorffs.alpha(section_data_b_krip, 
                                                level = "nominal", 
                                                control = list(parallel = FALSE,bootit=100), 
                                                verbose = TRUE)
 
 summary(fit.full_section)
 
 plot(fit.full_section, xlim = c(0.2, 0.5), 
      xlab = "Bootstrap Estimates", 
      main = "Nominal Data",
      density = FALSE)
 
 # Compute kapa-allows us to see which categories are performing better
 section_fleiss<- section_data_b %>%
    spread(analyst_id, section) %>%
    select(-flake_id)
 
 kappam.fleiss(section_fleiss, detail = T)
 
 ## Lateral edge type
 
 latedge_data_b<-comsafrica_data_cat_data_condb %>%
    select(flake_id,analyst_id,latedgetype) %>%
    mutate(latedgetype=as.factor(latedgetype),
           latedgetype=recode(latedgetype, 
                           AMORPH = "Amorphous",
                          CONV = "Convergent",
                          CONVERGENT = "Convergent",
                          DIAMOND = "Diamond",
                          DIV = "Divergent",
                          DIVERGENT = "Divergent",
                          INDET = "Indeterminate",
                          na = "Indeterminate",
                          OVAL = "Ovoid",
                          OVOID = "Ovoid",
                          PARALLEL = "Parallel")) %>%
    na_if("") %>% #delete coding episodes with no data
    na.omit
 
 latedge_data_b<-latedge_data_b[as.numeric(ave(latedge_data_b$flake_id, 
                                               latedge_data_b$flake_id, 
                                               FUN=length)) >=6, ]
 
 latedge_data_b_krip<- latedge_data_b %>%
    mutate(latedge_dummy=unclass(latedgetype)) %>% 
    select(-latedgetype) %>%
    spread(analyst_id, latedge_dummy) %>%
    select(-flake_id) %>%
    as.matrix()
 
 set.seed(42)
 fit.full_latedge<-krippendorffs.alpha(latedge_data_b_krip, 
                                       level = "nominal", 
                                       control = list(parallel = FALSE,bootit=100), 
                                       verbose = TRUE)
 
 summary(fit.full_latedge)
 
 plot(fit.full_latedge, xlim = c(0.2, 0.5), 
      xlab = "Bootstrap Estimates", 
      main = "Nominal Data",
      density = FALSE)
 
 # Compute kapa-allows us to see which categories are performing better
 latedge_fleiss<- latedge_data_b %>%
    spread(analyst_id, latedgetype) %>%
    select(-flake_id)
 
 kappam.fleiss(latedge_fleiss, detail = T)
 
## Flake termination
 
 flaketerm_data_b<-comsafrica_data_cat_data_condb %>%
    select(flake_id,analyst_id,flaketerm) %>%
    mutate(flaketerm=as.factor(flaketerm),
           flaketerm=recode(flaketerm, 
                              FEATHER = "Feather",
                              HINGE = "Hinge",
                              INDET = "Indeterminate",
                              OVERSHOT = "Overshot")) %>%
    na_if("") %>% #delete coding episodes with no data
    na.omit
 
 flaketerm_data_b<-flaketerm_data_b[as.numeric(ave(flaketerm_data_b$flake_id, 
                                                   flaketerm_data_b$flake_id, 
                                               FUN=length)) >=6, ]
 
 flaketerm_data_b_krip<- flaketerm_data_b %>%
    mutate(flaketerm_dummy=unclass(flaketerm)) %>% 
    select(-flaketerm) %>%
    spread(analyst_id, flaketerm_dummy) %>%
    select(-flake_id) %>%
    as.matrix()
 
 set.seed(42)
 fit.full_flaketerm<-krippendorffs.alpha(flaketerm_data_b_krip, 
                                       level = "nominal", 
                                       control = list(parallel = FALSE,bootit=100), 
                                       verbose = TRUE)
 
 summary(fit.full_flaketerm)
 
 plot(fit.full_flaketerm, xlim = c(0, 0.6), 
      xlab = "Bootstrap Estimates", 
      main = "Nominal Data",
      density = FALSE)
 
 # Compute kapa-allows us to see which categories are performing better
 flaketerm_fleiss<- flaketerm_data_b %>%
    spread(analyst_id, flaketerm) %>%
    select(-flake_id)
 
 kappam.fleiss(flaketerm_fleiss, detail = T)
 
## Kombewa
 
 kombewa_data_b<-comsafrica_data_cat_data_condb %>%
    select(flake_id,analyst_id,kombewa) %>%
    mutate(kombewa=as.factor(kombewa),
           kombewa=recode(kombewa, 
                            NO = "No",
                            YES = "Yes",
                          '0.84'="Indet")) %>%
    na_if("") %>% #delete coding episodes with no data
    na.omit
 
 kombewa_data_b<-kombewa_data_b[as.numeric(ave(kombewa_data_b$flake_id, 
                                               kombewa_data_b$flake_id, 
                                                   FUN=length)) > 4, ]
 
 kombewa_data_b_krip<- kombewa_data_b %>%
    mutate(kombewa_dummy=unclass(kombewa)) %>% 
    select(-kombewa) %>%
    spread(analyst_id, kombewa_dummy) %>%
    select(-flake_id) %>%
    as.matrix()
 
 set.seed(42)
 fit.full_kombewa<-krippendorffs.alpha(kombewa_data_b_krip, 
                                         level = "nominal", 
                                         control = list(parallel = FALSE,bootit=100), 
                                         verbose = TRUE)
 
 summary(fit.full_kombewa)
 
 plot(fit.full_kombewa, xlim = c(0, 1), 
      xlab = "Bootstrap Estimates", 
      main = "Nominal Data",
      density = FALSE)
 
 # Compute kapa-allows us to see which categories are performing better
 kombewa_fleiss<- kombewa_data_b %>%
    spread(analyst_id, kombewa) %>%
    select(-flake_id)
 
 kappam.fleiss(kombewa_fleiss, detail = T)
 
## Distal plan form
 
 distplanform_data_b<-comsafrica_data_cat_data_condb %>%
    select(flake_id,analyst_id,distplanform) %>%
    mutate(distplanform=as.factor(distplanform),
           distplanform=recode(distplanform, 
                               FLAT = "Flat",
                               INDET = "Indeterminate",
                               INDETERMINATE = "Indeterminate",
                               IRR = "Irregular",
                               Irreg = "Irregular",
                               POINTED = "Pointed",
                               rounded = "Rounded",
                               ROUNDED = "Rounded")) %>%
    na_if("") %>% #delete coding episodes with no data
    na.omit
 
 distplanform_data_b<-distplanform_data_b[as.numeric(ave(distplanform_data_b$flake_id, 
                                                         distplanform_data_b$flake_id, 
                                                         FUN=length)) > 4, ]
 
 distplanform_data_b_krip<- distplanform_data_b %>%
    mutate(distplanform_dummy=unclass(distplanform)) %>% 
    select(-distplanform) %>%
    spread(analyst_id, distplanform_dummy) %>%
    select(-flake_id) %>%
    as.matrix()
 
 set.seed(42)
 fit.full_distplanform<-krippendorffs.alpha(distplanform_data_b_krip, 
                                            level = "nominal", 
                                            control = list(parallel = FALSE,bootit=100), 
                                            verbose = TRUE)
 
 summary(fit.full_distplanform)
 
 plot(fit.full_distplanform, xlim = c(0.1, 0.5), 
      xlab = "Bootstrap Estimates", 
      main = "Nominal Data",
      density = FALSE)
 
 # Compute kapa-allows us to see which categories are performing better
 distplanform_fleiss<- distplanform_data_b %>%
    spread(analyst_id, distplanform) %>%
    select(-flake_id)
 
 kappam.fleiss(distplanform_fleiss, detail = T)
 
#### Testing Condition A vs. Condition B #####

comsafrica_data_complete_summary<-comsafrica_data_complete %>%
      group_by(flake_id) %>%
      summarise_at(c("mass","maximumdimension","maximumwidth","maximumthickness",
                     "techlength","techmaxwidth","techmaxthickness","techwidthprox","techwidthmes",
                     "techwidthdist","techthickprox","techthickmes","techthickdist","platfwidth",
                     "platfthickimpact","platfthickmid","platfthickmax","edgeplatf"),mean)

#### Bland altman plots ####

library(dplyr)
library(ggplot2)

pine_df <- Loblolly

sample_df <- data.frame(sample_n(pine_df, size = nrow(pine_df) * 0.5) %>%
                              select(height) %>% 
                              arrange(desc(height)), 
                        sample_n(pine_df, size = nrow(pine_df) * 0.5) %>%
                              select(height) %>%
                              arrange(desc(height)))
names(sample_df) <- c("Sample_1", "Sample_2")

sample_df$Avg <- (sample_df$Sample_1 + sample_df$Sample_2) / 2
sample_df$Dif <- sample_df$Sample_1 - sample_df$Sample_2

ggplot(sample_df, aes(x = Avg, y = Dif)) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = mean(sample_df$Dif), colour = "blue", size = 0.5) +
      geom_hline(yintercept = mean(sample_df$Dif) - (1.96 * sd(sample_df$Dif)), colour = "red", size = 0.5) +
      geom_hline(yintercept = mean(sample_df$Dif) + (1.96 * sd(sample_df$Dif)), colour = "red", size = 0.5) +
      ylab("Diff. Between Measures") +
      xlab("Average Measure")

library(tidyr)

wide_comsafrica<- comsafrica_data_complete_condb %>% select(analyst_id,maximumdimension) %>% spread(analyst_id, maximumdimension) %>%
      na.omit() %>%
      mutate(Avg=(`0202a`+`034b1`+`9d812`+db727+r42o8)/5)

# OR
library(BlandAltmanLeh)
A <- c(-0.358, 0.788, 1.23, -0.338, -0.789, -0.255, 0.645, 0.506, 
       0.774, -0.511, -0.517, -0.391, 0.681, -2.037, 2.019, -0.447, 
       0.122, -0.412, 1.273, -2.165)
B <- c(0.121, 1.322, 1.929, -0.339, -0.515, -0.029, 1.322, 0.951, 
       0.799, -0.306, -0.158, 0.144, 1.132, -0.675, 2.534, -0.398, 0.537, 
       0.173, 1.508, -1.955)
C <- c(0.121, 1.322, 1.929, -0.339, -0.515, -0.029, 1.322, 0.951, 
       0.799, -0.306, -0.158, 0.144, 1.132, -0.675, 2.534, -0.398, 0.537, 
       0.173, 1.508, -1.955)

bland.altman.plot(A, B, main="This is a Bland Altman Plot", xlab="Means", ylab="Differences")

#OR ggplot

pl <- bland.altman.plot(A, B, graph.sys = "ggplot2")
print(pl)

