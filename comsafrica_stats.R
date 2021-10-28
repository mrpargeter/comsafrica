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

############################## 
# Set working directory and load datafile
##############################

# Set to source directory with relevant .csv datafile provided
# through the Open Science repository
getwd()

## Add datasets

comsafrica_data<-read.csv("comsafrica_complete_adjusted.csv")
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

## Condition b
# cortex-NOT SURE WHAT TO DO WITH CORTEX-STRANGE DISTRIBUTION?
hist(log(comsafrica_data_complete_condb$dorsal_cortex))
comsafrica_cortex_boot<-rpt(maximumdimension ~ (1 | flake_id), 
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
comsafrica_mass<-rpt(mass ~ (1 | flake_id), 
                       grname = "flake_id", 
                       data = comsafrica_data_complete_condb, 
                       datatype = "Gaussian", 
                       nboot = 1000, npermut = 100)
summary(comsafrica_mass)
plot(comsafrica_mass, type = "boot", cex.main = 0.8,main="mass")

#flake width
comsafrica_maxwidth<-rpt(maximumwidth ~ (1 | flake_id), 
                     grname = "flake_id", 
                     data = comsafrica_data_complete_condb, 
                     datatype = "Gaussian", 
                     nboot = 1000, npermut = 100)
summary(comsafrica_maxwidth)
plot(comsafrica_maxwidth, type = "boot", cex.main = 0.8,main="max width")

#flake max thickness
comsafrica_maxthick<-rpt(maximumthickness ~ (1 | flake_id), 
                         grname = "flake_id", 
                         data = comsafrica_data_complete_condb, 
                         datatype = "Gaussian", 
                         nboot = 1000, npermut = 100)
plot(comsafrica_maxthick, type = "boot", cex.main = 0.8,main="max thickness")

#flake tech length
comsafrica_techlength<-rpt(techlength ~ (1 | flake_id), 
                         grname = "flake_id", 
                         data = comsafrica_data_complete_condb, 
                         datatype = "Gaussian", 
                         nboot = 1000, npermut = 100)
plot(comsafrica_techlength, type = "boot", cex.main = 0.8,main="tech length")

#flake tech max width
comsafrica_techmaxwidth<-rpt(techmaxwidth ~ (1 | flake_id), 
                           grname = "flake_id", 
                           data = comsafrica_data_complete_condb, 
                           datatype = "Gaussian", 
                           nboot = 1000, npermut = 100)
plot(comsafrica_techmaxwidth, type = "boot", cex.main = 0.8,main="tech max width")

#flake tech max thickness
comsafrica_techmaxthick<-rpt(techmaxthickness ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_condb, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 100)
plot(comsafrica_techmaxthick, type = "boot", cex.main = 0.8,main="tech max thickness")

#flake tech width prox
comsafrica_techwidthprox<-rpt(techwidthprox ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_condb, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 100)
plot(comsafrica_techwidthprox, type = "boot", cex.main = 0.8,main="tech width proximal")

#flake tech width mes
comsafrica_techwidthmes<-rpt(techwidthmes ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_condb, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 100)
plot(comsafrica_techwidthmes, type = "boot", cex.main = 0.8,main="tech width medial")

#flake tech width dist
comsafrica_techwidthdist<-rpt(techwidthdist ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_condb, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 100)
plot(comsafrica_techwidthdist, type = "boot", cex.main = 0.8,main="tech width dist")

#flake tech thick prox
comsafrica_techtechthickprox<-rpt(techthickprox ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_condb, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 100)
plot(comsafrica_techtechthickprox, type = "boot", cex.main = 0.8,main="tech thick prox")

#flake tech thick med
comsafrica_techtechthickmes<-rpt(techthickmes ~ (1 | flake_id), 
                                  grname = "flake_id", 
                                  data = comsafrica_data_complete_condb, 
                                  datatype = "Gaussian", 
                                  nboot = 1000, npermut = 100)
plot(comsafrica_techtechthickmes, type = "boot", cex.main = 0.8,main="tech thick medial")

#flake tech thick dist
comsafrica_techthickdist<-rpt(techthickdist ~ (1 | flake_id), 
                                 grname = "flake_id", 
                                 data = comsafrica_data_complete_condb, 
                                 datatype = "Gaussian", 
                                 nboot = 1000, npermut = 100)
plot(comsafrica_techthickdist, type = "boot", cex.main = 0.8,main="tech thick dist")

#flake platform width
comsafrica_platfwidth<-rpt(platfwidth ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_condb, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 100)
plot(comsafrica_platfwidth, type = "boot", cex.main = 0.8,main="platform width")

#flake platform thickness
comsafrica_platfthickmax<-rpt(platfthickmax ~ (1 | flake_id), 
                           grname = "flake_id", 
                           data = comsafrica_data_complete_condb, 
                           datatype = "Gaussian", 
                           nboot = 1000, npermut = 100)
plot(comsafrica_platfthickmax, type = "boot", cex.main = 0.8,main="platform thickness")

#flake EPA
comsafrica_edgeplatf<-rpt(edgeplatf ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_condb, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 100)
plot(comsafrica_edgeplatf, type = "boot", cex.main = 0.8,main="EPA")

## Condition a ###

# cortex-NOT SURE WHAT TO DO WITH CORTEX-STRANGE DISTRIBUTION?
hist(log(comsafrica_data_complete_conda$dorsal_cortex))
comsafrica_cortex_boot<-rpt(maximumdimension ~ (1 | flake_id), 
                            grname = "flake_id", 
                            data = comsafrica_data_complete_conda, 
                            datatype = "Gaussian", 
                            nboot = 1000, npermut = 100)
summary(comsafrica_cortex_boot)
summary(comsafrica_cortex_boot$mod)
plot(comsafrica_cortex_boot, type = "boot", cex.main = 0.8,main="maximum dimension")
plot(comsafrica_cortex_boot, type = "permut", cex.main = 1)

# maxdim
comsafrica_maxdim_boot<-rpt(maximumdimension ~ (1 | flake_id), 
                            grname = "flake_id", 
                            data = comsafrica_data_complete_conda, 
                            datatype = "Gaussian", 
                            nboot = 1000, npermut = 100)
plot(comsafrica_maxdim_boot, type = "boot", cex.main = 0.8,main="maximum dimension")

#mass
comsafrica_mass<-rpt(mass ~ (1 | flake_id), 
                     grname = "flake_id", 
                     data = comsafrica_data_complete_conda, 
                     datatype = "Gaussian", 
                     nboot = 1000, npermut = 100)
plot(comsafrica_mass, type = "boot", cex.main = 0.8,main="mass")

#flake width
comsafrica_maxwidth<-rpt(maximumwidth ~ (1 | flake_id), 
                         grname = "flake_id", 
                         data = comsafrica_data_complete_conda, 
                         datatype = "Gaussian", 
                         nboot = 1000, npermut = 100)
plot(comsafrica_maxwidth, type = "boot", cex.main = 0.8,main="max width")

#flake max thickness
comsafrica_maxthick<-rpt(maximumthickness ~ (1 | flake_id), 
                         grname = "flake_id", 
                         data = comsafrica_data_complete_conda, 
                         datatype = "Gaussian", 
                         nboot = 1000, npermut = 100)
plot(comsafrica_maxthick, type = "boot", cex.main = 0.8,main="max thickness")

#flake tech length
comsafrica_techlength<-rpt(techlength ~ (1 | flake_id), 
                           grname = "flake_id", 
                           data = comsafrica_data_complete_conda, 
                           datatype = "Gaussian", 
                           nboot = 1000, npermut = 100)
plot(comsafrica_techlength, type = "boot", cex.main = 0.8,main="tech length")

#flake tech max width
comsafrica_techmaxwidth<-rpt(techmaxwidth ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_conda, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 100)
plot(comsafrica_techmaxwidth, type = "boot", cex.main = 0.8,main="tech max width")

#flake tech max thickness
comsafrica_techmaxthick<-rpt(techmaxthickness ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_conda, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 0)
plot(comsafrica_techmaxthick, type = "boot", cex.main = 0.8,main="tech max thickness")

#flake tech width prox
comsafrica_techwidthprox<-rpt(techwidthprox ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_conda, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
plot(comsafrica_techwidthprox, type = "boot", cex.main = 0.8,main="tech width proximal")

#flake tech width mes
comsafrica_techwidthmes<-rpt(techwidthmes ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_conda, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 0)
plot(comsafrica_techwidthmes, type = "boot", cex.main = 0.8,main="tech width medial")

#flake tech width dist
comsafrica_techwidthdist<-rpt(techwidthdist ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_conda, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
plot(comsafrica_techwidthdist, type = "boot", cex.main = 0.8,main="tech width dist")

#flake tech thick prox
comsafrica_techtechthickprox<-rpt(techthickprox ~ (1 | flake_id), 
                                  grname = "flake_id", 
                                  data = comsafrica_data_complete_conda, 
                                  datatype = "Gaussian", 
                                  nboot = 1000, npermut = 0)
plot(comsafrica_techtechthickprox, type = "boot", cex.main = 0.8,main="tech thick prox")

#flake tech thick med
comsafrica_techtechthickmes<-rpt(techthickmes ~ (1 | flake_id), 
                                 grname = "flake_id", 
                                 data = comsafrica_data_complete_conda, 
                                 datatype = "Gaussian", 
                                 nboot = 1000, npermut = 0)
plot(comsafrica_techtechthickmes, type = "boot", cex.main = 0.8,main="tech thick medial")

#flake tech thick dist
comsafrica_techthickdist<-rpt(techthickdist ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_conda, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
plot(comsafrica_techthickdist, type = "boot", cex.main = 0.8,main="tech thick dist")

#flake platform width
comsafrica_platfwidth<-rpt(platfwidth ~ (1 | flake_id), 
                           grname = "flake_id", 
                           data = comsafrica_data_complete_conda, 
                           datatype = "Gaussian", 
                           nboot = 1000, npermut = 0)
plot(comsafrica_platfwidth, type = "boot", cex.main = 0.8,main="platform width")

#flake platform thickness
comsafrica_platfthickmax<-rpt(platfthickmax ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_conda, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
plot(comsafrica_platfthickmax, type = "boot", cex.main = 0.8,main="platform thickness")

#flake EPA
comsafrica_edgeplatf<-rpt(edgeplatf ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_conda, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
plot(comsafrica_edgeplatf, type = "boot", cex.main = 0.8,main="EPA")

### repeatability coefficients for Count data ####
comsafrica_data_count_data<-comsafrica_data %>%
   select(c(assemblage_code,analyst_id,flake_id,proximal_scars,left_scars,distal_scars,right_scars,
            dorsal_scar_count))

comsafrica_data_count_data_condb<-comsafrica_data_count_data %>%
   subset(assemblage_code=="chert_condition_B")

comsafrica_data_count_data_conda<-comsafrica_data_count_data %>%
   subset(assemblage_code=="chert_condition_A") 

## Condition b
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
            distplanform,kombewa))

comsafrica_data_cat_data_condb<-comsafrica_data_cat_data %>%
   subset(assemblage_code=="chert_condition_B")

comsafrica_data_cat_data_conda<-comsafrica_data_cat_data %>%
   subset(assemblage_code=="chert_condition_A") 

## Condition b
# completeness
 completeness_data_b<-comsafrica_data_cat_data_condb %>%
    select(flake_id,analyst_id,completeness) %>%
    mutate(completeness=as.factor(completeness),
       completeness_new=unclass(completeness)) %>%
    select(-completeness) %>% 
    spread(analyst_id, completeness_new) %>%
    select(-flake_id)
 
 completeness_data_b_matrix<-as.matrix(completeness_data_b)
 
 set.seed(42)
 fit.full<-krippendorffs.alpha(completeness_data_b_matrix, 
                               level = "nominal", 
                               control = list(parallel = FALSE,bootit=100), 
                               verbose = TRUE)
 summary(fit.full)
 
 plot(fit.full, xlim = c(0, 1), 
      xlab = "Bootstrap Estimates", 
      main = "Nominal Data",
      density = FALSE)
 
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

