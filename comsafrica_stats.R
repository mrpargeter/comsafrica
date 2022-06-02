#############################################################
#  Rcode Compendium for CoMSAfrica Geneva meetings
#############################################################

#######################
# R session information
#######################

sessionInfo()

#R version 4.0.3 (2020-10-10)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS Big Sur 10.16

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     

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
library(car)
library(dplyr)
library(rptR)
library(krippendorffsalpha)
library(tidyverse)
library(irr)
library(EnvStats)
library(naniar)
library(irrCAC)
library(stringr)
library(ggrepel)
library(stringr)
library(formattable)
library(broom)
library(ggforce)
library(gmodels)

##############################
# Set working directory and load datafile
##############################

# Set to source directory with relevant .csv datafile provided
# through the Open Science repository

setwd("/Volumes/GoogleDrive/My Drive/Projects/CoMSAfrica/Geneva/Data analysis//CoMSA_stats/comsafrica")

## Add datasets

comsafrica_data<-read.csv("comsafrica_complete_adjusted.csv",stringsAsFactors=TRUE) %>%
   subset(!flake_id == "NONR3495") #filter out this non numbered flake
individuals<-read.csv("individual_experience.csv",stringsAsFactors=TRUE) %>%
   rename(analyst_id=Analyst)
categorical_images<-read.csv("images_irr_categorical.csv",stringsAsFactors=TRUE)
continuous_images<-read.csv("images_irr_measurements_count.csv",stringsAsFactors=TRUE)

####################################
# Data cleaning
##################################

# change column names to lower case
colnames(comsafrica_data) <- tolower(colnames(comsafrica_data))

## clean categorical variables
comsafrica_data<-comsafrica_data %>%
   mutate(completeness=recode(completeness, 
                              fragment="Indeterminate",
                              shatter="Indeterminate",
                              lateral="Indeterminate",
                              indeterminate="Indeterminate",
                              Indet="Indeterminate",
                              other="Indeterminate"),
          red_syst=recode(red_syst,Discoidal = "Discoid",idnet = "Indet",inde = "Indet",
                          Indeterminate = "Indet",other = "Indet",Ind = "Indet",
                          indet = "Indet",Indet = "Indet",INDET = "Indet",'indeterminate (broken)' = "Indet",
                          'Indeterminate (broken)' = "Indet",indeterminate = "Indet",laminar = "Laminar",
                          'Levallois (pref)' = "Levallois",'Lev or Disc' = "Indet",'Levallois indet' = "Levallois",
                          'Levallois non-Nubian' = "Levallois",'levallois non nubian' = "Levallois",
                          'LEVALLOIS OR DISCOID' = "Levallois",'LEVALLOIS/LEVALLOIS-RELATED' = "Levallois",
                          na = "Indet",'NON-LEVALLOIS; ORTHOGONAL VOLUME EXPLOITATION' = "Indet",
                          none = "Indet",Nubian = "Levallois",'ON ANVIL' = "Bipolar",
                          'Platform (laminar?)' = "Platform",'Platform / Laminar' = "Platform",
                          'possible Levallois non-Nubian' = "Levallois",'Other (informal)' = "Indet",
                          'potential levallois' = "Levallois",levallois = "Levallois",
                          LEVALLOIS = "Levallois",'Levallois non Nubian' = "Levallois",'Levallois?' = "Levallois",
                          'potential Lev' = "Levallois",'potential Levallois' = "Levallois",
                          CENTRIPETAL = "Discoid",discoid = "Discoid",DISCOID = "Discoid",
                          'Discoid?' = "Discoid",'Core Edge Flake' = "Indet",FLAKE = "Flake",bipolar = "Bipolar"),
          red_syst=na_if(red_syst, "Indet"),                                                                    #replace indet by NAs
          platform_cortex=recode(platform_cortex, INDET = "Indeterminate",
                                 complete = "Complete", absent = "Absent"),
          platform_cortex=na_if(platform_cortex, "Indeterminate"),
          flk_form=recode(flk_form,BLADE = "Blade",CONVFLAKE = "Convflake",ELONG = "Blade",
                          Elong = "Blade",flake = "Flake",FLAKE = "Flake",INDET="Indeterminate"),
          flk_form=na_if(flk_form, "Indeterminate"), 
          directionality=recode(directionality,centripetal = "Centripetal",other = "Other"),
          platfmorph=recode(platfmorph,'Chapeau de Gendarme' = "ChapeauDeGendarme",linear = "Linear",
                            Diherdral = "Dihedral",facetted = "Facetted"),
          platflipp=recode_factor(platflipp, YES = "yes", Yes='yes',NO = "no", No="no", 'NOT APPLICABLE' = "Indeterminate"),
          platflipp=na_if(platflipp, "Indeterminate"),
          bulb=recode(bulb, YES = "yes", Yes="yes",NO = "no",No="no",Indet="Indeterminate"),
          bulb=na_if(bulb, "Indeterminate"),
          shattbulb=recode(shattbulb,Indet = "Indeterminate",Indeterminateerminate = "Indeterminate",
                           NO = "No",no="No",YES = "Yes"),
          initiation=recode(initiation,BENDING = "Bending",HERTZIAN = "Hertzian",hertzian = "Hertzian",
                            WEDGING = "Wedging"),
          initiation=na_if(initiation, "INDET"),
          ventr_plane_form=recode(ventr_plane_form,very_concave = "Very concave",Very_concave = "Very concave",
                                  BULBAR = "Bulbar",CONCAVE = "Concave",FLAT = "Flat",
                                  TWISTED = "Twisted",'VERY CONCAVE' = "Very concave",VERY_CONCAVE = "Very concave"),
          section=recode(section,DOMED = "Domed",INDET = "Indeterminate",LENTIC = "Lenticular",
                         LENTICULAR = "Lenticular",RIGHTTRI = "Righttriangle",RIGHTTRIANGLE = "Righttriangle",FLAT="Lenticular",
                         TRAP = "Trapezoidal",TRAPEZOIDAL = "Trapezoidal",TRI = "Triangular",TRIANGULAR = "Triangular"),
          section=na_if(section, "Indeterminate"),
          latedgetype=recode(latedgetype,AMORPH = "Amorphous",CONV = "Convergent",CONVERGENT = "Convergent",SQUARE = "Square",
                             'SUB-PARALLEL' = "Parallel",DIAMOND = "Diamond",DIV = "Divergent",DIVERGENT = "Divergent",
                             OVAL = "Ovoid",OVOID = "Ovoid",PARALLEL = "Parallel", INDET = "Indeterminate"),
          latedgetype=na_if(latedgetype, "na"),
          latedgetype=na_if(latedgetype, "Indeterminate"),
          flaketerm=recode(flaketerm,FEATHER = "Feather",HINGE = "Hinge",INDET = "Indeterminate",
                           OVERSHOT = "Overshot",AXIAL= "Axial",CRUSHED = "Crushed"),
          flaketerm=na_if(flaketerm, "Indeterminate"),
          kombewa=recode(kombewa,NO = "No",YES = "Yes",no="No", Indet="Indeterminate"),
          distplanform=recode(distplanform,FLAT = "Flat",INDET = "Indeterminate",INDETERMINATE = "Indeterminate",CONCAVE="Indeterminate",
                              IRR = "Irregular",Irreg = "Irregular",POINTED = "Pointed",rounded = "Rounded",ROUNDED = "Rounded"),
          distplanform=na_if(distplanform, "Indeterminate"))%>% 
   map_df(~ na_if(.x, "")) %>% 
   droplevels

#replacement of NA values by 'Indeterminate' for specific variables where NAs = indet
comsafrica_data$completeness <- fct_explicit_na(comsafrica_data$completeness, na_level = "Indeterminate") 
comsafrica_data$kombewa <- fct_explicit_na(comsafrica_data$kombewa, na_level = "Indeterminate") 

#summary of qualitative variables + explanations of changes above

summary(comsafrica_data$completeness)# I grouped together other and indeterminate too as it seems that analysts
#used one or the other, but rarely both
summary(comsafrica_data$red_syst) 
summary(comsafrica_data$platform_cortex) #indet used for distal fragments as well (as na), so grouped with NAs
summary(comsafrica_data$flk_form) #indet removed, not an option in the E4 file
summary(comsafrica_data$directionality) #indet and other kept as used by same analysts. NAs correspond to when it was not possible to record data (100% cortex) and some human errors
summary(comsafrica_data$platfmorph) #other and indet used by same analysts. Nas correspond to mostly fragments and a few human errors
summary(comsafrica_data$platflipp) #indet exclusively produced by E4 users while NAs correspond to fragments as well as likely indet from non-E4 users (as they are associated with 
#a determined platf) --> indet to be removed from the analysis.  
summary(comsafrica_data$bulb) #indet are also present when medial fragment so indet is also used as NAs. removed from analysis
summary(comsafrica_data$initiation) #only one INDET, removed from the analysis --> NA
summary(comsafrica_data$ventr_plane_form)
summary(comsafrica_data$section) #flat doesn't exist in E4, grouped with Lenticular; indeterminate can be NA or indet, so replaced by NA
summary(comsafrica_data$latedgetype) #indet is mostly na with a few true indet, so replaced by NA
summary(comsafrica_data$flaketerm) #indet can be both na and indet, so replaced by NA
summary(comsafrica_data$kombewa) #indet are indet and nas are indet with maybe few NAs, I think we can replace NAs by indet.
summary(comsafrica_data$distplanform) #concave does not exist --> indeterminate; indet seems to correspond to NAs (prox or medial fragments?) so grouped with NAs

## cleaning quantitative data##
summary(comsafrica_data$maximumdimension)
comsafrica_data$maximumdimension[comsafrica_data$maximumdimension==0] <- NA

summary(comsafrica_data$mass)
comsafrica_data$mass[comsafrica_data$mass==0] <- NA

summary(comsafrica_data$maximumwidth)
comsafrica_data$maximumwidth[comsafrica_data$maximumwidth==0] <- NA

summary(comsafrica_data$maximumthickness)
comsafrica_data$maximumthickness[comsafrica_data$maximumthickness==0] <- NA

summary(comsafrica_data$techlength)
comsafrica_data$techlength[comsafrica_data$techlength==0] <- NA

summary(comsafrica_data$techmaxwidth)
comsafrica_data$techmaxwidth[comsafrica_data$techmaxwidth==0] <- NA

summary(comsafrica_data$techmaxthickness)
comsafrica_data$techmaxthickness[comsafrica_data$techmaxthickness==0] <- NA

summary(comsafrica_data$techwidthprox)
comsafrica_data$techwidthprox[comsafrica_data$techwidthprox==0] <- NA

summary(comsafrica_data$techwidthmes)
comsafrica_data$techwidthmes[comsafrica_data$techwidthmes==0] <- NA

summary(comsafrica_data$techwidthdist)
comsafrica_data$techwidthdist[comsafrica_data$techwidthdist==0] <- NA

summary(comsafrica_data$techthickprox)
comsafrica_data$techthickprox[comsafrica_data$techthickprox==0] <- NA

summary(comsafrica_data$techthickmes)
comsafrica_data$techthickmes[comsafrica_data$techthickmes==0] <- NA

summary(comsafrica_data$techthickdist)
comsafrica_data$techthickdist[comsafrica_data$techthickdist==0] <- NA

summary(comsafrica_data$platfwidth)
comsafrica_data$platfwidth[comsafrica_data$platfwidth==0] <- NA

summary(comsafrica_data$platfthickmax)
comsafrica_data$platfthickmax[comsafrica_data$platfthickmax==0] <- NA

summary(comsafrica_data$platfthickimpact)
comsafrica_data$platfthickimpact[comsafrica_data$platfthickimpact==0] <- NA

summary(comsafrica_data$platfthickmid)
comsafrica_data$platfthickmid[comsafrica_data$platfthickmid==0] <- NA

summary(comsafrica_data$edgeplatf)
comsafrica_data$edgeplatf[comsafrica_data$edgeplatf==0] <- NA

##Recode flake ID ##

## subset

# renumber flakes to avoid duplicate numbers across technological conditions when running combined IRR analysis
# on all 100 flakes

data.A <- comsafrica_data[which(comsafrica_data$assemblage_code=="chert_condition_A"),]

data.A$new_flake_id <- 0
for(i in 1:length(unique(data.A$flake_id))){
   data.A[which(data.A$flake_id==unique(data.A$flake_id)[i]),"new_flake_id"] <- i
}


data.B <- comsafrica_data[which(comsafrica_data$assemblage_code=="chert_condition_B"),]

count = 51
data.B$new_flake_id <- 0
for(i in 1:length(unique(data.B$flake_id))){
   data.B[which(data.B$flake_id==unique(data.B$flake_id)[i]),"new_flake_id"] <- count
   count = count+1
}

new_comsafrica_data <- rbind(data.A, data.B) %>%
  filter(!new_flake_id %in% c(89,97)) #remove two flakes with fragmentation issues

#adjust cortex values to fix data entry errors
#A10 and B25 have strange values because of misinterpretations
#around cortex

new_comsafrica_data$dorsal_cortex[new_comsafrica_data$new_flake_id == 46
                                       & new_comsafrica_data$analyst_id == "46b96"] <- 100

new_comsafrica_data$dorsal_cortex[new_comsafrica_data$new_flake_id == 31
                                       & new_comsafrica_data$analyst_id == "46b96"] <- NA

new_comsafrica_data$dorsal_cortex[new_comsafrica_data$new_flake_id == 23
                                  & new_comsafrica_data$analyst_id == "46b96"] <- 100

new_comsafrica_data$dorsal_cortex[new_comsafrica_data$new_flake_id == 27
                                  & new_comsafrica_data$analyst_id == "46b96"] <- NA

###Corrections duplicates for analyst r42o8

new_comsafrica_data$maximumthickness[new_comsafrica_data$new_flake_id == 11
                                     & new_comsafrica_data$analyst_id == "r42o8"] <- 17.59
new_comsafrica_data$techlength[new_comsafrica_data$new_flake_id == 11
                               & new_comsafrica_data$analyst_id == "r42o8"] <- 83.46
new_comsafrica_data$techmaxwidth[new_comsafrica_data$new_flake_id == 11
                                 & new_comsafrica_data$analyst_id == "r42o8"] <- 48
new_comsafrica_data$techmaxthickness[new_comsafrica_data$new_flake_id == 11
                                     & new_comsafrica_data$analyst_id == "r42o8"] <- 12.77
new_comsafrica_data$techwidthprox[new_comsafrica_data$new_flake_id == 11
                                  & new_comsafrica_data$analyst_id == "r42o8"] <- NA
new_comsafrica_data$techwidthmes[new_comsafrica_data$new_flake_id == 11
                                 & new_comsafrica_data$analyst_id == "r42o8"] <- NA
new_comsafrica_data$techwidthdist[new_comsafrica_data$new_flake_id == 11
                                  & new_comsafrica_data$analyst_id == "r42o8"] <- NA

new_comsafrica_data$maximumwidth[new_comsafrica_data$new_flake_id == 40
                                 & new_comsafrica_data$analyst_id == "r42o8"] <- 27.05
new_comsafrica_data$maximumthickness[new_comsafrica_data$new_flake_id == 40
                                     & new_comsafrica_data$analyst_id == "r42o8"] <- 7.69
new_comsafrica_data$techlength[new_comsafrica_data$new_flake_id == 40
                               & new_comsafrica_data$analyst_id == "r42o8"] <- 50.42
new_comsafrica_data$techmaxwidth[new_comsafrica_data$new_flake_id == 40
                                 & new_comsafrica_data$analyst_id == "r42o8"] <- NA

new_comsafrica_data$maximumwidth[new_comsafrica_data$new_flake_id == 85
                                 & new_comsafrica_data$analyst_id == "r42o8"] <- 55.01
new_comsafrica_data$maximumthickness[new_comsafrica_data$new_flake_id == 83
                                     & new_comsafrica_data$analyst_id == "r42o8"] <- 12.59
new_comsafrica_data$techlength[new_comsafrica_data$new_flake_id == 83
                               & new_comsafrica_data$analyst_id == "r42o8"] <- 51.39
new_comsafrica_data$techmaxwidth[new_comsafrica_data$new_flake_id == 83
                                 & new_comsafrica_data$analyst_id == "r42o8"] <- NA

new_comsafrica_data$maximumwidth[new_comsafrica_data$new_flake_id == 85
                                 & new_comsafrica_data$analyst_id == "r42o8"] <- 19.93
new_comsafrica_data$maximumthickness[new_comsafrica_data$new_flake_id == 85
                                     & new_comsafrica_data$analyst_id == "r42o8"] <- NA

new_comsafrica_data$techwidthprox[new_comsafrica_data$new_flake_id == 41
                                  & new_comsafrica_data$analyst_id == "r42o8"] <- NA

new_comsafrica_data$techwidthdist[new_comsafrica_data$new_flake_id == 70
                                  & new_comsafrica_data$analyst_id == "r42o8"] <- 38.53
new_comsafrica_data$techthickprox[new_comsafrica_data$new_flake_id == 70
                                  & new_comsafrica_data$analyst_id == "r42o8"] <- 16.75
new_comsafrica_data$techthickmes[new_comsafrica_data$new_flake_id == 70
                                 & new_comsafrica_data$analyst_id == "r42o8"] <- 12.55
new_comsafrica_data$techthickdist[new_comsafrica_data$new_flake_id == 70
                                  & new_comsafrica_data$analyst_id == "r42o8"] <- NA

new_comsafrica_data$techwidthmes[new_comsafrica_data$new_flake_id == 33
                                 & new_comsafrica_data$analyst_id == "r42o8"] <- 41.17
new_comsafrica_data$techwidthdist[new_comsafrica_data$new_flake_id == 33
                                  & new_comsafrica_data$analyst_id == "r42o8"] <- 26.43
new_comsafrica_data$techthickprox[new_comsafrica_data$new_flake_id == 33
                                  & new_comsafrica_data$analyst_id == "r42o8"] <- 9.09
new_comsafrica_data$techthickmes[new_comsafrica_data$new_flake_id == 33
                                 & new_comsafrica_data$analyst_id == "r42o8"] <- 7.77
new_comsafrica_data$techthickdist[new_comsafrica_data$new_flake_id == 33
                                  & new_comsafrica_data$analyst_id == "r42o8"] <- 5.34
new_comsafrica_data$platfwidth[new_comsafrica_data$new_flake_id == 33
                               & new_comsafrica_data$analyst_id == "r42o8"] <- NA

##### Inter rater data analyses

comsafrica_data_complete<-new_comsafrica_data %>%
   select(c(assemblage_code,analyst_id,analysis_order,flake_id,new_flake_id,proximal_scars,left_scars,distal_scars,right_scars,
            dorsal_cortex,mass,maximumdimension,maximumwidth,maximumthickness,techlength,techmaxwidth,techmaxthickness,
            techwidthprox,techwidthmes,techwidthdist,techthickprox,techthickmes,techthickdist,
            platfwidth,platfthickimpact,platfthickmid,platfthickmax,edgeplatf,angle_height))

### repeatability coefficients for continuous (Gaussian) data ####
# first as a whole, then divide into assemblage variants to compare inter-rater variance
# technological strategies
   #### Both assemblages ####

# maxdim
hist(comsafrica_data_complete$maximumdimension)
set.seed(50)
comsafrica_maxdim_boot<-rpt(maximumdimension ~  (1 | assemblage_code)  + (1 | new_flake_id),
                  grname = c("assemblage_code","new_flake_id"),
                  data = comsafrica_data_complete,
                  datatype = "Gaussian",
                  nboot = 1000, npermut = 100)
print(comsafrica_maxdim_boot)
maxdim_irr<-comsafrica_maxdim_boot$R
maxdim_ci<-comsafrica_maxdim_boot$CI_emp

ggplot(comsafrica_data_complete, aes(x=assemblage_code, y=maximumdimension))+
   geom_boxplot()

#mass
set.seed(50)
comsafrica_mass<-rpt(log(mass) ~  (1 | assemblage_code)  + (1 | new_flake_id),
                       grname = c("assemblage_code","new_flake_id"),
                       data = filter(comsafrica_data_complete,mass>0),
                       datatype = "Gaussian",
                       nboot = 1000, npermut = 100)
print(comsafrica_mass)
mass_irr<-comsafrica_mass$R
mass_ci<-comsafrica_mass$CI_emp

#flake width
set.seed(50)
comsafrica_maxwidth<-rpt(maximumwidth ~  (1 | assemblage_code)  + (1 | new_flake_id),
                     grname = c("assemblage_code","new_flake_id"),
                     data = comsafrica_data_complete,
                     datatype = "Gaussian",
                     nboot = 1000, npermut = 100)
print(comsafrica_maxwidth)
maxwidth_irr<-comsafrica_maxwidth$R
maxwidth_ci<-comsafrica_maxwidth$CI_emp

#flake max thickness
set.seed(50)
comsafrica_maxthick<-rpt(maximumthickness ~  (1 | assemblage_code)  + (1 | new_flake_id),
                         grname = c("assemblage_code","new_flake_id"),
                         data = comsafrica_data_complete,
                         datatype = "Gaussian",
                         nboot = 1000, npermut = 100)
print(comsafrica_maxthick)
maxthick_irr<-comsafrica_maxthick$R
maxthick_ci<-comsafrica_maxthick$CI_emp

#flake tech length
set.seed(50)
comsafrica_techlength<-rpt(techlength ~  (1 | assemblage_code)  + (1 | new_flake_id),
                           grname = c("assemblage_code","new_flake_id"),
                           data = comsafrica_data_complete,
                           datatype = "Gaussian",
                           nboot = 1000, npermut = 100)
print(comsafrica_techlength)
maxthick_irr<-comsafrica_maxthick$R
maxthick_ci<-comsafrica_maxthick$CI_emp

#flake tech max width
set.seed(50)
comsafrica_techmaxwidth<-rpt(techmaxwidth ~  (1 | assemblage_code)  + (1 | new_flake_id),
                           grname = c("assemblage_code","new_flake_id"),
                           data = comsafrica_data_complete,
                           datatype = "Gaussian",
                           nboot = 1000, npermut = 100)
print(comsafrica_techmaxwidth)
techmaxwidth_irr<-comsafrica_techmaxwidth$R
techmaxwidth_ci<-comsafrica_techmaxwidth$CI_emp

#flake tech max thickness
set.seed(50)
comsafrica_techmaxthick<-rpt(techmaxthickness ~  (1 | assemblage_code)  + (1 | new_flake_id),
                             grname = c("assemblage_code","new_flake_id"),
                             data = comsafrica_data_complete,
                             datatype = "Gaussian",
                             nboot = 1000, npermut = 100)
print(comsafrica_techmaxthick)
techmaxthick_irr<-comsafrica_techmaxthick$R
techmaxthick_ci<-comsafrica_techmaxthick$CI_emp

#flake tech width prox
set.seed(50)
comsafrica_techwidthprox<-rpt(techwidthprox ~  (1 | assemblage_code)  + (1 | new_flake_id),
                             grname = c("assemblage_code","new_flake_id"),
                             data = comsafrica_data_complete,
                             datatype = "Gaussian",
                             nboot = 1000, npermut = 100)
print(comsafrica_techwidthprox)
techwidthprox_irr<-comsafrica_techwidthprox$R
techwidthprox_ci<-comsafrica_techwidthprox$CI_emp

#flake tech width mes
set.seed(50)
comsafrica_techwidthmes<-rpt(techwidthmes ~  (1 | assemblage_code)  + (1 | new_flake_id),
                              grname = c("assemblage_code","new_flake_id"),
                              data = comsafrica_data_complete,
                              datatype = "Gaussian",
                              nboot = 1000, npermut = 100)
print(comsafrica_techwidthmes)
techwidthmes_irr<-comsafrica_techwidthmes$R
techwidthmes_ci<-comsafrica_techwidthmes$CI_emp

#flake tech width dist
set.seed(50)
comsafrica_techwidthdist<-rpt(techwidthdist ~  (1 | assemblage_code)  + (1 | new_flake_id),
                             grname = c("assemblage_code","new_flake_id"),
                             data = comsafrica_data_complete,
                             datatype = "Gaussian",
                             nboot = 1000, npermut = 100)
print(comsafrica_techwidthdist)
techwidthdist_irr<-comsafrica_techwidthdist$R
techwidthdist_ci<-comsafrica_techwidthdist$CI_emp

#flake tech thick prox
#tech_thick_prox_data<-comsafrica_data_complete %>%
 #  filter(!new_flake_id %in% c(27,68,10))

set.seed(50)
comsafrica_techtechthickprox<-rpt(techthickprox ~  (1 | assemblage_code)  + (1 | new_flake_id),
                              grname = c("assemblage_code","new_flake_id"),
                              data = comsafrica_data_complete,
                              datatype = "Gaussian",
                              nboot = 1000, npermut = 100)
print(comsafrica_techtechthickprox)
techthickprox_irr<-comsafrica_techtechthickprox$R
techthickprox_ci<-comsafrica_techtechthickprox$CI_emp

#flake tech thick med
set.seed(50)
comsafrica_techtechthickmes<-rpt(techthickmes ~  (1 | assemblage_code)  + (1 | new_flake_id),
                                  grname = c("assemblage_code","new_flake_id"),
                                  data = comsafrica_data_complete,
                                  datatype = "Gaussian",
                                  nboot = 1000, npermut = 100)
print(comsafrica_techtechthickmes)
techthickmes_irr<-comsafrica_techtechthickmes$R
techthickmes_ci<-comsafrica_techtechthickmes$CI_emp

#flake tech thick dist
set.seed(50)
comsafrica_techthickdist<-rpt(techthickdist ~  (1 | assemblage_code)  + (1 | new_flake_id),
                                 grname = c("assemblage_code","new_flake_id"),
                                 data = comsafrica_data_complete,
                                 datatype = "Gaussian",
                                 nboot = 1000, npermut = 100)
print(comsafrica_techthickdist)
techthickdist_irr<-comsafrica_techthickdist$R
techthickdist_ci<-comsafrica_techthickdist$CI_emp

#flake platform width
set.seed(50)
comsafrica_platfwidth<-rpt(platfwidth ~  (1 | assemblage_code)  + (1 | new_flake_id),
                              grname = c("assemblage_code","new_flake_id"),
                              data = comsafrica_data_complete,
                              datatype = "Gaussian",
                              nboot = 1000, npermut = 100)
print(comsafrica_platfwidth)
platfwidth_irr<-comsafrica_platfwidth$R
platfwidth_ci<-comsafrica_platfwidth$CI_emp

#flake platform thickness
set.seed(50)
comsafrica_platfthickmax<-rpt(platfthickmax ~  (1 | assemblage_code)  + (1 | new_flake_id),
                              grname = c("assemblage_code","new_flake_id"),
                              data = comsafrica_data_complete,
                              datatype = "Gaussian",
                              nboot = 1000, npermut = 100)
print(comsafrica_platfthickmax)
platfthickmax_irr<-comsafrica_platfthickmax$R
platfthickmax_ci<-comsafrica_platfthickmax$CI_emp

#flake platform thickness impact
set.seed(50)
comsafrica_platfthicimpact<-rpt(platfthickimpact ~  (1 | assemblage_code)  + (1 | new_flake_id),
                           grname = c("assemblage_code","new_flake_id"),
                           data = comsafrica_data_complete,
                           datatype = "Gaussian",
                           nboot = 1000, npermut = 100)
print(comsafrica_platfthicimpact)
platfthickimpact_irr<-comsafrica_platfthicimpact$R
platfthickimpact_ci<-comsafrica_platfthicimpact$CI_emp

#flake platform thickness mid point
set.seed(50)
comsafrica_platfthickmid<-rpt(platfthickmid ~  (1 | assemblage_code)  + (1 | new_flake_id),
                                grname = c("assemblage_code","new_flake_id"),
                                data = comsafrica_data_complete,
                                datatype = "Gaussian",
                                nboot = 1000, npermut = 100)
print(comsafrica_platfthickmid)
platfthickmid_irr<-comsafrica_platfthickmid$R
platfthickmid_ci<-comsafrica_platfthickmid$CI_emp

## compile IRR continuous values into table
irr_continuous_data<-rbind(cortex_irr,maxdim_irr,mass_irr,
                           maxwidth_irr,maxthick_irr,techlength_irr,
                           techmaxwidth_irr,techmaxthick_irr,techwidthprox_irr,
                           techwidthmes_irr,techwidthdist_irr,techthickprox_irr,
                           techthickmes_irr,techthickdist_irr,platfwidth_irr,
                           platfthickmax_irr,platfthickimpact_irr,platfthickmid_irr) %>%
   rownames_to_column()

var_contnames<-c("dorsal_cortex","maximumdimension","mass",
                  "maximumwidth","maximumthickness","techlength",
                  "techmaxwidth","techmaxthick","techwidthprox",
                  "techwidthmes","techwidthdist","techthickprox",
                  "techthickmes","techthickdist","platfwidth",
                  "platfthickmax","platfthickimpact","platfthickmid")

irr_cont_data_merged<-cbind(irr_continuous_data,var_contnames) %>%
   select(-rowname) %>%
   select(c(var_contnames,assemblage_code,new_flake_id)) %>%
   pivot_longer(cols=c("assemblage_code","new_flake_id"),
                names_to = "variable", values_to = "irr")

## compile IRR continuous value CI data into table
irr_cont_cidata<-rbind(cortex_ci,maxdim_ci,mass_ci,
                       maxwidth_ci,maxthick_ci,techlength_ci,
                       techmaxwidth_ci,techmaxthick_ci,techwidthprox_ci,
                       techwidthmes_ci,techwidthdist_ci,techthickprox_ci,
                       techthickmes_ci,techthickdist_ci,platfwidth_ci,
                       platfthickmax_ci,platfthickimpact_ci,platfthickmid_ci) %>%
   rownames_to_column()

var_cicontnames<-c("dorsal_cortex","dorsal_cortex","maximumdimension","maximumdimension",
                   "mass","mass","maximumwidth","maximumwidth",
                   "maximumthickness","maximumthickness","techlength","techlength",
                   "techmaxwidth","techmaxwidth","techmaxthick","techmaxthick","techwidthprox","techwidthprox",
                   "techwidthmes","techwidthmes","techwidthdist","techwidthdist","techthickprox","techthickprox",
                   "techthickmes","techthickmes","techthickdist","techthickdist","platfwidth","platfwidth",
                   "platfthickmax","platfthickmax","platfthickimpact","platfthickimpact","platfthickmid","platfthickmid")

irr_cont_cidata_merged<-cbind(irr_cont_cidata,var_cicontnames) %>%
   rename(variable=rowname,
          var_contnames=var_cicontnames,
          lower='2.5%',
          upper='97.5%') %>%
   mutate(variable = gsub("[[:digit:]]", "", variable))

## merge IRR and CI continuous Assemblage data
irr_cont_data_merged_assemblage<-irr_cont_data_merged %>%
   filter(variable=="assemblage_code")
irr_cont_cidata_merged_assemblage<-irr_cont_cidata_merged %>%
   filter(variable=="assemblage_code")

irr_cont_data_complete_assemblage<-merge(irr_cont_data_merged_assemblage,
                                         irr_cont_cidata_merged_assemblage,
                                          by="var_contnames") %>%
   select(-c(variable.y)) %>%
   select(c(var_contnames,variable.x,irr,upper,lower)) %>%
   mutate(across(where(is.numeric), ~ round(., 4))) %>%
   rename(comparison=variable.x)

## merge IRR and CI  flake ID data
irr_cont_data_merged_flakeid<-irr_cont_data_merged %>%
   filter(variable=="new_flake_id")
irr_cont_cidata_merged_flakeid<-irr_cont_cidata_merged %>%
   filter(variable=="new_flake_id")

irr_cont_data_complete_flakeid<-merge(irr_cont_data_merged_flakeid,
                                      irr_cont_cidata_merged_flakeid,
                                       by="var_contnames") %>%
   select(-c(variable.y)) %>%
   select(c(var_contnames,variable.x,irr,lower,upper)) %>%
   mutate(across(where(is.numeric), ~ round(., 2))) %>%
   rename(comparison=variable.x)

   #### Assemblage A ####

a_data<-filter(comsafrica_data_complete, assemblage_code=="chert_condition_A") 

# maxdim
set.seed(50)
a_comsafrica_maxdim_boot<-rpt(maximumdimension ~  (1 | new_flake_id),
                            grname = ("new_flake_id"),
                            data = a_data,
                            datatype = "Gaussian",
                            nboot = 1000, npermut = 100)
print(a_comsafrica_maxdim_boot)
maxdim_irr_a<-a_comsafrica_maxdim_boot$R
maxdim_ci_a<-a_comsafrica_maxdim_boot$CI_emp

#mass
set.seed(50)
a_comsafrica_mass<-rpt(log(mass) ~  (1 | new_flake_id),
                     grname = ("new_flake_id"),
                     data = filter(a_data,mass>0),
                     datatype = "Gaussian",
                     nboot = 1000, npermut = 100)
print(a_comsafrica_mass)
mass_irr_a<-a_comsafrica_mass$R
mass_ci_a<-a_comsafrica_mass$CI_emp

#flake width
set.seed(50)
a_comsafrica_maxwidth<-rpt(maximumwidth ~  (1 | new_flake_id),
                         grname = ("new_flake_id"),
                         data = a_data,
                         datatype = "Gaussian",
                         nboot = 1000, npermut = 100)
print(a_comsafrica_maxwidth)
maxwidth_irr_a<-a_comsafrica_maxwidth$R
maxwidth_ci_a<-a_comsafrica_maxwidth$CI_emp

#flake max thickness
set.seed(50)
a_comsafrica_maxthick<-rpt(maximumthickness ~  (1 | new_flake_id),
                         grname = ("new_flake_id"),
                         data = a_data,
                         datatype = "Gaussian",
                         nboot = 1000, npermut = 100)
print(a_comsafrica_maxthick)
maxthick_irr_a<-a_comsafrica_maxthick$R
maxthick_ci_a<-a_comsafrica_maxthick$CI_emp

#flake tech length
set.seed(50)
a_comsafrica_techlength<-rpt(techlength ~  (1 | new_flake_id),
                           grname = ("new_flake_id"),
                           data = a_data,
                           datatype = "Gaussian",
                           nboot = 1000, npermut = 100)
print(a_comsafrica_techlength)
techlength_irr_a<-a_comsafrica_techlength$R
techlength_ci_a<-a_comsafrica_techlength$CI_emp

#flake tech max width
set.seed(50)
a_comsafrica_techmaxwidth<-rpt(techmaxwidth ~  (1 | new_flake_id),
                             grname = ("new_flake_id"),
                             data = a_data,
                             datatype = "Gaussian",
                             nboot = 1000, npermut = 100)
print(a_comsafrica_techmaxwidth)
techmaxwidth_irr_a<-a_comsafrica_techmaxwidth$R
techmaxwidth_ci_a<-a_comsafrica_techmaxwidth$CI_emp

#flake tech max thickness
set.seed(50)
a_comsafrica_techmaxthick<-rpt(techmaxthickness ~  (1 | new_flake_id),
                             grname = ("new_flake_id"),
                             data = a_data,
                             datatype = "Gaussian",
                             nboot = 1000, npermut = 100)
print(a_comsafrica_techmaxthick)
techmaxthick_irr_a<-a_comsafrica_techmaxthick$R
techmaxthick_ci_a<-a_comsafrica_techmaxthick$CI_emp

#flake tech width prox
set.seed(50)
a_comsafrica_techwidthprox<-rpt(techwidthprox ~  (1 | new_flake_id),
                              grname = ("new_flake_id"),
                              data = a_data,
                              datatype = "Gaussian",
                              nboot = 1000, npermut = 100)
print(a_comsafrica_techwidthprox)
techwidthprox_irr_a<-a_comsafrica_techwidthprox$R
techwidthprox_ci_a<-a_comsafrica_techwidthprox$CI_emp

#flake tech width mes
set.seed(50)
a_comsafrica_techwidthmes<-rpt(techwidthmes ~  (1 | new_flake_id),
                             grname = ("new_flake_id"),
                             data = a_data,
                             datatype = "Gaussian",
                             nboot = 1000, npermut = 100)
print(a_comsafrica_techwidthmes)
techwidthmes_irr_a<-a_comsafrica_techwidthmes$R
techwidthmes_ci_a<-a_comsafrica_techwidthmes$CI_emp

#flake tech width dist
set.seed(50)
a_comsafrica_techwidthdist<-rpt(techwidthdist ~  (1 | new_flake_id),
                              grname = ("new_flake_id"),
                              data = a_data,
                              datatype = "Gaussian",
                              nboot = 1000, npermut = 100)
print(a_comsafrica_techwidthdist)
techwidthdist_irr_a<-a_comsafrica_techwidthdist$R
techwidthdist_ci_a<-a_comsafrica_techwidthdist$CI_emp

#flake tech thick prox
#tech_thick_prox_data<-a_data %>%
#  filter(!new_flake_id %in% c(27,68,10))

set.seed(50)
a_comsafrica_techtechthickprox<-rpt(techthickprox ~  (1 | new_flake_id),
                                  grname = ("new_flake_id"),
                                  data = a_data,
                                  datatype = "Gaussian",
                                  nboot = 1000, npermut = 100)
print(a_comsafrica_techtechthickprox)
techthickprox_irr_a<-a_comsafrica_techtechthickprox$R
techthickprox_ci_a<-a_comsafrica_techtechthickprox$CI_emp

#flake tech thick med
set.seed(50)
a_comsafrica_techtechthickmes<-rpt(techthickmes ~  (1 | new_flake_id),
                                 grname = ("new_flake_id"),
                                 data = a_data,
                                 datatype = "Gaussian",
                                 nboot = 1000, npermut = 100)
print(a_comsafrica_techtechthickmes)
techthickmes_irr_a<-a_comsafrica_techtechthickmes$R
techthickmes_ci_a<-a_comsafrica_techtechthickmes$CI_emp

#flake tech thick dist
set.seed(50)
a_comsafrica_techthickdist<-rpt(techthickdist ~  (1 | new_flake_id),
                              grname = ("new_flake_id"),
                              data = a_data,
                              datatype = "Gaussian",
                              nboot = 1000, npermut = 100)
print(a_comsafrica_techthickdist)
techthickdist_irr_a<-a_comsafrica_techthickdist$R
techthickdist_ci_a<-a_comsafrica_techthickdist$CI_emp

#flake platform width
set.seed(50)
a_comsafrica_platfwidth<-rpt(platfwidth ~  (1 | new_flake_id),
                           grname = ("new_flake_id"),
                           data = a_data,
                           datatype = "Gaussian",
                           nboot = 1000, npermut = 100)
print(a_comsafrica_platfwidth)
platfwidth_irr_a<-a_comsafrica_platfwidth$R
platfwidth_ci_a<-a_comsafrica_platfwidth$CI_emp

#flake platform thickness
set.seed(50)
a_comsafrica_platfthickmax<-rpt(platfthickmax ~  (1 | new_flake_id),
                              grname = ("new_flake_id"),
                              data = a_data,
                              datatype = "Gaussian",
                              nboot = 1000, npermut = 100)
print(a_comsafrica_platfthickmax)
platfthickmax_irr_a<-a_comsafrica_platfthickmax$R
platfthickmax_ci_a<-a_comsafrica_platfthickmax$CI_emp

#flake platform thickness impact
set.seed(50)
a_comsafrica_platfthicimpact<-rpt(platfthickimpact ~  (1 | new_flake_id),
                                grname = ("new_flake_id"),
                                data = a_data,
                                datatype = "Gaussian",
                                nboot = 1000, npermut = 100)
print(a_comsafrica_platfthicimpact)
platfthickimpact_irr_a<-a_comsafrica_platfthicimpact$R
platfthickimpact_ci_a<-a_comsafrica_platfthicimpact$CI_emp

#flake platform thickness mid point
set.seed(50)
a_comsafrica_platfthickmid<-rpt(platfthickmid ~  (1 | new_flake_id),
                              grname = ("new_flake_id"),
                              data = a_data,
                              datatype = "Gaussian",
                              nboot = 1000, npermut = 100)
print(a_comsafrica_platfthickmid)
platfthickmid_irr_a<-a_comsafrica_platfthickmid$R
platfthickmid_ci_a<-a_comsafrica_platfthickmid$CI_emp

## compile IRR continuous values into table
irr_continuous_data_a<-rbind(maxdim_irr_a,mass_irr_a,
                           maxwidth_irr_a,maxthick_irr_a,techlength_irr_a,
                           techmaxwidth_irr_a,techmaxthick_irr_a,techwidthprox_irr_a,
                           techwidthmes_irr_a,techwidthdist_irr_a,techthickprox_irr_a,
                           techthickmes_irr_a,techthickdist_irr_a,platfwidth_irr_a,
                           platfthickmax_irr_a,platfthickimpact_irr_a,platfthickmid_irr_a) %>%
   rownames_to_column()

var_contnames<-c("maximumdimension","mass",
                 "maximumwidth","maximumthickness","techlength",
                 "techmaxwidth","techmaxthick","techwidthprox",
                 "techwidthmes","techwidthdist","techthickprox",
                 "techthickmes","techthickdist","platfwidth",
                 "platfthickmax","platfthickimpact","platfthickmid")

irr_cont_data_merged_a<-cbind(irr_continuous_data_a,var_contnames) %>%
   select(-rowname) %>%
   select(c(var_contnames,new_flake_id)) %>%
   pivot_longer(cols=c("new_flake_id"),
                names_to = "variable", values_to = "irr")

## compile IRR continuous value CI data into table
irr_cont_cidata_a<-rbind(maxdim_ci_a,mass_ci_a,
                       maxwidth_ci_a,maxthick_ci_a,techlength_ci_a,
                       techmaxwidth_ci_a,techmaxthick_ci_a,techwidthprox_ci_a,
                       techwidthmes_ci_a,techwidthdist_ci_a,techthickprox_ci_a,
                       techthickmes_ci_a,techthickdist_ci_a,platfwidth_ci_a,
                       platfthickmax_ci_a,platfthickimpact_ci_a,platfthickmid_ci_a) %>%
   rownames_to_column()

var_cicontnames<-c("maximumdimension","mass","maximumwidth","maximumthickness",
                   "techlength","techmaxwidth","techmaxthick","techwidthprox","techwidthmes",
                   "techwidthdist","techthickprox","techthickmes","techthickdist","platfwidth",
                   "platfthickmax","platfthickimpact","platfthickmid")

irr_cont_cidata_merged_a<-cbind(irr_cont_cidata_a,var_cicontnames) %>%
   rename(variable=rowname,
          var_contnames=var_cicontnames,
          lower='2.5%',
          upper='97.5%') %>%
   mutate(variable = gsub("[[:digit:]]", "", variable))

## merge IRR and CI  flake ID data
irr_cont_data_merged_flakeid_a<-irr_cont_data_merged_a %>%
   filter(variable=="new_flake_id")
irr_cont_cidata_merged_flakeid_a<-irr_cont_cidata_merged_a %>%
   filter(variable=="new_flake_id")

irr_cont_data_complete_flakeid_a<-merge(irr_cont_data_merged_flakeid_a,
                                        irr_cont_cidata_merged_flakeid_a,
                                      by="var_contnames") %>%
   select(-c(variable.y)) %>%
   select(c(var_contnames,variable.x,irr,lower,upper)) %>%
   mutate(across(where(is.numeric), ~ round(., 2)),
          assemblage_code=rep("a", times=length(lower))) %>%
   rename(comparison=variable.x)

   #### Assemblage B ####

b_data<-filter(comsafrica_data_complete, assemblage_code=="chert_condition_B") 

# maxdim
set.seed(50)
b_comsafrica_maxdim_boot<-rpt(maximumdimension ~  (1 | new_flake_id),
                              grname = ("new_flake_id"),
                              data = b_data,
                              datatype = "Gaussian",
                              nboot = 1000, npermut = 100)
print(b_comsafrica_maxdim_boot)
maxdim_irr_b<-b_comsafrica_maxdim_boot$R
maxdim_ci_b<-b_comsafrica_maxdim_boot$CI_emp

#mass
set.seed(50)
b_comsafrica_mass<-rpt(log(mass) ~  (1 | new_flake_id),
                       grname = ("new_flake_id"),
                       data = filter(b_data,mass>0),
                       datatype = "Gaussian",
                       nboot = 1000, npermut = 100)
print(b_comsafrica_mass)
mass_irr_b<-b_comsafrica_mass$R
mass_ci_b<-b_comsafrica_mass$CI_emp

#flake width
set.seed(50)
b_comsafrica_maxwidth<-rpt(maximumwidth ~  (1 | new_flake_id),
                           grname = ("new_flake_id"),
                           data = b_data,
                           datatype = "Gaussian",
                           nboot = 1000, npermut = 100)
print(b_comsafrica_maxwidth)
maxwidth_irr_b<-b_comsafrica_maxwidth$R
maxwidth_ci_b<-b_comsafrica_maxwidth$CI_emp

#flake max thickness
set.seed(50)
b_comsafrica_maxthick<-rpt(maximumthickness ~  (1 | new_flake_id),
                           grname = ("new_flake_id"),
                           data = b_data,
                           datatype = "Gaussian",
                           nboot = 1000, npermut = 100)
print(b_comsafrica_maxthick)
maxthick_irr_b<-b_comsafrica_maxthick$R
maxthick_ci_b<-b_comsafrica_maxthick$CI_emp

#flake tech length
set.seed(50)
b_comsafrica_techlength<-rpt(techlength ~  (1 | new_flake_id),
                             grname = ("new_flake_id"),
                             data = b_data,
                             datatype = "Gaussian",
                             nboot = 1000, npermut = 100)
print(b_comsafrica_techlength)
techlength_irr_b<-b_comsafrica_techlength$R
techlength_ci_b<-b_comsafrica_techlength$CI_emp

#flake tech max width
set.seed(50)
b_comsafrica_techmaxwidth<-rpt(techmaxwidth ~  (1 | new_flake_id),
                               grname = ("new_flake_id"),
                               data = b_data,
                               datatype = "Gaussian",
                               nboot = 1000, npermut = 100)
print(b_comsafrica_techmaxwidth)
techmaxwidth_irr_b<-b_comsafrica_techmaxwidth$R
techmaxwidth_ci_b<-b_comsafrica_techmaxwidth$CI_emp

#flake tech max thickness
set.seed(50)
b_comsafrica_techmaxthick<-rpt(techmaxthickness ~  (1 | new_flake_id),
                               grname = ("new_flake_id"),
                               data = b_data,
                               datatype = "Gaussian",
                               nboot = 1000, npermut = 100)
print(b_comsafrica_techmaxthick)
techmaxthick_irr_b<-b_comsafrica_techmaxthick$R
techmaxthick_ci_b<-b_comsafrica_techmaxthick$CI_emp

#flake tech width prox
set.seed(50)
b_comsafrica_techwidthprox<-rpt(techwidthprox ~  (1 | new_flake_id),
                                grname = ("new_flake_id"),
                                data = b_data,
                                datatype = "Gaussian",
                                nboot = 1000, npermut = 100)
print(b_comsafrica_techwidthprox)
techwidthprox_irr_b<-b_comsafrica_techwidthprox$R
techwidthprox_ci_b<-b_comsafrica_techwidthprox$CI_emp

#flake tech width mes
set.seed(50)
b_comsafrica_techwidthmes<-rpt(techwidthmes ~  (1 | new_flake_id),
                               grname = ("new_flake_id"),
                               data = b_data,
                               datatype = "Gaussian",
                               nboot = 1000, npermut = 100)
print(b_comsafrica_techwidthmes)
techwidthmes_irr_b<-b_comsafrica_techwidthmes$R
techwidthmes_ci_b<-b_comsafrica_techwidthmes$CI_emp

#flake tech width dist
set.seed(50)
b_comsafrica_techwidthdist<-rpt(techwidthdist ~  (1 | new_flake_id),
                                grname = ("new_flake_id"),
                                data = b_data,
                                datatype = "Gaussian",
                                nboot = 1000, npermut = 100)
print(b_comsafrica_techwidthdist)
techwidthdist_irr_b<-b_comsafrica_techwidthdist$R
techwidthdist_ci_b<-b_comsafrica_techwidthdist$CI_emp

#flake tech thick prox
#tech_thick_prox_data<-b_data %>%
#  filter(!new_flake_id %in% c(27,68,10))

set.seed(50)
b_comsafrica_techtechthickprox<-rpt(techthickprox ~  (1 | new_flake_id),
                                    grname = ("new_flake_id"),
                                    data = b_data,
                                    datatype = "Gaussian",
                                    nboot = 1000, npermut = 100)
print(b_comsafrica_techtechthickprox)
techthickprox_irr_b<-b_comsafrica_techtechthickprox$R
techthickprox_ci_b<-b_comsafrica_techtechthickprox$CI_emp

#flake tech thick med
set.seed(50)
b_comsafrica_techtechthickmes<-rpt(techthickmes ~  (1 | new_flake_id),
                                   grname = ("new_flake_id"),
                                   data = b_data,
                                   datatype = "Gaussian",
                                   nboot = 1000, npermut = 100)
print(b_comsafrica_techtechthickmes)
techthickmes_irr_b<-b_comsafrica_techtechthickmes$R
techthickmes_ci_b<-b_comsafrica_techtechthickmes$CI_emp

#flake tech thick dist
set.seed(50)
b_comsafrica_techthickdist<-rpt(techthickdist ~  (1 | new_flake_id),
                                grname = ("new_flake_id"),
                                data = b_data,
                                datatype = "Gaussian",
                                nboot = 1000, npermut = 100)
print(b_comsafrica_techthickdist)
techthickdist_irr_b<-b_comsafrica_techthickdist$R
techthickdist_ci_b<-b_comsafrica_techthickdist$CI_emp

#flake platform width
set.seed(50)
b_comsafrica_platfwidth<-rpt(platfwidth ~  (1 | new_flake_id),
                             grname = ("new_flake_id"),
                             data = b_data,
                             datatype = "Gaussian",
                             nboot = 1000, npermut = 100)
print(b_comsafrica_platfwidth)
platfwidth_irr_b<-b_comsafrica_platfwidth$R
platfwidth_ci_b<-b_comsafrica_platfwidth$CI_emp

#flake platform thickness
set.seed(50)
b_comsafrica_platfthickmax<-rpt(platfthickmax ~  (1 | new_flake_id),
                                grname = ("new_flake_id"),
                                data = b_data,
                                datatype = "Gaussian",
                                nboot = 1000, npermut = 100)
print(b_comsafrica_platfthickmax)
platfthickmax_irr_b<-b_comsafrica_platfthickmax$R
platfthickmax_ci_b<-b_comsafrica_platfthickmax$CI_emp

#flake platform thickness impact
set.seed(50)
b_comsafrica_platfthicimpact<-rpt(platfthickimpact ~  (1 | new_flake_id),
                                  grname = ("new_flake_id"),
                                  data = b_data,
                                  datatype = "Gaussian",
                                  nboot = 1000, npermut = 100)
print(b_comsafrica_platfthicimpact)
platfthickimpact_irr_b<-b_comsafrica_platfthicimpact$R
platfthickimpact_ci_b<-b_comsafrica_platfthicimpact$CI_emp

#flake platform thickness mid point
set.seed(50)
b_comsafrica_platfthickmid<-rpt(platfthickmid ~  (1 | new_flake_id),
                                grname = ("new_flake_id"),
                                data = b_data,
                                datatype = "Gaussian",
                                nboot = 1000, npermut = 100)
print(b_comsafrica_platfthickmid)
platfthickmid_irr_b<-b_comsafrica_platfthickmid$R
platfthickmid_ci_b<-b_comsafrica_platfthickmid$CI_emp

## compile IRR continuous values into table
irr_continuous_data_b<-rbind(maxdim_irr_b,mass_irr_b,
                             maxwidth_irr_b,maxthick_irr_b,techlength_irr_b,
                             techmaxwidth_irr_b,techmaxthick_irr_b,techwidthprox_irr_b,
                             techwidthmes_irr_b,techwidthdist_irr_b,techthickprox_irr_b,
                             techthickmes_irr_b,techthickdist_irr_b,platfwidth_irr_b,
                             platfthickmax_irr_b,platfthickimpact_irr_b,platfthickmid_irr_b) %>%
   rownames_to_column()

var_contnames<-c("maximumdimension","mass",
                 "maximumwidth","maximumthickness","techlength",
                 "techmaxwidth","techmaxthick","techwidthprox",
                 "techwidthmes","techwidthdist","techthickprox",
                 "techthickmes","techthickdist","platfwidth",
                 "platfthickmax","platfthickimpact","platfthickmid")

irr_cont_data_merged_b<-cbind(irr_continuous_data_b,var_contnames) %>%
   select(-rowname) %>%
   select(c(var_contnames,new_flake_id)) %>%
   pivot_longer(cols=c("new_flake_id"),
                names_to = "variable", values_to = "irr")

## compile IRR continuous value CI data into table
irr_cont_cidata_b<-rbind(maxdim_ci_b,mass_ci_b,
                         maxwidth_ci_b,maxthick_ci_b,techlength_ci_b,
                         techmaxwidth_ci_b,techmaxthick_ci_b,techwidthprox_ci_b,
                         techwidthmes_ci_b,techwidthdist_ci_b,techthickprox_ci_b,
                         techthickmes_ci_b,techthickdist_ci_b,platfwidth_ci_b,
                         platfthickmax_ci_b,platfthickimpact_ci_b,platfthickmid_ci_b) %>%
   rownames_to_column()

var_cicontnames<-c("maximumdimension","mass","maximumwidth","maximumthickness",
                   "techlength","techmaxwidth","techmaxthick","techwidthprox","techwidthmes",
                   "techwidthdist","techthickprox","techthickmes","techthickdist","platfwidth",
                   "platfthickmax","platfthickimpact","platfthickmid")

irr_cont_cidata_merged_b<-cbind(irr_cont_cidata_b,var_cicontnames) %>%
   rename(variable=rowname,
          var_contnames=var_cicontnames,
          lower='2.5%',
          upper='97.5%') %>%
   mutate(variable = gsub("[[:digit:]]", "", variable))

## merge IRR and CI  flake ID data
irr_cont_data_merged_flakeid_b<-irr_cont_data_merged_b %>%
   filter(variable=="new_flake_id")
irr_cont_cidata_merged_flakeid_b<-irr_cont_cidata_merged_b %>%
   filter(variable=="new_flake_id")

irr_cont_data_complete_flakeid_b<-merge(irr_cont_data_merged_flakeid_b,
                                        irr_cont_cidata_merged_flakeid_b,
                                        by="var_contnames") %>%
   select(-c(variable.y)) %>%
   select(c(var_contnames,variable.x,irr,lower,upper)) %>%
   mutate(across(where(is.numeric), ~ round(., 2)),
          assemblage_code=rep("b", times=length(lower))) %>%
   rename(comparison=variable.x)

   #### merge and compare two assemblage IRR data ####
merged_assemblage_irr_cont_data<-rbind(irr_cont_data_complete_flakeid_a,
                                       irr_cont_data_complete_flakeid_b) %>%
   select(c(var_contnames,assemblage_code,irr)) %>%
   spread(assemblage_code, irr) %>%
   mutate(irr_difference=(a-b))

library(ggtext)
ggplot(merged_assemblage_irr_cont_data,
       aes(y=irr_difference, x=reorder(var_contnames,irr_difference))) +
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10)) +
   ylab(expression(paste("IRR difference")))+
   xlab(label="")+
   theme(axis.title.y = element_text(face="bold"),
         axis.text.x = element_text(face = "bold"))

### repeatability coefficients for Count data ####

comsafrica_data_count_data<-new_comsafrica_data %>%
   select(c(assemblage_code,analyst_id,analysis_order,new_flake_id,proximal_scars,left_scars,distal_scars,right_scars,
            dorsal_scar_count)) %>%
   filter(!analyst_id == "r42o8")

   #### Both assemblages ####

# proximal_scars
hist(comsafrica_data_count_data$proximal_scars)
set.seed(50)
comsafrica_proximal_scars_boot<-rpt(proximal_scars ~ (1 | assemblage_code)  + (1 | new_flake_id),
                                    grname = c("assemblage_code","new_flake_id"),
                                    data = comsafrica_data_count_data,
                                    datatype = "Poisson",
                                    nboot = 100, npermut = 100)
print(comsafrica_proximal_scars_boot)
proximal_scars_irr<-comsafrica_proximal_scars_boot$R
proximal_scars_ci<-comsafrica_proximal_scars_boot$CI_emp$CI_org

# left_scars
hist(comsafrica_data_count_data$left_scars)
set.seed(50)
comsafrica_left_scars_boot<-rpt(left_scars ~ (1 | assemblage_code)  + (1 | new_flake_id),
                                grname = c("assemblage_code","new_flake_id"),
                                data = comsafrica_data_count_data,
                                datatype = "Poisson",
                                nboot = 100, npermut = 100)
print(comsafrica_left_scars_boot)
left_scars_irr<-comsafrica_left_scars_boot$R
left_scars_ci<-comsafrica_left_scars_boot$CI_emp$CI_org

# distal_scars
hist(comsafrica_data_count_data$distal_scars)
set.seed(50)
comsafrica_distal_scars_boot<-rpt(distal_scars ~ (1 | assemblage_code)  + (1 | new_flake_id),
                                  grname = c("assemblage_code","new_flake_id"),
                                  data = comsafrica_data_count_data,
                                  datatype = "Poisson",
                                  nboot = 100, npermut = 100)
print(comsafrica_distal_scars_boot)
distal_scars_irr<-comsafrica_distal_scars_boot$R
distal_scars_ci<-comsafrica_distal_scars_boot$CI_emp$CI_org

# right_scars
hist(comsafrica_data_count_data$right_scars)
set.seed(50)
comsafrica_right_scars_boot<-rpt(right_scars ~ (1 | assemblage_code)  + (1 | new_flake_id),
                                 grname = c("assemblage_code","new_flake_id"),
                                 data = comsafrica_data_count_data,
                                 datatype = "Poisson",
                                 nboot = 100, npermut = 100)
print(comsafrica_right_scars_boot)
right_scars_irr<-comsafrica_right_scars_boot$R
right_scars_ci<-comsafrica_right_scars_boot$CI_emp$CI_org

# dorsal scar count
hist(comsafrica_data_count_data$dorsal_scar_count)
set.seed(50)
comsafrica_dorsal_scar_count_boot<-rpt(dorsal_scar_count ~ (1 | assemblage_code)  + (1 | new_flake_id),
                                       grname = c("assemblage_code","new_flake_id"),
                                       data = comsafrica_data_count_data,
                                       datatype = "Poisson",
                                       nboot = 100, npermut = 100)
print(comsafrica_dorsal_scar_count_boot)
dorsal_scar_count_irr<-comsafrica_dorsal_scar_count_boot$R
dorsal_scar_count_ci<-comsafrica_dorsal_scar_count_boot$CI_emp$CI_org

## compile IRR count values into table
irr_count_data<-rbind(proximal_scars_irr,left_scars_irr,distal_scars_irr,right_scars_irr,dorsal_scar_count_irr) %>%
   rownames_to_column() %>%
   filter(rowname %in% c("R_org","R_org1","R_org2","R_org3", "R_org4"))

var_countnames<-c("proximal scars","left scars","distal scars","right scars","dorsal scars total")

irr_count_data_merged<-cbind(irr_count_data,var_countnames) %>%
   select(-rowname) %>%
   select(c(var_countnames,assemblage_code,new_flake_id)) %>%
   pivot_longer(cols=c("assemblage_code","new_flake_id"),
                names_to = "variable", values_to = "irr")

## compile IRR count value CI data into table
irr_count_cidata<-rbind(proximal_scars_ci,left_scars_ci,distal_scars_ci,right_scars_ci,dorsal_scar_count_ci) %>%
   rownames_to_column()

var_cicountnames<-c("proximal scars","proximal scars","left scars","left scars",
                    "distal scars","distal scars","right scars","right scars",
                    "dorsal scars total","dorsal scars total")

irr_count_cidata_merged<-cbind(irr_count_cidata,var_cicountnames) %>%
   rename(variable=rowname,
          var_countnames=var_cicountnames,
          lower='2.5%',
          upper='97.5%') %>%
   mutate(variable = gsub("[[:digit:]]", "", variable))

## merge IRR and CI  Assemblage data
irr_count_data_merged_assemblage<-irr_count_data_merged %>%
   filter(variable=="assemblage_code")
irr_count_cidata_merged_assemblage<-irr_count_cidata_merged %>%
   filter(variable=="assemblage_code")

irr_count_data_complete_assemblage<-merge(irr_count_data_merged_assemblage,
                                          irr_count_cidata_merged_assemblage,
                               by="var_countnames") %>%
   select(-c(variable.y)) %>%
   select(c(var_countnames,variable.x,irr,upper,lower)) %>%
   mutate(across(where(is.numeric), ~ round(., 4))) %>%
   rename(comparison=variable.x)

## merge IRR and CI  flake ID data
irr_count_data_merged_flakeid<-irr_count_data_merged %>%
   filter(variable=="new_flake_id")
irr_count_cidata_merged_flakeid<-irr_count_cidata_merged %>%
   filter(variable=="new_flake_id")

irr_count_data_complete_flakeid<-merge(irr_count_data_merged_flakeid,
                                       irr_count_cidata_merged_flakeid,
                                          by="var_countnames") %>%
   select(-c(variable.y)) %>%
   select(c(var_countnames,variable.x,irr,upper,lower)) %>%
   mutate(across(where(is.numeric), ~ round(., 2))) %>%
   rename(comparison=variable.x)

   #### Assemblage A ####
count_data_a<-comsafrica_data_count_data %>% filter(assemblage_code=="chert_condition_A")

# proximal_scars
hist(count_data_a$proximal_scars)
set.seed(50)
comsafrica_proximal_scars_boot_a<-rpt(proximal_scars ~    + (1 | new_flake_id),
                                    grname = "new_flake_id",
                                    data = count_data_a,
                                    datatype = "Poisson",
                                    nboot = 100, npermut = 100)
print(comsafrica_proximal_scars_boot_a)
proximal_scars_irr_a<-comsafrica_proximal_scars_boot_a$R

# left_scars
hist(count_data_a$left_scars)
set.seed(50)
comsafrica_left_scars_boot_a<-rpt(left_scars ~    + (1 | new_flake_id),
                                grname = "new_flake_id",
                                data = count_data_a,
                                datatype = "Poisson",
                                nboot = 100, npermut = 100)
print(comsafrica_left_scars_boot_a)
left_scars_irr_a<-comsafrica_left_scars_boot_a$R

# distal_scars
hist(count_data_a$distal_scars)
set.seed(50)
comsafrica_distal_scars_boot_a<-rpt(distal_scars ~    + (1 | new_flake_id),
                                  grname = "new_flake_id",
                                  data = count_data_a,
                                  datatype = "Poisson",
                                  nboot = 100, npermut = 100)
print(comsafrica_distal_scars_boot_a)
distal_scars_irr_a<-comsafrica_distal_scars_boot_a$R

# right_scars
hist(count_data_a$right_scars)
set.seed(50)
comsafrica_right_scars_boot_a<-rpt(right_scars ~    + (1 | new_flake_id),
                                 grname = "new_flake_id",
                                 data = count_data_a,
                                 datatype = "Poisson",
                                 nboot = 100, npermut = 100)
print(comsafrica_right_scars_boot_a)
right_scars_irr_a<-comsafrica_right_scars_boot_a$R

# dorsal scar count
hist(count_data_a$dorsal_scar_count)
set.seed(50)
comsafrica_dorsal_scar_count_boot_a<-rpt(dorsal_scar_count ~    + (1 | new_flake_id),
                                       grname = "new_flake_id",
                                       data = count_data_a,
                                       datatype = "Poisson",
                                       nboot = 100, npermut = 100)
print(comsafrica_dorsal_scar_count_boot_a)
dorsal_scar_count_irr_a<-comsafrica_dorsal_scar_count_boot_a$R

## compile IRR count values into table
irr_count_data_a<-rbind(proximal_scars_irr_a,left_scars_irr_a,
                      distal_scars_irr_a,right_scars_irr_a,dorsal_scar_count_irr_a) %>%
   rownames_to_column() %>%
   filter(rowname %in% c("R_org","R_org1","R_org2","R_org3", "R_org4"))

var_countnames<-c("proximal scars","left scars",
                  "distal scars","right scars",
                  "dorsal scars total")

irr_count_data_merged_a<-cbind(irr_count_data_a,var_countnames) %>%
   select(-rowname) %>%
   select(c(var_countnames,new_flake_id)) %>%
   pivot_longer(cols=c("new_flake_id"),
                names_to = "variable", values_to = "irr") %>%
   mutate(assemblage_code=rep("a", times=length(var_countnames)))

   #### Assemblage B ####

count_data_b<-comsafrica_data_count_data %>% filter(assemblage_code=="chert_condition_B")

# proximal_scars
hist(count_data_b$proximal_scars)
set.seed(50)
comsafrica_proximal_scars_boot_b<-rpt(proximal_scars ~    + (1 | new_flake_id),
                                      grname = "new_flake_id",
                                      data = count_data_b,
                                      datatype = "Poisson",
                                      nboot = 100, npermut = 100)
print(comsafrica_proximal_scars_boot_b)
proximal_scars_irr_b<-comsafrica_proximal_scars_boot_b$R

# left_scars
hist(count_data_b$left_scars)
set.seed(50)
comsafrica_left_scars_boot_b<-rpt(left_scars ~    + (1 | new_flake_id),
                                  grname = "new_flake_id",
                                  data = count_data_b,
                                  datatype = "Poisson",
                                  nboot = 100, npermut = 100)
print(comsafrica_left_scars_boot_b)
left_scars_irr_b<-comsafrica_left_scars_boot_b$R

# distal_scars
hist(count_data_b$distal_scars)
set.seed(50)
comsafrica_distal_scars_boot_b<-rpt(distal_scars ~    + (1 | new_flake_id),
                                    grname = "new_flake_id",
                                    data = count_data_b,
                                    datatype = "Poisson",
                                    nboot = 100, npermut = 100)
print(comsafrica_distal_scars_boot_b)
distal_scars_irr_b<-comsafrica_distal_scars_boot_b$R

# right_scars
hist(count_data_b$right_scars)
set.seed(50)
comsafrica_right_scars_boot_b<-rpt(right_scars ~    + (1 | new_flake_id),
                                   grname = "new_flake_id",
                                   data = count_data_b,
                                   datatype = "Poisson",
                                   nboot = 100, npermut = 100)
print(comsafrica_right_scars_boot_b)
right_scars_irr_b<-comsafrica_right_scars_boot_b$R

# dorsal scar count
hist(count_data_b$dorsal_scar_count)
set.seed(50)
comsafrica_dorsal_scar_count_boot_b<-rpt(dorsal_scar_count ~    + (1 | new_flake_id),
                                         grname = "new_flake_id",
                                         data = count_data_b,
                                         datatype = "Poisson",
                                         nboot = 100, npermut = 100)
print(comsafrica_dorsal_scar_count_boot_b)
dorsal_scar_count_irr_b<-comsafrica_dorsal_scar_count_boot_b$R

## compile IRR count values into table
irr_count_data_b<-rbind(proximal_scars_irr_b,left_scars_irr_b,
                        distal_scars_irr_b,right_scars_irr_b,dorsal_scar_count_irr_b) %>%
   rownames_to_column() %>%
   filter(rowname %in% c("R_org","R_org1","R_org2","R_org3", "R_org4"))

var_countnames<-c("proximal scars","left scars",
                  "distal scars","right scars",
                  "dorsal scars total")

irr_count_data_merged_b<-cbind(irr_count_data_b,var_countnames) %>%
   select(-rowname) %>%
   select(c(var_countnames,new_flake_id)) %>%
   pivot_longer(cols=c("new_flake_id"),
                names_to = "variable", values_to = "irr") %>%
   mutate(assemblage_code=rep("b", times=length(var_countnames)))

## merge assemblages a and b

combined_assemblage_count_data<-rbind(irr_count_data_merged_b,
                                      irr_count_data_merged_a) %>%
   spread(assemblage_code, irr) %>%
   mutate(irr_difference=(a-b))

ggplot(combined_assemblage_count_data,
       aes(y=irr_difference, x=reorder(var_countnames,irr_difference))) +
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10)) +
   ylab(expression(paste("IRR difference")))+
   xlab(label="")+
   theme(axis.title.y = element_text(face="bold"),
         axis.text.x = element_text(face = "bold"))

### repeatability coefficients for categorical data ####

comsafrica_data_cat_data<-new_comsafrica_data %>%
   select(c(assemblage_code,analyst_id,new_flake_id,completeness,platform_cortex,directionality,platfmorph,
            platflipp,bulb,shattbulb,initiation,ventr_plane_form,section,latedgetype,flaketerm,
            distplanform,kombewa,red_syst,flk_form))

comsafrica_data_cat_data<-as.data.frame(unclass(comsafrica_data_cat_data), stringsAsFactors = TRUE)

   #### Both assemblages ####

## Reduction system ##

red_syst_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,red_syst) %>% #delete coding episodes with no data
   na.omit

# delete flake #'s with < 4 observations
red_syst_data_a<-red_syst_data_a[as.numeric(ave(red_syst_data_a$new_flake_id,
                                                red_syst_data_a$new_flake_id,
                                                    FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
red_syst_data_a_krip<- red_syst_data_a %>%
   mutate(red_sys_dummy=unclass(red_syst)) %>%
   select(-red_syst) %>%
   spread(analyst_id,red_sys_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

a<-gwet.ac1.raw(red_syst_data_a_krip)$est

## flake form ##

flake_form_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,flk_form) %>%
   na.omit

# delete flake #'s with < 4 observations
flake_form_data_a<-flake_form_data_a[as.numeric(ave(flake_form_data_a$new_flake_id,
                                                    flake_form_data_a$new_flake_id,
                                                        FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
flake_form_data_a_krip<- flake_form_data_a %>%
   mutate(flk_form=as.factor(flk_form),
          flk_form_dummy=unclass(flk_form)) %>%
   select(-flk_form) %>%
   spread(analyst_id, flk_form_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

b<-gwet.ac1.raw(flake_form_data_a_krip)$est

## completeness ##

completeness_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,completeness) %>% 
   na.omit #delete coding episodes with no data

# delete flake #'s with < 4 observations
completeness_data_a<-completeness_data_a[as.numeric(ave(completeness_data_a$new_flake_id,
                                                        completeness_data_a$new_flake_id,
                                                        FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
completeness_data_a_krip<- completeness_data_a %>%
   mutate(completeness=as.factor(completeness),
          completeness_dummy=unclass(completeness)) %>%
   select(-completeness) %>%
   spread(analyst_id, completeness_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

c<-gwet.ac1.raw(completeness_data_a_krip)$est

## platform cortex ##

platform_cortex_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,platform_cortex) %>%
   na.omit

platform_cortex_data_a<-platform_cortex_data_a[as.numeric(ave(platform_cortex_data_a$new_flake_id,
                                                              platform_cortex_data_a$new_flake_id,
                                                              FUN=length)) >= 6, ]

platform_cortex_data_a_krip<- platform_cortex_data_a %>%
   mutate(platform_cortex=as.factor(platform_cortex),
          platform_cortex_dummy=unclass(platform_cortex)) %>%
   select(-platform_cortex) %>%
   spread(analyst_id, platform_cortex_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

d<-gwet.ac1.raw(platform_cortex_data_a_krip)$est

## scar directions ##

directionality_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,directionality) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

directionality_data_a<-directionality_data_a[as.numeric(ave(directionality_data_a$new_flake_id,
                                                            directionality_data_a$new_flake_id,
                                                            FUN=length)) >= 6, ]

directionality_data_a_krip<- directionality_data_a %>%
   mutate(directionality_dummy=unclass(directionality)) %>%
   select(-directionality) %>%
   spread(analyst_id, directionality_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

e<-gwet.ac1.raw(directionality_data_a_krip)$est

## platf morph ##

plat_morph_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,platfmorph) %>%
   na.omit

plat_morph_data_a<-plat_morph_data_a[as.numeric(ave(plat_morph_data_a$new_flake_id,
                                                    plat_morph_data_a$new_flake_id,
                                                    FUN=length)) >= 6, ]

plat_morph_data_a_krip<- plat_morph_data_a %>%
   mutate(plat_morph_dummy=unclass(platfmorph)) %>%
   select(-platfmorph) %>%
   spread(analyst_id, plat_morph_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

f<-gwet.ac1.raw(plat_morph_data_a_krip)$est

## platf lip ##

plat_lip_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,platflipp) %>%
   na.omit

plat_lip_data_a<-plat_lip_data_a[as.numeric(ave(plat_lip_data_a$new_flake_id,
                                                plat_lip_data_a$new_flake_id,
                                                FUN=length)) >=6, ]

plat_lip_data_a_krip<- plat_lip_data_a %>%
   mutate(plat_lip_dummy=unclass(platflipp)) %>%
   select(-platflipp) %>%
   spread(analyst_id, plat_lip_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

g<-gwet.ac1.raw(plat_lip_data_a_krip)$est

## bulb ##

plat_bulb_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,bulb) %>%
   na.omit 

plat_bulb_data_a<-plat_bulb_data_a[as.numeric(ave(plat_bulb_data_a$new_flake_id,
                                                  plat_bulb_data_a$new_flake_id,
                                                  FUN=length)) >=6, ]

plat_bulb_data_a_krip<- plat_bulb_data_a %>%
   mutate(plat_bulb_dummy=unclass(bulb)) %>%
   select(-bulb) %>%
   spread(analyst_id, plat_bulb_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

h<-gwet.ac1.raw(plat_bulb_data_a_krip)$est

## Shattbulb ##

plat_shattbulb_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,shattbulb) %>%
   na.omit

plat_shattbulb_data_a<-plat_shattbulb_data_a[as.numeric(ave(plat_shattbulb_data_a$new_flake_id,
                                                            plat_shattbulb_data_a$new_flake_id,
                                                            FUN=length)) >=6, ]

plat_shattbulb_data_a_krip<- plat_shattbulb_data_a %>%
   mutate(plat_shattbulb_dummy=unclass(shattbulb)) %>%
   select(-shattbulb) %>%
   spread(analyst_id, plat_shattbulb_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

i<-gwet.ac1.raw(plat_shattbulb_data_a_krip)$est

## initiation ##

plat_initiation_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,initiation) %>%
   na.omit

plat_initiation_data_a<-plat_initiation_data_a[as.numeric(ave(plat_initiation_data_a$new_flake_id,
                                                              plat_initiation_data_a$new_flake_id,
                                                              FUN=length)) >=6, ]

plat_initiation_data_a_krip<- plat_initiation_data_a %>%
   mutate(initiation_dummy=unclass(initiation)) %>%
   select(-initiation) %>%
   spread(analyst_id, initiation_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

j<-gwet.ac1.raw(plat_initiation_data_a_krip)$est

## ventr_plane_form ##

ventr_plane_form_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,ventr_plane_form) %>%
   na.omit

ventr_plane_form_data_a<-ventr_plane_form_data_a[as.numeric(ave(ventr_plane_form_data_a$new_flake_id,
                                                                ventr_plane_form_data_a$new_flake_id,
                                                                FUN=length)) >=6, ]

ventr_plane_form_data_a_krip<- ventr_plane_form_data_a %>%
   mutate(ventr_plane_form_dummy=unclass(ventr_plane_form)) %>%
   select(-ventr_plane_form) %>%
   spread(analyst_id, ventr_plane_form_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

k<-gwet.ac1.raw(ventr_plane_form_data_a_krip)$est

## Section ##

section_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,section) %>%
   na.omit

section_data_a<-section_data_a[as.numeric(ave(section_data_a$new_flake_id,
                                              section_data_a$new_flake_id,
                                              FUN=length)) >=6, ]

section_data_a_krip<- section_data_a %>%
   mutate(section_dummy=unclass(section)) %>%
   select(-section) %>%
   spread(analyst_id, section_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

l<-gwet.ac1.raw(section_data_a_krip)$est

## Lateral edge type ##

latedge_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,latedgetype) %>%
   na.omit

latedge_data_a<-latedge_data_a[as.numeric(ave(latedge_data_a$new_flake_id,
                                              latedge_data_a$new_flake_id,
                                              FUN=length)) >=6, ]

latedge_data_a_krip<- latedge_data_a %>%
   mutate(latedge_dummy=unclass(latedgetype)) %>%
   select(-latedgetype) %>%
   spread(analyst_id, latedge_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

m<-gwet.ac1.raw(latedge_data_a_krip)$est

## Flake termination ##

flaketerm_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,flaketerm) %>%
   na.omit

flaketerm_data_a<-flaketerm_data_a[as.numeric(ave(flaketerm_data_a$new_flake_id,
                                                  flaketerm_data_a$new_flake_id,
                                                  FUN=length)) >=6, ]

flaketerm_data_a_krip<- flaketerm_data_a %>%
   mutate(flaketerm_dummy=unclass(flaketerm)) %>%
   select(-flaketerm) %>%
   spread(analyst_id, flaketerm_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

n<-gwet.ac1.raw(flaketerm_data_a_krip)$est

## Kombewa

kombewa_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,kombewa) %>%
   na.omit

kombewa_data_a<-kombewa_data_a[as.numeric(ave(kombewa_data_a$new_flake_id,
                                              kombewa_data_a$new_flake_id,
                                              FUN=length)) > 4, ]

kombewa_data_a_krip<- kombewa_data_a %>%
   mutate(kombewa_dummy=unclass(kombewa)) %>%
   select(-kombewa) %>%
   spread(analyst_id, kombewa_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

o<-gwet.ac1.raw(kombewa_data_a_krip)$est

## Distal plan form

distplanform_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,distplanform) %>%
   na.omit

distplanform_data_a<-distplanform_data_a[as.numeric(ave(distplanform_data_a$new_flake_id,
                                                        distplanform_data_a$new_flake_id,
                                                        FUN=length)) > 4, ]

distplanform_data_a_krip<- distplanform_data_a %>%
   mutate(distplanform_dummy=unclass(distplanform)) %>%
   select(-distplanform) %>%
   spread(analyst_id, distplanform_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

p<-gwet.ac1.raw(distplanform_data_a_krip)$est

## collate gwet results
gwet_data<-rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
var_names<-c("reduction_system","flake_form","completeness","platform_cortex",
             "scar_directionality", "platform_morphology", "platform_lipping",
             "bulb","shattered_bulb", "initiation","ventral_plan_form","cross_section_shape",
             "lateral_edge_shape","flake_termination","Kombewa","distal_plan_form")

gwet_data_merged<-cbind(gwet_data,var_names) %>%
   select(c(var_names,coeff.val,conf.int)) %>%
   mutate(conf.int=str_replace_all(conf.int,c("[(]"="","[)]"=""), "")) %>%
   separate(conf.int, c("lower", "upper"), sep = ",") %>%
   mutate(lower=as.numeric(lower),
          upper=as.numeric(upper),
          across(where(is.numeric), ~ round(., 2))) %>%
   arrange(-coeff.val)
   
write.csv(gwet_data_merged,"categorical_summary.csv")

   #### Assemblage A ####

categ_data_a<-comsafrica_data_cat_data %>% filter(assemblage_code=="chert_condition_A")

## Reduction system ##

red_syst_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,red_syst) %>%
   na.omit

# delete flake #'s with < 4 observations
red_syst_data_a<-red_syst_data_a[as.numeric(ave(red_syst_data_a$new_flake_id,
                                                red_syst_data_a$new_flake_id,
                                                FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
red_syst_data_a_krip_a<- red_syst_data_a %>%
   mutate(red_sys_dummy=unclass(red_syst)) %>%
   select(-red_syst) %>%
   spread(analyst_id,red_sys_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

q<-gwet.ac1.raw(red_syst_data_a_krip_a)$est

## flake form ##

flake_form_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,flk_form) %>%
   na.omit

# delete flake #'s with < 4 observations
flake_form_data_a<-flake_form_data_a[as.numeric(ave(flake_form_data_a$new_flake_id,
                                                    flake_form_data_a$new_flake_id,
                                                    FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
flake_form_data_a_krip_a<- flake_form_data_a %>%
   mutate(flk_form=as.factor(flk_form),
          flk_form_dummy=unclass(flk_form)) %>%
   select(-flk_form) %>%
   spread(analyst_id, flk_form_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

r<-gwet.ac1.raw(flake_form_data_a_krip_a)$est

## completeness ##

completeness_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,completeness) %>% 
   na.omit #delete coding episodes with no data

# delete flake #'s with < 4 observations
completeness_data_a<-completeness_data_a[as.numeric(ave(completeness_data_a$new_flake_id,
                                                        completeness_data_a$new_flake_id,
                                                        FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
completeness_data_a_krip_a<- completeness_data_a %>%
   mutate(completeness=as.factor(completeness),
          completeness_dummy=unclass(completeness)) %>%
   select(-completeness) %>%
   spread(analyst_id, completeness_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

s<-gwet.ac1.raw(completeness_data_a_krip_a)$est

## platform cortex ##

platform_cortex_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,platform_cortex) %>%
   na.omit

platform_cortex_data_a<-platform_cortex_data_a[as.numeric(ave(platform_cortex_data_a$new_flake_id,
                                                              platform_cortex_data_a$new_flake_id,
                                                              FUN=length)) >= 6, ]

platform_cortex_data_a_krip_a<- platform_cortex_data_a %>%
   mutate(platform_cortex=as.factor(platform_cortex),
          platform_cortex_dummy=unclass(platform_cortex)) %>%
   select(-platform_cortex) %>%
   spread(analyst_id, platform_cortex_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

t<-gwet.ac1.raw(platform_cortex_data_a_krip_a)$est

## scar directions ##

directionality_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,directionality) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

directionality_data_a<-directionality_data_a[as.numeric(ave(directionality_data_a$new_flake_id,
                                                            directionality_data_a$new_flake_id,
                                                            FUN=length)) >= 6, ]

directionality_data_a_krip_a<- directionality_data_a %>%
   mutate(directionality_dummy=unclass(directionality)) %>%
   select(-directionality) %>%
   spread(analyst_id, directionality_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

u<-gwet.ac1.raw(directionality_data_a_krip_a)$est

## platf morph ##

plat_morph_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,platfmorph) %>%
   na.omit

plat_morph_data_a<-plat_morph_data_a[as.numeric(ave(plat_morph_data_a$new_flake_id,
                                                    plat_morph_data_a$new_flake_id,
                                                    FUN=length)) >= 6, ]

plat_morph_data_a_krip_a<- plat_morph_data_a %>%
   mutate(plat_morph_dummy=unclass(platfmorph)) %>%
   select(-platfmorph) %>%
   spread(analyst_id, plat_morph_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

v<-gwet.ac1.raw(plat_morph_data_a_krip_a)$est

## platf lip ##

plat_lip_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,platflipp) %>%
   na.omit

plat_lip_data_a<-plat_lip_data_a[as.numeric(ave(plat_lip_data_a$new_flake_id,
                                                plat_lip_data_a$new_flake_id,
                                                FUN=length)) >=6, ]

plat_lip_data_a_krip_a<- plat_lip_data_a %>%
   mutate(plat_lip_dummy=unclass(platflipp)) %>%
   select(-platflipp) %>%
   spread(analyst_id, plat_lip_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

w<-gwet.ac1.raw(plat_lip_data_a_krip_a)$est

## bulb ##

plat_bulb_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,bulb) %>%
   na.omit 

plat_bulb_data_a<-plat_bulb_data_a[as.numeric(ave(plat_bulb_data_a$new_flake_id,
                                                  plat_bulb_data_a$new_flake_id,
                                                  FUN=length)) >=6, ]

plat_bulb_data_a_krip_a<- plat_bulb_data_a %>%
   mutate(plat_bulb_dummy=unclass(bulb)) %>%
   select(-bulb) %>%
   spread(analyst_id, plat_bulb_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

x<-gwet.ac1.raw(plat_bulb_data_a_krip_a)$est

## Shattbulb ##

plat_shattbulb_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,shattbulb) %>%
   na.omit

plat_shattbulb_data_a<-plat_shattbulb_data_a[as.numeric(ave(plat_shattbulb_data_a$new_flake_id,
                                                            plat_shattbulb_data_a$new_flake_id,
                                                            FUN=length)) >=6, ]

plat_shattbulb_data_a_krip_a<- plat_shattbulb_data_a %>%
   mutate(plat_shattbulb_dummy=unclass(shattbulb)) %>%
   select(-shattbulb) %>%
   spread(analyst_id, plat_shattbulb_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

y<-gwet.ac1.raw(plat_shattbulb_data_a_krip_a)$est

## initiation ##

plat_initiation_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,initiation) %>%
   na.omit

plat_initiation_data_a<-plat_initiation_data_a[as.numeric(ave(plat_initiation_data_a$new_flake_id,
                                                              plat_initiation_data_a$new_flake_id,
                                                              FUN=length)) >=6, ]

plat_initiation_data_a_krip_a<- plat_initiation_data_a %>%
   mutate(initiation_dummy=unclass(initiation)) %>%
   select(-initiation) %>%
   spread(analyst_id, initiation_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

z<-gwet.ac1.raw(plat_initiation_data_a_krip_a)$est

## ventr_plane_form ##

ventr_plane_form_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,ventr_plane_form) %>%
   na.omit

ventr_plane_form_data_a<-ventr_plane_form_data_a[as.numeric(ave(ventr_plane_form_data_a$new_flake_id,
                                                                ventr_plane_form_data_a$new_flake_id,
                                                                FUN=length)) >=6, ]

ventr_plane_form_data_a_krip_a<- ventr_plane_form_data_a %>%
   mutate(ventr_plane_form_dummy=unclass(ventr_plane_form)) %>%
   select(-ventr_plane_form) %>%
   spread(analyst_id, ventr_plane_form_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

aa<-gwet.ac1.raw(ventr_plane_form_data_a_krip_a)$est

## Section ##

section_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,section) %>%
   na.omit

section_data_a<-section_data_a[as.numeric(ave(section_data_a$new_flake_id,
                                              section_data_a$new_flake_id,
                                              FUN=length)) >=6, ]

section_data_a_krip_a<- section_data_a %>%
   mutate(section_dummy=unclass(section)) %>%
   select(-section) %>%
   spread(analyst_id, section_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

bb<-gwet.ac1.raw(section_data_a_krip_a)$est

## Lateral edge type ##

latedge_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,latedgetype) %>%
   na.omit

latedge_data_a<-latedge_data_a[as.numeric(ave(latedge_data_a$new_flake_id,
                                              latedge_data_a$new_flake_id,
                                              FUN=length)) >=6, ]

latedge_data_a_krip_a<- latedge_data_a %>%
   mutate(latedge_dummy=unclass(latedgetype)) %>%
   select(-latedgetype) %>%
   spread(analyst_id, latedge_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

cc<-gwet.ac1.raw(latedge_data_a_krip_a)$est

## Flake termination ##

flaketerm_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,flaketerm) %>%
   na.omit

flaketerm_data_a<-flaketerm_data_a[as.numeric(ave(flaketerm_data_a$new_flake_id,
                                                  flaketerm_data_a$new_flake_id,
                                                  FUN=length)) >=6, ]

flaketerm_data_a_krip_a<- flaketerm_data_a %>%
   mutate(flaketerm_dummy=unclass(flaketerm)) %>%
   select(-flaketerm) %>%
   spread(analyst_id, flaketerm_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

dd<-gwet.ac1.raw(flaketerm_data_a_krip_a)$est

## Kombewa

kombewa_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,kombewa) %>%
   na.omit

kombewa_data_a<-kombewa_data_a[as.numeric(ave(kombewa_data_a$new_flake_id,
                                              kombewa_data_a$new_flake_id,
                                              FUN=length)) > 4, ]

kombewa_data_a_krip_a<- kombewa_data_a %>%
   mutate(kombewa_dummy=unclass(kombewa)) %>%
   select(-kombewa) %>%
   spread(analyst_id, kombewa_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

ee<-gwet.ac1.raw(kombewa_data_a_krip_a)$est

## Distal plan form

distplanform_data_a<-categ_data_a %>%
   select(new_flake_id,analyst_id,distplanform) %>%
   na.omit

distplanform_data_a<-distplanform_data_a[as.numeric(ave(distplanform_data_a$new_flake_id,
                                                        distplanform_data_a$new_flake_id,
                                                        FUN=length)) > 4, ]

distplanform_data_a_krip_a<- distplanform_data_a %>%
   mutate(distplanform_dummy=unclass(distplanform)) %>%
   select(-distplanform) %>%
   spread(analyst_id, distplanform_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

ff<-gwet.ac1.raw(distplanform_data_a_krip_a)$est

## collate gwet results
gwet_data_a<-rbind(q,r,s,t,u,v,w,x,y,z,aa,bb,cc,dd,ee,ff)
var_names<-c("reduction_system","flake_form","completeness","platform_cortex",
             "scar_directionality", "platform_morphology", "platform_lipping",
             "bulb","shattered_bulb", "initiation","ventral_plan_form","cross_section_shape",
             "lateral_edge_shape","flake_termination","Kombewa","distal_plan_form")

gwet_data_merged_a<-cbind(gwet_data_a,var_names) %>%
   select(c(var_names,coeff.val)) %>%
   mutate(assemblage_code=rep("a",times=length(var_names)),
          across(where(is.numeric), ~ round(., 2))) %>%
   arrange(-coeff.val)

   #### Assemblage B ####

categ_data_b<-comsafrica_data_cat_data %>% filter(assemblage_code=="chert_condition_B")

## Reduction system ##

red_syst_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,red_syst) %>%
   na.omit

# delete flake #'s with < 4 observations
red_syst_data_b<-red_syst_data_b[as.numeric(ave(red_syst_data_b$new_flake_id,
                                                red_syst_data_b$new_flake_id,
                                                FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
red_syst_data_b_krip_b<- red_syst_data_b %>%
   mutate(red_sys_dummy=unclass(red_syst)) %>%
   select(-red_syst) %>%
   spread(analyst_id,red_sys_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

gg<-gwet.ac1.raw(red_syst_data_b_krip_b)$est

## flake form ##

flake_form_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,flk_form) %>%
   na.omit

# delete flake #'s with < 4 observations
flake_form_data_b<-flake_form_data_b[as.numeric(ave(flake_form_data_b$new_flake_id,
                                                    flake_form_data_b$new_flake_id,
                                                    FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
flake_form_data_b_krip_b<- flake_form_data_b %>%
   mutate(flk_form=as.factor(flk_form),
          flk_form_dummy=unclass(flk_form)) %>%
   select(-flk_form) %>%
   spread(analyst_id, flk_form_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

hh<-gwet.ac1.raw(flake_form_data_b_krip_b)$est

## completeness ##

completeness_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,completeness) %>% 
   na.omit #delete coding episodes with no data

# delete flake #'s with < 4 observations
completeness_data_b<-completeness_data_b[as.numeric(ave(completeness_data_b$new_flake_id,
                                                        completeness_data_b$new_flake_id,
                                                        FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
completeness_data_b_krip_b<- completeness_data_b %>%
   mutate(completeness=as.factor(completeness),
          completeness_dummy=unclass(completeness)) %>%
   select(-completeness) %>%
   spread(analyst_id, completeness_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

ii<-gwet.ac1.raw(completeness_data_b_krip_b)$est

## platform cortex ##

platform_cortex_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,platform_cortex) %>%
   na.omit

platform_cortex_data_b<-platform_cortex_data_b[as.numeric(ave(platform_cortex_data_b$new_flake_id,
                                                              platform_cortex_data_b$new_flake_id,
                                                              FUN=length)) >= 6, ]

platform_cortex_data_b_krip_b<- platform_cortex_data_b %>%
   mutate(platform_cortex=as.factor(platform_cortex),
          platform_cortex_dummy=unclass(platform_cortex)) %>%
   select(-platform_cortex) %>%
   spread(analyst_id, platform_cortex_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

jj<-gwet.ac1.raw(platform_cortex_data_b_krip_b)$est

## scar directions ##

directionality_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,directionality) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

directionality_data_b<-directionality_data_b[as.numeric(ave(directionality_data_b$new_flake_id,
                                                            directionality_data_b$new_flake_id,
                                                            FUN=length)) >= 6, ]

directionality_data_b_krip_b<- directionality_data_b %>%
   mutate(directionality_dummy=unclass(directionality)) %>%
   select(-directionality) %>%
   spread(analyst_id, directionality_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

kk<-gwet.ac1.raw(directionality_data_b_krip_b)$est

## platf morph ##

plat_morph_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,platfmorph) %>%
   na.omit

plat_morph_data_b<-plat_morph_data_b[as.numeric(ave(plat_morph_data_b$new_flake_id,
                                                    plat_morph_data_b$new_flake_id,
                                                    FUN=length)) >= 6, ]

plat_morph_data_b_krip_b<- plat_morph_data_b %>%
   mutate(plat_morph_dummy=unclass(platfmorph)) %>%
   select(-platfmorph) %>%
   spread(analyst_id, plat_morph_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

ll<-gwet.ac1.raw(plat_morph_data_b_krip_b)$est

## platf lip ##

plat_lip_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,platflipp) %>%
   na.omit

plat_lip_data_b<-plat_lip_data_b[as.numeric(ave(plat_lip_data_b$new_flake_id,
                                                plat_lip_data_b$new_flake_id,
                                                FUN=length)) >=6, ]

plat_lip_data_b_krip_b<- plat_lip_data_b %>%
   mutate(plat_lip_dummy=unclass(platflipp)) %>%
   select(-platflipp) %>%
   spread(analyst_id, plat_lip_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

mm<-gwet.ac1.raw(plat_lip_data_b_krip_b)$est

## bulb ##

plat_bulb_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,bulb) %>%
   na.omit 

plat_bulb_data_b<-plat_bulb_data_b[as.numeric(ave(plat_bulb_data_b$new_flake_id,
                                                  plat_bulb_data_b$new_flake_id,
                                                  FUN=length)) >=6, ]

plat_bulb_data_b_krip_b<- plat_bulb_data_b %>%
   mutate(plat_bulb_dummy=unclass(bulb)) %>%
   select(-bulb) %>%
   spread(analyst_id, plat_bulb_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

nn<-gwet.ac1.raw(plat_bulb_data_b_krip_b)$est

## Shattbulb ##

plat_shattbulb_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,shattbulb) %>%
   na.omit

plat_shattbulb_data_b<-plat_shattbulb_data_b[as.numeric(ave(plat_shattbulb_data_b$new_flake_id,
                                                            plat_shattbulb_data_b$new_flake_id,
                                                            FUN=length)) >=6, ]

plat_shattbulb_data_b_krip_b<- plat_shattbulb_data_b %>%
   mutate(plat_shattbulb_dummy=unclass(shattbulb)) %>%
   select(-shattbulb) %>%
   spread(analyst_id, plat_shattbulb_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

oo<-gwet.ac1.raw(plat_shattbulb_data_b_krip_b)$est

## initiation ##

plat_initiation_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,initiation) %>%
   na.omit

plat_initiation_data_b<-plat_initiation_data_b[as.numeric(ave(plat_initiation_data_b$new_flake_id,
                                                              plat_initiation_data_b$new_flake_id,
                                                              FUN=length)) >=6, ]

plat_initiation_data_b_krip_b<- plat_initiation_data_b %>%
   mutate(initiation_dummy=unclass(initiation)) %>%
   select(-initiation) %>%
   spread(analyst_id, initiation_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

pp<-gwet.ac1.raw(plat_initiation_data_b_krip_b)$est

## ventr_plane_form ##

ventr_plane_form_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,ventr_plane_form) %>%
   na.omit

ventr_plane_form_data_b<-ventr_plane_form_data_b[as.numeric(ave(ventr_plane_form_data_b$new_flake_id,
                                                                ventr_plane_form_data_b$new_flake_id,
                                                                FUN=length)) >=6, ]

ventr_plane_form_data_b_krip_b<- ventr_plane_form_data_b %>%
   mutate(ventr_plane_form_dummy=unclass(ventr_plane_form)) %>%
   select(-ventr_plane_form) %>%
   spread(analyst_id, ventr_plane_form_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

qq<-gwet.ac1.raw(ventr_plane_form_data_b_krip_b)$est

## Section ##

section_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,section) %>%
   na.omit

section_data_b<-section_data_b[as.numeric(ave(section_data_b$new_flake_id,
                                              section_data_b$new_flake_id,
                                              FUN=length)) >=6, ]

section_data_b_krip_b<- section_data_b %>%
   mutate(section_dummy=unclass(section)) %>%
   select(-section) %>%
   spread(analyst_id, section_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

rr<-gwet.ac1.raw(section_data_b_krip_b)$est

## Lateral edge type ##

latedge_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,latedgetype) %>%
   na.omit

latedge_data_b<-latedge_data_b[as.numeric(ave(latedge_data_b$new_flake_id,
                                              latedge_data_b$new_flake_id,
                                              FUN=length)) >=6, ]

latedge_data_b_krip_b<- latedge_data_b %>%
   mutate(latedge_dummy=unclass(latedgetype)) %>%
   select(-latedgetype) %>%
   spread(analyst_id, latedge_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

ss<-gwet.ac1.raw(latedge_data_b_krip_b)$est

## Flake termination ##

flaketerm_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,flaketerm) %>%
   na.omit

flaketerm_data_b<-flaketerm_data_b[as.numeric(ave(flaketerm_data_b$new_flake_id,
                                                  flaketerm_data_b$new_flake_id,
                                                  FUN=length)) >=6, ]

flaketerm_data_b_krip_b<- flaketerm_data_b %>%
   mutate(flaketerm_dummy=unclass(flaketerm)) %>%
   select(-flaketerm) %>%
   spread(analyst_id, flaketerm_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

tt<-gwet.ac1.raw(flaketerm_data_b_krip_b)$est

## Kombewa

kombewa_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,kombewa) %>%
   na.omit

kombewa_data_b<-kombewa_data_b[as.numeric(ave(kombewa_data_b$new_flake_id,
                                              kombewa_data_b$new_flake_id,
                                              FUN=length)) > 4, ]

kombewa_data_b_krip_b<- kombewa_data_b %>%
   mutate(kombewa_dummy=unclass(kombewa)) %>%
   select(-kombewa) %>%
   spread(analyst_id, kombewa_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

uu<-gwet.ac1.raw(kombewa_data_b_krip_b)$est

## Distal plan form

distplanform_data_b<-categ_data_b %>%
   select(new_flake_id,analyst_id,distplanform) %>%
   na.omit

distplanform_data_b<-distplanform_data_b[as.numeric(ave(distplanform_data_b$new_flake_id,
                                                        distplanform_data_b$new_flake_id,
                                                        FUN=length)) > 4, ]

distplanform_data_b_krip_b<- distplanform_data_b %>%
   mutate(distplanform_dummy=unclass(distplanform)) %>%
   select(-distplanform) %>%
   spread(analyst_id, distplanform_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

vv<-gwet.ac1.raw(distplanform_data_b_krip_b)$est

## collate gwet results
gwet_data_b<-rbind(gg,hh,ii,jj,kk,ll,mm,nn,oo,pp,qq,rr,ss,tt,uu,vv)
var_names<-c("reduction_system","flake_form","completeness","platform_cortex",
             "scar_directionality", "platform_morphology", "platform_lipping",
             "bulb","shattered_bulb", "initiation","ventral_plan_form","cross_section_shape",
             "lateral_edge_shape","flake_termination","Kombewa","distal_plan_form")

gwet_data_merged_b<-cbind(gwet_data_a,var_names) %>%
   select(c(var_names,coeff.val)) %>%
   mutate(assemblage_code=rep("b",times=length(var_names)),
          across(where(is.numeric), ~ round(., 2))) %>%
   arrange(-coeff.val)

##merge two assemblage datasets
combined_categorical_irr<-rbind(gwet_data_merged_b,
                                gwet_data_merged_a) %>%
   #filter(!var_names=="reduction_system") %>%
   spread(assemblage_code, coeff.val) %>%
   mutate(irr_difference=(a-b))

ggplot(combined_categorical_irr,
       aes(y=irr_difference, x=reorder(var_names,irr_difference))) +
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10)) +
   ylab(expression(paste("IRR difference")))+
   xlab(label="")+
   theme(axis.title.y = element_text(face="bold"),
         axis.text.x = element_text(face = "bold"))

### IRR visualizations ####

data_1<-irr_count_data_complete_flakeid %>% rename(variable=var_countnames) %>%
   mutate(data_class=as.factor(rep("Count",times=length(variable))))
data_2<-irr_count_data_complete_assemblage %>% rename(variable=var_countnames)%>%
   mutate(data_class=as.factor(rep("Count",times=length(variable))))
data_3<-irr_cont_data_complete_flakeid %>% rename(variable=var_contnames)%>%
   mutate(data_class=as.factor(rep("Continuous",times=length(variable))))
data_4<-irr_cont_data_complete_assemblage %>% rename(variable=var_contnames)%>%
   mutate(data_class=as.factor(rep("Continuous",times=length(variable))))

irr_summary<-rbind(data_1,data_2,data_3,data_4)

## Continuous data, analyst ID
ggplot(data=filter(irr_summary, data_class=="Continuous" & comparison =="new_flake_id" & !variable =="dorsal_cortex"),
       aes(y=irr, x=reorder(variable,irr))) +
   geom_bar(position=position_dodge(), stat="identity") +
   geom_errorbar(aes(ymin=lower, ymax=upper),
                 width=.2,                    # Width of the error bars
                 position=position_dodge(.9))+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   labs(x = "",
        y = "IRR value") +
   theme(axis.title = element_text(face="bold"),
         axis.text.x = element_text(face = "bold"))

## Continuous data, assemblage type

ggplot(data=filter(irr_summary, data_class=="Continuous" & comparison =="assemblage_code"),
       aes(y=irr, x=reorder(variable,irr))) +
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10)) +
   labs(x = "",
        y = "IRR value")

## Count data, analyst ID

ggplot(data=filter(irr_summary, data_class=="Count" & comparison =="new_flake_id"),
       aes(y=irr, x=reorder(variable,irr))) +
   geom_bar(position=position_dodge(), stat="identity") +
   geom_errorbar(aes(ymin=lower, ymax=upper),
                 width=.2,                    # Width of the error bars
                 position=position_dodge(.9))+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red")+
   theme(axis.title = element_text(face="bold"),
         axis.text.x = element_text(face = "bold"))

## Count data, assemblage type

ggplot(data=filter(irr_summary, data_class=="Count" & comparison =="assemblage_code"),
       aes(y=irr, x=reorder(variable,irr))) +
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")

## Categorical data

ggplot(gwet_data_merged,aes(y=coeff.val, x=reorder(var_names,coeff.val))) +
   geom_bar(position=position_dodge(), stat="identity") +
   geom_errorbar(aes(ymin=lower, ymax=upper),
                 width=.2,                    # Width of the error bars
                 position=position_dodge(.9))+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "AC1 value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red")+
   theme(axis.title = element_text(face="bold"),
         axis.text.x = element_text(face = "bold"))

############################################### ######################
###### SUMMARY TABLES FOR CATEGORICAL VARIABLES ####

data1 <- comsafrica_data_cat_data

#completeness
tableCOMPLETENESS <-  table(data1$new_flake_id, data1$completeness)
tabCOMP=cbind(addmargins(round(prop.table(addmargins(tableCOMPLETENESS,1),1),2)*100,2), 
              c(margin.table(tableCOMPLETENESS,1),sum(tableCOMPLETENESS))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename("New Flake ID"=rowname,
          "N Analysts"=V9) %>%
   mutate(variable=as.factor(rep("completeness",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/tablecompleteness.csv", row.names=FALSE)

#platform_cortex
table3 <- table(data1$new_flake_id, data1$platform_cortex)
tabCORTEXPLATF=cbind(addmargins(round(prop.table(addmargins(table3,1),1),2)*100,2), 
                     c(margin.table(table3,1),sum(table3))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("platform cortex",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/platformcortex.csv", row.names=FALSE)

#directionality
table4 <- table(data1$new_flake_id, data1$directionality)
tabDIRECTIONALITY=cbind(addmargins(round(prop.table(addmargins(table4,1),1),2)*100,2), 
                        c(margin.table(table4,1),sum(table4))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("directionality",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/directionality.csv", row.names=FALSE)

#platf morph
table5 <- table(data1$new_flake_id, data1$platfmorph)
tabPLATFMORPH=cbind(addmargins(round(prop.table(addmargins(table5,1),1),2)*100,2), 
                    c(margin.table(table5,1),sum(table5))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("platform morphology",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/platformorph.csv", row.names=FALSE)

#platf lip
table6 <- table(data1$new_flake_id, data1$platflipp)
tabPLATFLIPP=cbind(addmargins(round(prop.table(addmargins(table6,1),1),2)*100,2), 
                   c(margin.table(table6,1),sum(table6))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("platform lipping",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/platformlip.csv", row.names=FALSE)

#bulb
table7 <- table(data1$new_flake_id, data1$bulb)
tabBULB=cbind(addmargins(round(prop.table(addmargins(table7,1),1),2)*100,2), 
              c(margin.table(table7,1),sum(table7))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("bulb",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/bulb.csv", row.names=FALSE)

#shattbulb
table8 <- table(data1$new_flake_id, data1$shattbulb)
tabSHATTBULB=cbind(addmargins(round(prop.table(addmargins(table8,1),1),2)*100,2), 
                   c(margin.table(table8,1),sum(table8))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("shattbulb",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/shattbulb.csv", row.names=FALSE)

#initiation
table9 <- table(data1$new_flake_id, data1$initiation)
tabINITIATION=cbind(addmargins(round(prop.table(addmargins(table9,1),1),2)*100,2), 
                    c(margin.table(table9,1),sum(table9))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("initiation",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/initiation.csv", row.names=FALSE)

#ventral plane form
table10 <- table(data1$new_flake_id, data1$ventr_plane_form)
tabVENTRPLANEFORM=cbind(addmargins(round(prop.table(addmargins(table10,1),1),2)*100,2), 
                        c(margin.table(table10,1),sum(table10))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("ventral plan form",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/ventral plan form.csv", row.names=FALSE)

#section
table11 <- table(data1$new_flake_id, data1$section)
tabSECTION=cbind(addmargins(round(prop.table(addmargins(table11,1),1),2)*100,2), 
                 c(margin.table(table11,1),sum(table11))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("section",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/section.csv", row.names=FALSE)

#lateral edge type
table12 <- table(data1$new_flake_id, data1$latedgetype)
tabLATEDGE=cbind(addmargins(round(prop.table(addmargins(table12,1),1),2)*100,2), 
                 c(margin.table(table12,1),sum(table12))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("lateral edge type",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/lateral edge type.csv", row.names=FALSE)

#Flake termination
table13 <- table(data1$new_flake_id, data1$flaketerm)
tabFLAKETERM=cbind(addmargins(round(prop.table(addmargins(table13),1),2)*100,2), 
                   c(margin.table(table13,1),sum(table13))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("flake termination",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/flake termination.csv", row.names=FALSE)

#Kombewa
table14 <- table(data1$new_flake_id, data1$kombewa)
tabKOMB=cbind(addmargins(round(prop.table(addmargins(table14),1),2)*100,2), 
              c(margin.table(table14,1),sum(table14))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("kombewa",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/kombewa.csv", row.names=FALSE)

#Distal PLan form
table15 <- table(data1$new_flake_id, data1$distplanform)
tabDISTPLANFORM=cbind(addmargins(round(prop.table(addmargins(table15),1),2)*100,2), 
                      c(margin.table(table15,1),sum(table15))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("distal plan form",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/distal plan form.csv", row.names=FALSE)

#Reduction System
table16 <- table(data1$new_flake_id, data1$red_syst)
tabREDSYST=cbind(addmargins(round(prop.table(addmargins(table16),1),2)*100,2), 
                 c(margin.table(table16,1),sum(table16))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("reduction system",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/reduction system.csv", row.names=FALSE)

#flake form
table17 <- table(data1$new_flake_id, data1$flk_form)
tabFLKFORM=cbind(addmargins(round(prop.table(addmargins(table17),1),2)*100,2), 
                 c(margin.table(table17,1),sum(table17))) %>%
   data.frame %>%
   rownames_to_column() %>%
   rename_at(ncol(.), ~"N Analysts") %>%
   rename("New Flake ID"=rowname) %>%
   mutate(variable=as.factor(rep("flake form",times=length("New Flake ID")))) %>%
   slice(1:(n() - 1)) %>%
   write.csv("Supplementary tables/flake form.csv", row.names=FALSE)

############################################### ######################
#######SUMMARY TABLES FOR COUNT AND CONTINUOUS DATA#######
##############################################################

#dorsal scar counts
summary(data1$dorsal_scar_count)
data1$dorsal_scar_count <- as.factor(data1$dorsal_scar_count)

table18 <- table(data1$cond_flake_id, data1$dorsal_scar_count)
tabDORSSCARCOUNT=cbind(addmargins(round(prop.table(addmargins(table18,1),1),2)*100,2), c(margin.table(table18,1),sum(table18)))
write.csv(tabDORSSCARCOUNT, file="Tables/tabledorsscars.csv")

#proximal scar counts
summary(data1$proximal_scars)
data1$proximal_scars <- as.factor(data1$proximal_scars)

table19 <- table(data1$cond_flake_id, data1$proximal_scars)
tabPROXSCARS=cbind(addmargins(round(prop.table(addmargins(table19,1),1),2)*100,2),
                   c(margin.table(table19,1),sum(table19)))
write.csv(tabPROXSCARS, file="Tables/tableproxscars.csv")

#left scar counts
summary(data1$left_scars)
data1$left_scars <- as.factor(data1$left_scars)

table20 <- table(data1$cond_flake_id, data1$left_scars)
tabLEFTSCARS=cbind(addmargins(round(prop.table(addmargins(table20,1),1),2)*100,2),
                   c(margin.table(table20,1),sum(table20)))
write.csv(tabLEFTSCARS, file="Tables/tableleftscars.csv")

#distal scar counts
summary(data1$distal_scars)
data1$distal_scars <- as.factor(data1$distal_scars)

table21 <- table(data1$cond_flake_id, data1$distal_scars)
tabDISTSCARS=cbind(addmargins(round(prop.table(addmargins(table21,1),1),2)*100,2),
                   c(margin.table(table21,1),sum(table21)))
write.csv(tabDISTSCARS, file="Tables/tabledistscars.csv")

#right scar counts
summary(data1$right_scars)
data1$right_scars <- as.factor(data1$right_scars)

table22 <- table(data1$cond_flake_id, data1$right_scars)
tabRIGHTSCARS=cbind(addmargins(round(prop.table(addmargins(table22,1),1),2)*100,2),
                   c(margin.table(table22,1),sum(table22)))
write.csv(tabRIGHTSCARS, file="Tables/tablerightscars.csv")

##numerical variables
#cleaning data

tbl1 <-  data1 %>%
      tbl_summary(
            include = c(mass, maximumdimension, maximumwidth,
                        maximumthickness,
                  ),
            statistic = all_continuous() ~ "{mean} ({sd}) - {median} [{min} - {max}]",
            by = cond_flake_id,
            digits = all_continuous() ~ 1,
            missing = "always",
            missing_text = "NAs"
      )%>%
      italicize_levels()

tbl1

as_gt(tbl1)


tbl2 <-  data1 %>%
      tbl_summary(
            include = c(techlength, techmaxwidth, techmaxthickness,
                        techwidthprox, techwidthmes, techwidthdist,
                        techthickprox, techthickmes, techthickdist
            ),
            statistic = all_continuous() ~ "{mean} ({sd}) - {median} [{min} - {max}]",
            by = cond_flake_id,
            digits = all_continuous() ~ 1,
            missing = "always",
            missing_text = "NAs"
      )



as_gt(tbl2)
tbl2

tbl3 <-
 data1 %>%
      tbl_summary(
            include = c(platfwidth, platfthickimpact, platfthickmax,
                        platfthickmid, edgeplatf, angle_height
            ),
            statistic = all_continuous() ~ "{mean} ({sd}) - {median} [{min} - {max}]",
            by = cond_flake_id,
            digits = all_continuous() ~ 1,
            missing = "always",
            missing_text = "NAs"
      )



as_gt(tbl3)
tbl3

#### Summary data with CV ####

round_df <- function(x, digits) {
   # round all numeric variables
   # x: data frame
   # digits: number of digits to round
   numeric_columns <- sapply(x, mode) == 'numeric'
   x[numeric_columns] <-  round(x[numeric_columns], digits)
   x
}

flake_measurements_summary<- comsafrica_data_complete %>%
   select(c(flake_id,new_flake_id,assemblage_code,dorsal_cortex,mass,maximumdimension,maximumwidth,maximumthickness,techlength,techmaxwidth,techmaxthickness,
            techwidthprox,techwidthmes,techwidthdist,techthickprox,techthickmes,techthickdist,
            platfwidth,platfthickimpact,platfthickmid,platfthickmax)) %>%
   filter(!new_flake_id %in% c(1,89,97)) %>%
   mutate(across(c(5:21), na_if, 0)) %>%
   pivot_longer(!new_flake_id &!assemblage_code &!flake_id,
      names_to = "variable",
      values_to = "value") %>%
   drop_na(value) %>%
   group_by(new_flake_id,variable) %>%
   mutate(cv=cv(value, na.rm=T),
          mean=mean(value, na.rm=T),
          sd=sd(value, na.rm=T),
          min=min(value, na.rm=T),
          max=max(value, na.rm=T),
          median=median(value, na.rm=T),
          range=max-min)  %>%
   distinct(flake_id,new_flake_id,assemblage_code,
            variable,cv,mean,sd,min,max,median,range) %>%
   mutate_at(vars(cv, mean,sd,min,max,median,range), funs(round(., 2))) %>%
   rename("new flake id"=new_flake_id,
          "original flake id"=flake_id) %>%
   na.omit()

# summarize cont vars to see how far individuals ranged from average measurements

individuals_summary<- comsafrica_data_complete %>%
   select(c(analyst_id,new_flake_id,assemblage_code,dorsal_cortex,mass,maximumdimension,maximumwidth,maximumthickness,techlength,techmaxwidth,techmaxthickness,
            techwidthprox,techwidthmes,techwidthdist,techthickprox,techthickmes,techthickdist,
            platfwidth,platfthickimpact,platfthickmid,platfthickmax)) %>%
   filter(!new_flake_id %in% c(1,89,97)) %>%
   mutate(across(c(5:21), na_if, 0)) %>%
   pivot_longer(!new_flake_id &!assemblage_code &!analyst_id,
                names_to = "variable",
                values_to = "value") %>%
   drop_na(value) %>%
   group_by(new_flake_id,variable) %>%
   mutate(mean=mean(value, na.rm=T),
          analyst_to_mean=abs((mean-value)/mean)*100) %>%
   group_by(analyst_id,variable) %>%
   mutate(mean_analyst_to_mean=mean(analyst_to_mean, na.rm=T)) %>%
   distinct(analyst_id,variable,mean_analyst_to_mean) %>%
   mutate_at(vars(mean_analyst_to_mean), funs(round(., 2))) %>%
   na.omit()

#summarize individuals overall
individuals_overall_summary<- comsafrica_data_complete %>%
   select(c(analyst_id,new_flake_id,assemblage_code,dorsal_cortex,mass,maximumdimension,maximumwidth,maximumthickness,techlength,techmaxwidth,techmaxthickness,
            techwidthprox,techwidthmes,techwidthdist,techthickprox,techthickmes,techthickdist,
            platfwidth,platfthickimpact,platfthickmid,platfthickmax)) %>%
   filter(!new_flake_id %in% c(1,89,97)) %>%
   mutate(across(c(5:21), na_if, 0)) %>%
   pivot_longer(!new_flake_id &!assemblage_code &!analyst_id,
                names_to = "variable",
                values_to = "value") %>%
   drop_na(value) %>%
   group_by(new_flake_id,variable) %>%
   mutate(mean=mean(value, na.rm=T),
          analyst_to_mean=abs((mean-value)/mean)*100) %>%
   group_by(analyst_id,variable) %>%
   mutate(mean_analyst_to_mean=mean(analyst_to_mean, na.rm=T)) %>%
   group_by(analyst_id) %>%
   mutate(mean_analyst_to_mean_overall=mean(mean_analyst_to_mean, na.rm=T)) %>%
   distinct(analyst_id,mean_analyst_to_mean_overall) %>%
   mutate_at(vars(mean_analyst_to_mean_overall), funs(round(., 2))) %>%
   na.omit()

#
range_summary<-flake_measurements_summary %>%
   select(c(variable,mean,sd,range)) %>%
   group_by(variable) %>%
   mutate(sd_mean=mean(sd, na.rm=T),
          mean_mean=mean(mean, na.rm=T),
          cv_mean=(sd_mean/mean_mean*100),
          range_mean=mean(range, na.rm=T)) %>%
   distinct(sd_mean, .keep_all=T) %>%
   mutate_at(vars(cv_mean,sd_mean,range_mean), funs(round(., 2))) %>%
   select(variable,range_mean,sd_mean,cv_mean) %>%
   arrange(cv_mean)

#how to IRR and CV values compare
range_irr_comparisons<-cbind(range_summary,
                             irr_cont_data_complete_flakeid,by="variable")
 
write_csv(flake_measurements_summary,"flake_summary_measures.csv")

## Visualize average measurement SD values
ggplot(data=filter(range_summary,!variable=="dorsal_cortex"),
       aes(y=cv_mean, x=reorder(variable,cv_mean))) +
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10)) +
   ylab(bquote("Average coefficient of variation")) +
   xlab("") +
   theme(axis.title = element_text(face="bold"),
         axis.text.x = element_text(face = "bold"))+
   geom_hline(yintercept = mean(range_summary$cv_mean), color="red")+
   geom_hline(yintercept = mean(range_summary$cv_mean)-sd(range_summary$cv_mean), 
              color="blue",linetype='dotted')+
   geom_hline(yintercept = mean(range_summary$cv_mean)+sd(range_summary$cv_mean), 
              color="blue",linetype='dotted')

#####Follow up questions####

## Does experience impact performance?

individuals_combined<-merge(individuals_summary, individuals,
                            by="analyst_id")

individuals_overall_combined<-merge(individuals_overall_summary, individuals,
                            by="analyst_id") %>%
   mutate(combined_training_years=(training_quant/training_chaine_op))

library("PerformanceAnalytics")
my_data <- individuals[,c(2:8)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

#overall
library(performance)
model_1<-lm(mean_analyst_to_mean_overall~training_quant,
   data=individuals_overall_combined,
   subset = mean_analyst_to_mean_overall <11)
summary(model_1)
avPlots(model_1)
check_model(model_1)

model_2<-lm(mean_analyst_to_mean_overall~training_chaine_op,
            data=individuals_overall_combined,
            subset = mean_analyst_to_mean_overall <11)
summary(model_2)
avPlots(model_2)
check_model(model_2)


model_3<-lm(mean_analyst_to_mean_overall~years_experience,
            data=individuals_overall_combined,
            subset = mean_analyst_to_mean_overall <11)
summary(model_3)
avPlots(model_2)
check_model(model_2)

model_4<-lm(mean_analyst_to_mean_overall~years_experience,
            data=individuals_overall_combined,
            subset = mean_analyst_to_mean_overall <11 & training_quant<5)
summary(model_4)
check_model(model_4)

plot_1<-ggplot(subset(individuals_overall_combined,mean_analyst_to_mean_overall<11),
       aes(y=mean_analyst_to_mean_overall,x=training_quant))+
   geom_point() +
   geom_smooth(method="lm") +
   xlab("Quantitative training\n(1=lowest, 5=highest)") + 
   ylab("Average distance from mean measures")+
   theme(axis.title = element_text(face="bold"),
         axis.text.x = element_text(face = "bold"))

plot_2<-ggplot(subset(individuals_overall_combined,mean_analyst_to_mean_overall<11),
       aes(y=mean_analyst_to_mean_overall,x=training_chaine_op))+
   geom_point() +
   geom_smooth(method="lm") +
   xlab("Chaîne opératoire training\n(1=lowest, 5=highest)") + 
   ylab("Average distance from mean measures")+
   theme(axis.title = element_text(face="bold"),
         axis.text.x = element_text(face = "bold"))

library(ggpubr)
ggarrange(plot_1, plot_2,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 1)

#dorsal_cortex
model_1<-lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="dorsal_cortex")
summary(model_1)
model_1_data<-data.frame(summary(model_1)$coefficients) %>%
   slice(-1) %>%
   rownames_to_column() %>%
   rename(variable=rowname) %>%
   mutate(model=rep("dorsal_cortex",times=length(variable)))

#maximumdimension
model_2<-lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="maximumdimension")
summary(model_2)

#maximumthickness
model_3<-lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="maximumthickness")
summary(model_3)
model_3_data<-data.frame(summary(model_3)$coefficients) %>%
   slice(-1) %>%
   rownames_to_column() %>%
   rename(variable=rowname) %>%
   mutate(model=rep("maximumthickness",times=length(variable)))

#maximumwidth
model_4<-lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="maximumwidth")
summary(model_4)

#platfthickimpact
model_5<-lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="platfthickimpact")
summary(model_5)

#platfthickmax
model_6<-lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="platfthickmax")
summary(model_6)

#platfthickmid
model_7<-lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="platfthickmid")
summary(model_7)

#platfwidth
model_8<-lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="platfwidth")
summary(model_8)

#techlength
model_9<-lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="techlength")
summary(model_9)

#techmaxthickness
summary(lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="techmaxthickness"))

#techmaxwidth
summary(lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="techmaxwidth"))

#techthickdist
summary(lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="techthickdist"))

#techthickmes
summary(lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="techthickmes"))

#techthickprox
summary(lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="techthickprox"))

#techwidthdist
model_4<-lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="techwidthdist")
model_4_data<-data.frame(summary(model_4)$coefficients) %>%
   slice(-1) %>%
   rownames_to_column() %>%
   rename(variable=rowname) %>%
   mutate(model=rep("techwidthdist",times=length(variable)))

#techwidthmes
summary(lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="techwidthmes"))

#techwidthprox
model_5<-lm(mean_analyst_to_mean~years_experience+training_quant,
           data=individuals_combined,
           subset=variable=="techwidthprox")
model_5_data<-data.frame(summary(model_5)$coefficients) %>%
   slice(-1) %>%
   rownames_to_column() %>%
   rename(variable=rowname) %>%
   mutate(model=rep("techwidthprox",times=length(variable)))

# merge regression tables together for significant experience results

sig_experience_results<-rbind(model_1_data,model_3_data,
                              model_4_data, model_5_data) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
   rename(est=Estimate,
          "std error"=Std..Error,
          t_value=t.value,
          p_value=Pr...t..) %>%
   select(c("model","variable","est","std error","t_value","p_value")) %>%
   filter(!p_value>0.05)
   
## check if factor levels determine rater reliability

var_names<-c("reduction_system","flake_form","completeness","platform_cortex",
             "scar_directionality", "platform_morphology", "platform_lipping",
             "bulb","shattered_bulb", "initiation","ventral_plan_form","cross_section_shape",
             "lateral_edge_shape","flake_termination","Kombewa","distal_plan_form")

reduction_system<-7
flake_form<-2
completeness<-8
platform_cortex<-4
scar_directionality<-10
platform_morphology<-10
platform_lipping<-3
bulb<-2
shattered_bulb<-2
initiation<-3
ventral_plan_form<-5
cross_section_shape<-6
lateral_edge_shape<-8
flake_termination<-6
Kombewa<-3
distal_plan_form<-4

level_counts<-data.frame(rbind(reduction_system,flake_form,completeness,platform_cortex,
                       scar_directionality,platform_morphology,platform_lipping,bulb,
                       shattered_bulb,initiation,ventral_plan_form,cross_section_shape,
                       lateral_edge_shape,flake_termination,Kombewa,distal_plan_form)) %>%
   rownames_to_column() %>%
   rename("factor_levels"=rbind.reduction_system..flake_form..completeness..platform_cortex..,
          var_names=rowname)
   
test_2<-merge(gwet_data_merged,level_counts, by="var_names")

ggplot(subset(test_2, !var_names=="reduction_system"), aes(x=factor_levels, y=coeff.val,label=var_names))+
   geom_point(size=3)+
   labs(title="", x ="Number of variable states", y = "AC1")+ 
   guides(color=guide_legend(title="Variable names")) +
   theme_classic()+ 
   scale_x_continuous(breaks=seq(0,10,1)) +
   geom_smooth(method="lm")+
   geom_text(aes(label=var_names),hjust=0,vjust=1.5)

#test_2_subset<-subset(test_2, !factor_levels>6.9 | !coeff.val>0.8)
test_2_subset_2<-subset(test_2, !var_names=="reduction_system")
summary(glm(factor_levels~coeff.val,data=test_2_subset_2,family="poisson"))

# Dorsal scars by section vs. total

dorsal_scars<-comsafrica_data_count_data %>%
   mutate(sum_scars=left_scars+right_scars+distal_scars+proximal_scars,
          difference_scars=sum_scars-dorsal_scar_count) %>%
   select(c(analyst_id,new_flake_id,assemblage_code,sum_scars,dorsal_scar_count,difference_scars))

summary(lm(sum_scars~dorsal_scar_count, data=dorsal_scars))

ggplot(dorsal_scars, aes(x=sum_scars, y=dorsal_scar_count))+
   geom_point()

## do images impact measurement repeatability?

categorical_images<-read.csv("images_irr_categorical.csv",stringsAsFactors=TRUE)
continuous_images<-read.csv("images_irr_measurements_count.csv",stringsAsFactors=TRUE)

# plot categorical
ggplot(categorical_images,aes(x=as.factor(image), y=irr))+
   geom_boxplot() +
   geom_jitter(size=2) +
   scale_x_discrete(name="",labels = c('Image absent','Image present')) +
   scale_y_continuous(name="Inter-rater reliability score")

summary(aov(image~irr, data=categorical_images))

# plot continuous
ggplot(continuous_images,aes(x=as.factor(image), y=irr))+
   geom_boxplot(outlier.size = 0, outlier.colour = "white") +
   geom_jitter(size=3, aes(shape=data.class)) +
   scale_x_discrete(name="",labels = c('Image absent','Image present')) +
   scale_y_continuous(name="Inter-rater reliability score")+
   theme(legend.title=element_blank())

summary(aov(image~irr, data=continuous_images))

## how do technological features impact measurement variance? ##

test_data<-new_comsafrica_data %>%
   distinct(new_flake_id,.keep_all=T)

features_variance_data<-merge(test_data[c("new_flake_id","platfmorph","latedgetype",
                                          "ventr_plane_form","platform_cortex",
                                          "distplanform","flaketerm")], 
                              flake_measurements_summary[c("new_flake_id","variable","sd")], 
                              by="new_flake_id")

# platform morphology and platform measurements

features_variance_data_platform<-features_variance_data %>%
   filter(variable == "platfwidth")
aov_1<-aov(sd~platfmorph,data=features_variance_data_platform)
as.data.frame(tidy(TukeyHSD(aov_1))) %>% 
   filter(adj.p.value < .05)

features_variance_data_platform<-features_variance_data %>%
   filter(variable == "platfthickimpact")
aov_2<-aov(sd~platfmorph,data=features_variance_data_platform)
as.data.frame(tidy(TukeyHSD(aov_2))) %>% 
   filter(adj.p.value < .05)

features_variance_data_platform<-features_variance_data %>%
   filter(variable == "platfthickmid")
aov_3<-aov(sd~platfmorph,data=features_variance_data_platform)
b<-as.data.frame(tidy(TukeyHSD(aov_3))) %>% 
   filter(adj.p.value < .05)

features_variance_data_platform<-features_variance_data %>%
   filter(variable == "platfthickmax")
aov_4<-aov(sd~platfmorph,data=features_variance_data_platform)
as.data.frame(tidy(TukeyHSD(aov_4))) %>% 
   filter(adj.p.value < .05)

# platform cortex and platform measurements
features_variance_data_platform<-features_variance_data %>%
   filter(variable == "platfwidth")
aov_5<-aov(sd~platform_cortex,data=features_variance_data_platform)
c<-as.data.frame(tidy(TukeyHSD(aov_5))) %>% 
   filter(adj.p.value < .05)

features_variance_data_platform<-features_variance_data %>%
   filter(variable == "platfthickimpact")
aov_6<-aov(sd~platform_cortex,data=features_variance_data_platform)
as.data.frame(tidy(TukeyHSD(aov_6))) %>% 
   filter(adj.p.value < .05)

features_variance_data_platform<-features_variance_data %>%
   filter(variable == "platfthickmid")
aov_7<-aov(sd~platform_cortex,data=features_variance_data_platform)
d<-as.data.frame(tidy(TukeyHSD(aov_7))) %>% 
   filter(adj.p.value < .05)

features_variance_data_platform<-features_variance_data %>%
   filter(variable == "platfthickmax")
aov_8<-aov(sd~platform_cortex,data=features_variance_data_platform)
e<-as.data.frame(tidy(TukeyHSD(aov_8))) %>% 
   filter(adj.p.value < .05)

# lateral edge type and tech measurements
features_variance_data_edge<-features_variance_data %>%
   filter(variable == "techlength")
aov_9<-aov(sd~latedgetype,data=features_variance_data_edge)
as.data.frame(tidy(TukeyHSD(aov_9))) %>% 
   filter(adj.p.value < .05)

features_variance_data_edge<-features_variance_data %>%
   filter(variable == "techwidthdist")
aov_10<-aov(sd~latedgetype,data=features_variance_data_edge)
as.data.frame(tidy(TukeyHSD(aov_10))) %>% 
   filter(adj.p.value < .05)

features_variance_data_edge<-features_variance_data %>%
   filter(variable == "techwidthmes")
aov_11<-aov(sd~latedgetype,data=features_variance_data_edge)
f<-as.data.frame(tidy(TukeyHSD(aov_11))) %>% 
   filter(adj.p.value < .05)

features_variance_data_edge<-features_variance_data %>%
   filter(variable == "techwidthprox")
aov_12<-aov(sd~latedgetype,data=features_variance_data_edge)
g<-as.data.frame(tidy(TukeyHSD(aov_12))) %>% 
   filter(adj.p.value < .05)

features_variance_data_edge<-features_variance_data %>%
   filter(variable == "techwidthprox")
aov_12<-aov(sd~latedgetype,data=features_variance_data_edge)
h<-as.data.frame(tidy(TukeyHSD(aov_12))) %>% 
   filter(adj.p.value < .05)

features_variance_data_edge<-features_variance_data %>%
   filter(variable == "techmaxwidth")
aov_13<-aov(sd~latedgetype,data=features_variance_data_edge)
as.data.frame(tidy(TukeyHSD(aov_13))) %>% 
   filter(adj.p.value < .05)

features_variance_data_edge<-features_variance_data %>%
   filter(variable == "maximumwidth")
aov_13a<-aov(sd~latedgetype,data=features_variance_data_edge)
as.data.frame(tidy(TukeyHSD(aov_13a))) %>% 
   filter(adj.p.value < .05)

# ventral plane form and thickness measurements

features_variance_data_thick<-features_variance_data %>%
   filter(variable == "maximumthickness")
aov_14<-aov(sd~ventr_plane_form,data=features_variance_data_thick)
i<-as.data.frame(tidy(TukeyHSD(aov_14))) %>% 
   filter(adj.p.value < .05)

features_variance_data_thick<-features_variance_data %>%
   filter(variable == "techmaxthickness")
aov_15<-aov(sd~ventr_plane_form,data=features_variance_data_thick)
j<-as.data.frame(tidy(TukeyHSD(aov_15))) %>% 
   filter(adj.p.value < .05)

features_variance_data_thick<-features_variance_data %>%
   filter(variable == "techthickdist")
aov_16<-aov(sd~ventr_plane_form,data=features_variance_data_thick)
as.data.frame(tidy(TukeyHSD(aov_16))) %>% 
   filter(adj.p.value < .05)

features_variance_data_thick<-features_variance_data %>%
   filter(variable == "techthickmes")
aov_17<-aov(sd~ventr_plane_form,data=features_variance_data_thick)
as.data.frame(tidy(TukeyHSD(aov_17))) %>% 
   filter(adj.p.value < .05)

features_variance_data_thick<-features_variance_data %>%
   filter(variable == "techthickprox")
aov_18<-aov(sd~ventr_plane_form,data=features_variance_data_thick)
as.data.frame(tidy(TukeyHSD(aov_18))) %>% 
   filter(adj.p.value < .05)

# flake termination and distal measurements

features_variance_data_dist<-features_variance_data %>%
   filter(variable == "techthickdist")
aov_19<-aov(sd~flaketerm,data=features_variance_data_dist)
k<-as.data.frame(tidy(TukeyHSD(aov_19))) %>% 
   filter(adj.p.value < .05)

features_variance_data_dist<-features_variance_data %>%
   filter(variable == "techwidthdist")
aov_20<-aov(sd~flaketerm,data=features_variance_data_dist)
as.data.frame(tidy(TukeyHSD(aov_20))) %>% 
   filter(adj.p.value < .05)

# distal plan form and distal measurements

features_variance_data_dist<-features_variance_data %>%
   filter(variable == "techthickdist")
aov_21<-aov(sd~distplanform,data=features_variance_data_dist)
as.data.frame(tidy(TukeyHSD(aov_21))) %>% 
   filter(adj.p.value < .05)

features_variance_data_dist<-features_variance_data %>%
   filter(variable == "techwidthdist")
aov_22<-aov(sd~distplanform,data=features_variance_data_dist)
as.data.frame(tidy(TukeyHSD(aov_22))) %>% 
   filter(adj.p.value < .05)

# combined influences table

combined_variance_attribute_data<-data.frame(rbind(a,b,c,d,e,f,g,h,i,j,k)) %>%
   rename("Variable"=term,
          "Comparison"=contrast) %>%
   select(c(Variable, Comparison, estimate, conf.low, conf.high, adj.p.value)) %>%
   mutate_at(vars(estimate,conf.low,conf.high), funs(round(., 2))) %>%
   mutate_at(vars(adj.p.value), funs(round(., 3)))

write.csv(combined_variance_attribute_data,"combined_variance_attribute_data.csv")





