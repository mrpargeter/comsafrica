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

##############################
# Set working directory and load datafile
##############################

# Set to source directory with relevant .csv datafile provided
# through the Open Science repository
getwd()
setwd("/Volumes/GoogleDrive/My Drive/Projects/CoMSAfrica/Geneva/Data analysis/CoMSA_stats")

## Add datasets

comsafrica_data<-read.csv("comsafrica_complete_adjusted.csv",stringsAsFactors=TRUE) %>%
   subset(!flake_id == "NONR3495") #filter out this non numbered flake

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

# remove 4 extra flake IDs that have crept into the analysis since we had
# the last COMSAFRICA meeting

new_comsafrica_data <- new_comsafrica_data %>%
      filter(!new_flake_id %in% c(101:106))

##### Inter rater data analyses

comsafrica_data_complete<-new_comsafrica_data %>%
   select(c(assemblage_code,analyst_id,analysis_order,flake_id,new_flake_id,proximal_scars,left_scars,distal_scars,right_scars,
            dorsal_cortex,mass,maximumdimension,maximumwidth,maximumthickness,techlength,techmaxwidth,techmaxthickness,
            techwidthprox,techwidthmes,techwidthdist,techthickprox,techthickmes,techthickdist,
            platfwidth,platfthickimpact,platfthickmid,platfthickmax,edgeplatf,angle_height))

### repeatability coefficients for continuous (Gaussian) data ####

# cortex-NOT SURE WHAT TO DO WITH CORTEX-STRANGE DISTRIBUTION?
hist(log(comsafrica_data_complete$dorsal_cortex))
set.seed(50)
comsafrica_cortex_boot<-rpt(dorsal_cortex ~ new_flake_id*analysis_order + (1 | new_flake_id),
                            grname = c("new_flake_id","Fixed"),
                            data = comsafrica_data_complete,
                            datatype = "Gaussian",
                            nboot = 1000, npermut = 100)
print(comsafrica_cortex_boot)

#fixed effects (analysis order) alone explain almost none of the variance in the response variable

# maxdim
hist(comsafrica_data_complete$maximumdimension)
set.seed(50)
comsafrica_maxdim_boot<-rpt(maximumdimension ~ new_flake_id*analysis_order + (1 | new_flake_id),
                  grname = c("new_flake_id","Fixed"),
                  data = comsafrica_data_complete,
                  datatype = "Gaussian",
                  nboot = 1000, npermut = 100)
print(comsafrica_maxdim_boot)

#mass
hist(log(comsafrica_data_complete$mass))
set.seed(50)
comsafrica_mass<-rpt(log(mass) ~ new_flake_id*analysis_order + (1 | new_flake_id),
                       grname = c("new_flake_id","Fixed"),
                       data = filter(comsafrica_data_complete,mass>0),
                       datatype = "Gaussian",
                       nboot = 1000, npermut = 100)
print(comsafrica_mass)

#flake width
set.seed(50)
comsafrica_maxwidth<-rpt(maximumwidth ~ new_flake_id*analysis_order + (1 | new_flake_id),
                     grname = c("new_flake_id","Fixed"),
                     data = comsafrica_data_complete,
                     datatype = "Gaussian",
                     nboot = 1000, npermut = 100)
print(comsafrica_maxwidth)

#flake max thickness
set.seed(50)
comsafrica_maxthick<-rpt(maximumthickness ~ new_flake_id*analysis_order + (1 | new_flake_id),
                         grname = c("new_flake_id","Fixed"),
                         data = comsafrica_data_complete,
                         datatype = "Gaussian",
                         nboot = 1000, npermut = 100)
print(comsafrica_maxthick)

#flake tech length
set.seed(50)
comsafrica_techlength<-rpt(techlength ~ new_flake_id*analysis_order + (1 | new_flake_id),
                         grname = c("new_flake_id","Fixed"),
                         data = comsafrica_data_complete,
                         datatype = "Gaussian",
                         nboot = 1000, npermut = 100)
print(comsafrica_techlength)

#flake tech max width
set.seed(50)
comsafrica_techmaxwidth<-rpt(techmaxwidth ~ new_flake_id*analysis_order + (1 | new_flake_id),
                           grname = c("new_flake_id","Fixed"),
                           data = comsafrica_data_complete,
                           datatype = "Gaussian",
                           nboot = 1000, npermut = 100)
print(comsafrica_techmaxwidth)

#flake tech max thickness
set.seed(50)
comsafrica_techmaxthick<-rpt(techmaxthickness ~ new_flake_id*analysis_order + (1 | new_flake_id),
                             grname = c("new_flake_id","Fixed"),
                             data = comsafrica_data_complete,
                             datatype = "Gaussian",
                             nboot = 1000, npermut = 100)
print(comsafrica_techmaxthick)

#flake tech width prox
set.seed(50)
comsafrica_techwidthprox<-rpt(techwidthprox ~ new_flake_id*analysis_order + (1 | new_flake_id),
                             grname = c("new_flake_id","Fixed"),
                             data = comsafrica_data_complete,
                             datatype = "Gaussian",
                             nboot = 1000, npermut = 100)
print(comsafrica_techwidthprox)

#flake tech width mes
set.seed(50)
comsafrica_techwidthmes<-rpt(techwidthmes ~ new_flake_id*analysis_order + (1 | new_flake_id),
                              grname = c("new_flake_id","Fixed"),
                              data = comsafrica_data_complete,
                              datatype = "Gaussian",
                              nboot = 1000, npermut = 100)
print(comsafrica_techwidthmes)

#flake tech width dist
set.seed(50)
comsafrica_techwidthdist<-rpt(techwidthdist ~ new_flake_id*analysis_order + (1 | new_flake_id),
                             grname = c("new_flake_id","Fixed"),
                             data = comsafrica_data_complete,
                             datatype = "Gaussian",
                             nboot = 1000, npermut = 100)
summary(comsafrica_techwidthdist)
print(comsafrica_techwidthdist)

#flake tech thick prox
tech_thick_prox_data<-comsafrica_data_complete %>%
   filter(!new_flake_id %in% c(27,68,10))

set.seed(50)
comsafrica_techtechthickprox<-rpt(techthickprox ~ new_flake_id*analysis_order + (1 | new_flake_id),
                              grname = c("new_flake_id","Fixed"),
                              data = comsafrica_data_complete,
                              datatype = "Gaussian",
                              nboot = 1000, npermut = 100)
summary(comsafrica_techtechthickprox)
print(comsafrica_techtechthickprox)

#flake tech thick med
set.seed(50)
comsafrica_techtechthickmes<-rpt(techthickmes ~ new_flake_id*analysis_order + (1 | new_flake_id),
                                  grname = c("new_flake_id","Fixed"),
                                  data = comsafrica_data_complete,
                                  datatype = "Gaussian",
                                  nboot = 1000, npermut = 100)
summary(comsafrica_techtechthickmes)
print(comsafrica_techtechthickmes)

#flake tech thick dist
set.seed(50)
comsafrica_techthickdist<-rpt(techthickdist ~ new_flake_id*analysis_order + (1 | new_flake_id),
                                 grname = c("new_flake_id","Fixed"),
                                 data = comsafrica_data_complete,
                                 datatype = "Gaussian",
                                 nboot = 1000, npermut = 100)
summary(comsafrica_techthickdist)
print(comsafrica_techthickdist)

#flake platform width
set.seed(50)
comsafrica_platfwidth<-rpt(platfwidth ~ new_flake_id*analysis_order + (1 | new_flake_id),
                              grname = c("new_flake_id","Fixed"),
                              data = comsafrica_data_complete,
                              datatype = "Gaussian",
                              nboot = 1000, npermut = 100)
summary(comsafrica_platfwidth)
print(comsafrica_platfwidth)

#flake platform thickness impact
set.seed(50)
comsafrica_platfthicimpact<-rpt(platfthickimpact ~ new_flake_id*analysis_order + (1 | new_flake_id),
                           grname = c("new_flake_id","Fixed"),
                           data = comsafrica_data_complete,
                           datatype = "Gaussian",
                           nboot = 1000, npermut = 100)
summary(comsafrica_platfthicimpact)
print(comsafrica_platfthicimpact)

#flake platform thickness mid point
set.seed(50)
comsafrica_platfthickmid<-rpt(platfthickmid ~ new_flake_id*analysis_order + (1 | new_flake_id),
                                grname = c("new_flake_id","Fixed"),
                                data = comsafrica_data_complete,
                                datatype = "Gaussian",
                                nboot = 1000, npermut = 100)
summary(comsafrica_platfthickmid)
print(comsafrica_platfthickmid)

#flake platform thickness
set.seed(50)
comsafrica_platfthickmax<-rpt(platfthickmax ~ new_flake_id*analysis_order + (1 | new_flake_id),
                           grname = c("new_flake_id","Fixed"),
                           data = comsafrica_data_complete,
                           datatype = "Gaussian",
                           nboot = 1000, npermut = 100)
summary(comsafrica_platfthickmax)
print(comsafrica_platfthickmax)

#flake EPA
EPA_data<-comsafrica_data_complete %>%
   filter(!new_flake_id %in% c(53,64,82,56,54,55,60) & edgeplatf < 25)

set.seed(50)
comsafrica_edgeplatf<-rpt(edgeplatf ~ new_flake_id*analysis_order + (1 | new_flake_id),
                              grname = c("new_flake_id","Fixed"),
                              data = comsafrica_data_complete,
                              datatype = "Gaussian",
                              nboot = 1000, npermut = 100)
summary(comsafrica_edgeplatf)
print(comsafrica_edgeplatf)

#angle height
set.seed(50)
comsafrica_angle_height<-rpt(angle_height ~ new_flake_id*analysis_order + (1 | new_flake_id),
                          grname = c("new_flake_id","Fixed"),
                          data = comsafrica_data_complete,
                          datatype = "Gaussian",
                          nboot = 1000, npermut = 100)
summary(comsafrica_angle_height)
print(comsafrica_angle_height)

### repeatability coefficients for Count data ####

comsafrica_data_count_data<-new_comsafrica_data %>%
   select(c(assemblage_code,analyst_id,analysis_order,new_flake_id,proximal_scars,left_scars,distal_scars,right_scars,
            dorsal_scar_count))

# proximal_scars
hist(comsafrica_data_count_data$proximal_scars)
set.seed(50)
comsafrica_proximal_scars_boot<-rpt(proximal_scars ~ new_flake_id*analysis_order + (1 | new_flake_id),
                                    grname = c("new_flake_id","Fixed"),
                                    data = comsafrica_data_count_data,
                                    datatype = "Poisson",
                                    nboot = 100, npermut = 100)
summary(comsafrica_proximal_scars_boot)
print(comsafrica_proximal_scars_boot)

# left_scars
hist(comsafrica_data_count_data$left_scars)
set.seed(50)
comsafrica_left_scars_boot<-rpt(left_scars ~ new_flake_id*analysis_order + (1 | new_flake_id),
                                grname = c("new_flake_id","Fixed"),
                                data = comsafrica_data_count_data,
                                datatype = "Poisson",
                                nboot = 100, npermut = 100)
summary(comsafrica_left_scars_boot)
print(comsafrica_left_scars_boot)

# distal_scars
hist(comsafrica_data_count_data$distal_scars)
set.seed(50)
comsafrica_distal_scars_boot<-rpt(distal_scars ~ new_flake_id*analysis_order + (1 | new_flake_id),
                                  grname = c("new_flake_id","Fixed"),
                                  data = comsafrica_data_count_data,
                                  datatype = "Poisson",
                                  nboot = 100, npermut = 100)
summary(comsafrica_distal_scars_boot)
print(comsafrica_distal_scars_boot)

# right_scars
hist(comsafrica_data_count_data$right_scars)
set.seed(50)
comsafrica_right_scars_boot<-rpt(right_scars ~ new_flake_id*analysis_order + (1 | new_flake_id),
                                 grname = c("new_flake_id","Fixed"),
                                 data = comsafrica_data_count_data,
                                 datatype = "Poisson",
                                 nboot = 100, npermut = 100)
summary(comsafrica_right_scars_boot)
print(comsafrica_right_scars_boot)

# dorsal scar count
hist(comsafrica_data_count_data$dorsal_scar_count)
set.seed(50)
comsafrica_dorsal_scar_count_boot<-rpt(dorsal_scar_count ~ new_flake_id*analysis_order + (1 | new_flake_id),
                                       grname = c("new_flake_id","Fixed"),
                                       data = comsafrica_data_count_data,
                                       datatype = "Poisson",
                                       nboot = 100, npermut = 100)
summary(comsafrica_dorsal_scar_count_boot)
print(comsafrica_dorsal_scar_count_boot)

### repeatability coefficients for categorical data ####

comsafrica_data_cat_data<-new_comsafrica_data %>%
   select(c(assemblage_code,analyst_id,new_flake_id,completeness,platform_cortex,directionality,platfmorph,
            platflipp,bulb,shattbulb,initiation,ventr_plane_form,section,latedgetype,flaketerm,
            distplanform,kombewa,red_syst,flk_form))

comsafrica_data_cat_data<-as.data.frame(unclass(comsafrica_data_cat_data), stringsAsFactors = TRUE)

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
var_names<-c("reduction system","flake form","completeness","platform cortex",
             "scar directionality", "platform morphology", "platform lipping",
             "bulb","shattered bulb", "initiation","ventral plan form","cross section shape",
             "lateral edge shape","flake termination","Kombewa","distal plan form")

gwet_data_merged<-cbind(gwet_data,var_names) %>%
   select(c(var_names,pa,pe))

write.csv(gwet_data_merged,"categorical_summary.csv")

## check if factor levels determine rater reliability

aa<-nlevels(comsafrica_data_cat_data$red_syst)
ab<-nlevels(comsafrica_data_cat_data$flk_form)
ac<-nlevels(comsafrica_data_cat_data$completeness)
ad<-nlevels(comsafrica_data_cat_data$platform_cortex)
ae<-nlevels(comsafrica_data_cat_data$directionality)
af<-nlevels(comsafrica_data_cat_data$platfmorph)
ag<-nlevels(comsafrica_data_cat_data$platflipp)
ah<-nlevels(comsafrica_data_cat_data$bulb)
ai<-nlevels(comsafrica_data_cat_data$shattbulb)
aj<-nlevels(comsafrica_data_cat_data$initiation)
ak<-nlevels(comsafrica_data_cat_data$ventr_plane_form)
al<-nlevels(comsafrica_data_cat_data$section)
am<-nlevels(comsafrica_data_cat_data$latedgetype)
an<-nlevels(comsafrica_data_cat_data$flaketerm)
ao<-nlevels(comsafrica_data_cat_data$kombewa)
ap<-nlevels(comsafrica_data_cat_data$distplanform)

test<-data.frame(rbind(aa,ab,ac,ad,ae,af,ag,ah,ai,aj,
            ak,al,am,an,ao,ap)) %>%
   rename("factor_levels"=rbind.aa..ab..ac..ad..ae..af..ag..ah..ai..aj..ak..al..am..an..) 

test_2<-cbind(gwet_data_merged,test[,1])%>%
   rename("factor_levels"='test[, 1]') 

ggplot(test_2, aes(x=factor_levels, y=pa,label=var_names))+
   geom_point(size=3)+
   labs(title="", x ="Number of categories", y = "IRR")+ 
   guides(color=guide_legend(title="Variable names"))+ 
   geom_label_repel(aes(label = var_names),
                    box.padding   = 0.35, 
                    point.padding = 0.5,
                    segment.color = 'grey50') +
   theme_classic()+ 
   scale_x_continuous(breaks=seq(0,10,1))

summary(glm(factor_levels~pa,data=test_2,family = "poisson"))

### IRR visualizations ####

irr_summary<-read.csv("irr_summary_data.csv")

## Continuous data, analyst ID

ggplot(data=filter(irr_summary, data_class=="Continuous" & measure =="irr_analyst"),
       aes(y=irr_value, x=reorder(variable,irr_value))) +
   geom_bar(position=position_dodge(), stat="identity") +
   geom_errorbar(aes(ymin=lower, ymax=upper),
                 width=.2,                    # Width of the error bars
                 position=position_dodge(.9))+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   labs(x = "",
        y = "IRR value")

## Continuous data, order

ggplot(data=filter(irr_summary, data_class=="Continuous" & measure =="irr_order"),
       aes(y=irr_value, x=reorder(variable,irr_value))) +
   geom_bar(position=position_dodge(), stat="identity") +
   geom_errorbar(aes(ymin=lower, ymax=upper),
                 width=.2,                    # Width of the error bars
                 position=position_dodge(.9))+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10)) +
   labs(x = "",
        y = "IRR value")

## Count data, analyst ID

ggplot(data=filter(irr_summary, data_class=="Count" & measure =="irr_analyst"),
       aes(y=irr_value, x=reorder(variable,irr_value))) +
   geom_bar(position=position_dodge(), stat="identity") +
   geom_errorbar(aes(ymin=lower, ymax=upper),
                 width=.2,                    # Width of the error bars
                 position=position_dodge(.9))+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red")

## Count data, order

ggplot(data=filter(irr_summary, data_class=="Count" & measure =="irr_order"),
       aes(y=irr_value, x=reorder(variable,irr_value))) +
   geom_bar(position=position_dodge(), stat="identity") +
   geom_errorbar(aes(ymin=lower, ymax=upper),
                 width=.2,                    # Width of the error bars
                 position=position_dodge(.9))+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")

## Categorical data

ggplot(gwet_data_merged,aes(y=pa, x=reorder(var_names,pa))) +
   geom_bar(position=position_dodge(), stat="identity") +
   geom_errorbar(aes(ymin=pa-pe, ymax=pa+pe),
                 width=.2,                    # Width of the error bars
                 position=position_dodge(.9))+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red")

############################################### ######################
###### SUMMARY TABLES FOR CATEGORICAL VARIABLES ####
# CHANGE TO USING comsafrica_data_cat_data to ensure category states 
# are the same

data1 <- comsafrica_data_cat_data

# another option using tbl_summary
library(gtsummary)
data1 %>% 
   select(c(new_flake_id,completeness)) %>% 
   tbl_summary(by = new_flake_id,
               statistic = list(all_categorical() ~ "{p}%")
   ) %>%
   add_overall() %>% # add an overall column
   add_n() %>% # add column with total number of non-missing observations
   bold_labels() 

#completeness
tableCOMPLETENESS <-  table(data1$new_flake_id, data1$completeness)
tabCOMP=cbind(addmargins(round(prop.table(addmargins(tableCOMPLETENESS,1),1),2)*100,2), c(margin.table(tableCOMPLETENESS,1),sum(tableCOMPLETENESS)))
write.csv(tabCOMP, file="Tables/tablecompleteness.csv")

#damage
table2 <- table(data1$cond_flake_id, data1$damage)
tabDAM=cbind(addmargins(round(prop.table(addmargins(table2,1),1),2)*100,2), c(margin.table(table2,1),sum(table2)))
write.csv(tabDAM, file="Tables/tabledamage.csv")

#platform_cortex
table3 <- table(data1$cond_flake_id, data1$platform_cortex)
tabCORTEXPLATF=cbind(addmargins(round(prop.table(addmargins(table3,1),1),2)*100,2), c(margin.table(table3,1),sum(table3)))
write.csv(tabCORTEXPLATF, file="Tables/tablecortexplatf.csv")

#directionality
table4 <- table(data1$cond_flake_id, data1$directionality)
tabDIRECTIONALITY=cbind(addmargins(round(prop.table(addmargins(table4,1),1),2)*100,2), c(margin.table(table4,1),sum(table4)))
write.csv(tabDIRECTIONALITY, file="Tables/tabledirectionality.csv")

#platf morph
table5 <- table(data1$cond_flake_id, data1$platfmorph)
tabPLATFMORPH=cbind(addmargins(round(prop.table(addmargins(table5,1),1),2)*100,2), c(margin.table(table5,1),sum(table5)))
write.csv(tabPLATFMORPH, file="Tables/tableplatfmorph.csv")

#platf lip
table6 <- table(data1$cond_flake_id, data1$platflipp)
tabPLATFLIPP=cbind(addmargins(round(prop.table(addmargins(table6,1),1),2)*100,2), c(margin.table(table6,1),sum(table6)))
write.csv(tabPLATFLIPP, file="Tables/tableplatflipp.csv")

#bulb
table7 <- table(data1$cond_flake_id, data1$bulb)
tabBULB=cbind(addmargins(round(prop.table(addmargins(table7,1),1),2)*100,2), c(margin.table(table7,1),sum(table7)))
write.csv(tabBULB, file="Tables/tablebulb.csv")

#shattbulb
table8 <- table(data1$cond_flake_id, data1$shattbulb)
tabSHATTBULB=cbind(addmargins(round(prop.table(addmargins(table8,1),1),2)*100,2), c(margin.table(table8,1),sum(table8)))
write.csv(tabSHATTBULB, file="Tables/tableshattbulb.csv")

#initiation
table9 <- table(data1$cond_flake_id, data1$initiation)
tabINITIATION=cbind(addmargins(round(prop.table(addmargins(table9,1),1),2)*100,2), c(margin.table(table9,1),sum(table9)))
write.csv(tabINITIATION, file="Tables/tableinitiation.csv")

#ventral plane form
table10 <- table(data1$cond_flake_id, data1$ventr_plane_form)
tabVENTRPLANEFORM=cbind(addmargins(round(prop.table(addmargins(table10,1),1),2)*100,2), c(margin.table(table10,1),sum(table10)))
write.csv(tabVENTRPLANEFORM, file="Tables/tableventralplaneform.csv")

#section
table11 <- table(data1$cond_flake_id, data1$section)
tabSECTION=cbind(addmargins(round(prop.table(addmargins(table11,1),1),2)*100,2), c(margin.table(table11,1),sum(table11)))
write.csv(tabSECTION, file="Tables/tablesection.csv")

#lateral edge type
table12 <- table(data1$cond_flake_id, data1$latedgetype)
tabLATEDGE=cbind(addmargins(round(prop.table(addmargins(table12,1),1),2)*100,2), c(margin.table(table12,1),sum(table12)))
write.csv(tabLATEDGE, file="Tables/tablelatedge.csv")

#Flake termination
table13 <- table(data1$cond_flake_id, data1$flaketerm)
tabFLAKETERM=cbind(addmargins(round(prop.table(addmargins(table13,1),1),2)*100,2), c(margin.table(table13,1),sum(table13)))
write.csv(tabFLAKETERM, file="Tables/tableflaketerm.csv")

#Kombewa
table14 <- table(data1$cond_flake_id, data1$kombewa)
tabKOMB=cbind(addmargins(round(prop.table(addmargins(table14,1),1),2)*100,2), c(margin.table(table14,1),sum(table14)))
write.csv(tabKOMB, file="Tables/tablekombewa.csv")

#Distal PLan form
table15 <- table(data1$cond_flake_id, data1$distplanform)
tabDISTPLANFORM=cbind(addmargins(round(prop.table(addmargins(table15,1),1),2)*100,2), c(margin.table(table15,1),sum(table15)))
write.csv(tabDISTPLANFORM, file="Tables/tabledistplanform.csv")

#Reduction System
table16 <- table(data1$cond_flake_id, data1$red_syst)
tabREDSYST=cbind(addmargins(round(prop.table(addmargins(table16,1),1),2)*100,2), c(margin.table(table16,1),sum(table16)))
write.csv(tabREDSYST, file="Tables/tableredsyst.csv")

#flake form
table17 <- table(data1$cond_flake_id, data1$flk_form)
tabFLKFORM=cbind(addmargins(round(prop.table(addmargins(table17,1),1),2)*100,2), c(margin.table(table17,1),sum(table17)))
write.csv(tabFLKFORM, file="Tables/tableflakeform.csv")

write.csv(data1, file="comsafrica_complete_adjusted_catcleaned.csv")


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

library(gtsummary)

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
            platfwidth,platfthickimpact,platfthickmid,platfthickmax,edgeplatf)) %>%
   filter(!new_flake_id %in% c(1,89,97)) %>%
   mutate(across(c(5:21), na_if, 0)) %>%
   pivot_longer(!new_flake_id &!assemblage_code &!flake_id,
      names_to = "variable",
      values_to = "value") %>%
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
   mutate_at(vars(cv, mean,sd,min,max,median,range), funs(round(., 2)))

range_summary<-flake_measurements_summary %>%
   filter(!platfthickimpact > -Inf) %>%
   select(c(variable,range)) %>%
   group_by(variable) %>%
   mutate(range_mean=mean(range, na.rm=T),
          range_sd=sd(range,rm=T))

write_csv(flake_measurements_summary,"flake_summary_measures.csv")

## Range histograms-arranged by IRR performance (worst to best)

# edgeplatf
ggplot(data=filter(test,variable=="edgeplatf"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="EPA [raw measurement]",
       x ="Range of measurements", y = "Density")

# platfthickmid
ggplot(data=filter(test,variable=="platfthickmid"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Platform thickness mid-point",
       x ="Range of measurements", y = "Density")

# techwidthdist
ggplot(data=filter(test,variable=="techwidthdist"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Technological width distal",
       x ="Range of measurements", y = "Density")

# techthickprox
ggplot(data=filter(test,variable=="techthickprox"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Technological thickness proximal",
       x ="Range of measurements", y = "Density")

# techlength
ggplot(data=filter(test,variable=="techlength"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Technological length",
       x ="Range of measurements", y = "Density")

# platfthickmax
ggplot(data=filter(test,variable=="platfthickmax"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Platform thickness maximum",
       x ="Range of measurements", y = "Density")

# platfwidth
ggplot(data=filter(test,variable=="platfwidth"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Platform width",
       x ="Range of measurements", y = "Density")

# platfthickimpact
ggplot(data=filter(test,variable=="platfthickimpact"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Platform thickness impact",
       x ="Range of measurements", y = "Density")

# techwidthprox
ggplot(data=filter(test,variable=="techwidthprox"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Technological width proximal",
       x ="Range of measurements", y = "Density")

# techmaxthickness
ggplot(data=filter(test,variable=="techmaxthickness"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Technological max thickness",
       x ="Range of measurements", y = "Density")

# techmaxwidth
ggplot(data=filter(test,variable=="techmaxwidth"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Technological max width",
       x ="Range of measurements", y = "Density")

# techthickdist
ggplot(data=filter(test,variable=="techthickdist"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Technological thickness distal",
       x ="Range of measurements", y = "Density")

# techwidthmes
ggplot(data=filter(test,variable=="techwidthmes"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Technological width mesial",
       x ="Range of measurements", y = "Density")

# techthickmes
ggplot(data=filter(test,variable=="techthickmes"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Technological thickness mesial",
       x ="Range of measurements", y = "Density")

# maximumthickness
ggplot(data=filter(test,variable=="maximumthickness"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Maximum thickness",
       x ="Range of measurements", y = "Density")

# dorsal_cortex COME BACK TO THIS AFTER CHECKING DATA FOR WEIRD RANGE
# VALUES
ggplot(data=filter(test,variable=="dorsal_cortex"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Dorsal cortex",
       x ="Range of measurements", y = "Density")

# maximumwidth
ggplot(data=filter(test,variable=="maximumwidth"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Maximum width",
       x ="Range of measurements", y = "Density")

# Max dimension
ggplot(data=filter(test,variable=="maximumdimension"), aes(x=range)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  labs(title="Maximum dimension",
       x ="Range of measurements", y = "Density")

# mass
ggplot(data=filter(test,variable=="mass"), aes(x=range)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   labs(title="Mass",
        x ="Range of measurements", y = "Density")

### outlier detection 

## MASS
mass <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "mass",
             mean = mean(mass, na.rm = TRUE),
             sd = sd(mass, na.rm = TRUE),
             min = min(mass, na.rm=T),
             max = max(mass, na.rm=T),
             median = median(mass, na.rm=T),
             count=n(),
             count2 = sum(!is.na(mass)))

mass$maxmin <- (mass$max - mass$min)

boxplot(mass$maxmin)$out
Q <- quantile(mass$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(mass$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

massoutliers <- mass %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
massoutliers

#Flk new ID 58 --> fragmentation during transport
#Flk new ID 83 --> one abnormal entry (human error?)
#others are probably in the range of errors of a scale?

## DORSAL CORTEX
cortex <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "cortex",
             mean = mean(dorsal_cortex, na.rm = TRUE),
             sd = sd(dorsal_cortex, na.rm = TRUE),
             min = min(dorsal_cortex, na.rm=T),
             max = max(dorsal_cortex, na.rm=T),
             median = median(dorsal_cortex, na.rm=T),
             count=n(),
             count2 = sum(!is.na(dorsal_cortex)))

cortex$maxmin <- (cortex$max - cortex$min)

boxplot(cortex$maxmin)$out
Q <- quantile(cortex$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(cortex$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

cortexoutliers <- cortex %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
cortexoutliers

#New flk IDs 3, 73 and 81 have discrepancies on dorsal cortex because of what is considered 'cortex'

## MAX DIM
maxdim <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "Max Dim",
             mean = mean(maximumdimension, na.rm = TRUE),
             sd = sd(maximumdimension, na.rm = TRUE),
             min = min(maximumdimension, na.rm=T),
             max = max(maximumdimension, na.rm=T),
             median = median(maximumdimension, na.rm=T),
             count=n(),
             count2 = sum(!is.na(maximumdimension)))

maxdim$maxmin <- (maxdim$max - maxdim$min)

boxplot(maxdim$maxmin)$out
Q <- quantile(maxdim$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(maxdim$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

maxdimoutliers <- maxdim %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
maxdimoutliers

#interesting to see that most outliers are due to one analyst having a strong difference with the others
#looking at the flakes, they are of a quadrangular / squarish shape: difficulty to eyeball the location of the max dim?
#flk id 58, like for mass, the discrepancy is due to fragmentation during transport
#flk id 52 (B80), analyst 0202a human error? no mistake in flk number

## MAX WIDTH
maxwidth <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "Max Width",
             mean = mean(maximumwidth, na.rm = TRUE),
             sd = sd(maximumwidth, na.rm = TRUE),
             min = min(maximumwidth, na.rm=T),
             max = max(maximumwidth, na.rm=T),
             median = median(maximumwidth, na.rm=T),
             count=n(),
             count2 = sum(!is.na(maximumwidth)))

maxwidth$maxmin <- (maxwidth$max - maxwidth$min)

boxplot(maxwidth$maxmin)$out
Q <- quantile(maxwidth$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(maxwidth$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

maxwidthoutliers <- maxwidth %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
maxwidthoutliers

#flk id 6, wide platform --> definition problem?
#flk id 58, one value distinct from the others --> definition problem for width?
#flk id 59, wide range, probably related to quadrangular flake morphology 

## MAX THICKNESS
maxT <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "Max Thick",
             mean = mean(maximumthickness, na.rm = TRUE),
             sd = sd(maximumthickness, na.rm = TRUE),
             min = min(maximumthickness, na.rm=T),
             max = max(maximumthickness, na.rm=T),
             median = median(maximumthickness, na.rm=T),
             count=n(),
             count2 = sum(!is.na(maximumthickness)))

maxT$maxmin <- (maxT$max - maxT$min)

boxplot(maxT$maxmin)$out
Q <- quantile(maxT$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(maxT$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

maxToutliers <- maxT %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
maxToutliers

#flks 14, 51, 62, 67 are all relatively large flakes with prominent bulbs and thick platforms

## TECHL

summary(new_comsafrica_data$techlength)
techL <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "Tech Length",
             mean = mean(techlength, na.rm = TRUE),
             sd = sd(techlength, na.rm = TRUE),
             min = min(techlength, na.rm=T),
             max = max(techlength, na.rm=T),
             median = median(techlength, na.rm=T),
             count=n(),
             count2 = sum(!is.na(techlength)))


techL$maxmin <- (techL$max - techL$min)

boxplot(techL$maxmin)$out
Q <- quantile(techL$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(techL$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

techLoutliers <- techL %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
techLoutliers

#flk id 40  - one abherrant value
#flk id 58 - discrepancy due to transport
#flk id 68 - discrepancy due to clarity of definition for length / shape of flake (one side of the flake much longer than the other)
#flk id 84 - clarity of technological axis?
#flk id 91 - shape of the flake

## TECHMAXWIDTH
techmaxwidth <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "techmaxwidth",
             mean = mean(techmaxwidth, na.rm = TRUE),
             sd = sd(techmaxwidth, na.rm = TRUE),
             min = min(techmaxwidth, na.rm=T),
             max = max(techmaxwidth, na.rm=T),
             median = median(techmaxwidth, na.rm=T),
             count=n(),
             count2 = sum(!is.na(techmaxwidth)))

techmaxwidth$maxmin <- (techmaxwidth$max - techmaxwidth$min)

boxplot(techmaxwidth$maxmin)$out
Q <- quantile(techmaxwidth$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(techmaxwidth$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

techmaxwidthoutliers <- techmaxwidth %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
techmaxwidthoutliers


#flk id 6, shape flake?
#flk id 58, combination of analysis order and shape flake?

##TECHMAXTHICKNESS
techmaxthickness <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "techmaxthickness",
             mean = mean(techmaxthickness, na.rm = TRUE),
             sd = sd(techmaxthickness, na.rm = TRUE),
             min = min(techmaxthickness, na.rm=T),
             max = max(techmaxthickness, na.rm=T),
             median = median(techmaxthickness, na.rm=T),
             count=n(),
             count2 = sum(!is.na(techmaxthickness)))

techmaxthickness$maxmin <- (techmaxthickness$max - techmaxthickness$min)

boxplot(techmaxthickness$maxmin)$out
Q <- quantile(techmaxthickness$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(techmaxthickness$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

techmaxthicknessoutliers <- techmaxthickness %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
techmaxthicknessoutliers

#flk ids 12, 51, 67 no straightforward explanation. Flakes with thick platforms - drift from definition of techW

##TECHWIDTHPROX

techwidthprox <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "techwidthprox",
             mean = mean(techwidthprox, na.rm = TRUE),
             sd = sd(techwidthprox, na.rm = TRUE),
             min = min(techwidthprox, na.rm=T),
             max = max(techwidthprox, na.rm=T),
             median = median(techwidthprox, na.rm=T),
             count=n(),
             count2 = sum(!is.na(techwidthprox)))

techwidthprox$maxmin <- (techwidthprox$max - techwidthprox$min)
techwidthprox
boxplot(techwidthprox$maxmin)$out
Q <- quantile(techwidthprox$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(techwidthprox$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

techwidthproxoutliers <- techwidthprox %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
techwidthproxoutliers


#newflkid 79 - one abherrant value
# all other flakes (15, 20, 56, 59, 62, 84) --> relatively large flakes - weird shape or expanding and then converging edges


##TECHWIDTHMES
techwidthmes <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "techwidthmes",
             mean = mean(techwidthmes, na.rm = TRUE),
             sd = sd(techwidthmes, na.rm = TRUE),
             min = min(techwidthmes, na.rm=T),
             max = max(techwidthmes, na.rm=T),
             median = median(techwidthmes, na.rm=T),
             count=n(),
             count2 = sum(!is.na(techwidthmes)))

techwidthmes$maxmin <- (techwidthmes$max - techwidthmes$min)
techwidthmes
boxplot(techwidthmes$maxmin)$out
Q <- quantile(techwidthmes$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(techwidthmes$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

techwidthmesoutliers <- techwidthmes %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
techwidthmesoutliers

#id 9,59,71,84 --> flake shapes for 59 and 84?

##TECHWIDTHDIST
techwidthdist <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "techwidthdist",
             mean = mean(techwidthdist, na.rm = TRUE),
             sd = sd(techwidthdist, na.rm = TRUE),
             min = min(techwidthdist, na.rm=T),
             max = max(techwidthdist, na.rm=T),
             median = median(techwidthdist, na.rm=T),
             count=n(),
             count2 = sum(!is.na(techwidthdist)))

techwidthdist$maxmin <- (techwidthdist$max - techwidthdist$min)
techwidthdist
boxplot(techwidthdist$maxmin)$out
Q <- quantile(techwidthdist$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(techwidthdist$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

techwidthdistoutliers <- techwidthdist %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
techwidthdistoutliers

#id 71, 84, 91 < weird shape

###TECHTHICKPROX
techthickprox <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "techthickprox",
             mean = mean(techthickprox, na.rm = TRUE),
             sd = sd(techthickprox, na.rm = TRUE),
             min = min(techthickprox, na.rm=T),
             max = max(techthickprox, na.rm=T),
             median = median(techthickprox, na.rm=T),
             count=n(),
             count2 = sum(!is.na(techthickprox)))


techthickprox$maxmin <- (techthickprox$max - techthickprox$min)
techthickprox
boxplot(techthickprox$maxmin)$out
Q <- quantile(techthickprox$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(techthickprox$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

techthickproxoutliers <- techthickprox %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
techthickproxoutliers


#43 - human error (a8acd - 63.48 instead of 6.48?), #79 one abherrant value
#3, 7, 11, 12, 49, 62 --> flake shape?


###TECHTHICKMES
techthickmes <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "techthickmes",
             mean = mean(techthickmes, na.rm = TRUE),
             sd = sd(techthickmes, na.rm = TRUE),
             min = min(techthickmes, na.rm=T),
             max = max(techthickmes, na.rm=T),
             median = median(techthickmes, na.rm=T),
             count=n(),
             count2 = sum(!is.na(techthickmes)))


techthickmes$maxmin <- (techthickmes$max - techthickmes$min)
techthickmes
boxplot(techthickmes$maxmin)$out
Q <- quantile(techthickmes$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(techthickmes$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

techthickmesoutliers <- techthickmes %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
techthickmesoutliers


#flk id 1 < one abherrant value: human error?
#flk id 3 < flk shape?

###TECHTHICKDIST
techthickdist <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "techthickdist",
             mean = mean(techthickdist, na.rm = TRUE),
             sd = sd(techthickdist, na.rm = TRUE),
             min = min(techthickdist, na.rm=T),
             max = max(techthickdist, na.rm=T),
             median = median(techthickdist, na.rm=T),
             count=n(),
             count2 = sum(!is.na(techthickdist)))

techthickdist$maxmin <- (techthickdist$max - techthickdist$min)
techthickdist
boxplot(techthickdist$maxmin)$out
Q <- quantile(techthickdist$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(techthickdist$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

techthickdistoutliers <- techthickdist %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
techthickdistoutliers

#flk 58 < no immediate explanation, flk shape?

###PLATFWIDTH
platfwidth <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "platfwidth",
             mean = mean(platfwidth, na.rm = TRUE),
             sd = sd(platfwidth, na.rm = TRUE),
             min = min(platfwidth, na.rm=T),
             max = max(platfwidth, na.rm=T),
             median = median(platfwidth, na.rm=T),
             count=n(),
             count2 = sum(!is.na(platfwidth)))

platfwidth$maxmin <- (platfwidth$max - platfwidth$min)
platfwidth
boxplot(platfwidth$maxmin)$out
Q <- quantile(platfwidth$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(platfwidth$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

platfwidthoutliers <- platfwidth %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
platfwidthoutliers

#flk id 2 one abherrant value
#flk id 9, 56, 63, 67 : cortical platform, platform not well defined
#flk id 14-15, 74 : debordant flakes
#flk 33 - human error, duplicates?

###PLATFTHICKIMPACT
platfthickimpact <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "platfthickimpact",
             mean = mean(platfthickimpact, na.rm = TRUE),
             sd = sd(platfthickimpact, na.rm = TRUE),
             min = min(platfthickimpact, na.rm=T),
             max = max(platfthickimpact, na.rm=T),
             median = median(platfthickimpact, na.rm=T),
             count=n(),
             count2 = sum(!is.na(platfthickimpact)))

platfthickimpact$maxmin <- (platfthickimpact$max - platfthickimpact$min)
platfthickimpact
boxplot(platfthickimpact$maxmin)$out
Q <- quantile(platfthickimpact$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(platfthickimpact$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

platfthickimpactoutliers <- platfthickimpact %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
platfthickimpactoutliers



## PLATFTHICKMAX
platfthickmax <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "platfthickmax",
             mean = mean(platfthickmax, na.rm = TRUE),
             sd = sd(platfthickmax, na.rm = TRUE),
             min = min(platfthickmax, na.rm=T),
             max = max(platfthickmax, na.rm=T),
             median = median(platfthickmax, na.rm=T),
             count=n(),
             count2 = sum(!is.na(platfthickmax)))


platfthickmax$maxmin <- (platfthickmax$max - platfthickmax$min)
platfthickmax
boxplot(platfthickmax$maxmin)$out
Q <- quantile(platfthickmax$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(platfthickmax$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

platfthickmaxoutliers <- platfthickmax %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
platfthickmaxoutliers

## PLATFTHICKMID
platfthickmid <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "platfthickmid",
             mean = mean(platfthickmid, na.rm = TRUE),
             sd = sd(platfthickmid, na.rm = TRUE),
             min = min(platfthickmid, na.rm=T),
             max = max(platfthickmid, na.rm=T),
             median = median(platfthickmid, na.rm=T),
             count=n(),
             count2 = sum(!is.na(platfthickmid)))


platfthickmid$maxmin <- (platfthickmid$max - platfthickmid$min)
platfthickmid
boxplot(platfthickmid$maxmin)$out
Q <- quantile(platfthickmid$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(platfthickmid$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

platfthickmidoutliers <- platfthickmid %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
platfthickmidoutliers

## EDGEPLATF
edgeplatf <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "edgeplatf",
             mean = mean(edgeplatf, na.rm = TRUE),
             sd = sd(edgeplatf, na.rm = TRUE),
             min = min(edgeplatf, na.rm=T),
             max = max(edgeplatf, na.rm=T),
             median = median(edgeplatf, na.rm=T),
             count=n(),
             count2 = sum(!is.na(edgeplatf)))


edgeplatf$maxmin <- (edgeplatf$max - edgeplatf$min)
edgeplatf
boxplot(edgeplatf$maxmin)$out
Q <- quantile(edgeplatf$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(edgeplatf$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

edgeplatfoutliers <- edgeplatf %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
edgeplatfoutliers

#check this variable for consistency (mm or degrees), remove all  that are degrees (NA)

angle_height <- new_comsafrica_data %>%
   group_by(new_flake_id) %>%
   summarize(variable = "angle_height",
             mean = mean(angle_height, na.rm = TRUE),
             sd = sd(angle_height, na.rm = TRUE),
             min = min(angle_height, na.rm=T),
             max = max(angle_height, na.rm=T),
             median = median(angle_height, na.rm=T),
             count=n(),
             count2 = sum(!is.na(angle_height)))

angle_height
angle_height$maxmin <- (angle_height$max - angle_height$min)
angle_height
boxplot(angle_height$maxmin)$out
Q <- quantile(angle_height$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(angle_height$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

angle_heightoutliers <- angle_height %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
angle_heightoutliers

##problems with angle height: is 0 a real value or automatic fill when not recorded?  (as seen for other measures?) seems to depend...

#DORSAL SCAR COUNT
new_comsafrica_data_scar <- new_comsafrica_data %>%
   filter(!new_flake_id %in% c(3))


dorsal_scar_count <- new_comsafrica_data_scar %>%
   group_by(new_flake_id) %>%
   summarize(variable = "dorsal_scar_count",
             mean = mean(dorsal_scar_count, na.rm = TRUE),
             sd = sd(dorsal_scar_count, na.rm = TRUE),
             min = min(dorsal_scar_count, na.rm=T),
             max = max(dorsal_scar_count, na.rm=T),
             median = median(dorsal_scar_count, na.rm=T),
             count=n(),
             count2 = sum(!is.na(dorsal_scar_count)))

dorsal_scar_count
dorsal_scar_count$maxmin <- (dorsal_scar_count$max - dorsal_scar_count$min)
dorsal_scar_count
boxplot(dorsal_scar_count$maxmin)$out
Q <- quantile(dorsal_scar_count$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(dorsal_scar_count$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

dorsal_scar_countoutliers <- dorsal_scar_count %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
dorsal_scar_countoutliers

#flk id 3 < depends on cortex definitions too
#if flk id 3 removed, no outliers, but looking at those with a min-max range >10, it seems that problems linked to cortex definitions and what is a scar

#PROX SCAR COUNT
proximal_scars <- new_comsafrica_data_scar %>%
   group_by(new_flake_id) %>%
   summarize(variable = "proximal_scars",
             mean = mean(proximal_scars, na.rm = TRUE),
             sd = sd(proximal_scars, na.rm = TRUE),
             min = min(proximal_scars, na.rm=T),
             max = max(proximal_scars, na.rm=T),
             median = median(proximal_scars, na.rm=T),
             count=n(),
             count2 = sum(!is.na(proximal_scars)))

proximal_scars
proximal_scars$maxmin <- (proximal_scars$max - proximal_scars$min)
proximal_scars
boxplot(proximal_scars$maxmin)$out
Q <- quantile(proximal_scars$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(proximal_scars$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

proximal_scarsoutliers <- proximal_scars %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
proximal_scarsoutliers

##LEFT SCARS
left_scars <- new_comsafrica_data_scar %>%
   group_by(new_flake_id) %>%
   summarize(variable = "left_scars",
             mean = mean(left_scars, na.rm = TRUE),
             sd = sd(left_scars, na.rm = TRUE),
             min = min(left_scars, na.rm=T),
             max = max(left_scars, na.rm=T),
             median = median(left_scars, na.rm=T),
             count=n(),
             count2 = sum(!is.na(left_scars)))

left_scars
left_scars$maxmin <- (left_scars$max - left_scars$min)
left_scars
boxplot(left_scars$maxmin)$out
Q <- quantile(left_scars$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(left_scars$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

left_scarsoutliers <- left_scars %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
left_scarsoutliers

#no outliers

##RIGHT SCARS
right_scars <- new_comsafrica_data_scar %>%
   group_by(new_flake_id) %>%
   summarize(variable = "right_scars",
             mean = mean(right_scars, na.rm = TRUE),
             sd = sd(right_scars, na.rm = TRUE),
             min = min(right_scars, na.rm=T),
             max = max(right_scars, na.rm=T),
             median = median(right_scars, na.rm=T),
             count=n(),
             count2 = sum(!is.na(right_scars)))

right_scars
right_scars$maxmin <- (right_scars$max - right_scars$min)
right_scars
boxplot(right_scars$maxmin)$out
Q <- quantile(right_scars$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(right_scars$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

right_scarsoutliers <- right_scars %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
right_scarsoutliers

#id8 - human error? count does not add up between total number scars and number scars per sector
#id46 - same + difference in interpretation? often same analyst (46b96) has much larger count scars
#id60 - same, often does not add up to total count scars
#id62 - same

##DISTAL SCARS
distal_scars <- new_comsafrica_data_scar %>%
   group_by(new_flake_id) %>%
   summarize(variable = "distal_scars",
             mean = mean(distal_scars, na.rm = TRUE),
             sd = sd(distal_scars, na.rm = TRUE),
             min = min(distal_scars, na.rm=T),
             max = max(distal_scars, na.rm=T),
             median = median(distal_scars, na.rm=T),
             count=n(),
             count2 = sum(!is.na(distal_scars)))

distal_scars
distal_scars$maxmin <- (distal_scars$max - distal_scars$min)
distal_scars
boxplot(distal_scars$maxmin)$out
Q <- quantile(distal_scars$maxmin, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(distal_scars$maxmin)
up <-  Q[2]+1.5*iqr # Upper Range  
up

distal_scarsoutliers <- distal_scars %>%
   select(new_flake_id, maxmin, min, max) %>%
   filter(maxmin > up)
distal_scarsoutliers

#no outliers

###Alternative summary table for quantitative variables using dplyr ####

###tables per quantitative variables

mass <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "mass",
            mean = mean(mass, na.rm = TRUE),
            sd = sd(mass, na.rm = TRUE),
            min = min(mass, na.rm=T),
            max = max(mass, na.rm=T),
            median = median(mass, na.rm=T),
            count=n(),
            count2 = sum(!is.na(mass)))

cortex <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "cortex",
            mean = mean(dorsal_cortex, na.rm = TRUE),
            sd = sd(dorsal_cortex, na.rm = TRUE),
            min = min(dorsal_cortex, na.rm=T),
            max = max(dorsal_cortex, na.rm=T),
            median = median(dorsal_cortex, na.rm=T),
            count=n(),
            count2 = sum(!is.na(dorsal_cortex)))



maxdim <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "Max Dim",
            mean = mean(maximumdimension, na.rm = TRUE),
            sd = sd(maximumdimension, na.rm = TRUE),
            min = min(maximumdimension, na.rm=T),
            max = max(maximumdimension, na.rm=T),
            median = median(maximumdimension, na.rm=T),
            count=n(),
            count2 = sum(!is.na(maximumdimension)))

maxwidth <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "Max Width",
            mean = mean(maximumwidth, na.rm = TRUE),
            sd = sd(maximumwidth, na.rm = TRUE),
            min = min(maximumwidth, na.rm=T),
            max = max(maximumwidth, na.rm=T),
            median = median(maximumwidth, na.rm=T),
            count=n(),
            count2 = sum(!is.na(maximumwidth)))

maxT <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "Max Thick",
            mean = mean(maximumthickness, na.rm = TRUE),
            sd = sd(maximumthickness, na.rm = TRUE),
            min = min(maximumthickness, na.rm=T),
            max = max(maximumthickness, na.rm=T),
            median = median(maximumthickness, na.rm=T),
            count=n(),
            count2 = sum(!is.na(maximumthickness)))


techL <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "Tech Length",
            mean = mean(techlength, na.rm = TRUE),
            sd = sd(techlength, na.rm = TRUE),
            min = min(techlength, na.rm=T),
            max = max(techlength, na.rm=T),
            median = median(techlength, na.rm=T),
            count=n(),
            count2 = sum(!is.na(techlength)))

techmaxwidth <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "techmaxwidth",
            mean = mean(techmaxwidth, na.rm = TRUE),
            sd = sd(techmaxwidth, na.rm = TRUE),
            min = min(techmaxwidth, na.rm=T),
            max = max(techmaxwidth, na.rm=T),
            median = median(techmaxwidth, na.rm=T),
            count=n(),
            count2 = sum(!is.na(techmaxwidth)))

techmaxthickness <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "techmaxthickness",
            mean = mean(techmaxthickness, na.rm = TRUE),
            sd = sd(techmaxthickness, na.rm = TRUE),
            min = min(techmaxthickness, na.rm=T),
            max = max(techmaxthickness, na.rm=T),
            median = median(techmaxthickness, na.rm=T),
            count=n(),
            count2 = sum(!is.na(techmaxthickness)))

techwidthprox <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "techwidthprox",
            mean = mean(techwidthprox, na.rm = TRUE),
            sd = sd(techwidthprox, na.rm = TRUE),
            min = min(techwidthprox, na.rm=T),
            max = max(techwidthprox, na.rm=T),
            median = median(techwidthprox, na.rm=T),
            count=n(),
            count2 = sum(!is.na(techwidthprox)))

techwidthmes <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "techwidthmes",
            mean = mean(techwidthmes, na.rm = TRUE),
            sd = sd(techwidthmes, na.rm = TRUE),
            min = min(techwidthmes, na.rm=T),
            max = max(techwidthmes, na.rm=T),
            median = median(techwidthmes, na.rm=T),
            count=n(),
            count2 = sum(!is.na(techwidthmes)))

techwidthdist <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "techwidthdist",
            mean = mean(techwidthdist, na.rm = TRUE),
            sd = sd(techwidthdist, na.rm = TRUE),
            min = min(techwidthdist, na.rm=T),
            max = max(techwidthdist, na.rm=T),
            median = median(techwidthdist, na.rm=T),
            count=n(),
            count2 = sum(!is.na(techwidthdist)))

techthickprox <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "techthickprox",
            mean = mean(techthickprox, na.rm = TRUE),
            sd = sd(techthickprox, na.rm = TRUE),
            min = min(techthickprox, na.rm=T),
            max = max(techthickprox, na.rm=T),
            median = median(techthickprox, na.rm=T),
            count=n(),
            count2 = sum(!is.na(techthickprox)))

techthickmes <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "techthickmes",
            mean = mean(techthickmes, na.rm = TRUE),
            sd = sd(techthickmes, na.rm = TRUE),
            min = min(techthickmes, na.rm=T),
            max = max(techthickmes, na.rm=T),
            median = median(techthickmes, na.rm=T),
            count=n(),
            count2 = sum(!is.na(techthickmes)))

techthickdist <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "techthickdist",
            mean = mean(techthickdist, na.rm = TRUE),
            sd = sd(techthickdist, na.rm = TRUE),
            min = min(techthickdist, na.rm=T),
            max = max(techthickdist, na.rm=T),
            median = median(techthickdist, na.rm=T),
            count=n(),
            count2 = sum(!is.na(techthickdist)))

platfwidth <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "platfwidth",
            mean = mean(platfwidth, na.rm = TRUE),
            sd = sd(platfwidth, na.rm = TRUE),
            min = min(platfwidth, na.rm=T),
            max = max(platfwidth, na.rm=T),
            median = median(platfwidth, na.rm=T),
            count=n(),
            count2 = sum(!is.na(platfwidth)))

platfthickimpact <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "platfthickimpact",
            mean = mean(platfthickimpact, na.rm = TRUE),
            sd = sd(platfthickimpact, na.rm = TRUE),
            min = min(platfthickimpact, na.rm=T),
            max = max(platfthickimpact, na.rm=T),
            median = median(platfthickimpact, na.rm=T),
            count=n(),
            count2 = sum(!is.na(platfthickimpact)))

platfthickmax <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "platfthickmax",
            mean = mean(platfthickmax, na.rm = TRUE),
            sd = sd(platfthickmax, na.rm = TRUE),
            min = min(platfthickmax, na.rm=T),
            max = max(platfthickmax, na.rm=T),
            median = median(platfthickmax, na.rm=T),
            count=n(),
            count2 = sum(!is.na(platfthickmax)))

platfthickmid <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "platfthickmid",
            mean = mean(platfthickmid, na.rm = TRUE),
            sd = sd(platfthickmid, na.rm = TRUE),
            min = min(platfthickmid, na.rm=T),
            max = max(platfthickmid, na.rm=T),
            median = median(platfthickmid, na.rm=T),
            count=n(),
            count2 = sum(!is.na(platfthickmid)))

edgeplatf <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "edgeplatf",
            mean = mean(edgeplatf, na.rm = TRUE),
            sd = sd(edgeplatf, na.rm = TRUE),
            min = min(edgeplatf, na.rm=T),
            max = max(edgeplatf, na.rm=T),
            median = median(edgeplatf, na.rm=T),
            count=n(),
            count2 = sum(!is.na(edgeplatf)))

angle_height <- data1 %>%
  group_by(new_flake_id) %>%
  summarize(variable = "angle_height",
            mean = mean(angle_height, na.rm = TRUE),
            sd = sd(angle_height, na.rm = TRUE),
            min = min(angle_height, na.rm=T),
            max = max(angle_height, na.rm=T),
            median = median(angle_height, na.rm=T),
            count=n(),
            count2 = sum(!is.na(angle_height)))

sumquant <- rbind(mass, cortex,maxdim, maxwidth, maxT, techL, techmaxwidth,
                  techmaxthickness, techwidthprox, techwidthmes,
                  techwidthdist, techthickprox, techthickmes,
                  techthickdist, platfwidth, platfthickimpact,
                  platfthickmax, platfthickmid, edgeplatf, angle_height)

write.csv(sumquant, file="Tables/sumquant.csv")


