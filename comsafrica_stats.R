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
library(EnvStats)

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
#in total, around 25 pieces were reassigned. Other numbering mistakes may 
# appear and in case of too strong inter-analyst discrepancies, this is a factor that should be considered

detach()

############################## 
# Trim/tidy data and subset data for analyses
##############################

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

new_comsafrica_data <- rbind(data.A, data.B)

comsafrica_data_complete<-new_comsafrica_data %>%
      select(c(assemblage_code,analyst_id,analysis_order,flake_id,new_flake_id,proximal_scars,left_scars,distal_scars,right_scars,
               dorsal_cortex,mass,maximumdimension,maximumwidth,maximumthickness,techlength,techmaxwidth,techmaxthickness,
               techwidthprox,techwidthmes,techwidthdist,techthickprox,techthickmes,techthickdist,
               platfwidth,platfthickimpact,platfthickmid,platfthickmax,edgeplatf,angle_height))

##### Inter rater data analyses

### repeatability coefficients for continuous (Gaussian) data ####

# cortex-NOT SURE WHAT TO DO WITH CORTEX-STRANGE DISTRIBUTION?
hist(log(comsafrica_data_complete$dorsal_cortex))
set.seed(50)
comsafrica_cortex_boot<-rpt(dorsal_cortex ~ new_flake_id*analysis_order + (1 | new_flake_id), 
                            grname = c("new_flake_id","Fixed"), 
                            data = comsafrica_data_complete, 
                            datatype = "Gaussian", 
                            nboot = 1000, npermut = 100)

summary(comsafrica_cortex_boot)
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

summary(comsafrica_maxdim_boot)
print(comsafrica_maxdim_boot)

#mass
hist(log(comsafrica_data_complete$mass))
set.seed(50)
comsafrica_mass<-rpt(log(mass) ~ new_flake_id*analysis_order + (1 | new_flake_id), 
                       grname = c("new_flake_id","Fixed"), 
                       data = filter(comsafrica_data_complete,mass>0), 
                       datatype = "Gaussian", 
                       nboot = 1000, npermut = 100)
summary(comsafrica_mass)
print(comsafrica_mass)

#flake width
set.seed(50)
comsafrica_maxwidth<-rpt(maximumwidth ~ new_flake_id*analysis_order + (1 | new_flake_id), 
                     grname = c("new_flake_id","Fixed"), 
                     data = comsafrica_data_complete, 
                     datatype = "Gaussian", 
                     nboot = 1000, npermut = 100)

summary(comsafrica_maxwidth)
print(comsafrica_maxwidth)

#flake max thickness
set.seed(50)
comsafrica_maxthick<-rpt(maximumthickness ~ new_flake_id*analysis_order + (1 | new_flake_id), 
                         grname = c("new_flake_id","Fixed"), 
                         data = comsafrica_data_complete, 
                         datatype = "Gaussian", 
                         nboot = 1000, npermut = 100)

summary(comsafrica_maxthick)
print(comsafrica_maxthick)

#flake tech length
set.seed(50)
comsafrica_techlength<-rpt(techlength ~ new_flake_id*analysis_order + (1 | new_flake_id), 
                         grname = c("new_flake_id","Fixed"), 
                         data = comsafrica_data_complete, 
                         datatype = "Gaussian", 
                         nboot = 1000, npermut = 100)

summary(comsafrica_techlength)
print(comsafrica_techlength)

#flake tech max width
set.seed(50)
comsafrica_techmaxwidth<-rpt(techmaxwidth ~ new_flake_id*analysis_order + (1 | new_flake_id), 
                           grname = c("new_flake_id","Fixed"), 
                           data = comsafrica_data_complete, 
                           datatype = "Gaussian", 
                           nboot = 1000, npermut = 100)

summary(comsafrica_techmaxwidth)
print(comsafrica_techmaxwidth)

#flake tech max thickness
set.seed(50)
comsafrica_techmaxthick<-rpt(techmaxthickness ~ new_flake_id*analysis_order + (1 | new_flake_id), 
                             grname = c("new_flake_id","Fixed"), 
                             data = comsafrica_data_complete, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 100)
summary(comsafrica_techmaxthick)
print(comsafrica_techmaxthick)

#flake tech width prox
set.seed(50)
comsafrica_techwidthprox<-rpt(techwidthprox ~ new_flake_id*analysis_order + (1 | new_flake_id), 
                             grname = c("new_flake_id","Fixed"), 
                             data = comsafrica_data_complete, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 100)
summary(comsafrica_techwidthprox)
print(comsafrica_techwidthprox)

#flake tech width mes
set.seed(50)
comsafrica_techwidthmes<-rpt(techwidthmes ~ new_flake_id*analysis_order + (1 | new_flake_id), 
                              grname = c("new_flake_id","Fixed"), 
                              data = comsafrica_data_complete, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 100)
summary(comsafrica_techwidthmes)
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
   filter(!new_flake_id %in% c(53,64,82,56,54,55,60))

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

## Reduction system

red_syst_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,red_syst) %>% 
   mutate(red_syst=as.factor(red_syst),
          red_syst=recode(red_syst,
                          Discoidal = "Discoid",
                          idnet = "Indet",
                          inde = "Indet",
                          Indeterminate = "Indet",
                          other = "Indet",
                          Ind = "Indet",
                          indet = "Indet",
                          Indet = "Indet",
                          INDET = "Indet",
                          'indeterminate (broken)' = "Indet",
                          'Indeterminate (broken)' = "Indet",
                          indeterminate = "Indet",
                          laminar = "Laminar",
                          'Levallois (pref)' = "Levallois",
                          'Lev or Disc' = "Indet",
                          'Levallois indet' = "Levallois",
                          'Levallois non-Nubian' = "Levallois",
                          'levallois non nubian' = "Levallois",
                          'LEVALLOIS OR DISCOID' = "Levallois",
                          'LEVALLOIS/LEVALLOIS-RELATED' = "Levallois",
                           na = "Indet",
                          'NON-LEVALLOIS; ORTHOGONAL VOLUME EXPLOITATION' = "Indet",
                          none = "Indet",
                          Nubian = "Levallois",
                          'ON ANVIL' = "Bipolar",
                          'Platform (laminar?)' = "Platform",
                          'Platform / Laminar' = "Platform",
                          'possible Levallois non-Nubian' = "Levallois",
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
                          'Core Edge Flake' = "Indet",
                          FLAKE = "Flake",
                          bipolar = "Bipolar")) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

#option for transforming Indet into NA
#levels(red_syst_data_a$red_syst)[levels(red_syst_data_a$red_syst)=='Indet'] <- NA

# delete flake #'s with < 4 observations
red_syst_data_a<-red_syst_data_a[as.numeric(ave(red_syst_data_a$new_flake_id, 
                                                red_syst_data_a$new_flake_id, 
                                                    FUN=length)) >= 6, ]

# reclass cat variables into numeric values for the krip stat
red_syst_data_a_krip<- red_syst_data_a %>%
   mutate(red_sys_dummy=unclass(red_syst)) %>% 
   select(-red_syst) %>%
   spread(analyst_id, red_sys_dummy) %>%
   select(-new_flake_id) %>% 
   as.matrix() 

set.seed(42)
fit.full_red_syst<-krippendorffs.alpha(red_syst_data_a_krip, 
                                       level = "nominal", 
                                       control = list(parallel = FALSE,bootit=100), 
                                       verbose = TRUE)
summary(fit.full_red_syst)

# Compute kapa-allows us to see which categories are performing better
# requires actual categorical variables not reclasssed values
red_sys_data_a_fleiss<- red_syst_data_a %>%
   mutate(red_syst=as.factor(red_syst)) %>%
   spread(analyst_id, red_syst) %>%
   select(-new_flake_id) 

red_sys_data_a_fleiss<-red_sys_data_a_fleiss %>%
   mutate_if(is.factor, funs(factor(replace(as.character(.), is.na(.), "Other"))))

kappam.fleiss(red_sys_data_a_fleiss, detail = T)

## flake form

flake_form_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,flk_form) %>% 
   mutate(flk_form=recode(comsafrica_data_cat_data$flk_form, 
                          BLADE = "Blade",
                          CONVFLAKE = "Convflake", 
                          ELONG = "Blade",
                          Elong = "Blade",
                          flake = "Flake",
                          FLAKE = "Flake")) %>%
   na_if("") %>% #delete coding episodes with no data
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

set.seed(42)
fit.full_flk_form<-krippendorffs.alpha(flake_form_data_a_krip, 
                                           level = "nominal", 
                                           control = list(parallel = FALSE,bootit=100), 
                                           verbose = TRUE)
summary(fit.full_flk_form)

# Compute kapa-allows us to see which categories are performing better
# requires actual categorical variables not reclasssed values
flake_form_data_a_fleiss<- flake_form_data_a %>%
   mutate(flk_form=as.factor(flk_form)) %>%
   spread(analyst_id, flk_form) %>%
   select(-new_flake_id) 

flake_form_data_a<-flake_form_data_a %>%
   mutate_if(is.factor, funs(factor(replace(as.character(.), is.na(.), "Other"))))

kappam.fleiss(flake_form_data_a_fleiss, detail = T)

## completeness

completeness_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,completeness) %>% 
   mutate(completeness=recode(comsafrica_data_cat_data$completeness, 
                          fragment = "indeterminate",
                          other = "indeterminate")) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit

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
   select(-new_flake_id) 

kappam.fleiss(completeness_data_a_fleiss, detail = T)

## platform cortex

platform_cortex_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,platform_cortex) %>%
   mutate(platform_cortex=recode(comsafrica_data_cat_data$platform_cortex, INDET = "Indeterminate",
                                 complete = "Complete", absent = "Absent"),
          platform_cortex=as.factor(platform_cortex),
          platform_cortex_dummy=unclass(platform_cortex)) %>% 
   na_if("") %>% #delete coding episodes with no data
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

set.seed(42)
fit.full_cortex<-krippendorffs.alpha(platform_cortex_data_a_krip, 
                                     level = "nominal", 
                                     control = list(parallel = FALSE,bootit=100), 
                                     verbose = TRUE)
summary(fit.full_cortex)

# Compute kapa-allows us to see which categories are performing better
platform_cortex_data_a_fleiss<-platform_cortex_data_a %>%
   select(new_flake_id,analyst_id,platform_cortex) %>%
   spread(analyst_id,platform_cortex) %>%
   select(-new_flake_id)

kappam.fleiss(platform_cortex_data_a_fleiss, detail = T)

## scar directions

directionality_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,directionality) %>% 
   na_if("") %>% #delete coding episodes with no data
   na.omit %>%
   mutate(directionality=as.factor(directionality),
          directionality=recode(directionality, 
                                centripetal = "Centripetal",
                                Other = "Indeterminate",
                                other = "Indeterminate"))

directionality_data_a<-directionality_data_a[as.numeric(ave(directionality_data_a$new_flake_id, 
                                                            directionality_data_a$new_flake_id, 
                                                            FUN=length)) >= 6, ]

directionality_data_a_krip<- directionality_data_a %>%
   mutate(directionality_dummy=unclass(directionality)) %>% 
   select(-directionality) %>%
   spread(analyst_id, directionality_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_directions<-krippendorffs.alpha(directionality_data_a_krip, 
                                         level = "nominal", 
                                         control = list(parallel = FALSE,bootit=100), 
                                         verbose = TRUE)
summary(fit.full_directions)

# Compute kapa-allows us to see which categories are performing better
directionality_data_a_fleiss<- directionality_data_a %>%
   spread(analyst_id, directionality) %>%
   select(-new_flake_id)

kappam.fleiss(directionality_data_a_fleiss, detail = T)

## platf morph

plat_morph_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,platfmorph) %>% 
   na_if("") %>% #delete coding episodes with no data
   na.omit %>%
   mutate(platfmorph=recode(platfmorph, 
                            'Chapeau de Gendarme' = "ChapeauDeGendarme",
                            linear = "Linear",
                            Diherdral = "Dihedral",
                            Other = "Indeterminate",
                            facetted = "Facetted"),
          platfmorph=as.factor(platfmorph))

plat_morph_data_a<-plat_morph_data_a[as.numeric(ave(plat_morph_data_a$new_flake_id, 
                                                    plat_morph_data_a$new_flake_id, 
                                                    FUN=length)) >= 6, ]

plat_morph_data_a_krip<- plat_morph_data_a %>%
   mutate(plat_morph_dummy=unclass(platfmorph)) %>% 
   select(-platfmorph) %>%
   spread(analyst_id, plat_morph_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_platmorphs<-krippendorffs.alpha(plat_morph_data_a_krip, 
                                         level = "nominal", 
                                         control = list(parallel = FALSE,bootit=100), 
                                         verbose = TRUE)
summary(fit.full_platmorphs)

# Compute kapa-allows us to see which categories are performing better
plat_morph_data_a_fleiss<- plat_morph_data_a %>%
   spread(analyst_id, platfmorph) %>%
   select(-new_flake_id)

kappam.fleiss(plat_morph_data_a_fleiss, detail = T)

## platf lip

plat_lip_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,platflipp) %>% 
   na_if("") %>% #delete coding episodes with no data
   na.omit %>%
   mutate(platflipp=as.factor(platflipp),
          platflipp=recode_factor(platflipp, YES = "yes", Yes='yes',
                                  NO = "no", No="no", 'NOT APPLICABLE' = "Indeterminate"),
          platflipp=as.factor(platflipp))

plat_lip_data_a<-plat_lip_data_a[as.numeric(ave(plat_lip_data_a$new_flake_id, 
                                                plat_lip_data_a$new_flake_id, 
                                                FUN=length)) >=6, ]

plat_lip_data_a_krip<- plat_lip_data_a %>%
   mutate(plat_lip_dummy=unclass(platflipp)) %>% 
   select(-platflipp) %>%
   spread(analyst_id, plat_lip_dummy) %>%
   select(-new_flake_id) %>% 
   as.matrix()

set.seed(42)
fit.full_platlip<-krippendorffs.alpha(plat_lip_data_a_krip, 
                                      level = "nominal", 
                                      control = list(parallel = FALSE,bootit=100), 
                                      verbose = TRUE)

summary(fit.full_platlip)

# Compute kapa-allows us to see which categories are performing better
plat_lip_data_a_fleiss<- plat_lip_data_a %>%
   spread(analyst_id, platflipp) %>%
   select(-new_flake_id)

kappam.fleiss(plat_lip_data_a_fleiss, detail = T)

## bulb

plat_bulb_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,bulb) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit %>%
   mutate(bulb=recode(bulb, YES = "yes", Yes="yes",
                      NO = "no",No="no"),
          bulb=as.factor(bulb)) 


plat_bulb_data_a<-plat_bulb_data_a[as.numeric(ave(plat_bulb_data_a$new_flake_id, 
                                                  plat_bulb_data_a$new_flake_id, 
                                                  FUN=length)) >=6, ]

plat_bulb_data_a_krip<- plat_bulb_data_a %>%
   mutate(plat_bulb_dummy=unclass(bulb)) %>% 
   select(-bulb) %>%
   spread(analyst_id, plat_bulb_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_bulb<-krippendorffs.alpha(plat_bulb_data_a_krip, 
                                   level = "nominal", 
                                   control = list(parallel = FALSE,bootit=100), 
                                   verbose = TRUE)

summary(fit.full_bulb)

# Compute kapa-allows us to see which categories are performing better
bulb_data_a_fleiss<- plat_bulb_data_a %>%
   spread(analyst_id, bulb) %>%
   select(-new_flake_id)

kappam.fleiss(bulb_data_a_fleiss, detail = T)

## Shattbulb 

plat_shattbulb_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,shattbulb) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit %>%
   mutate(shattbulb=recode(shattbulb, 
                           Indet = "Indeterminate",
                           Indeterminateerminate = "Indeterminate",
                           NO = "No",
                           no="No",
                           YES = "Yes"),
          shattbulb=as.factor(shattbulb))

plat_shattbulb_data_a<-plat_shattbulb_data_a[as.numeric(ave(plat_shattbulb_data_a$new_flake_id, 
                                                            plat_shattbulb_data_a$new_flake_id, 
                                                            FUN=length)) >=6, ]

plat_shattbulb_data_a_krip<- plat_shattbulb_data_a %>%
   mutate(plat_shattbulb_dummy=unclass(shattbulb)) %>% 
   select(-shattbulb) %>%
   spread(analyst_id, plat_shattbulb_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_shattbulb<-krippendorffs.alpha(plat_shattbulb_data_a_krip, 
                                        level = "nominal", 
                                        control = list(parallel = FALSE,bootit=100), 
                                        verbose = TRUE)

summary(fit.full_shattbulb)

# Compute kapa-allows us to see which categories are performing better
shattbulb_data_a_fleiss<- plat_shattbulb_data_a %>%
   spread(analyst_id, shattbulb) %>%
   select(-new_flake_id)

kappam.fleiss(shattbulb_data_a_fleiss, detail = T)

## initiation

plat_initiation_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,initiation) %>%
   na_if("") %>% #delete coding episodes with no data
   na.omit %>%
   mutate(initiation=recode(initiation, 
                            BENDING = "Bending",
                            HERTZIAN = "Hertzian",
                            hertzian = "Hertzian",
                            WEDGING = "Wedging",
                            INDET = "Other"),
          initiation=as.factor(initiation))

plat_initiation_data_a<-plat_initiation_data_a[as.numeric(ave(plat_initiation_data_a$new_flake_id, 
                                                              plat_initiation_data_a$new_flake_id, 
                                                              FUN=length)) >=6, ]

plat_initiation_data_a_krip<- plat_initiation_data_a %>%
   mutate(initiation_dummy=unclass(initiation)) %>% 
   select(-initiation) %>%
   spread(analyst_id, initiation_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_initiation<-krippendorffs.alpha(plat_initiation_data_a_krip, 
                                         level = "nominal", 
                                         control = list(parallel = FALSE,bootit=100), 
                                         verbose = TRUE)

summary(fit.full_initiation)

# Compute kapa-allows us to see which categories are performing better
levels(plat_initiation_data_a$initiation)

initiation_data_a_fleiss<- plat_initiation_data_a  %>%
   spread(analyst_id, initiation) %>%
   select(-new_flake_id)

initiation_data_a_fleiss<-initiation_data_a_fleiss %>%
   mutate_if(is.factor, funs(factor(replace(as.character(.), is.na(.), "Other"))))

kappam.fleiss(initiation_data_a_fleiss, detail = T)

## ventr_plane_form

ventr_plane_form_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,ventr_plane_form) %>%
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

ventr_plane_form_data_a<-ventr_plane_form_data_a[as.numeric(ave(ventr_plane_form_data_a$new_flake_id, 
                                                                ventr_plane_form_data_a$new_flake_id, 
                                                                FUN=length)) >=6, ]

ventr_plane_form_data_a_krip<- ventr_plane_form_data_a %>%
   mutate(ventr_plane_form_dummy=unclass(ventr_plane_form)) %>% 
   select(-ventr_plane_form) %>%
   spread(analyst_id, ventr_plane_form_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_ventr_plane_form<-krippendorffs.alpha(ventr_plane_form_data_a_krip, 
                                               level = "nominal", 
                                               control = list(parallel = FALSE,bootit=100), 
                                               verbose = TRUE)

summary(fit.full_ventr_plane_form)

# Compute kapa-allows us to see which categories are performing better
ventr_plane_form_fleiss<- ventr_plane_form_data_a %>%
   spread(analyst_id, ventr_plane_form) %>%
   select(-new_flake_id)

kappam.fleiss(ventr_plane_form_fleiss, detail = T)

## Section

section_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,section) %>%
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

section_data_a<-section_data_a[as.numeric(ave(section_data_a$new_flake_id, 
                                              section_data_a$new_flake_id, 
                                              FUN=length)) >=6, ]

section_data_a_krip<- section_data_a %>%
   mutate(section_dummy=unclass(section)) %>% 
   select(-section) %>%
   spread(analyst_id, section_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_section<-krippendorffs.alpha(section_data_a_krip, 
                                      level = "nominal", 
                                      control = list(parallel = FALSE,bootit=100), 
                                      verbose = TRUE)

summary(fit.full_section)

# Compute kapa-allows us to see which categories are performing better
section_fleiss<- section_data_a %>%
   spread(analyst_id, section) %>%
   select(-new_flake_id)

kappam.fleiss(section_fleiss, detail = T)

## Lateral edge type

latedge_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,latedgetype) %>%
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

latedge_data_a<-latedge_data_a[as.numeric(ave(latedge_data_a$new_flake_id, 
                                              latedge_data_a$new_flake_id, 
                                              FUN=length)) >=6, ]

latedge_data_a_krip<- latedge_data_a %>%
   mutate(latedge_dummy=unclass(latedgetype)) %>% 
   select(-latedgetype) %>%
   spread(analyst_id, latedge_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_latedge<-krippendorffs.alpha(latedge_data_a_krip, 
                                      level = "nominal", 
                                      control = list(parallel = FALSE,bootit=100), 
                                      verbose = TRUE)

summary(fit.full_latedge)

# Compute kapa-allows us to see which categories are performing better
latedge_fleiss<- latedge_data_a %>%
   spread(analyst_id, latedgetype) %>%
   select(-new_flake_id)

kappam.fleiss(latedge_fleiss, detail = T)

## Flake termination

flaketerm_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,flaketerm) %>%
   mutate(flaketerm=as.factor(flaketerm),
          flaketerm=recode(flaketerm, 
                           FEATHER = "Feather",
                           HINGE = "Hinge",
                           INDET = "Indeterminate",
                           OVERSHOT = "Overshot",
                           AXIAL= "Axial",
                           CRUSHED = "Crushed")) %>%
   na_if("") %>% #delete coding episodes with no data
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

set.seed(42)
fit.full_flaketerm<-krippendorffs.alpha(flaketerm_data_a_krip, 
                                        level = "nominal", 
                                        control = list(parallel = FALSE,bootit=100), 
                                        verbose = TRUE)

summary(fit.full_flaketerm)

# Compute kapa-allows us to see which categories are performing better
flaketerm_fleiss<- flaketerm_data_a %>%
   spread(analyst_id, flaketerm) %>%
   select(-new_flake_id)

kappam.fleiss(flaketerm_fleiss, detail = T)

## Kombewa

kombewa_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,kombewa) %>%
   mutate(kombewa=as.factor(kombewa),
          kombewa=recode(kombewa, 
                         NO = "No",
                         YES = "Yes",
                         '0.84'="Indet")) %>%
   na_if("") %>% #delete coding episodes with no data
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

set.seed(42)
fit.full_kombewa<-krippendorffs.alpha(kombewa_data_a_krip, 
                                      level = "nominal", 
                                      control = list(parallel = FALSE,bootit=100), 
                                      verbose = TRUE)

summary(fit.full_kombewa)

# Compute kapa-allows us to see which categories are performing better
kombewa_fleiss<- kombewa_data_a %>%
   spread(analyst_id, kombewa) %>%
   select(-new_flake_id)

kappam.fleiss(kombewa_fleiss, detail = T)

## Distal plan form

distplanform_data_a<-comsafrica_data_cat_data %>%
   select(new_flake_id,analyst_id,distplanform) %>%
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

distplanform_data_a<-distplanform_data_a[as.numeric(ave(distplanform_data_a$new_flake_id, 
                                                        distplanform_data_a$new_flake_id, 
                                                        FUN=length)) > 4, ]

distplanform_data_a_krip<- distplanform_data_a %>%
   mutate(distplanform_dummy=unclass(distplanform)) %>% 
   select(-distplanform) %>%
   spread(analyst_id, distplanform_dummy) %>%
   select(-new_flake_id) %>%
   as.matrix()

set.seed(42)
fit.full_distplanform<-krippendorffs.alpha(distplanform_data_a_krip, 
                                           level = "nominal", 
                                           control = list(parallel = FALSE,bootit=100), 
                                           verbose = TRUE)

summary(fit.full_distplanform)

# Compute kapa-allows us to see which categories are performing better
distplanform_fleiss<- distplanform_data_a %>%
   spread(analyst_id, distplanform) %>%
   select(-new_flake_id)

kappam.fleiss(distplanform_fleiss, detail = T)

### IRR visualizations ####

irr_summary<-read.csv("irr_summary_data.csv")
irr_summary_cat<-read.csv("irr_summary_data_categorical.csv")
irr_summary_subcat<-read.csv("irr_summary_data_subcategorical.csv", stringsAsFactors = T)

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

ggplot(irr_summary_cat,aes(y=irr_value, x=reorder(variable,irr_value))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   geom_errorbar(aes(ymin=lower, ymax=upper),
                 width=.2,                    # Width of the error bars
                 position=position_dodge(.9))+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red")

## sub categorical data

ggplot(data=filter(irr_summary_subcat,variable == "reduction_system"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "flake_form"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "Completeness"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "platform_cortex"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "scar_directionality"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "platform_morphology"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "platform_lipping"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "Bulb"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "shattered_bulb"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "Initiation"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "ventral_plan_form"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "Section"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "lateral_edge"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "flake_termination"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "Kombewa"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

ggplot(data=filter(irr_summary_subcat,variable == "distal_plan_form"),
       aes(y=alpha, x=reorder(sub_variable,alpha))) + 
   geom_bar(position=position_dodge(), stat="identity") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.99),
         axis.text = element_text(size = 10))+
   labs(x = "",
        y = "IRR value")+
   geom_hline(yintercept = 0.6, linetype = 2, colour = "red") +
   facet_wrap(~variable)

############################################### ######################
###### SUMMARY TABLES FOR CATEGORICAL VARIABLES ####
data1 <- comsafrica_data

#create a unique flake ID for each flake
summary(data1$assemblage_code)

data1$assemblage_code <- as.character(data1$assemblage_code)
data1$assemblage_code[data1$assemblage_code =="chert_condition_A"] <- "A"
data1$assemblage_code[data1$assemblage_code =="chert_condition_B"] <- "B"
data1$assemblage_code <- as.factor(data1$assemblage_code)

data1$cond_flake_id <- paste(data1$assemblage_code, data1$flake_id)
data1$cond_flake_id <- as.factor(data1$cond_flake_id)

summary(data1$cond_flake_id)

#completeness
summary(data1$completeness)
data1$completeness <- as.character(data1$completeness)
data1$completeness[data1$completeness %in% c("fragment", "shatter", "lateral")] <- "indeterminate"
data1$completeness[data1$completeness == ""] <- NA #could possibly be all grouped with indeterminate
data1$completeness <- as.factor(data1$completeness)

tableCOMPLETENESS <-  table(data1$cond_flake_id, data1$completeness)
tabCOMP=cbind(addmargins(round(prop.table(addmargins(tableCOMPLETENESS,1),1),2)*100,2), c(margin.table(tableCOMPLETENESS,1),sum(tableCOMPLETENESS)))
write.csv(tabCOMP, file="Tables/tablecompleteness.csv")

#damage
summary(data1$damage)
data1$damage <- as.character(data1$damage)
data1$damage[data1$damage ==""] <- "yes" #in that case, NAs correspond to yes
data1$damage <- as.factor(data1$damage)

table2 <- table(data1$cond_flake_id, data1$damage)
tabDAM=cbind(addmargins(round(prop.table(addmargins(table2,1),1),2)*100,2), c(margin.table(table2,1),sum(table2)))
write.csv(tabDAM, file="Tables/tabledamage.csv")

#platform_cortex
summary(data1$platform_cortex)

data1$platform_cortex <- as.character(data1$platform_cortex)
data1$platform_cortex[data1$platform_cortex == ""] <- NA
data1$platform_cortex[data1$platform_cortex == "INDET"] <- "indeterminate"
data1$platform_cortex[data1$platform_cortex == "Indeterminate"] <- "indeterminate"
data1$platform_cortex[data1$platform_cortex =="absent"] <- "Absent"
data1$platform_cortex[data1$platform_cortex =="complete"] <- "Complete"
data1$platform_cortex <- as.factor(data1$platform_cortex)
 

table3 <- table(data1$cond_flake_id, data1$platform_cortex)
tabCORTEXPLATF=cbind(addmargins(round(prop.table(addmargins(table3,1),1),2)*100,2), c(margin.table(table3,1),sum(table3)))
write.csv(tabCORTEXPLATF, file="Tables/tablecortexplatf.csv")

#directionality
summary(data1$directionality)

data1$directionality <- as.character(data1$directionality)
data1$directionality[data1$directionality == ""] <- NA
data1$directionality[data1$directionality == "centripetal"] <- "Centripetal"
data1$directionality[data1$directionality == "other"] <- "Other"
data1$directionality <- as.factor(data1$directionality)

table4 <- table(data1$cond_flake_id, data1$directionality)
tabDIRECTIONALITY=cbind(addmargins(round(prop.table(addmargins(table4,1),1),2)*100,2), c(margin.table(table4,1),sum(table4)))
write.csv(tabDIRECTIONALITY, file="Tables/tabledirectionality.csv")

#platf morph
summary(data1$platfmorph) #did not group together other with indet as separated in our original attribute list?

data1$platfmorph <- as.character(data1$platfmorph)
data1$platfmorph[data1$platfmorph == ""] <- NA
data1$platfmorph[data1$platfmorph == "Chapeau de Gendarme"] <- "ChapeauDeGendarme"
data1$platfmorph[data1$platfmorph == "Diherdral"] <- "Dihedral"
data1$platfmorph[data1$platfmorph == "facetted"] <- "Facetted"
data1$platfmorph[data1$platfmorph == "linear"] <- "Linear"
data1$platfmorph <- as.factor(data1$platfmorph)

table5 <- table(data1$cond_flake_id, data1$platfmorph)
tabPLATFMORPH=cbind(addmargins(round(prop.table(addmargins(table5,1),1),2)*100,2), c(margin.table(table5,1),sum(table5)))
write.csv(tabPLATFMORPH, file="Tables/tableplatfmorph.csv")

#platf lip
summary(data1$platflipp)

data1$platflipp <- as.character(data1$platflipp)
data1$platflipp[data1$platflipp == ""] <- NA
data1$platflipp[data1$platflipp %in% c("no", "NO")] <- "No"
data1$platflipp[data1$platflipp %in% c("yes", "YES")] <- "Yes"
data1$platflipp[data1$platflipp == "NOT APPLICABLE"] <- "Indeterminate"
data1$platflipp <- as.factor(data1$platflipp)

table6 <- table(data1$cond_flake_id, data1$platflipp)
tabPLATFLIPP=cbind(addmargins(round(prop.table(addmargins(table6,1),1),2)*100,2), c(margin.table(table6,1),sum(table6)))
write.csv(tabPLATFLIPP, file="Tables/tableplatflipp.csv")

#bulb
summary(data1$bulb)

data1$bulb <- as.character(data1$bulb)
data1$bulb[data1$bulb == ""] <- NA
data1$bulb[data1$bulb == "Indet"] <- "Indeterminate"
data1$bulb[data1$bulb == "NO"] <- "No"
data1$bulb[data1$bulb == "YES"] <- "Yes"
data1$bulb <- as.factor(data1$bulb)

table7 <- table(data1$cond_flake_id, data1$bulb)
tabBULB=cbind(addmargins(round(prop.table(addmargins(table7,1),1),2)*100,2), c(margin.table(table7,1),sum(table7)))
write.csv(tabBULB, file="Tables/tablebulb.csv")


#shattbulb

summary(data1$shattbulb) #one line from the dataset was corrected in the csv.

data1$shattbulb <- as.character(data1$shattbulb)
data1$shattbulb[data1$shattbulb == ""] <- NA
data1$shattbulb[data1$shattbulb %in% c("Indet","Indeterminateerminate" )] <- "Indeterminate"
data1$shattbulb[data1$shattbulb %in% c("no","NO" )] <- "No"
data1$shattbulb[data1$shattbulb =="YES"] <- "Yes"
data1$shattbulb <- as.factor(data1$shattbulb)

table8 <- table(data1$cond_flake_id, data1$shattbulb)
tabSHATTBULB=cbind(addmargins(round(prop.table(addmargins(table8,1),1),2)*100,2), c(margin.table(table8,1),sum(table8)))
write.csv(tabSHATTBULB, file="Tables/tableshattbulb.csv")

#initiation
summary(data1$initiation)

data1$initiation <- as.character(data1$initiation)
data1$initiation[data1$initiation == ""] <- NA #indet (N=1) should be as NA too?
data1$initiation[data1$initiation == "BENDING"] <- "Bending"
data1$initiation[data1$initiation %in% c("hertzian", "HERTZIAN")] <- "Hertzian"
data1$initiation[data1$initiation == "WEDGING"] <- "Wedging"
data1$initiation <- as.factor(data1$initiation)

table9 <- table(data1$cond_flake_id, data1$initiation)
tabINITIATION=cbind(addmargins(round(prop.table(addmargins(table9,1),1),2)*100,2), c(margin.table(table9,1),sum(table9)))
write.csv(tabINITIATION, file="Tables/tableinitiation.csv")

#ventral plane form

summary(data1$ventr_plane_form)

data1$ventr_plane_form <- as.character(data1$ventr_plane_form)
data1$ventr_plane_form[data1$ventr_plane_form == ""] <- NA
data1$ventr_plane_form[data1$ventr_plane_form == "BULBAR"] <- "Bulbar"
data1$ventr_plane_form[data1$ventr_plane_form == "CONCAVE"] <- "Concave"
data1$ventr_plane_form[data1$ventr_plane_form == "FLAT"] <- "Flat"
data1$ventr_plane_form[data1$ventr_plane_form == "TWISTED"] <- "Twisted"
data1$ventr_plane_form[data1$ventr_plane_form %in% c("VERY CONCAVE", "VERY_CONCAVE")] <- "Very_concave"
data1$ventr_plane_form <- as.factor(data1$ventr_plane_form)

table10 <- table(data1$cond_flake_id, data1$ventr_plane_form)
tabVENTRPLANEFORM=cbind(addmargins(round(prop.table(addmargins(table10,1),1),2)*100,2), c(margin.table(table10,1),sum(table10)))
write.csv(tabVENTRPLANEFORM, file="Tables/tableventralplaneform.csv")


#section

summary(data1$section)

data1$section <- as.character(data1$section)
data1$section[data1$section == ""] <- NA
data1$section[data1$section == "DOMED"] <- "Domed"
data1$section[data1$section == "INDET"] <- "Indeterminate"
data1$section[data1$section %in% c("LENTIC", "LENTICULAR")] <- "Lenticular"
data1$section[data1$section %in%c("RIGHTTRI", "RIGHTTRIANGLE")] <- "Righttriangle"
data1$section[data1$section %in% c( "TRAP", "TRAPEZOIDAL")] <- "Trapezoidal"
data1$section[data1$section %in% c("TRI", "TRIANGULAR")] <- "Triangular"
data1$section <- as.factor(data1$section)


table11 <- table(data1$cond_flake_id, data1$section)
tabSECTION=cbind(addmargins(round(prop.table(addmargins(table11,1),1),2)*100,2), c(margin.table(table11,1),sum(table11)))
write.csv(tabSECTION, file="Tables/tablesection.csv")

#lateral edge type

summary(data1$latedgetype)

data1$latedgetype <- as.character(data1$latedgetype)
data1$latedgetype[data1$latedgetype == ""] <- NA
data1$latedgetype[data1$latedgetype == "AMORPH"] <- "Amorphous"
data1$latedgetype[data1$latedgetype %in% c("CONV", "CONVERGENT")] <- "Convergent"
data1$latedgetype[data1$latedgetype == "DIAMOND"] <- "Diamond"
data1$latedgetype[data1$latedgetype %in% c("DIV", "DIVERGENT")] <- "Divergent"
data1$latedgetype[data1$latedgetype == "na"] <- "Indeterminate" #why not NA?
data1$latedgetype[data1$latedgetype == "INDET"] <- "Indeterminate"
data1$latedgetype[data1$latedgetype %in% c("OVAL", "OVOID")] <- "Ovoid"
data1$latedgetype[data1$latedgetype == "PARALLEL"] <- "Parallel"
data1$latedgetype <- as.factor(data1$latedgetype)

table12 <- table(data1$cond_flake_id, data1$latedgetype)
tabLATEDGE=cbind(addmargins(round(prop.table(addmargins(table12,1),1),2)*100,2), c(margin.table(table12,1),sum(table12)))
write.csv(tabLATEDGE, file="Tables/tablelatedge.csv")


#Flake termination

summary(data1$flaketerm)
data1$flaketerm <- as.character(data1$flaketerm)
data1$flaketerm[data1$flaketerm == ""] <- NA
data1$flaketerm[data1$flaketerm == "AXIAL"] <- "Axial"
data1$flaketerm[data1$flaketerm == "FEATHER"] <- "Feather"
data1$flaketerm[data1$flaketerm == "HINGE"] <- "Hinge"
data1$flaketerm[data1$flaketerm == "INDET"] <- "Indeterminate"
data1$flaketerm[data1$flaketerm == "OVERSHOT"] <- "Overshot"
data1$flaketerm <- as.factor(data1$flaketerm)

table13 <- table(data1$cond_flake_id, data1$flaketerm)
tabFLAKETERM=cbind(addmargins(round(prop.table(addmargins(table13,1),1),2)*100,2), c(margin.table(table13,1),sum(table13)))
write.csv(tabFLAKETERM, file="Tables/tableflaketerm.csv")


#Kombewa

summary(data1$kombewa) #0.84 value corresponds to a line that was offset compared to the columns

data1$kombewa <- as.character(data1$kombewa)
data1$kombewa[data1$kombewa == ""] <- NA
data1$kombewa[data1$kombewa == "YES"] <- "Yes"
data1$kombewa <- as.character(data1$kombewa)
data1$kombewa[data1$kombewa %in% c("no", "NO")] <- "No"
data1$kombewa <- as.factor(data1$kombewa)

table14 <- table(data1$cond_flake_id, data1$kombewa)
tabKOMB=cbind(addmargins(round(prop.table(addmargins(table14,1),1),2)*100,2), c(margin.table(table14,1),sum(table14)))
write.csv(tabKOMB, file="Tables/tablekombewa.csv")

#Distal PLan form

summary(data1$distplanform)

data1$distplanform <- as.character(data1$distplanform)
data1$distplanform[data1$distplanform == ""] <- NA
data1$distplanform[data1$distplanform == "FLAT"] <- "Flat"
data1$distplanform[data1$distplanform == "INDETERMINATE"] <- "INDET"
data1$distplanform[data1$distplanform %in%c( "IRR", "Irreg")] <- "Irregular"
data1$distplanform[data1$distplanform == "POINTED"] <- "Pointed"
data1$distplanform[data1$distplanform %in% c("rounded", "ROUNDED")] <- "Rounded"
data1$distplanform <- as.factor(data1$distplanform)

table15 <- table(data1$cond_flake_id, data1$distplanform)
tabDISTPLANFORM=cbind(addmargins(round(prop.table(addmargins(table15,1),1),2)*100,2), c(margin.table(table15,1),sum(table15)))
write.csv(tabDISTPLANFORM, file="Tables/tabledistplanform.csv")

#Reduction System

summary(data1$red_syst)

data1$red_syst <- as.character(data1$red_syst)
data1$red_syst[data1$red_syst == ""] <- NA
data1$red_syst[data1$red_syst %in% c ("idnet", "inde", "Indeterminate",
                                      "other", "Ind", "indet", "indeterminate",
                                      "INDET", "indeterminate (broken)",
                                      "Indeterminate (broken)",
                                      "LEVALLOIS OR DISCOID", "na", "none",
                                      "Lev or Disc", "Other (informal)")] <- "Indet"

data1$red_syst[data1$red_syst %in% c("Levallois indet", "Levallois non-Nubian",
                                     "levallois non nubian", "possible Levallois non-Nubian",
                                     "LEVALLOIS-RELATED", "LEVALLOIS/LEVALLOIS-RELATED", "Levallois (pref)",
                                     "potential levallois", "LEVALLOIS", 
                                     "levallois", "Levallois non Nubian",
                                     "Levallois?", "potential Lev",
                                     "potential Levallois", "Nubian")] <- "Levallois"

data1$red_syst[data1$red_syst %in% c("Discoidal", "CENTRIPETAL", 
                                     "discoid", "DISCOID",
                                     "Discoid?")] <- "Discoid"

data1$red_syst[data1$red_syst == "BIPOLAR"] <- "Bipolar"
data1$red_syst[data1$red_syst == "ON ANVIL"] <- "Bipolar"
data1$red_syst[data1$red_syst == "bipolar"] <- "Bipolar"

data1$red_syst[data1$red_syst %in% c("laminar", "Platform (laminar?)", 
                                     "Platform / Laminar", "multidirectional",
                                     "NON-LEVALLOIS; ORTHOGONAL VOLUME EXPLOITATION")] <-  "Platform"

data1$red_syst <- as.factor(data1$red_syst)

table16 <- table(data1$cond_flake_id, data1$red_syst)
tabREDSYST=cbind(addmargins(round(prop.table(addmargins(table16,1),1),2)*100,2), c(margin.table(table16,1),sum(table16)))
write.csv(tabREDSYST, file="Tables/tableredsyst.csv")


#flake form

summary(data1$flk_form)
data1$flk_form <- as.character(data1$flk_form)
data1$flk_form[data1$flk_form == ""] <- NA
data1$flk_form[data1$flk_form %in% c("Elong", "ELONG")] <- "BLADE"
data1$flk_form[data1$flk_form %in% c("flake", "Flake")] <- "FLAKE"
data1$flk_form[data1$flk_form == "Convflake"] <- "CONVFLAKE"
data1$flk_form <- as.factor(data1$flk_form)

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
summary(data1)

summary(data1$dorsal_cortex)

summary(data1$maximumdimension)
data1$maximumdimension[data1$maximumdimension==0] <- NA

summary(data1$mass)
data1$mass[data1$mass==0] <- NA

summary(data1$maximumwidth)
data1$maximumwidth[data1$maximumwidth==0] <- NA

summary(data1$maximumthickness)
data1$maximumthickness[data1$maximumthickness==0] <- NA

summary(data1$techlength)
data1$techlength[data1$techlength==0] <- NA

summary(data1$techmaxwidth)
data1$techmaxwidth[data1$techmaxwidth==0] <- NA

summary(data1$techmaxthickness)
data1$techmaxthickness[data1$techmaxthickness==0] <- NA

summary(data1$techwidthprox)
data1$techwidthprox[data1$techwidthprox==0] <- NA

summary(data1$techwidthmes)
data1$techwidthmes[data1$techwidthmes==0] <- NA

summary(data1$techwidthdist)
data1$techwidthdist[data1$techwidthdist==0] <- NA

summary(data1$techthickprox)
data1$techthickprox[data1$techthickprox==0] <- NA

summary(data1$techthickmes)
data1$techthickmes[data1$techthickmes==0] <- NA

summary(data1$techthickdist)
data1$techthickdist[data1$techthickdist==0] <- NA

summary(data1$platfwidth)
data1$platfwidth[data1$platfwidth==0] <- NA

summary(data1$platfthickmax)
data1$platfthickmax[data1$platfthickmax==0] <- NA

summary(data1$platfthickimpact)
data1$platfthickimpact[data1$platfthickimpact==0] <- NA


summary(data1$platfthickmid)
data1$platfthickmid <- as.numeric(data1$platfthickmid)
data1$platfthickmid[data1$platfthickmid==0] <- NA

summary(data1$edgeplatf)
data1$edgeplatf[data1$edgeplatf==0] <- NA

summary(data1$angle_height)
data1$angle_height <- as.numeric(data1$angle_height)


summary(data1$edgeplatf_deg) #should be recalculated

write.csv(data1, file="comsafrica_complete_adjusted_cleaned.csv")

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

test<- comsafrica_data_complete %>%
   select(c(new_flake_id,assemblage_code,dorsal_cortex,mass,maximumdimension,maximumwidth,maximumthickness,techlength,techmaxwidth,techmaxthickness,
            techwidthprox,techwidthmes,techwidthdist,techthickprox,techthickmes,techthickdist,
            platfwidth,platfthickimpact,platfthickmid,platfthickmax,edgeplatf)) %>%
   filter(!new_flake_id %in% c(1,89,97)) %>%
   mutate(across(c(4:21), na_if, 0)) %>%
   pivot_longer(!new_flake_id &!assemblage_code,
      names_to = "variable",
      values_to = "value") %>%
   group_by(new_flake_id,variable) %>%
   mutate(cv=cv(value, na.rm=T),
          mean=mean(value, na.rm=T),
          sd=sd(value, na.rm=T),
          min=min(value, na.rm=T),
          max=max(value, na.rm=T),
          median=median(value, na.rm=T),
          range=max-min) %>%
   distinct(new_flake_id,assemblage_code,variable,cv,mean,sd,min,max,median,range) %>%
   round_df(2)

## Range histograms

# dorsal_cortex COME BACK TO THIS AFTER CHECKING DATA FOR WEIRD RANGE
# VALUES
ggplot(data=filter(test,variable=="dorsal_cortex"), aes(x=range)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   labs(title="Cortex",
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

# maximumwidth
ggplot(data=filter(test,variable=="maximumwidth"), aes(x=range)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   labs(title="Maximum width",
        x ="Range of measurements", y = "Density")

# maximumthickness
ggplot(data=filter(test,variable=="maximumthickness"), aes(x=range)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   labs(title="Maximum thickness",
        x ="Range of measurements", y = "Density")

# techlength
ggplot(data=filter(test,variable=="techlength"), aes(x=range)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   labs(title="Technological length",
        x ="Range of measurements", y = "Density")

# techmaxwidth
ggplot(data=filter(test,variable=="techmaxwidth"), aes(x=range)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   labs(title="Technological max width",
        x ="Range of measurements", y = "Density")

# techmaxthickness
ggplot(data=filter(test,variable=="techmaxthickness"), aes(x=range)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   labs(title="Technological max thickness",
        x ="Range of measurements", y = "Density")

# techwidthprox
ggplot(data=filter(test,variable=="techwidthprox"), aes(x=range)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   labs(title="Technological width proximal",
        x ="Range of measurements", y = "Density")

# techwidthmes
ggplot(data=filter(test,variable=="techwidthmes"), aes(x=range)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   labs(title="Technological width mesial",
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

# techthickmes
ggplot(data=filter(test,variable=="techthickmes"), aes(x=range)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   labs(title="Technological thickness mesial",
        x ="Range of measurements", y = "Density")

# techthickdist
ggplot(data=filter(test,variable=="techthickdist"), aes(x=range)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   labs(title="Technological thickness distal",
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

# platfthickmid
ggplot(data=filter(test,variable=="platfthickmid"), aes(x=range)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   labs(title="Platform thickness mid-point",
        x ="Range of measurements", y = "Density")

# platfthickmax
ggplot(data=filter(test,variable=="platfthickmax"), aes(x=range)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   labs(title="Platform thickness maximum",
        x ="Range of measurements", y = "Density")

# edgeplatf
ggplot(data=filter(test,variable=="edgeplatf"), aes(x=range)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   labs(title="EPA",
        x ="Range of measurements", y = "Density")
