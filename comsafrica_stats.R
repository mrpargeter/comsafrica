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

############################## 
# Set working directory and load datafile
##############################

# Set to source directory with relevant .csv datafile provided
# through the Open Science repository
getwd()

## Add datasets

comsafrica_data<-read.csv("comsafrica_complete_adjusted.csv")


####################################
# exploring data to identify and rectify potential numbering mistakes
##################################

summary(comsafrica_data)
attach(comsafrica_data)
cols <- c("assemblage_code", "analyst_id", "flake_id", "completeness", "damage",
          "dorsal_cortex", "Dorsal_Cortex_Location", "dorsal_cortex_location_other", 
          "platform_cortex", "dorsal_scar_count", "directionality", "Proximal_Scars",
          "Left_Scars", "Distal_Scars", "Right_Scars", "PLATFMORPH", "PLATFLIPP", "BULB",
          "SHATTBULB", "INITIATION", "VENTR_PLANE_FORM", "SECTION", "LATEDGETYPE", "FLAKETERM",
          "DISTPLANFORM", "KOMBEWA", "FLK_TYPE", "RED_SYST", "FLK_FORM")
data1[cols] <- lapply(comsafrica_data[cols], factor)


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

# change column names to lower case

colnames(comsafrica_data) <- tolower(colnames(comsafrica_data))

## subset

comsafrica_data_complete<-comsafrica_data %>%
      subset(completeness=="complete") %>%
      select(c(assemblage_code,analyst_id,flake_id,proximal_scars,left_scars,distal_scars,right_scars,
               mass,maximumdimension,maximumwidth,maximumthickness,techlength,techmaxwidth,techmaxthickness,
               techwidthprox,techwidthmes,techwidthdist,techthickprox,techthickmes,techthickdist,
               platfwidth,platfthickimpact,platfthickmid,platfthickmax,edgeplatf_deg))

distinct(comsafrica_data_complete, flake_id, .keep_all = TRUE)

comsafrica_data_complete_condb<-comsafrica_data_complete %>%
      subset(assemblage_code=="chert_condition_B")

comsafrica_data_complete_conda<-comsafrica_data_complete %>%
      subset(assemblage_code=="chert_condition_A") #%>%
      #filter(distinct(comsafrica_data_complete$flake_id))

##### Inter rater visualizations and data analyses

# irr

theta = 0.5
N = 20
flips1 <- rbinom(n = N, size = 1, prob = theta)
flips2 <- rbinom(n = N, size = 1, prob = theta)
flips3 <- rbinom(n = N, size = 1, prob = theta)
coins<-cbind(flips1, flips2,flips3)

library(irr)

agree(coins, tolerance=0)
kappa2(coins)

### repeatability coefficients

library(rptR)
## Condition b
# maxdim
hist(comsafrica_data_complete_condb$maximumdimension)
comsafrica_maxdim_boot<-rpt(maximumdimension ~ (1 | flake_id), 
                  grname = "flake_id", 
                  data = comsafrica_data_complete_condb, 
                  datatype = "Gaussian", 
                  nboot = 1000, npermut = 0)
summary(comsafrica_maxdim_boot)
plot(comsafrica_maxdim_boot, type = "boot", cex.main = 0.8,main="maximum dimension")

#mass
comsafrica_mass<-rpt(mass ~ (1 | flake_id), 
                       grname = "flake_id", 
                       data = comsafrica_data_complete_condb, 
                       datatype = "Gaussian", 
                       nboot = 1000, npermut = 0)
summary(comsafrica_mass)
plot(comsafrica_mass, type = "boot", cex.main = 0.8,main="mass")

#flake width
comsafrica_maxwidth<-rpt(maximumwidth ~ (1 | flake_id), 
                     grname = "flake_id", 
                     data = comsafrica_data_complete_condb, 
                     datatype = "Gaussian", 
                     nboot = 1000, npermut = 0)
plot(comsafrica_maxwidth, type = "boot", cex.main = 0.8,main="max width")

#flake max thickness
comsafrica_maxthick<-rpt(maximumthickness ~ (1 | flake_id), 
                         grname = "flake_id", 
                         data = comsafrica_data_complete_condb, 
                         datatype = "Gaussian", 
                         nboot = 1000, npermut = 0)
plot(comsafrica_maxthick, type = "boot", cex.main = 0.8,main="max thickness")

#flake tech length
comsafrica_techlength<-rpt(techlength ~ (1 | flake_id), 
                         grname = "flake_id", 
                         data = comsafrica_data_complete_condb, 
                         datatype = "Gaussian", 
                         nboot = 1000, npermut = 0)
plot(comsafrica_techlength, type = "boot", cex.main = 0.8,main="tech length")

#flake tech max width
comsafrica_techmaxwidth<-rpt(techmaxwidth ~ (1 | flake_id), 
                           grname = "flake_id", 
                           data = comsafrica_data_complete_condb, 
                           datatype = "Gaussian", 
                           nboot = 1000, npermut = 0)
plot(comsafrica_techmaxwidth, type = "boot", cex.main = 0.8,main="tech max width")

#flake tech max thickness
comsafrica_techmaxthick<-rpt(techmaxthickness ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_condb, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 0)
plot(comsafrica_techmaxthick, type = "boot", cex.main = 0.8,main="tech max thickness")

#flake tech width prox
comsafrica_techwidthprox<-rpt(techwidthprox ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_condb, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 0)
plot(comsafrica_techwidthprox, type = "boot", cex.main = 0.8,main="tech width proximal")

#flake tech width mes
comsafrica_techwidthmes<-rpt(techwidthmes ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_condb, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
plot(comsafrica_techwidthmes, type = "boot", cex.main = 0.8,main="tech width medial")

#flake tech width dist
comsafrica_techwidthdist<-rpt(techwidthdist ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_condb, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 0)
plot(comsafrica_techwidthdist, type = "boot", cex.main = 0.8,main="tech width dist")

#flake tech thick prox
comsafrica_techtechthickprox<-rpt(techthickprox ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_condb, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
plot(comsafrica_techtechthickprox, type = "boot", cex.main = 0.8,main="tech thick prox")

#flake tech thick med
comsafrica_techtechthickmes<-rpt(techthickmes ~ (1 | flake_id), 
                                  grname = "flake_id", 
                                  data = comsafrica_data_complete_condb, 
                                  datatype = "Gaussian", 
                                  nboot = 1000, npermut = 0)
plot(comsafrica_techtechthickmes, type = "boot", cex.main = 0.8,main="tech thick medial")

#flake tech thick dist
comsafrica_techthickdist<-rpt(techthickdist ~ (1 | flake_id), 
                                 grname = "flake_id", 
                                 data = comsafrica_data_complete_condb, 
                                 datatype = "Gaussian", 
                                 nboot = 1000, npermut = 0)
plot(comsafrica_techthickdist, type = "boot", cex.main = 0.8,main="tech thick dist")

#flake platform width
comsafrica_platfwidth<-rpt(platfwidth ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_condb, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
plot(comsafrica_platfwidth, type = "boot", cex.main = 0.8,main="platform width")

#flake platform thickness
comsafrica_platfthickmax<-rpt(platfthickmax ~ (1 | flake_id), 
                           grname = "flake_id", 
                           data = comsafrica_data_complete_condb, 
                           datatype = "Gaussian", 
                           nboot = 1000, npermut = 0)
plot(comsafrica_platfthickmax, type = "boot", cex.main = 0.8,main="platform thickness")

#flake EPA
comsafrica_edgeplatf_deg<-rpt(edgeplatf_deg ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_condb, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
plot(comsafrica_edgeplatf_deg, type = "boot", cex.main = 0.8,main="EPA")

## Condition a ###

# maxdim
comsafrica_maxdim_boot<-rpt(maximumdimension ~ (1 | flake_id), 
                            grname = "flake_id", 
                            data = comsafrica_data_complete_conda, 
                            datatype = "Gaussian", 
                            nboot = 1000, npermut = 0)
plot(comsafrica_maxdim_boot, type = "boot", cex.main = 0.8,main="maximum dimension")

#mass
comsafrica_mass<-rpt(mass ~ (1 | flake_id), 
                     grname = "flake_id", 
                     data = comsafrica_data_complete_conda, 
                     datatype = "Gaussian", 
                     nboot = 1000, npermut = 0)
plot(comsafrica_mass, type = "boot", cex.main = 0.8,main="mass")

#flake width
comsafrica_maxwidth<-rpt(maximumwidth ~ (1 | flake_id), 
                         grname = "flake_id", 
                         data = comsafrica_data_complete_conda, 
                         datatype = "Gaussian", 
                         nboot = 1000, npermut = 0)
plot(comsafrica_maxwidth, type = "boot", cex.main = 0.8,main="max width")

#flake max thickness
comsafrica_maxthick<-rpt(maximumthickness ~ (1 | flake_id), 
                         grname = "flake_id", 
                         data = comsafrica_data_complete_conda, 
                         datatype = "Gaussian", 
                         nboot = 1000, npermut = 0)
plot(comsafrica_maxthick, type = "boot", cex.main = 0.8,main="max thickness")

#flake tech length
comsafrica_techlength<-rpt(techlength ~ (1 | flake_id), 
                           grname = "flake_id", 
                           data = comsafrica_data_complete_conda, 
                           datatype = "Gaussian", 
                           nboot = 1000, npermut = 0)
plot(comsafrica_techlength, type = "boot", cex.main = 0.8,main="tech length")

#flake tech max width
comsafrica_techmaxwidth<-rpt(techmaxwidth ~ (1 | flake_id), 
                             grname = "flake_id", 
                             data = comsafrica_data_complete_conda, 
                             datatype = "Gaussian", 
                             nboot = 1000, npermut = 0)
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
comsafrica_edgeplatf_deg<-rpt(edgeplatf_deg ~ (1 | flake_id), 
                              grname = "flake_id", 
                              data = comsafrica_data_complete_conda, 
                              datatype = "Gaussian", 
                              nboot = 1000, npermut = 0)
plot(comsafrica_edgeplatf_deg, type = "boot", cex.main = 0.8,main="EPA")

#### Testing Condition A vs. Condition B

comsafrica_data_complete_summary<-comsafrica_data_complete %>%
      group_by(flake_id) %>%
      summarise_at(c("mass","maximumdimension","maximumwidth","maximumthickness",
                     "techlength","techmaxwidth","techmaxthickness","techwidthprox","techwidthmes",
                     "techwidthdist","techthickprox","techthickmes","techthickdist","platfwidth",
                     "platfthickimpact","platfthickmid","platfthickmax","edgeplatf_deg"),mean)

# Bland altman plots

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

wide_comsafrica<- comsafrica_data_complete_condb_maximumdimension %>% spread(analyst_id, maximumdimension) %>%
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

# correlation matrices
wide_comsafrica_matrix<-wide_comsafrica %>%
      select(-c(assemblage_code,flake_id,Avg))

res <- cor(wide_comsafrica_matrix)

