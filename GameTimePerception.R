#######################################################################################
#template_secondary_data_analysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky

#link to manuscript

## TEST - THIAGO RULES!!!!!!

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
#remove all objects and then check
rm(list = ls())
ls()
#dettach all packages
detach()

#command below will install all packages and is only run once. remove the #if this is the first time you are running the code on RStudio, and then you can add the hash tag again
#lapply(c("ggplot2", "psych", "RCurl", "irr", "car","Hmisc", "gmodels", "DAAG"), install.packages, character.only=T)


#####################################################################################
#TEST PACKAGES
#####################################################################################

if ("psych" %in% rownames(installed.packages()) == 1){
  cat("Pacote Instalado");
}else {
  install.packages("psych", repos = "http://cran-r.c3sl.ufpr.br", dep = TRUE)
}
if ("ggplot2" %in% rownames(installed.packages()) == 1){
  cat("Pacote Instalado");
}else {
  install.packages("ggplot2", repos = "http://cran-r.c3sl.ufpr.br", dep = TRUE)
}
if ("RCurl" %in% rownames(installed.packages()) == 1){
  cat("Pacote Instalado");
}else {
  install.packages("RCurl", repos = "http://cran-r.c3sl.ufpr.br", dep = TRUE)
}
if ("irr" %in% rownames(installed.packages()) == 1){
  cat("Pacote Instalado");
}else {
  install.packages("irr", repos = "http://cran-r.c3sl.ufpr.br", dep = TRUE)
}
if ("car" %in% rownames(installed.packages()) == 1){
  cat("Pacote Instalado");
}else {
  install.packages("car", repos = "http://cran-r.c3sl.ufpr.br", dep = TRUE)
}
if ("Hmisc" %in% rownames(installed.packages()) == 1){
  cat("Pacote Instalado");
}else {
  install.packages("Hmisc", repos = "http://cran-r.c3sl.ufpr.br", dep = TRUE)
}
if ("gmodels" %in% rownames(installed.packages()) == 1){
  cat("Pacote Instalado");
}else {
  install.packages("gmodels", repos = "http://cran-r.c3sl.ufpr.br", dep = TRUE)
}
if ("qpcR" %in% rownames(installed.packages()) == 1){
  cat("Pacote Instalado");
}else {
  install.packages("qpcR", repos = "http://cran-r.c3sl.ufpr.br", dep = TRUE)
}

#Apply packages
lapply(c("ggplot2", "psych", "RCurl", "irr", "car","Hmisc", "gmodels","qpcR"), library, character.only=T)

#####################################################################################
#IMPORTING DATA
#####################################################################################


#to load csv table you need this path on google drive /Google Drive/GameTimePerception/demographics_table.csv
path_csv <- paste("/Users/",Sys.getenv(c("USERNAME")),"/Google Drive/GameTimePerception/demographics_table.csv" , sep = "")
game_time_data <- read.csv(path_cvs)



#below will view data in a spreadsheet format. notice that in this all subsequent commands you have to replace templateData with whatever name you chose for your data object in the previous command

#View(templateData)

#below will list variable names, classes (integer, factor, etc), alternative responses
str(game_time_data)
#list variable names so that they can be used later
names(game_time_data)
#below will attach your data so that when you execute a command you don't have to write the name of your data object over and over again
attach(game_time_data)

#function below is used to recode variables. things to notice: replace old.var with the variable you are recoding, replace new.var with the variable you want to create. the whole recoding happens within " ". all character and factor variables will be within '', numbers will be displayed with digits (not inside '') or NA (also without '')
new.var  <- car::recode(old.var, " c(1,2) = 'A'; else = 'B' ")
game_time_data<- transform(game_time_data, group = factor(group, label = c("occasional", "chronic"), levels = 1:2)

###########################################################################################
#TABLE 1: DEMOGRAPHICS
###########################################################################################
#describes your entire dataset
describe(game_time_data)

summary(game_time_data)
qplot(age,group, geom="line")

#t.test, where outcome is a continuous variable and predictor is a dichotomous variable
t.test(outcome~predictor)

#chi square test where both outcome and predictor are categorical variables
CrossTable(outcome, predictor, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)


########################################################################################
# Table with models
########################################################################################

logisticmodel  <- glm(outcome ~ predictor + confounder,family=binomial(link="logit"))
summary(logisticmodel) #gives you model results
coefficients(logisticmodel) # model coefficients
confint(logisticmodel, level=0.95) # CIs for model parameters 
fitted(logisticmodel) # predicted values
residuals(logisticmodel) # residuals
influence(logisticmodel) # regression diagnostics
layout(matrix(c(1,2,3,4),2,2)) # creates the white space for 4 graphs/page 
plot(logisticmodel) #generates 4 graphs/page


#######################################################################################
#template_secondary_data_analysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. You are free: to Share — to copy, distribute and transmit the work to Remix — to adapt the work, under the following conditions: Attribution — You must attribute the work in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work). Noncommercial — You may not use this work for commercial purposes. With the understanding that: Waiver — Any of the above conditions can be waived if you get permission from the copyright holder. Public Domain — Where the work or any of its elements is in the public domain under applicable law, that status is in no way affected by the license. Other Rights — In no way are any of the following rights affected by the license: Your fair dealing or fair use rights, or other applicable copyright exceptions and limitations; The author's moral rights; Rights other persons may have either in the work itself or in how the work is used, such as publicity or privacy rights. Notice — For any reuse or distribution, you must make clear to others the license terms of this work. The best way to do this is with a link to this web page. For more details see http://creativecommons.org/licenses/by-nc/3.0/
#######################################################################################
