######################
# Title:    All about feelings? Emotional appeals as Drivers of User Engagement with Facebook Posts
# Date:     17.12.2021
# Output:   R script
######################




#Load libraries
library(tidyverse) #data manipulation, analysis
library(dplyr)
library(knitr) #for generating tables
library(kableExtra) #save kable/table
library(corrr) #correlations
library(GGally) #plot correlations, extension ggplot2
library(ggplot2) #graphs
library(gridExtra) #combine graphs into a grid
library(lme4) #multilevel model
library(merTools) #analysis multilevel models
library(jtools) #summary statistics multilevel model
library(lattice) #plots assumptions checks
library(car) #multicollinearity assumption mixed effects model
library(ggstatsplot) #outliers engagement variable
library(stringr) #length of post as control variable
library(extrafont) #changing graphs to APA fonts


## Data cannot be provided due to Facebook TOS restrictions
data = read.csv("*.csv", header = TRUE, sep = ";" ) 
summary(data)

options(scipen = 999) #displaying decimals, not scientific notation

################################### Create DV Engagement variable ########################################


#Remove obs. with missing data
data <- data %>% drop_na()


#Create DV=engagement variable
data <- data %>% 
  mutate(Followers = 
           case_when(data$Profile == "Boris Johnson" ~ 1811737,
                     data$Profile == "Conservatives" ~ 751561,
                     data$Profile == "David Cameron" ~ 1147033,
                     data$Profile == "Jeremy Corbyn" ~ 1641689,
                     data$Profile == "Labour" ~ 1096195,
                     data$Profile == "Liberal Democrats" ~ 204371,
                     data$Profile == "Nicola Sturgeon" ~ 409672,
                     data$Profile == "Nigel Farage" ~ 1002437,
                     data$Profile == "SNP" ~ 325702,
                     data$Profile == "Tim Farron" ~ 36321,
                     data$Profile == "UKIP" ~ 515912))

data <- data %>%
  mutate(Engagement = (Reactions + Comments + Shares)/Followers) #change to unweighted engagement variable, but accounting for follower base

#Histogram engagement variable

hist(data$Engagement)
hist(log(data$Engagement))
summary(data$Engagement) # Min=0.00001, Max=0.22, M=0.01
summary(log(data$Engagement)) # Min=-11.13, Max=-1.52, M=-5.50



################################### Create control variables ########################################


#Create frequency of posts/account variable
table(data$Profile)

data <- data %>% 
  mutate(Freq_post = 
           case_when(data$Profile == "Boris Johnson" ~ 86,
                     data$Profile == "Conservatives" ~ 37,
                     data$Profile == "David Cameron" ~ 104,
                     data$Profile == "Jeremy Corbyn" ~ 163,
                     data$Profile == "Labour" ~ 174,
                     data$Profile == "Liberal Democrats" ~ 146,
                     data$Profile == "Nicola Sturgeon" ~ 33,
                     data$Profile == "Nigel Farage" ~ 142,
                     data$Profile == "SNP" ~ 138,
                     data$Profile == "Tim Farron" ~ 48,
                     data$Profile == "UKIP" ~ 132))

#Create politician/party variable
data <- data %>% 
  mutate(Party_pol = 
           case_when(data$Profile == "Boris Johnson" |
                       data$Profile == "David Cameron" |
                       data$Profile == "Jeremy Corbyn" |
                       data$Profile == "Nicola Sturgeon" |
                       data$Profile == "Nigel Farage" |
                       data$Profile == "Tim Farron"~ 1, 
                     data$Profile == "Conservatives" |
                       data$Profile == "Labour" |
                       data$Profile == "Liberal Democrats" |
                       data$Profile == "SNP" |
                       data$Profile == "UKIP" ~ 0))


#Create populist content variable (any of the 3 elements present)
data <- data %>% 
  mutate(Pop_cont = 
           case_when(People == 1 |
                       Elites == 1 |
                       Others == 1 ~ 1,
                     TRUE ~ 0))

data %>% group_by(Pop_cont) %>% tally()
table(data$Others)


#Create emotional appeal variable (any of the 4 emotions present)
data <- data %>%
  mutate(Emo_cont =
           case_when(Pride == 1 |
                       Anger == 1 |
                       Fear == 1 |
                       Enthusiasm == 1 ~ 1,
                     TRUE ~ 0))

data %>% group_by(Emo_cont) %>% tally()

#Create emotional appeal co-occurence variable
data <- data %>%
  mutate(Emo_coocurrence = (Pride + Anger + Fear + Enthusiasm))

data %>% group_by(Emo_coocurrence) %>% tally() %>% mutate(freq = n /sum(n)) #proportions of co-ocurrence


#Create post length variable
data_length = read.csv("*.csv", header = TRUE, sep = "," ) 
data_length <- data_length %>% drop_na() #remove the same posts without text, and other missing data
data_length = subset(data_length, select = c(Message, Time))

data_length <- data_length[!duplicated(data_length$Time), ] #Remove duplicates on time, so they won't create more than one obs in the left join

data_complete <- merge(x = data, y = data_length, by = "Time", all.x = TRUE)
sum(is.na(data_complete)) #no missing data, removed before

data <- data_complete
data$Post_length = str_length(data$Message) #length of string
hist(data$Post_length) #histogram length of post
sum(is.na(data$Post_length)) #no missing values


################################### Variable renaming/aggregation ########################################

#Creating normalized engagement variable
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
} #normalization function range 0-1

data$Engagement_norm<-normalize(data$Engagement)

hist(data$Engagement)
hist(data$Engagement_norm)


#Aggregate tradition & values 
data <- data %>%
mutate(Tradition_Values =
         case_when(Tradition == 1 |
                     Values == 1 ~ 1,
                   TRUE ~ 0))

data %>% group_by(Tradition_Values) %>% tally()


#Aggregate Labour & healthcare 
data <- data %>%
  mutate(Labour_Welfare =
           case_when(Labour == 1 |
                       Healthcare == 1 ~ 1,
                     TRUE ~ 0))

data %>% group_by(Labour_Welfare) %>% tally()


#Aggregate Foreign, Incident & Security.defense
data <- data %>%
  mutate(security_Foreign =
           case_when(Foreign == 1 |
                       Incident == 1 |
                       Security.defence == 1 ~ 1,
                     TRUE ~ 0))

data %>% group_by(security_Foreign) %>% tally()


################################### Descriptive statistics ########################################

#Table 1 - mean and sd engagement per profile
data %>% group_by(Profile) %>%
  summarize(n=n(), mean_eng = mean(Engagement_norm), sd_eng = sd(Engagement_norm)) %>%
  kable() %>%
  kable_styling ()

mean(data$Engagement_norm)
sd(data$Engagement_norm)


#Table 2 - descriptives post-level variables
table(data$Information) #absolute values for each variable in table 2
prop.table(table(data$Information))

#Table 3 - emotional appeals for each political profile
data %>% group_by(Profile) %>%
  summarize(mean_anger = mean(Anger), mean_fear = mean(Fear),
            mean_pride = mean(Pride), mean_enthusiasm = mean(Enthusiasm), 
            mean_total = mean(Emo_cont)) %>%
  kable() %>%
  kable_styling ()




################################### Hypotheses testing + additional analyses ########################################

#Checking correlations between appeals to emotions and populist content
data_corr <- data %>% dplyr::select(c("Fear", "Anger", "Pride", "Enthusiasm", "People", "Elites", "Others"))
correlate(data_corr)

#Correlation graph

font_import()
loadfonts(device = "win")

ggcorr(data_corr, method = c("everything", "pearson"), label = TRUE, nbreaks = 9, palette = "Greys")+
  labs(title="Figure 1. Correlation matrix between appeals to emotions and populist content")+
  theme(plot.title = element_text(hjust = 2.0), 
        text = element_text(family = "Calibri"),
        axis.text.y= element_text(family = "Calibri"))



#Random intercept model - null model (Model 0)
model0<-lmer(log(Engagement) ~ 1 + (1|Profile), REML=FALSE, data=data)
summ(model0)
(exp(fixef(model0))-1)*100
(exp(se.fixef(model0))-1)*100

#Model including emotions (model 1)
model1<-lmer(log(Engagement) ~ Fear + Anger + Pride + Enthusiasm + (1|Profile), REML=FALSE, data=data)
summ(model1) #from jtools package
(exp(fixef(model1))-1)*100
(exp(se.fixef(model1))-1)*100

#Comparing models
anova(model0, model1)

#Full model engagement - emotions + controls (model 2)
model2<-lmer(log(Engagement) ~ Fear + Anger + Pride + Enthusiasm + 
               People + Elites + Others + 
               Party_pol + Campaign + Freq_post + Post_length + 
               Immiration + Soveregnty + Economy + (1|Profile), REML=FALSE, data=data)

summ(model2) #from jtools package
(exp(fixef(model2))-1)*100
(exp(se.fixef(model2))-1)*100

#Comparing models
anova(model1, model2)

#Check coefficient engagement for brexit referendum
data %>% group_by(Campaign) %>%
  summarize(mean_eng = mean(Engagement_norm))
#confirmed, eng lower during campaign, higher after the vote overall


## Assumptions Full model engagement - emotions + controls (model 2)
model2<-lmer(log(Engagement) ~ Fear + Anger + Pride + Enthusiasm + 
               People + Elites + Others + 
               Party_pol + Campaign + Freq_post + Post_length + 
               Immiration + Soveregnty + Economy + (1|Profile), REML=FALSE, data=data)

summ(model2) #from jtools package
(exp(fixef(model2))-1)*100

#Checking linearity
plot(resid(model2),log(data$Engagement))

#Checking homogeneity of variance 
plot(model2) #creates a fitted vs residual plot

#Normal distribution of residuals
qqmath(model2)

#Checking multicollinearity
vif(model2)



################################### Robustness checks ########################################

#Create alternative engagement var

data <- data %>%
  mutate(Engagement_alt2 = Reactions + Comments + Shares) #not weighted by followers
summary(data$Engagement_alt2) # Min=3.0, Max=357655.0, M=8225.6
hist(data$Engagement_alt2) #histogram positively skewed, will take log
table(data$Engagement_alt2)

#Model 3 - Engagement_alt2
model3<-lmer(log(Engagement_alt2) ~ Fear + Anger + Pride + Enthusiasm + 
               People + Elites + Others + 
               Party_pol + Campaign + Freq_post + Post_length +
               Immiration + Soveregnty + Economy + (1|Profile), REML=FALSE, data=data)

summ(model3) #from jtools package
(exp(fixef(model3))-1)*100
(exp(se.fixef(model3))-1)*100


