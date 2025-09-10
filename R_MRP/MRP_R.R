#Delete all variables
rm(list=ls(all=TRUE))

#set working directory 
setwd('C:\\Users\\zhaoye\\Desktop')

install.packages("haven")
install.packages("lme4")
library("haven")
library("arm")
library("foreign")
library("lme4")
library("dplyr")

library(rstan)
library(rstanarm)
library(data.table)
library(dplyr)
library(forcats)
library(tidyr)
library(reshape2)
library(stringr)
library(readr)
library(ggplot2)
library(scales)
library(bayesplot)
library(gridExtra)
install.packages("ggalt")
library(ggalt)
install.packages("usmap")
library(usmap)
library(gridExtra)
library(scales)
install.packages("kableExtra")
library(kableExtra)
install.packages("formatR")
library(formatR)


#read in LRFWRP Postdata Country_pred
LRFWRP <- read_dta("LRFWRP.dta")
LRFWRP_data <- LRFWRP[order(LRFWRP$country_code),]

# We set the seed to an arbitrary number for reproducibility.
set.seed(1010)
                                                                                                        
#read in country-level dataset
country_pred <- read_dta("country_pred.dta") 
country_pred <- country_pred[order(country_pred$country_code),]

#read in Poststratification data
Postdata <- read_dta("Postdata.dta")
Postdata <- Postdata[order(Postdata$country_code),]

# Expand country-level predictors to the individual level
post_countrylevel <- match(Postdata$country_code, country_pred$country_code)

# Fit in stan_glmer
install.packages("rstanarm")
library("rstanarm")
library("rstan")

#run individual-level opinion model
MRPsmodel <- stan_glmer(climate ~ (1 | country_code) + (1 | gender) + (1 | age) + (1|education) 
                        +(1|income) + (1|PGDP) + (1|WGI) + (1|PEA) + (1|TUR) + (1|Tmax) + (1|FAAW),
                        family = binomial(link = "logit"),
                        data = LRFWRP_data,
                        prior = normal(0, 1, autoscale = TRUE),
                        prior_covariance = decov(scale = 0.50),
                        adapt_delta = 0.99,
                        refresh = 0)
saveRDS(MRPsmodel , file = "MRPs.model.rds")
MRPsmodel  <- readRDS("MRPs.model.rds")
print(MRPsmodel)

#examine random effects and standard errors
ranef(MRPsmodel)$country_code
ranef(MRPsmodel)$gender
ranef(MRPsmodel)$age
ranef(MRPsmodel)$education
ranef(MRPsmodel)$income
ranef(MRPsmodel)$PGDP
ranef(MRPsmodel)$WGI
ranef(MRPsmodel)$PEA
ranef(MRPsmodel)$TUR
ranef(MRPsmodel)$Tmax
ranef(MRPsmodel)$FAAW
se.ranef(MRPsmodel)$gender

# Expand country level predictors to the individual level
postspred <- merge(posts, predsm, by = "country_code",  all=T)

##create vector of country ranefs and then fill in missing ones
country.ranefs <- array(NA, c(133,1))
dimnames(country.ranefs) <- list(c(country_pred$country_code), "effect")
for(i in country_pred$country_code){
  country.ranefs[i,1] <- ranef(MRPsmodel)$country_code[i,1]
}
country.ranefs[,1][is.na(country.ranefs[,1])] <-0

#create a prediction for each cell in Census data
cellpred <- invlogit(fixef(MRPsmodel)["(Intercept)"]
                     +ranef(MRPsmodel)$gender[LRFWRP_data$gender,1]
                     +ranef(MRPsmodel)$age[LRFWRP_data$age,1]
                     +ranef(MRPsmodel)$education[LRFWRP_data$education,1]
                     +country.ranefs[LRFWRP_data$country_code,1]
                     +ranef(MRPsmodel)$income[LRFWRP_data$income,1]
                     +ranef(MRPsmodel)$PGDP[LRFWRP_data$PGDP,1]
                     +ranef(MRPsmodel)$WGI[LRFWRP_data$WGI,1]
                     +ranef(MRPsmodel)$PEA[LRFWRP_data$PEA,1]
                     +ranef(MRPsmodel)$TUR[LRFWRP_data$TUR,1]
                     +ranef(MRPsmodel)$Tmax[LRFWRP_data$Tmax,1]
                     +ranef(MRPsmodel)$FAAW[LRFWRP_data$FAAW,1]
)

#weight the prediction by the frequency of each cell
cellpredweighted <- cellpred * Postdata$countn

#now calculate the percent within each country (weighted average of responses)
countrypred <- 100* as.vector(tapply(cellpredweighted,Postdata$country_code,sum))
countrypred

# Posterior_epred returns the posterior estimates for the different subgroups stored in the poststrat_df dataframe. 
epred_mat <- posterior_epred(MRPsmodel, newdata = Postdata, draws = 500)
mrp_estimates <- epred_mat %*% Postdata$countn / sum(Postdata$countn)
MRPestimate <- c(mean = mean(mrp_estimates), sd = sd(mrp_estimates))
cat("MRP estimate mean, sd: ", round(MRPestimate, 3))
# MRP estimate mean, sd:  0.674 0.052 