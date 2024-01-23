# ACCT337 STATISTICAL PROGRAMMING FINAL PROJECT
# AUTHORS: G1 GROUP 5

########## SETTING UP WORKSPACE ########## 
options(scipen=999, digits=4)
rm(list=ls()) 
setwd("/Users/Clarice/Desktop/SP Project")

# install often-used packages
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("car")
install.packages("psych")
install.packages("forecast")
install.packages("lubridate")
install.packages("zoo")
install.packages("caret")
install.packages("lfe")
install.packages("broom")
install.packages("stargazer")
install.packages("fixest")
install.packages("hrbrthemes")

# load often-used user-defined functions and packages
library(readr)
library(dplyr)
library(ggplot2) 
library(corrplot)
library(car)
library(psych)
library(forecast)
library(lubridate)
library(caret)
library(lfe)
library(broom)
library(stargazer)
library(fixest)
library(hrbrthemes)

finitize <- function(x)	ifelse(is.finite(x), x, NA)

mergify <- function(x, index, suffix, ...){
  require(dplyr)
  x_vars <- x[c(index, ...)]
  colnames(x_vars) <- paste(colnames(x_vars), suffix, sep = "_")
  left_join(x, x_vars, by = paste(index, suffix, sep = "_"))
}
averagefy <- function(data, group_column, impute_columns) {
  data %>%
    group_by({{group_column}}) %>%
    mutate(across(all_of(impute_columns), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    ungroup()
}

completify <- function(x, ...)	x[complete.cases(x[c(...)]),]

inspectify <- function(x , d=3){
  nums = c(mean(x, na.rm=TRUE),  median(x, na.rm=TRUE), min(x, na.rm=TRUE), max(x, na.rm=TRUE), sd(x, na.rm=TRUE))
  names(nums) = c("Mean", "Median", "Min",  "Max", "Stdev")
  round(nums, digits=d) 
}


### LOAD execucomp and computstat INTO R STUDIO ENVIRONMENT BEFORE PROCEEDING ###
execucomp <- read_csv("execucomp_19922022.csv")
compustat <- read_csv("compustat_19502023.csv")

########## INITIAL DATA CLEANING ##########

# check whether fyear/year has null values
mean(is.na(execucomp$YEAR)) 
mean(is.na(compustat$fyear))

# fill in fyear/year if missing, based on compustat’s May cut-off
compustat$fyear <- ifelse(
  is.na(compustat$fyear),
  ifelse(
    as.numeric(format(compustat$datadate, format = "%m")) > 5,
    as.numeric(format(compustat$datadate, format = "%Y")),
    as.numeric(format(compustat$datadate, format = "%Y")) - 1
  ), compustat$fyear
)

# remove duplicate firm-year observations from compustat
compustat_a <- compustat %>% distinct(gvkey, fyear, .keep_all = T)

# remove non-US-listed observations
compustat_b <- filter(compustat_a, fic == "USA")
# remove non-CEO executives
execucomp_a <- filter(execucomp, CEOANN == "CEO")

# remove observations not in 2010 to 2019
compustat_b <- filter(compustat_b, between(fyear, 2010, 2019))
execucomp_a <- filter(execucomp_a, between(YEAR, 2010, 2019))

# join the database to master data
data_raw<- execucomp_a %>% left_join(compustat_b, by = c("GVKEY" = "gvkey", "YEAR" = "fyear"))

# verify that all observations have GVKEY and YEAR
nrow(data_raw) - nrow(subset(data_raw, !is.na(GVKEY) & !is.na(YEAR)))

# check number of rows
nrow(data_raw)

# understand structure
str(data_raw)

# set up firm-year indices based on GVKEY and YEAR for merging data
data_raw$index_n2 <- paste(data_raw$GVKEY, data_raw$YEAR - 2, sep = "_")
data_raw$index_n1 <- paste(data_raw$GVKEY, data_raw$YEAR - 1, sep = "_")
data_raw$index <- paste(data_raw$GVKEY, data_raw$YEAR, sep = "_")
data_raw$index_p1 <- paste(data_raw$GVKEY, data_raw$YEAR + 1, sep = "_")
data_raw$index_p2 <- paste(data_raw$GVKEY, data_raw$YEAR + 2, sep = "_")

# remove CEOs who were replaced/appointed that year
# lag EXECID for same GVKEY if !lag_execid = execid or lag_execid = NULL, means newly appointed
data_raw <- arrange(data_raw, GVKEY, YEAR)
data_raw<- data_raw %>%group_by(GVKEY) %>% mutate(EXECID_lag = lag(EXECID, n = 1)) %>% ungroup()
data_raw <- filter(data_raw, EXECID_lag == EXECID)

# final check to see if each observation corresponds to a unique GVKEY-YEAR combo
nrow(data_raw) 
length(unique(data_raw$index)) 

# choose specific variables pertaining to our research
funda_raw <- data_raw %>% select(EXECID, SIC, conm, GVKEY, YEAR, TDC1, AGE, SHROWN_EXCL_OPTS_PCT, TOTAL_CURR, EXECDIR, BECAMECEO, at, capx, xrd, oiadp, prcc_f, csho, lt, ni, index_n2, index_n1, index, index_p1, index_p2)

########## ACCOUNTING FOR MISSING DATA ##########

# exclude missing observations: TDC1
funda_a <- filter(funda_raw,!is.na(TDC1))

# replace missing/negative with 0: SHROWN_EXCL_OPTS_PCT
funda_b <- funda_a %>% mutate(SHROWN_EXCL_OPTS_PCT = ifelse(is.na(SHROWN_EXCL_OPTS_PCT) | SHROWN_EXCL_OPTS_PCT < 0,0, SHROWN_EXCL_OPTS_PCT))

# replace with industry average

funda_c <- averagefy(funda_b, SIC, c("AGE", "lt", "at", "capx", "xrd", "oiadp", "ni", "prcc_f", "csho"))
summary(funda_c)

# for majority of these variables like at, averagefy does not work on 11 observations as there is no financial data available for the entire industry/SIC. we will proceed to exclude these industries from our sample
funda_d <- filter(funda_c, !is.na(at))

# xrd data is not available for entire industries as well e.g., SIC == 6020 which is creation of television programmes from purchased components. we can assume that these industries do not have any R&D expenses so we will replace these with 0.
funda <- funda_d %>% mutate(xrd = ifelse(is.na(xrd), 0, xrd))

########## CALCULATION OF RATIOS ########## 

# arrange dataset in ascending order by index
arrange(funda, index)

# get lagged total assets (at)
funda_lag <- mergify(funda, "index", "n1", "at") 

# average at calculation
funda_lag <- mutate(funda_lag, ave_at = (at + at_n1) / 2)

# incentive compensation calculation
funda_lag <- mutate(funda_lag, inc = finitize((TDC1-TOTAL_CURR)/TDC1))

# ROA calculation 
funda_lag <- mutate(funda_lag, roa = finitize(oiadp / ave_at))

# Tobin’s Q calculation 
funda_lag <- mutate(funda_lag, tobinsq = finitize((prcc_f*csho+lt)/ave_at))

# ROE calculation 
funda_lag <- mutate(funda_lag, roe = finitize(ni/(csho*prcc_f)))

# financial leverage calculation 
funda_lag <- mutate(funda_lag, fl= finitize(lt/ave_at))

# ratio of CAPEX to total assets 
funda_lag <- mutate(funda_lag, capexat= finitize(capx/at))

# R&D: AT ratio 
funda_lag <- mutate(funda_lag, rdat = finitize(xrd/at))

# firm size 
funda_lag <- mutate(funda_lag, firmsize = finitize(log(at)))

# CEO Tenure
funda_lag <- mutate(funda_lag, tenure = YEAR - as.numeric(format(BECAMECEO, format = "%Y")))

# some tenure are negative. a closer look in the data reveals that the data was input incorrectly (e.g., BECAMECEO date is after the fyear observation) hence, we filter away negative or 0 tenure
funda_final <- filter(funda_lag, tenure > 0)

# get future profitability
funda_final <- mergify(funda_final, "index", "p1", "roa", "tobinsq", "roe")

########## EXPLORATORY DATA ANALYSIS ########## 

# frequency of companies and industries 

# count number of industries 
length(unique(funda_final$SIC))

# count number of companies
length(unique(funda_final$GVKEY))

# count number of companies with industries 
compIndustries <- distinct(funda_final, GVKEY, SIC)
View(compIndustries)

# frequency of companies in industry 
freqconm<- funda_final %>%group_by(SIC) %>% distinct(GVKEY) %>% summarise(ncompanies=n()) %>% arrange(desc(ncompanies))
ggplot(freqconm, aes(x=SIC, y=ncompanies))+geom_point()

# analyse data of freqconm
View(freqconm)
summary(freqconm)

########## SELECTED CONTINUOUS VARIABLES ##########

# plot distribution of selected variables
ggplot(funda_final, aes(x = TDC1)) + geom_histogram(binwidth = 12, fill = "blue", color = "black") #right-skewed
ggplot(funda_final, aes(x = AGE)) + geom_histogram(binwidth = 12, fill = "blue", color = "black") #normal
ggplot(funda_final, aes(x = SHROWN_EXCL_OPTS_PCT)) + geom_histogram(binwidth = 12, fill = "blue", color = "black") #right-skewed
ggplot(funda_final, aes(x = TOTAL_CURR)) + geom_histogram(binwidth = 12, fill = "blue", color = "black") #right-skewed
ggplot(funda_final, aes(x = at)) + geom_histogram(binwidth = 12, fill = "blue", color = "black") #boxplot
ggplot(funda_final, aes(x = capx)) + geom_histogram(binwidth = 12, fill = "blue", color = "black") #boxplot
ggplot(funda_final, aes(x = xrd)) + geom_histogram(binwidth = 12, fill = "blue", color = "black") #boxplot
ggplot(funda_final, aes(x = oiadp)) + geom_histogram(binwidth = 12, fill = "blue", color = "black") #boxplot
ggplot(funda_final, aes(x = prcc_f)) + geom_histogram(binwidth = 12, fill = "blue", color = "black") #boxplot
ggplot(funda_final, aes(x = csho)) + geom_histogram(binwidth = 12, fill = "blue", color = "black") #right-skewed
ggplot(funda_final, aes(x = lt)) + geom_histogram(binwidth = 12, fill = "blue", color = "black") #boxplot
ggplot(funda_final, aes(x = ni)) + geom_histogram(binwidth = 12, fill = "blue", color = "black") #boxplot

# boxplot of selected continuous variables - before adjustment of outliers
boxplot(funda_final$TDC1, horizontal=TRUE, main = "TDC1") # right winsor
boxplot(funda_final$TOTAL_CURR, horizontal=TRUE, main="TOTAL_CURR") #right winsor
boxplot(funda_final$csho, horizontal=TRUE, main = "csho") # right winsor
boxplot(funda_final$at, horizontal=TRUE, main="at") #right winsor
boxplot(funda_final$capx, horizontal=TRUE, main="capx") #right winsor
boxplot(funda_final$xrd, horizontal=TRUE, main="xrd") #right winsor
boxplot(funda_final$oiadp, horizontal=TRUE, main="oiadp") #right winsor
boxplot(funda_final$prcc_f, horizontal=TRUE, main="prcc_f") #right winsor
boxplot(funda_final$lt, horizontal=TRUE, main="lt") #right winsor
boxplot(funda_final$ni, horizontal=TRUE, main="ni") #right winsor

# adjustment of outliers
funda_winsor <- funda_final %>%
  group_by(YEAR) %>%
  mutate(TDC1 = ifelse(TDC1 > quantile(TDC1, 0.99, na.rm = TRUE), quantile(TDC1, 0.99, na.rm = TRUE), TDC1)) %>%
  mutate(TOTAL_CURR = ifelse(TOTAL_CURR > quantile(TOTAL_CURR, 0.99, na.rm = TRUE), quantile(TOTAL_CURR, 0.99, na.rm = TRUE), TOTAL_CURR)) %>%
  mutate(capx = ifelse(capx > quantile(capx, 0.99, na.rm = TRUE), quantile(capx, 0.99, na.rm = TRUE), capx)) %>%
  mutate(xrd = ifelse(xrd > quantile(xrd, 0.99, na.rm = TRUE), quantile(xrd, 0.99, na.rm = TRUE), xrd)) %>% 
  mutate(at = ifelse(at > quantile(at, 0.99, na.rm = TRUE), quantile(at, 0.99, na.rm = TRUE), xrd)) %>% 
  mutate(xrd = ifelse(csho > quantile(csho, 0.99, na.rm = TRUE), quantile(csho, 0.99, na.rm = TRUE), csho)) %>% 
  mutate(oiadp = ifelse(oiadp > quantile(oiadp, 0.99, na.rm = TRUE), quantile(oiadp, 0.99, na.rm = TRUE), oiadp)) %>% 
  mutate(prcc_f = ifelse(prcc_f > quantile(prcc_f, 0.99, na.rm = TRUE), quantile(prcc_f, 0.99, na.rm = TRUE), prcc_f)) %>% 
  mutate(lt = ifelse(lt > quantile(lt, 0.99, na.rm = TRUE), quantile(lt, 0.99, na.rm = TRUE), lt)) %>%
  mutate(ni = winsor(ni,trim=0.01)) %>%
  ungroup()

# boxplot of selected continuous variables - after adjustment of outliers
boxplot(funda_winsor$TDC1, horizontal=TRUE, main = "TDC1")
boxplot(funda_winsor$TOTAL_CURR, horizontal=TRUE, main="TOTAL_CURR")
boxplot(funda_winsor$csho, horizontal=TRUE, main = "csho") 
boxplot(funda_winsor$at, horizontal=TRUE, main="at")
boxplot(funda_winsor$capx, horizontal=TRUE, main="capx") 
boxplot(funda_winsor$xrd, horizontal=TRUE, main="xrd") 
boxplot(funda_winsor$oiadp, horizontal=TRUE, main="oiadp") 
boxplot(funda_winsor$prcc_f, horizontal=TRUE, main="prcc_f") 
boxplot(funda_winsor$lt, horizontal=TRUE, main="lt") 
boxplot(funda_winsor$ni, horizontal=TRUE, main="ni")

# correlation matrix
funda_corr_all <- funda_final %>% 
  select(TDC1, TOTAL_CURR, csho, at, capx, xrd, prcc_f, lt, ni, roa_p1, tobinsq_p1, roe_p1) %>% 
  completify("TDC1", "TOTAL_CURR", "csho", "at", "capx", "xrd", "prcc_f", "lt", "ni", "roa_p1", "tobinsq_p1", "roe_p1")

corrplot(cor(funda_corr_all),
         method = "shade",
         order = "AOE",
         type = "lower",
         tl.pos = "ld",
         tl.col = "black",
         addCoef.col = "black",
         number.cex = .7,
         tl.cex = .8)

########## PREPARATION OF DATASET FOR REGRESSION ########## 

# remove missing observations
funda_last <- funda_winsor[complete.cases(funda_winsor),]

# boxplot of calculated variables
boxplot(funda_last$roa_p1, horizontal=TRUE, main="roa_p1")
boxplot(funda_last$tobinsq_p1, horizontal=TRUE, main="tobinsq_p1")
boxplot(funda_last$roe_p1, horizontal=TRUE, main="roe_p1")
boxplot(funda_last$fl, horizontal=TRUE, main="fl")
boxplot(funda_last$firmsize, horizontal=TRUE, main="firmsize")
boxplot(funda_last$SHROWN_EXCL_OPTS_PCT, horizontal=TRUE, main="SHROWN_EXCL_OPTS_PCT")
boxplot(funda_last$inc, horizontal=TRUE, main="inc")
boxplot(funda_last$rdat, horizontal=TRUE, main="rdat")
boxplot(funda_last$capexat, horizontal=TRUE, main="capexat")
boxplot(funda_last$AGE, horizontal=TRUE, main="AGE")
boxplot(funda_last$tenure, horizontal=TRUE, main="tenure")


########## DESCRIPTIVE STATISTICS ########## 
inspectify(funda$TDC1)
inspectify(funda$TOTAL_CURR)
inspectify(funda$csho)
inspectify(funda$at)
inspectify(funda$capx)
inspectify(funda$xrd)
inspectify(funda$prcc_f)
inspectify(funda$lt)
inspectify(funda$ni)

inspectify(funda_last$TDC1)
inspectify(funda_last$TOTAL_CURR)
inspectify(funda_last$csho)
inspectify(funda_last$at)
inspectify(funda_last$capx)
inspectify(funda_last$xrd)
inspectify(funda_last$prcc_f)
inspectify(funda_last$lt)
inspectify(funda_last$ni)

########## SIMPLE REGRESSION ########## 

# set the seed
set.seed(1234)

# split data into training and test samples
train <- sample_frac(funda_last, 0.6)
test <- anti_join(funda_last, train)

# we run simple regression first without fixed effects
# normal regression
reg_roa <- lm(log(roa_p1 + 8) ~inc + SHROWN_EXCL_OPTS_PCT + firmsize + log(rdat + 8) + log(capexat + 8) + log(fl + 8) + EXECDIR + AGE + log(tenure + 8), data = train)
summary(reg_roa)

reg_tobinsq <- lm(log(tobinsq_p1 + 1) ~ inc + SHROWN_EXCL_OPTS_PCT + firmsize + log(rdat +1) + log(capexat + 1) + log(fl + 1) + EXECDIR + AGE + log(tenure + 1), data = train)
summary(reg_tobinsq)

reg_roe <- lm(log(roe_p1 + 77552) ~ inc + log(SHROWN_EXCL_OPTS_PCT + 77552) + firmsize + log(rdat + 77552) + log(capexat + 77552) + log(fl + 77552) + EXECDIR + AGE + log(tenure + 77552), data = train)
summary(reg_roe)

# choose reg_tobinsq as our best model (highest adj R-square); drop the insig variable of fl

# final normal regression model
reg_tobinsq_normal <- lm(log(tobinsq_p1 + 1) ~ inc + SHROWN_EXCL_OPTS_PCT + firmsize + log(rdat +1) + log(capexat + 1) + EXECDIR + AGE + log(tenure + 1), data = train)
summary(reg_tobinsq_normal)

vif(reg_tobinsq_normal)

##### FORWARD REGRESSION #####
forward_tobinsq <- step(reg_tobinsq, direction = "forward")
tobinsq_forward_pred <- predict(forward_tobinsq, test)
summary(forward_tobinsq)
accuracy(tobinsq_forward_pred,test$tobinsq_p1)

##### BACKWARD REGRESSION #####
backward_tobinsq <- step(reg_tobinsq, direction = "backward")
tobinsq_backward_pred <- predict(backward_tobinsq, test)
summary(backward_tobinsq) 
accuracy(tobinsq_backward_pred,test$tobinsq_p1)

##### STEPWISE REGRESSION #####
stepwise_tobinsq <- step(reg_tobinsq, direction = "both")
tobinsq_stepwise_pred <- predict(stepwise_tobinsq, test)
summary(stepwise_tobinsq) 
accuracy(tobinsq_stepwise_pred,test$tobinsq_p1)

########## SELECTION OF MODEL ##########

# We will build our model based on tobinsq dependent variable as it offers the highest adjusted R-square while having a relatively lower RMSE
linearreg <- lm(log(tobinsq_p1 + 1) ~ inc + SHROWN_EXCL_OPTS_PCT + firmsize + log(rdat +1) + log(capexat + 1) + EXECDIR + AGE + log(tenure + 1), data = train)
summary(linearreg)
vif(linearreg)

# test accuracy of simple linear regression
lm_pred <- predict(linearreg, test)
lm_error <- test$tobinsq_p1 - lm_pred
lm_final <- data.frame("Predicted" = lm_pred, "Actual" = test$tobinsq_p1, "Error" = lm_error)
accuracy(lm_pred, test$tobinsq_p1)

##### ADDITION OF FIXED EFFECTS #####
foels_reg <- feols(data = train, log(tobinsq_p1 + 1) ~ inc + SHROWN_EXCL_OPTS_PCT + firmsize + log(rdat +1) + log(capexat + 1) + EXECDIR + AGE + log(tenure + 1) | GVKEY + YEAR)
summary(foels_reg)
final_pred <- predict(foels_reg, test)
final_error <- test$tobinsq_p1 - final_pred
final_final <- data.frame("Predicted" = final_pred, "Actual" = test$tobinsq_p1, "Error" = final_error)
accuracy(final_pred, test$tobinsq_p1)

##### TWO-WAY CLUSTERING FIXED EFFECTS #####
cluster_reg <- feols(data = train, log(tobinsq_p1 + 1) ~ inc + SHROWN_EXCL_OPTS_PCT + firmsize + log(rdat +1) + log(capexat + 1) + EXECDIR + AGE + log(tenure + 1) | GVKEY + YEAR, cluster = c("GVKEY", "YEAR"))
summary(cluster_reg)
# we conclude here that the two-way clustering has negligible effect on the metrics

#### OVERALL FINAL MODEL #####
final_reg <- feols(data = train, log(tobinsq_p1 + 1) ~ inc + SHROWN_EXCL_OPTS_PCT + firmsize | GVKEY + YEAR)
summary(final_reg)

########## OPTIMAL PROPORTION OF INCENTIVE COMPENSATION ##########
ggplot(data = funda_last, aes(x=inc, y=log(tobinsq_p1 + 1))) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "dodgerblue3") + 
  theme(panel.background = element_rect(fill = "white"), 
        axis.line.x=element_line(), 
        axis.line.y=element_line()) + 
  ggtitle("Relationship between Incentive Compensation & Firm Performance")
