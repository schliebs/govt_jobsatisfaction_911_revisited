# Loading of packages
source("R/packages.R")

# Load custom spss-dictionary-file
source("R/make_dictionary.R")

# Loading of Dataset
gss_all <- read_spss("data-offline/GSS7216_R2.sav")

# Creation of Dictionary/In-R-Codebook
meta_file <- make_dictionary(gss,format = "wide")


# Data management

# Filter only years 2000, 2002 and full-time employses (!) [try what happens including part-time]
gss <- 
  gss_all %>% 
  filter(YEAR %in% c(2000,2002))# %>%  filter(PARTFULL %in% 1)

# Employer

gss$employ <- c()
gss$employ [gss$WRKGOVT %in% c(1)] <- "GOV"
gss$employ [gss$WRKGOVT %in% c(2)] <- "PRIV"

gss$employ %>% table()

# Job satisfaction 

unique(gss$SATJOB)

gss$satis_cat <- c()
gss$satis_cat [gss$SATJOB %in% c(1,2)] <- 1
gss$satis_cat [gss$SATJOB %in% c(3,4)] <- 0

gss$satis_cat %>% table(satis = .,year = gss$YEAR) %>% prop.table(2) %>% round(2)

# Dep. Var

gss$satis <- c()
gss$satis [gss$SATJOB %in% c(1)] <- 1
gss$satis [gss$SATJOB %in% c(2)] <- 2
gss$satis [gss$SATJOB %in% c(3)] <- 3
gss$satis [gss$SATJOB %in% c(4)] <- 4

gss$satis %>% table(satis = .,year = gss$YEAR) %>% prop.table(2) %>% round(2)



# fear of losing JOB

gss$fear_loss <- NA
gss$fear_loss [gss$JOBLOSE %in% c(1,2)] <- "fear"
gss$fear_loss [gss$JOBLOSE %in% c(3,4)] <- "no fear"

table(gss$fear_loss,gss$YEAR) %>% prop.table(2) %>% round(2)
table(gss$fear_loss,gss$employ)%>% prop.table(2) %>% round(2)
table(gss$fear_loss,gss$satis_cat)%>% prop.table(1) %>% round(2)

# Period
gss$period <- NA
gss$period [gss$YEAR %in% c(2000)] <- 0
gss$period [gss$YEAR %in% c(2002)] <- 1

# Period 04
gss$period04 <- NA
gss$period04 [gss$YEAR %in% c(2000)] <- 0
gss$period04 [gss$YEAR %in% c(2002,2004)] <- 1

# weight variable

gss$WTSSALL %>% psych::describe()
gss$weight <- gss$WTSSALL

# Weight data
library(survey)

gss_w <- svydesign(ids = ~1, data = gss, weights = gss$weight)
summary(gss_w)

prop.table(table(gss$SEX))
prop.table(svytable(~SEX, design = gss_w))

