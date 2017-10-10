library(plyr)
library(tidyverse)
library(haven)
library(magrittr)

source("make_dictionary.R")

gss_all <- read_spss("GSS7216_R2.sav")
meta_file <- make_dictionary(gss,format = "wide")


gss <- 
  gss_all %>% 
  filter(YEAR %in% c(2000,2002)) %>% 
  filter(PARTFULL %in% 1)

# Employer

gss$employ <- c()
gss$employ [gss$WRKGOVT %in% c(1)] <- "GOV"
gss$employ [gss$WRKGOVT %in% c(2)] <- "PRIV"

gss$employ %>% table()

# Job satisfaction 

unique(gss$SATJOB)

gss$satis_cat <- c()
gss$satis_cat [gss$SATJOB %in% c(1,2)] <- "+"
gss$satis_cat [gss$SATJOB %in% c(3,4)] <- "-"

gss$satis_cat %>% table(satis = .,year = gss$YEAR) %>% prop.table(2) %>% round(2)

#########



##########

# 2000
gss2000 <- gss %>% 
  filter(YEAR == 2000) 

gss2002 <- gss %>% 
  filter(YEAR == 2002) 

# 2000
table(satis = gss2000$satis_cat,y2000 = gss2000$employ) %>% prop.table(2) %>% round(2)
table(satis = gss2002$satis_cat,y2002 = gss2002$employ) %>% prop.table(2) %>% round(2)

model <- glm(factor(satis_cat) ~ YEAR * employ,family=binomial(link='logit'),data=gss)
summary(model)

library(ggplot2)
library(extrafont)
library(hrbrthemes)
library(extrafont) # neue Schriftarten



## lm

gg <- 
  ggplot(data = gss %>% filter(!is.na(employ))
           ,mapping = aes(x = YEAR,
                                  y = satis,
                                  color = employ)) + 
  geom_jitter(
             size = 2) + 
  scale_y_reverse()+
geom_smooth(method = "lm",size = 2) + 
  labs(x = "Jahr", 
       y = "Zufriedenheit",
       title = "9/11 Job Satisfaction",
       subtitle = "Quasi-Continuous Dep. Var") + 
  theme_ipsum(grid = "Y")

gg

ggsave(filename = "mod1.pdf",
       plot = gg,
       width = 16,
       height = 9,
       device = "pdf",
       dpi = 1000)

### glm





# 2 separate Modelle

mod_govt <- glm(factor(satis_cat) ~ 
                  YEAR ,
                family=binomial(link='logit'),
                data=gss %>% 
                  filter(employ == "GOV"))
summary(mod_govt)

mod_priv <- glm(factor(satis_cat) ~ 
                  YEAR ,
                family=binomial(link='logit'),
                data=gss %>% 
                  filter(employ == "PRIV"))
summary(mod_priv)


# Dep. Var

gss$satis <- c()
gss$satis [gss$SATJOB %in% c(1)] <- 1
gss$satis [gss$SATJOB %in% c(2)] <- 2
gss$satis [gss$SATJOB %in% c(3)] <- 3
gss$satis [gss$SATJOB %in% c(4)] <- 4

gss$satis %>% table(satis = .,year = gss$YEAR) %>% prop.table(2) %>% round(2)


# 2000
gss2000 <- gss %>% 
  filter(YEAR == 2000) 

gss2002 <- gss %>% 
  filter(YEAR == 2002) 

# 2000
table(satis = gss2000$satis,y2000 = gss2000$employ) %>% prop.table(2) %>% round(2)
table(satis = gss2002$satis,y2002 = gss2002$employ) %>% prop.table(2) %>% round(2)

model <- lm(satis ~ YEAR * employ,data=gss)
summary(model)


# Search for Latent variable bias

# fear of losing JOB

gss$fear_loss <- NA
gss$fear_loss [gss$JOBLOSE %in% c(1,2)] <- "fear"
gss$fear_loss [gss$JOBLOSE %in% c(3,4)] <- "no fear"

table(gss$fear_loss,gss$YEAR) %>% prop.table(2) %>% round(2)
table(gss$fear_loss,gss$employ)%>% prop.table(2) %>% round(2)
table(gss$fear_loss,gss$satis_cat)%>% prop.table(1) %>% round(2)


model <- glm(factor(satis_cat) ~ YEAR * employ + employ*fear_loss,family=binomial(link='logit'),data=gss)
summary(model)

# Wir nehmen Zeit komplett raus

model <- glm(factor(satis_cat) ~ employ ,family=binomial(link='logit'),data=gss)
summary(model)


gg2a <- 
  ggplot(data = gss %>% filter(!is.na(employ) & fear_loss == "fear")
         ,mapping = aes(x = YEAR,
                        y = satis,
                        color = employ)) + 
  geom_jitter(
    size = 2) + 
  scale_y_reverse()+
  geom_smooth(method = "lm",size = 2) + 
  labs(x = "Jahr", 
       y = "Zufriedenheit",
       title = "Fear of Jobloss-ers",
       subtitle = "Quasi-Continuous Dep. Var") + 
  theme_ipsum(grid = "Y")

gg2a

ggsave(filename = "mod_fear.pdf",
       plot = gg2a,
       width = 16,
       height = 9,
       device = "pdf",
       dpi = 1000)
## no fear

gg2b <- 
  ggplot(data = gss %>% filter(!is.na(employ) & fear_loss == "no fear")
         ,mapping = aes(x = YEAR,
                        y = satis,
                        color = employ)) + 
  geom_jitter(
    size = 2) + 
  scale_y_reverse()+
  geom_smooth(method = "lm",size = 2) + 
  labs(x = "Jahr", 
       y = "Zufriedenheit",
       title = "Fearless-ers",
       subtitle = "Quasi-Continuous Dep. Var") + 
  theme_ipsum(grid = "Y")

gg2b

ggsave(filename = "mod_fearless.pdf",
       plot = gg2b,
       width = 16,
       height = 9,
       device = "pdf",
       dpi = 1000)

