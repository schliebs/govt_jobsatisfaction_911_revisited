# source data
source("R/data_loading_management.R")

# Replication of Paper base models

# Remove parttime-workers

gss_full <- 
  gss %>% 
  filter(PARTFULL == 1)

# Only 2000 and 2002 data 

# FUll-time only

base_mod_unweighted <- glm(factor(satis_cat) ~ 
                  period*employ,
                family=binomial(link='probit'),
                data=gss_full)
summary(base_mod_unweighted)

base_mod_weighted <- glm(satis_cat ~ 
                             period*employ,
                           family=binomial(link='probit'),
                           weights = weight,
                           data=gss_full )
summary(base_mod_weighted)

# FULL-AND-PARTTIME

base_mod_unweighted <- glm(factor(satis_cat) ~ 
                             period*employ,
                           family=binomial(link='probit'),
                           data=gss)
summary(base_mod_unweighted)

base_mod_weighted <- glm(satis_cat ~ 
                           period*employ,
                         family=binomial(link='probit'),
                         weights = weight,
                         data=gss )
summary(base_mod_weighted)




















#########################
# base_mod_weighted <- svyglm(satis_cat ~ 
#                             period*employ,
#                            family=binomial(link='probit'),
#                             design = gss_w)
# summary(base_mod_weighted)





confint(base_mod)

wald.test(b = coef(myprobit), Sigma = vcov(myprobit), Terms = 4:6)


##
newdata <- data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 
                                4 * 4), gpa = rep(c(2.5, 3, 3.5, 4), each = 100 * 4), rank = factor(rep(rep(1:4, 
                                                                                                            each = 100), 4)))

head(newdata)
newdata[, c("p", "se")] <- predict(myprobit, newdata, type = "response", se.fit = TRUE)[-3]

ggplot(newdata, aes(x = gre, y = p, colour = rank)) + geom_line() + facet_wrap(~gpa)
