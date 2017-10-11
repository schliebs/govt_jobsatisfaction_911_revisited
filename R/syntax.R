##########

# 2000
gss2000 <- gss %>% 
  filter(YEAR == 2000) 

gss2002 <- gss %>% 
  filter(YEAR == 2002) 

# 2000
table(satis = gss2000$satis_cat,y2000 = gss2000$employ) %>% prop.table(2) %>% round(2)
table(satis = gss2002$satis_cat,y2002 = gss2002$employ) %>% prop.table(2) %>% round(2)





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

