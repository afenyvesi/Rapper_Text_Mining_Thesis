library(dplyr)
library(ggplot2)
library(lmtest)
library(car)
library(corrplot)
library(sandwich)

#install.packages("sandwich")

setwd('C:/Users/HP/Documents/00_EGYETEM/SZAKDOLGOZAT/CODE/FINAL_DATA')

artists <- read.csv('modeldf_FINAL.csv', sep = ';')
years <- read.csv('year_dataframe_FINAL.csv', sep = ';')

df <-  data.frame(artists)

bbs <- artists %>% filter(bb100 > 0)

goats <- artists %>% filter(vote100 > 0)


# Explore and plot

artists %>% summary()

pairs(df[c('bb100', 'vote100', 'avg_wordpersong', 'avg_uniquewordpersong')])

artists[,c('bb100', 'vote100', 'avg_wordpersong', 'avg_uniquewordpersong')] %>% cor() %>% corrplot(method='circle')

artists[,c('bb100', 'vote100', 'topic_street_percent', 'topic_music_percent', 'topic_money_percent', 'topic_sex_percent')] %>% cor() %>% corrplot(method='circle')


#plot(artists$bb100,artists$avg_uniquewordpersong)

#plot(artists$vote100,artists$avg_uniquewordpersong)

plot(artists$avg_uniquewordpersong,artists$bb100)

plot(artists$avg_uniquewordpersong, artists$vote100)

plot(artists$brand_ps, artists$bb100)

plot(artists$brand_ps, artists$vote100)

plot(artists$gang_ps, artists$bb100)

plot(artists$gang_ps, artists$vote100)

plot(artists$anger_ps, artists$bb100)

plot(artists$anger_ps, artists$vote100)

plot(artists$joy_ps, artists$bb100)

plot(artists$joy_ps, artists$vote100)

plot(artists$topic_money_percent, artists$bb100)

plot(artists$topic_money_percent, artists$vote100)

plot(artists$topic_street_percent, artists$bb100)

plot(artists$topic_street_percent, artists$vote100)

plot(artists$topic_sex_percent, artists$bb100)

plot(artists$topic_sex_percent, artists$vote100)

plot(artists$topic_music_percent, artists$bb100)

plot(artists$topic_music_percent, artists$vote100)

# Models

# Models with vocabulary

model1b <- artists %>% lm(bb100 ~ avg_wordpersong + avg_uniquewordpersong + avg_wordlength, data = .)

model1b %>% summary()

model1g <- artists %>% lm(vote100 ~ avg_wordpersong + avg_uniquewordpersong + avg_wordlength, data = .)

model1g %>% summary()

#model1b2 <- bbs %>% lm(bb100 ~ avg_wordpersong + avg_uniquewordpersong + avg_wordlength, data = .)
#model1b2 %>% summary()
#model1g2 <- goats %>% lm(vote100 ~ avg_wordpersong + avg_uniquewordpersong + avg_wordlength, data = .)
#model1g2 %>% summary()
#

# Models with dictionaries

model2b <- artists %>% lm(bb100 ~ brand_ps + gang_ps + profan_ps, data = .)

model2b %>% summary()

model2g <- artists %>% lm(vote100 ~ brand_ps + gang_ps + profan_ps, data = .)

model2g %>% summary()

#model2b2 <- bbs %>% lm(bb100 ~ brand_ps + gang_ps + profan_ps, data = .)
#model2b2 %>% summary()
#model2g2 <- goats %>% lm(vote100 ~ brand_ps + gang_ps + profan_ps, data = .)
#model2g2 %>% summary()
#
# Models with emotions

model3b <- artists %>% lm(bb100 ~ anger_ps + fear_ps + joy_ps, data = .)

model3b %>% summary()

model3g <- artists %>% lm(vote100 ~ anger_ps + fear_ps + joy_ps, data = .)

model3g %>% summary()

#model3b2 <- bbs %>% lm(bb100 ~ anger_ps + fear_ps + joy_ps + anticipation_ps, data = .)
#model3b2 %>% summary()
#model3g2 <- goats %>% lm(vote100 ~ anger_ps + fear_ps + joy_ps + anticipation_ps, data = .)
#model3g2 %>% summary()

# Models with topics

model4b <- artists %>% lm(bb100 ~ topic_money_percent + topic_sex_percent + topic_street_percent + topic_music_percent, data = .)

model4b %>% summary()

model4g <- artists %>% lm(vote100 ~ topic_money_percent + topic_sex_percent + topic_street_percent + topic_music_percent, data = .)

model4g %>% summary()

#model4b2 <- bbs %>% lm(bb100 ~ topic_money_percent + topic_sex_percent + topic_street_percent + topic_music_percent, data = .)
#model4b2 %>% summary()
#model4g2 <- goats %>% lm(vote100 ~ topic_money_percent + topic_sex_percent + topic_street_percent + topic_music_percent, data = .)
#model4g2 %>% summary()

# Create the output file for these models
stargazer(model1b, model1g, model2b, model2g, align = TRUE, no.space = TRUE, type="latex")

stargazer(model3b, model3g, align = TRUE, no.space = TRUE, type="latex")

stargazer(model4b, model4g, align = TRUE, no.space = TRUE, type="latex")

# Test these models for heteroscedasticity
## The null hypothesis is that there is no heteroskedasticity

bptest(model1b)
bptest(model1g) # heteroskedastic!
bptest(model2b)
bptest(model2g) # heteroskedastic!
bptest(model3b)
bptest(model3g) # heteroskedastic!
bptest(model4b)
bptest(model4g) # heteroskedastic!

# Robust standard error models
model1g %>% coeftest(vcov=vcovHC(model1g,type='HC0')) %>% round(3)

model2g %>% coeftest(vcov=vcovHC(model2g,type='HC0')) %>% round(3)

model3g %>% coeftest(vcov=vcovHC(model3g,type='HC0')) %>% round(3)

model4g %>% coeftest(vcov=vcovHC(model4g,type='HC0')) %>% round(3)

# Output
stargazer(model1g %>% coeftest(vcov=vcovHC(model1g,type='HC0')) %>% round(3),
          model2g %>% coeftest(vcov=vcovHC(model2g,type='HC0')) %>% round(3),
          model3g %>% coeftest(vcov=vcovHC(model3g,type='HC0')) %>% round(3),
          model4g %>% coeftest(vcov=vcovHC(model4g,type='HC0')) %>% round(3),
          align = TRUE, no.space = TRUE, type="latex")



# Mixed models

#model5b <- artists %>% lm(bb100 ~ avg_wordpersong + avg_uniquewordpersong +
#                                  gang_ps + brand_ps +
#                                  anger_ps + fear_ps + joy_ps +
#                                  topic_street_percent + topic_music_percent, data = .)
#
#model5b %>% summary()
#
#model5g <- artists %>% lm(vote100 ~ avg_wordpersong + avg_uniquewordpersong +
#                                    gang_ps + brand_ps +
#                                    anger_ps + fear_ps + joy_ps +
#                                    topic_street_percent + topic_music_percent, data = .)
#
#model5g %>% summary()



model6b <-  artists %>% lm(bb100 ~avg_uniquewordpersong +
                                  gang_ps +
                                  anger_ps + fear_ps +
                                  topic_street_percent + topic_music_percent, data = .)

model6b %>% summary()

model6g <-  artists %>% lm(vote100 ~ avg_uniquewordpersong + avg_wordpersong + topic_street_percent + topic_money_percent, data = .)

model6g %>% summary()

# Test the no.6 models
bptest(model6b)
bptest(model6g) # Heteroscedasticity!

# Robust SE
model6g %>% coeftest(vcov=vcovHC(model6g,type='HC0')) %>% round(3)

# Output
stargazer(model6b,
          model6g,
          align = TRUE, no.space = TRUE, type="latex")

stargazer(model6g %>% coeftest(vcov=vcovHC(model6g,type='HC0')) %>% round(3),
          align = TRUE, no.space = TRUE, type="latex")


## Multicollinearity
## A VIF value of <10 is okay, but it is ideal <5
car::vif(model6b)
car::vif(model6g)


# A possible solution: robust standard errors
model1g %>% summary()

model1g %>% coeftest(vcov=vcovHC(model1g,type='HC0')) %>% round(3)

model5g %>% summary()



# 4 diagnostics plots
plot(model1g, las = 1)

# Polinomiality test



# Export with stargazer
stargazer(model1g,type="latex")

