df <- read.csv('Data_JellyBeansStudent_3.csv')

colnames(df)[colnames(df) == 'ad1'] <- 'TV'
colnames(df)[colnames(df) == 'ad2'] <- 'banner'
colnames(df)[1] <- 'index'

require(dummies)

# Dummy variables 
df$North <- as.numeric(df$region == 'TheNorth')
df$South <- as.numeric(df$region == 'TheSouth')
df$West <- as.numeric(df$region == 'West')
df$East <- as.numeric(df$region == 'East')
df$Capital <- as.numeric(df$region == 'Capital')

df$Jan <- as.numeric(df$month == 'Jan')
df$Feb <- as.numeric(df$month == 'Feb')
df$Mar <- as.numeric(df$month == 'Mar')
df$Apr <- as.numeric(df$month == 'Apr')
df$May <- as.numeric(df$month == 'May')
df$Jun <- as.numeric(df$month == 'Jun')
df$Jul <- as.numeric(df$month == 'Jul')
df$Aug <- as.numeric(df$month == 'Aug')
df$Sep <- as.numeric(df$month == 'Sep')
df$Oct <- as.numeric(df$month == 'Oct')
df$Nov <- as.numeric(df$month == 'Nov')
df$Dec <- as.numeric(df$month == 'Dec')

df$year1 <- as.numeric(df$time >= 1 & df$time <= 12)
df$year2 <- as.numeric(df$time >= 13 & df$time <= 24)
df$year3 <- as.numeric(df$time >= 25 & df$time <= 36)

df$q1 <- as.numeric((df$time >=1 & df$time <= 3) | (df$time >=13 & df$time <= 15) | (df$time >=25 & df$time <= 27))
df$q2 <- as.numeric((df$time >=4 & df$time <= 6) | (df$time >=16 & df$time <= 18) | (df$time >=28 & df$time <= 30))
df$q3 <- as.numeric((df$time >=7 & df$time <= 9) | (df$time >=19 & df$time <= 21) | (df$time >=31 & df$time <= 33))
df$q4 <- as.numeric((df$time >=10 & df$time <= 12) | (df$time >=22 & df$time <= 24) | (df$time >=34 & df$time <= 36))

region <- c('North', 'South', 'West', 'East', 'Capital')
df[region] <- lapply(df[region], factor)

month <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
df[month] <- lapply(df[month], factor)

year <- c('year1', 'year2', 'year3')
df[year] <- lapply(df[year], factor)

str(df) #check each columns' situation

#1) How many packs did we sell associated to the advertisements and promotions? And by type of marketing activity?

#model1
model <- lm(sales ~ price + TV + banner + prom + North + South + West + East
            + Jan 
            + Feb
            + Mar 
            + Apr
            + May 
            + Jun 
            + Jul 
            + Aug 
            + Sep 
            + Oct 
            + Nov
            + year1 
            + year2,
            data = df)
summary(model)
par(mfrow = c(2,2))
plot(model)
tv = sum(df$TV)*model$coefficients[3]
tv #37666.54 packs
banner = sum(df$banner)*model$coefficients[4]
banner #34989.2 packs
prom = sum(df$prom)*model$coefficients[5]
prom #202172.8 packs

cor(df$TV, df$sales, method = 'pearson')
cor(df$banner, df$sales, method = 'pearson')
cor(df$prom, df$sales, method = 'pearson')
df$sum1 <- sum(tv + banner + prom)

vif(model)

#model2
library(olsrr)
forward <- ols_step_forward_aic(model) 
plot(forward) 
summary(model)
model_for <- lm(sales ~ prom + Apr + East + West + North
                + South 
                + May
                + price 
                + banner
                + TV 
                + year1 
                + Jun 
                + Mar 
                + Aug 
                + Jan,
                data = df)
summary(model_for)
tv2 = sum(df$TV)*model_for$coefficients[11]
tv2 #37216.49 packs
banner2 = sum(df$banner)*model_for$coefficients[10]
banner2 #35783.99 packs
prom2 = sum(df$prom)*model_for$coefficients[2]
prom2 #201328.1 packs
df$sum2 <- sum(tv2 + banner2 + prom2)

df1 <- rbind(sum1, sum2)
colnames(df1)[1] <- 'index'
hist((df1)[1],
     bty = 'n',
     col = rgb(.69,.4,1),
     freq = TRUE,
     main = 'Base Sales')
ggplot(df,aes(x=sum1, sum2, y=sales)) + geom_bar(stat='identity', fill = '#7700BB') + xlab('region')
data <- c(rep(1, 274828.5), rep(2, 274328.6))
hist(data, breaks = c(0.5, 1.5, 2.5), axes = T, labels = c('initial model (274828.5)','optimal model (274328.6)'))

names<-c("initial model", "optimal model")
ssum <- cbind(sum1,sum2)
barplot(ssum,names.arg = names, col = '#7700BB', ylab = 'sales')
abline(h = 274328.6,
       col = 'red')

par(mfrow = c(1,1))
plot(model_for)
durbinWatsonTest(model_for)
vif(model_for)

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
a <- ggplot(df,aes(x= region, y=sales)) + 
  geom_boxplot(color = 'black',
               fill = 'mediumpurple2') + 
  xlab('region')

b <- ggplot(df,aes(x=month, y=sales)) + 
  geom_boxplot(color = 'black',
               fill = 'mediumpurple2') + 
  xlab('month')

grid.arrange(a, b, ncol = 2)

tv_grp = sum(df$TV) #4455
banner_grp = sum(df$banner) #1275
prom_grp = sum(df$prom) #4380

# Intercept 
model$coefficients[1]

library(ggplot2)
library(Rmisc)
#no log
x11 <- ggplot(df, aes(x = TV, 
                     y = sales,
                     col = region)) +
  geom_point() + scale_color_brewer(palette='PRGn') +
  geom_smooth(method = 'lm',
              aes(group = 2),
              col = 'black',
              se = F)

x22 <- ggplot(df, aes(x = banner, 
                     y = sales,
                     col = region)) +
  geom_point() + scale_color_brewer(palette='PRGn') +
  geom_smooth(method = 'lm',
              aes(group = 1),
              col = 'black',
              se = F)

x33 <- ggplot(df, aes(x = prom, 
                     y = sales,
                     col = month)) +
  geom_point() + scale_color_brewer(palette='PRGn') +
  geom_smooth(method = 'lm',
              aes(group = 1),
              col = 'black',
              se = F)

multiplot(x11,x22,x33)

#2) Our TV ads cost us 2 million Pounds, our Banners 500,000 Pounds. Which one is more efficient?
# To increase sale by 1, this is how much you need to invest in ad1 and ad2
2000000/tv2 #cost for per pack TV 90.91933
500000/banner2 #cost for per pack banner 16.92763
#hence, banner is more efficient

tv2/2000000
banner2/500000

#3) Our sales often show a lot of variation. Can you explain to us possible sources of the variation, other than our marketing activities?
#install.packages('caTools')
require(caTools)


#to find that sales is not normal distribute
par(mfrow = c(2,2))

hist(df$sales,
     bty = 'n',
     col = rgb(.69,.4,1),
     freq = TRUE,
     main = 'Base Sales')

hist(df$price,
     bty = 'n',
     col = rgb(.69,.4,1),
     freq = TRUE,
     main = 'Base Price')

hist(log(df$sales),
     bty = 'n',
     col = rgb(.89, .8, 1),
     freq = TRUE,
     main = 'Log Sales')

hist(log(df$price),
     bty = 'n',
     col = rgb(.89, .8, 1),
     freq = TRUE,
     main = 'Log Price')


backward <- ols_step_backward_aic(model) 
plot(backward) #Nov Jul yesr2 Aug Sep
model_bac <- lm(sales ~ year2 + Aug + Jan,
                data = df)
summary(model_bac)

both <- ols_step_both_aic(model) 
plot(both)
summary(both)
model_both <- lm(sales ~ prom + Apr + East + West + North
                + South 
                + May
                + price 
                + banner
                + TV 
                + year1 
                + Jun 
                + Mar 
                + Aug 
                + Jan,
                data = df)
summary(model_both)

confint(model,'Apr1',level = 0.95)
confint(model,'East1',level = 0.95)
confint(model,'West1',level = 0.95)
confint(model,'North1',level = 0.95)
confint(model,'South1',level = 0.95)
confint(model,'May1',level = 0.95)
confint(model,'price',level = 0.95)
confint(model,'year11',level = 0.95)
confint(model,'Jun1',level = 0.95)
confint(model,'Mar1',level = 0.95)
confint(model,'Aug1',level = 0.95)
confint(model,'Jan1',level = 0.95)

par(mfrow = c(1,1))
corr1 <- cor(df[, 2:6])
corrplot(corr1
         , method = "color"
         , outline = T
         , addgrid.col = "darkgray"
         , addrect = 4 # border highest corr 
         , rect.lwd = 3 # border thickness
         , order = "hclust" # cluster correlation 
         , cl.pos = "b" # Add corr color bar 
         , rect.col = "black"
         , tl.col = "steelblue"
         , addCoef.col = "black"
         , number.digits = 2
         , number.cex = 0.60
         , tl.cex = 0.7
         , cl.cex = 1
         , col = colorRampPalette(c("maroon"
                                    , "white"
                                    , "slateblue"))(100))

hist(df$month,
     bty = 'n',
     col = rgb(.69,.4,1),
     freq = TRUE,
     main = 'Base Sales')

bar <- cbind(df$Jan + df$Feb)
ggplot(df,aes(x=factor(month), y=sales)) + geom_bar(stat='identity', fill = '#7700BB') + xlab('month')
ggplot(df,aes(x=factor(region), y=sales)) + geom_bar(stat='identity', fill = '#7700BB') + xlab('region')

mean1 <- cbind(df[,4:6] == 0 )
mean_mar <- mean(df[,2])

t.test(df$marketing)

df$marketing <- df$TV + df$prom + df$banner
ggplot(df,aes(x=marketing, y=sales)) + geom_bar(stat = 'identity', fill = '#4400CC')

#therefore, here we use log(sales)
mm <- lm(sales ~ price + North + South + West + East
         + Jan 
         + Feb
         + Mar 
         + Apr
         + May 
         + Jun 
         + Jul 
         + Aug 
         + Sep 
         + Oct 
         + Nov
         + year1 
         + year2,
         data = df)
plot(mm)
#plot1 is linear since it is a horizonal line
#plot2 is quite normal distribution but at the end it's not very normal
#plot3 is homo
#plot4 data 20 92 56 influence a lot
summary(mm)

aa <- aov(sales ~ month, data = df)
summary(aov(sales ~ month, data = df))
bb <- aov(sales ~ region, data = df)
summary(aov(sales ~ region, data = df))
summary(aov(sales ~ time, data = df))

library(agricolae)
TukeyHSD(aa)

#8) Validation and robustness checks. Build more trust in the model. Is it over-fitted? Others?
library(caTools)
set.seed(42)

sample =  sample.split(df$sales, SplitRatio = .7)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)
#forward_p forward_aic do cv and find that whose residuals are smaller
m1 <- lm(sales ~ price + TV + banner + prom + North + South + West + East
         + Jan 
         + Feb
         + Mar 
         + Apr
         + May 
         + Jun 
         + Jul 
         + Aug 
         + Sep 
         + Oct 
         + Nov
         + year1 
         + year2,
         data = train)

require(car)
vif(m1) # No multicollinearity 
summary(m1)

# Validation
require(DAAG)
#residuals predicted errors
sigma(m1) / mean(train$sales) 

#install.packages('caret')
require(caret)
trc <- trainControl(method = 'LOOCV')

cv1 <- train(sales ~ price + TV + banner + prom + North + South + West + East
             + Jan 
             + Feb
             + Mar 
             + Apr
             + May 
             + Jun 
             + Jul 
             + Aug 
             + Sep 
             + Oct 
             + Nov
             + year1 
             + year2,
             data = train,
             method = 'lm',
             trControl = trc)
# Root mean squared error 
cv1 #0.3006


library(olsrr)
forward <- ols_step_forward_aic(m1) 
plot(forward) #prom Apr West East North South May banner price TV year1 Jun Mar Aug Jan
summary(forward)

backward <- ols_step_backward_aic(m1) 
plot(backward) #Nov Jul yesr2 Aug Sep

both <- ols_step_both_aic(m1) 
plot(both) #prom Apr West East North South May banner price TV year1 Jun Mar Aug Jan

#p-value
p_forward <- ols_step_forward_p(m1)
summary(p_forward)
plot(p_forward)

cv2 <- train(sales ~ prom + Apr + West + East + North
             + South
             + May
             + banner
             + price
             + TV
             + year1
             + Jun
             + Mar
             + Jan
             + Aug, 
             data = train,
             method = 'lm',
             trControl = trc)
cv2 

cv3 <- train(sales ~ year1 + Feb + Mar + Apr + May + Jun + Jul + Sep + Nov + TV + banner + prom, 
             data = train,
             method = 'lm',
             trControl = trc)
cv3 #0.439

#forward aic
m2 <- lm(sales ~ prom + Apr + West + East + North
         + South
         + May
         + banner
         + price
         + TV
         + year1
         + Jun
         + Mar
         + Jan
         + Aug, 
         data = train)

par(mfrow = c(1, 1))
par(mar = rep(2, 4))
plot(m2)
summary(m2)
vif(m2) # No multicollinearity 

#test

#test$sales <- log(test$sales)
test_res <- predict(m2, newdata = test)

sqrt(sum(test[,'sales'] - test_res)^2/nrow(test))

sum(test[,'sales'] - test_res)^2 / nrow(test)

test$predictions <- predict(m2, newdata = test)
train$predictions <- predict(m2, newdata = train)

m4 <- lm(sales ~ prom + Apr + West + East + North
         + South
         + May
         + banner
         + price
         + TV
         + year1
         + Jun
         + Mar
         + Jan
         + Aug, 
         data = train)

library(ModelMetrics)
rmse(test$sales, test$predictions) 
# on average your predictions of sales is off by 499 packs
#498.9963
install.packages('Metrics')
require(Metrics)
mape(test$sales, test$predictions)
# mean absolute percent error 
# on average your model's prediction of sales is wrong by 7.7% 

rmse(train$sales, train$predictions)
#416.1977

#train$sales <- log(train$sales)
m2_resid <-train$sales - test$predictions

plot(m2$fitted.values,
     m2_resid,
     bty = 'n',
     pch = 5,
     col = rgb(.5, .1, .5),
     xlab = 'Fitted',
     ylab = 'Residuals',
     main = 'Test fitted values')

abline(h = 0,
       col = 'black')


#Expected value of error around 0
plot(1:length(m2$residuals),
     m2$residuals,
     bty = 'o',
     pch = 5,
     col = rgb(.0, .0, .0),
     xlab = 'Index',
     ylab = 'Residuals',
     main = 'Index & Residuals')

abline(h = 0,
       col = 'red')

test$residuals <- test$sales - test$predictions

ggplot(test, aes(x = predictions,
                 y = sales)) +
  geom_point(color = 'purple',
             alpha = 0.8) + 
  geom_abline(color = 'firebrick') + 
  ggtitle('Test prediction')

ggplot(data = test,
       aes(x = predictions,
           y = residuals)) + 
  geom_pointrange(aes(ymin = 0,
                      ymax = residuals),
                  color = 'black',
                  alpha = 0.8) + 
  geom_hline(yintercept = 0,
             linetype = 1,
             color = 'firebrick') + 
  ggtitle('Test fit')


train$residuals <- train$sales - train$predictions

par(mfrow = c(1,2))

ggplot(train, aes(x = predictions,
                 y = sales)) +
  geom_point(color = 'deepskyblue',
             alpha = 0.8) + 
  geom_abline(color = 'firebrick') + 
  ggtitle('Training prediction')

ggplot(data = train,
       aes(x = predictions,
           y = residuals)) + 
  geom_pointrange(aes(ymin = 0,
                      ymax = residuals),
                  color = 'black',
                  alpha = 0.8) + 
  geom_hline(yintercept = 0,
             linetype = 1,
             color = 'firebrick') + 
  ggtitle('Training fit')

