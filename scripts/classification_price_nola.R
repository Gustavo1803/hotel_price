
# Clear workspace

rm(list = ls())

# Set working directory

### Initial path ###

user_path = "C:/Users/genin/Box Sync/MY DOCUMENTS/Precios Nola"  

setwd(user_path) ### Change the path in your computer

# Define function for loading packages

pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

# Load packages

packages <- c("caret","dplyr", "stargazer", "eeptools", "collapse","lubridate","tidyverse", "lfe", 'fixest', 'broom',
              "ggplot2", "binsreg", "caret")
lapply(packages, pkgTest)

prices = read.csv("Data_hoteles_v2.csv")
cities = read.csv('cities.csv')



###############################
##### Cleanning Procedure #####
###############################

prices_clean = left_join(prices, cities, by='address') %>% mutate(review_score = as.numeric(review_score),
                                 price = str_extract(price, "(?<=COP\\s).*"), 
                                 price = as.numeric(gsub(",", "", price))/1000,
                                 distance = as.numeric(stringr::str_extract(distance, "\\d+\\.?\\d*")),
                                 Date= as.Date(dates_in,'%m/%d/%y'),
                                 Year = year(Date),
                                 Week = week(Date),
                                 DayOfWeek = wday(Date, label = TRUE),
                                 sost = relevel(as.factor(sost), ref =1),
                                 Beds = as.numeric(Beds),
                                 Rooms = as.numeric(Rooms)) 

write.csv(prices_clean, file = 'data_clean_v2.csv', row.names = FALSE)

price_graphs = prices_clean %>% filter(review_number>10) %>% group_by(name,address,distance) %>% 
  summarise(review_score = mean(review_score, na.rm = TRUE),
            price = mean(price, na.rm = TRUE),
            distance = mean(distance, na.rm = TRUE))

price_graphs = as.data.frame(price_graphs) %>% mutate(ln_score = log(review_score),
                                                      ln_price = log(price))

###########################################
######### Regressions plot   ##############
###########################################

reg = lm(log(price) ~ . -name - link - review  - address - image -dates_in-dates_out -X -Bathroom -Bedroom -View -Outdoors -Room.Amenities -Activities -Services -Safety...security -General,  data = prices_clean)
summary(reg)

reg2 = lm(log(review_score) ~. -name - link - review - review_score - address - image -dates_in-dates_out -X -Bathroom -Bedroom -View -Outdoors -Room.Amenities -Activities -Services -Safety...security -General   , data = prices_clean)
summary(reg2)


coef_data1 <- tidy(reg)  %>% mutate(model = 'Price')
coef_data2 =  tidy(reg2) %>% mutate(model = 'Score')

filter_out = c('(Intercept)', "CityApulo", "CityBojaca" ,"CityCachipay" ,"CityLa Aurora",                                                        
               "CityLa Mesa", "CityMadrid", "CityMesitas", "CitySan Antonio del Tequendama",                                       
               "CitySan Francisco", "CitySilvania", "CitySoacha", "CitySubata",                                                        
               "CityTena" ,"CityViota", "CityZipacon"  , 'CityAnolaima', "DayOfWeek.C", "DayOfWeek.L",
               "DayOfWeek.Q", "DayOfWeek^4", 'Week', 'Date', 'Spanish', 'Wellness', 'Spa.facilities',
               'Toilet', 'Food...Drink')


coef_data1 = coef_data1 %>% mutate(conf.low = estimate -1.65*std.error,
                                 conf.high = estimate + 1.65*std.error) %>% drop_na(estimate) %>% 
  filter(!(term %in% filter_out))

coef_data2 = coef_data2 %>% mutate(conf.low = estimate -1.65*std.error,
                                   conf.high = estimate + 1.65*std.error) %>% drop_na(estimate) %>% 
  filter(!(term %in% filter_out))

sorted_coefficients2 <- coef_data2 %>% arrange(estimate, decreasing = FALSE)

top_positive2 <- head(sorted_coefficients2, 40)
top_negative2 <- tail(sorted_coefficients2, 1)

coefficient_data2 = rbind(top_positive2,top_negative2)
list_coef = coefficient_data2$term

coefficient_data1 = coef_data1 %>% filter(term %in% list_coef)

coefficient_data = rbind(coefficient_data1,coefficient_data2)

# Create a plot of coefficients with confidence intervals
ggplot(coefficient_data, aes(x = reorder(term, estimate), y = estimate, ymin = conf.low, ymax = conf.high, color =model)) +
  geom_point(stat = "identity", size = 3, alpha= 0.5) +
  geom_errorbar(width = 0.2) + geom_hline(yintercept = 0.0, color = "black", linetype = "dashed") +
  coord_flip() +
  labs(x = "Coefficient", y = "Estimate") +
  ggtitle("Regression Coefficients with Confidence Intervals") +
  theme_minimal() 


##### Constraint to the places that are outside with views

reg = lm(log(price) ~ . -name - link - review  - address - image -dates_in-dates_out -X -Bathroom -Bedroom -View -Outdoors -Room.Amenities -Activities -Services -Safety...security -General,  data = subset(prices_clean,View==1))
summary(reg)

reg2 = lm(log(review_score) ~. -name - link - review - review_score - address - image -dates_in-dates_out -X -Bathroom -Bedroom -View -Outdoors -Room.Amenities -Activities -Services -Safety...security -General ,  data = subset(prices_clean,View==1))
summary(reg2)


coef_data1 <- tidy(reg)  %>% mutate(model = 'Price')
coef_data2 =  tidy(reg2) %>% mutate(model = 'Score')

filter_out = c('(Intercept)', "CityApulo", "CityBojaca" ,"CityCachipay" ,"CityLa Aurora",                                                        
               "CityLa Mesa", "CityMadrid", "CityMesitas", "CitySan Antonio del Tequendama",                                       
               "CitySan Francisco", "CitySilvania", "CitySoacha", "CitySubata",                                                        
               "CityTena" ,"CityViota", "CityZipacon"  , 'CityAnolaima', "DayOfWeek.C", "DayOfWeek.L",
               "DayOfWeek.Q", "DayOfWeek^4", 'Week', 'Date', 'Spanish', 'Wellness', 'Spa.facilities',
               'Toilet', 'Food...Drink')


coef_data1 = coef_data1 %>% mutate(conf.low = estimate -1.65*std.error,
                                   conf.high = estimate + 1.65*std.error) %>% drop_na(estimate) %>% 
  filter(!(term %in% filter_out))

coef_data2 = coef_data2 %>% mutate(conf.low = estimate -1.65*std.error,
                                   conf.high = estimate + 1.65*std.error) %>% drop_na(estimate) %>% 
  filter(!(term %in% filter_out))

sorted_coefficients2 <- coef_data2 %>% arrange(estimate, decreasing = TRUE)

top_positive2 <- head(sorted_coefficients2, 25)
top_negative2 <- tail(sorted_coefficients2, 25)

coefficient_data2 = rbind(top_positive2,top_negative2)
list_coef = coefficient_data2$term

coefficient_data1 = coef_data1 %>% filter(term %in% list_coef)

coefficient_data = rbind(coefficient_data1,coefficient_data2)

# Create a plot of coefficients with confidence intervals
ggplot(coefficient_data, aes(x = reorder(term, estimate), y = estimate, ymin = conf.low, ymax = conf.high, color =model)) +
  geom_point(stat = "identity", size = 3, alpha= 0.5) +
  geom_errorbar(width = 0.2) + geom_hline(yintercept = 0.0, color = "black", linetype = "dashed") +
  coord_flip() +
  labs(x = "Coefficient", y = "Estimate") +
  ggtitle("Regression Coefficients with Confidence Intervals") +
  theme_minimal() 

#########################################
########## Lasso Model ################
#####################################

library(gamlr)
library(glmnet)


nola = read_csv('nola_feat_2.0.csv')
nola = nola %>% mutate_all(~ ifelse(. == "YES", 1, ifelse(. == "NO", 0,0)))
nola = as.data.frame(nola)
nola = model.matrix(~.,nola)[,-1]

####################################
########### Training Model #########
###################################

data_na = na.omit(prices_clean)
price = data_na$price

irrelevant_city = c('Bojaca','La Aurora', 'La Vega', 'Madrid', 'San Francisco', 'Soacha', 'Subata')

data_price = cbind(price,data_na[,14:(ncol(data_na)-4)]) %>% filter(price<1250) %>% filter(!City %in% irrelevant_city)
price = data_price$price
data_matrix <- model.matrix(price ~ ., data_price)[,-1]


set.seed(123)
split_index <- createDataPartition(data_price$price, p = 0.8, list = FALSE)
train_data <- data_matrix[split_index, ]
test_data <- data_matrix[-split_index, ]

# Separate the response variable from predictors
train_price <- data_price$price[split_index]
test_price <- data_price$price[-split_index]


lasso_model <- glmnet(x = train_data, 
                      y = log(train_price), 
                      alpha = 1)

plot(lasso_model)
cv_lasso <- cv.glmnet(x = train_data, 
                      y = log(train_price), 
                      alpha = 1)

plot(cv_lasso)

best_lambda <- cv_lasso$lambda.min
lasso_coefficients <- coef(lasso_model, s = best_lambda)


# Obtain predicted values from the Lasso model
predicted_values <- exp(predict(lasso_model, newx =test_data, s = best_lambda))

# Calculate the mean of the observed values (y) and the total sum of squares (TSS)
mean_y <- mean(test_price)
TSS <- sum((test_price - mean_y)^2)

# Calculate the residual sum of squares (RSS)
RSS <- sum((test_price - predicted_values)^2)

# Calculate R-squared (R^2)
R_squared <- 1 - (RSS / TSS)

R_squared

lands = binsreg(test_price,predicted_values, nbins=50, polyreg = 1, bycolors = 'blue4') 


lands$bins_plot + theme(axis.text=element_text(size=18),
                        axis.title=element_text(size=18,face="bold"))

predicted_p_nola = exp(predict(lasso_model, newx =nola , s = best_lambda))

##################
##### Review #####

data_review = cbind(score,data_na[,14:(ncol(data_na)-4)])
data_matrix = model.matrix(score~.,data_review)[,-1]

lasso_model <- glmnet(x = data_matrix, 
                      y = score, 
                      alpha = 1)

plot(lasso_model)
cv_lasso <- cv.glmnet(x = data_matrix, 
                      y = score, 
                      alpha = 1)

plot(cv_lasso)

best_lambda <- cv_lasso$lambda.min
lasso_coefficients <- coef(lasso_model, s = best_lambda)


# Obtain predicted values from the Lasso model
predicted_values <- predict(lasso_model, newx =data_matrix , s = best_lambda)
predicted_values = ifelse(predicted_values>10,10,predicted_values)

# Calculate the mean of the observed values (y) and the total sum of squares (TSS)
mean_y <- mean(score)
TSS <- sum((score - mean_y)^2)

# Calculate the residual sum of squares (RSS)
RSS <- sum((score - predicted_values)^2)

# Calculate R-squared (R^2)
R_squared <- 1 - (RSS / TSS)

R_squared

plot(predicted_values,score)


predicted_s_nola= predict(lasso_model, newx =nola , s = best_lambda)

l_p_nola = predicted_p_nola - 150
h_p_nola = predicted_p_nola + 150


#######################
#### Nola In Market ###
#######################


lands = binsreg(price_graphs$review_score, price_graphs$price, nbins=50, polyreg = 4, bycolors = 'blue4') 


lands$bins_plot + theme(axis.text=element_text(size=18),
                        axis.title=element_text(size=18,face="bold"))  + 
  labs(x = 'Price', y = 'Score') + ylim(8,10) + xlim(0,1800) + 
  geom_vline(xintercept = 500, linetype = "dashed", color = "black") + geom_hline(yintercept = 8.6, linetype = "dashed", color = "black") + 
  geom_rect(aes(xmin=l_p_nola,xmax=h_p_nola,ymin=8.0,ymax=9.2), fill= 'red', alpha=0.2) +
  geom_point(aes(x=predicted_p_nola, y=mean(c(9.2,8))), size = 3, color = "red", shape = 16)


#####################
#### Clustering #####
#####################

price_scale  = scale(log(price))
review_scale = scale(score)



data_total = cbind(price_scale,review_scale,data_na[,14:(ncol(data_na)-4)])
data_matrix = model.matrix(~.,data_total)[,-1]

set.seed(200)
grpMeat<- kmeans(data_matrix, center=5, nstart=100)
plot(price_scale, review_scale,pch=19,col=grpMeat$cluster,cex=0.9)

means<-grpMeat$centers

points(means,col=c(1,2),cex=2,pch=3, col = 'black')

library(ggExtra)
data_plot = as.data.frame(cbind(price_scale, review_scale,grpMeat$cluster))
clusters = grpMeat$cluster
data_total = cbind(data_total,clusters)
centers = as.data.frame(grpMeat$centers) %>% dplyr::select(price_scale,review_scale)


plot = ggplot() +
  geom_point(data = data_plot, aes(x = V1, y = V2, color = as.factor(V3)),
             position = position_jitter(width = 0.2, height = 0.2), size = 1.3) +
  geom_point(data = centers, aes(x = price_scale, y = review_scale), size = 6, color = "black") +
  labs(x = "Log(Price)", y = "Review Score") + theme_minimal() +
  theme(legend.position = 'none') 

p1 <-  ggMarginal(plot,
  fill = "grey",  # Set the color of the marginal histograms
  type = "histogram", 
  bins = 30,
  alpha = 0.8  # Adjust alpha for better clarity
)
p1



ggplot(data_total) + 
geom_bar(aes(y=price_scale, x =clusters), position="stack", stat="identity")

ggplot(data_total) + 
  geom_bar(aes(y=review_scale, x =clusters), position="stack", stat="identity")





require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

test <- multinom(clusters ~ ., data = data_total)
summary(test)
