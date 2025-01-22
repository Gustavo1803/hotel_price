
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
              "ggplot2", "binsreg")
lapply(packages, pkgTest)

prices = read.csv("Data_hoteles.csv")
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
                                 sost = relevel(as.factor(sost), ref =1)) 

price_graphs = prices_clean %>% filter(review_number>10) %>% group_by(name,address,distance) %>% 
  summarise(review_score = mean(review_score, na.rm = TRUE),
            price = mean(price, na.rm = TRUE),
            distance = mean(distance, na.rm = TRUE))

price_graphs = as.data.frame(price_graphs) %>% mutate(ln_score = log(review_score),
                                                      ln_price = log(price))

#### Price vs Distance Graph ###

lands = binsreg(price_graphs$price,price_graphs$distance, nbins=20, ci = c(1,0), polyreg = 7, bycolors = 'brown2') 


lands$bins_plot + theme(axis.text=element_text(size=18),
                        axis.title=element_text(size=18,face="bold"))  + 
  labs(x = 'Distance (Km)', y = 'Price Thousands') + ylim(0,1000) + xlim(0,30) + 
   geom_vline(xintercept = 3, linetype = "dashed", color = "black") + 
   geom_vline(xintercept = 11, linetype = "dashed", color = "black")


#### Score Vs Distance ####

lands = binsreg(price_graphs$review_score,price_graphs$distance, nbins=10, ci = c(1,0), polyreg = 3, bycolors = 'brown2') 

lands$bins_plot + theme(axis.text=element_text(size=18),
                        axis.title=element_text(size=18,face="bold"))  + 
  labs(x = 'Distance (Km)', y = 'Score') + ylim(6,10) + xlim(0,30) 


#### Price Vs Score ####

lands = binsreg(price_graphs$review_score, price_graphs$price, nbins=20, polyreg = 4, bycolors = 'blue4') 


lands$bins_plot + theme(axis.text=element_text(size=18),
                        axis.title=element_text(size=18,face="bold"))  + 
  labs(x = 'Price', y = 'Score') + ylim(8,10) + xlim(0,1800) + 
  geom_vline(xintercept = 500, linetype = "dashed", color = "black")


#### Price Vs month ####

lands = binsreg(prices_clean$price, prices_clean$Week, nbins=20, ci = c(1,0), polyreg = 4, bycolors = 'blue4') 

lands$bins_plot + theme(axis.text=element_text(size=18),
                        axis.title=element_text(size=18,face="bold"))  + 
  labs(x = 'Week of the Year', y = 'Price Thousands')  + xlim(0,53) + 
  geom_vline(xintercept = 5, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 48, linetype = "dashed", color = "black")





###########################################
######### Places with Prices ##############
###########################################

summary_data <- prices_clean %>%
  group_by(City) %>%
  summarize(
    Average = mean(price, na.rm = TRUE),
    CI_lower = mean(price, na.rm = TRUE) - (1.96 * sd(price, na.rm = TRUE) / sqrt(n())),
    CI_upper = mean(price, na.rm = TRUE) + (1.96 * sd(price, na.rm = TRUE) / sqrt(n()))
  )

summary_data <- summary_data %>%
  mutate(city = reorder(City, Average))

# Create the horizontal bar graph
ggplot(summary_data, aes(x = Average, y = City)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  geom_errorbarh(
    aes(xmin = CI_lower, xmax = CI_upper),
    height = 0.2,
    position = position_dodge(0.7)
  ) +
  labs(
    x = "Average Value Price",
    y = "Group",
    title = "Price 95% Confidence Interval"
  ) +
  theme_minimal() 


###########################################
######### Places with Scores ##############
###########################################

summary_data <- prices_clean %>%
  group_by(City) %>%
  summarize(
    Average = mean(review_score, na.rm = TRUE),
    CI_lower = mean(review_score, na.rm = TRUE) - (1.96 * sd(review_score, na.rm = TRUE) / sqrt(n())),
    CI_upper = mean(review_score, na.rm = TRUE) + (1.96 * sd(review_score, na.rm = TRUE) / sqrt(n()))
  )

summary_data <- summary_data %>%
  mutate(city = reorder(City, Average))

# Create the horizontal bar graph
ggplot(summary_data, aes(x = Average, y = City)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  geom_errorbarh(
    aes(xmin = CI_lower, xmax = CI_upper),
    height = 0.2,
    position = position_dodge(0.7)
  ) +
  labs(
    x = "Average Review Score",
    y = "Group",
    title = "Price 95% Confidence Interval"
  ) +
  theme_minimal() 


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


nola = read_csv('nola_feat.csv')
nola = model.matrix(~.,nola)[,-1]

data_na = na.omit(prices_clean)
price = data_na$price
score = data_na$review_score

##################
##### Prices #####

data_price = cbind(price,data_na[,14:(ncol(data_na)-4)])
data_matrix = model.matrix(price~.,data_price)[,-1]

lasso_model <- glmnet(x = data_matrix, 
                      y = log(data_na$price), 
                      alpha = 1)

plot(lasso_model)
cv_lasso <- cv.glmnet(x = data_matrix, 
                      y = log(data_na$price), 
                      alpha = 1)

plot(cv_lasso)

best_lambda <- cv_lasso$lambda.min
lasso_coefficients <- coef(lasso_model, s = best_lambda)


# Obtain predicted values from the Lasso model
predicted_values <- exp(predict(lasso_model, newx =data_matrix , s = best_lambda))

# Calculate the mean of the observed values (y) and the total sum of squares (TSS)
mean_y <- mean(data_na$price)
TSS <- sum((data_na$price - mean_y)^2)

# Calculate the residual sum of squares (RSS)
RSS <- sum((data_na$price - predicted_values)^2)

# Calculate R-squared (R^2)
R_squared <- 1 - (RSS / TSS)

R_squared

lands = binsreg(price,predicted_values, nbins=50, polyreg = 1, bycolors = 'blue4') 


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

#############################
##### Definition of Clients##
#############################

####### Group #1 ############
# High price, high score, 
## Enviroment: Good infrastructure, good furniture outside, good views
## Bedroom: Private bath, cleaning, refrigerator, good amenities
## Facilities: some private pool, poor indoor, family rooms, private entrance, security, room service, some banquets , housekeeping, restaurant, bar, good english
## Relax:some fitnsess, some sauna, spa center, massage, jacuzzi, beauty service, spa
## Entertainment: High family service, some games room, high hiking, high cycling, high walking tours

##### Group #2 ############
# Low price, high score
## Enviroment: medium infrastructure, medium furniture, bad views
## Bedroom  : private bathroom, no kitchen, no cleaning, nor ref, average amenities
## Facilities: no private pool, no poor indoor, family rooms,no  private entrance, security, room service, good banquets, housekeeping, low restaurant, some bar, bad english
## Relax:no fitness, some sauna, no welnnes center, some massage, low jacuzzi, no beauty, no spa
## Entertainment: High family service, high games room, medium hiking, low cycling, medium walking tours

##### Group #3 #######
## High volatility of price(tend to be high price), high score
## Enviroment: good infrastructure, medium furniture, good views
## Bedroom  : private bathroom, kitchen, cleaning,ref, good amenities
## Facilities: private pool, no other pools, family rooms, private entrance, no security,no room service, no banquets, no housekeeping, no restaurant, some bar, good english
## Relax: no fitness, some sauna, no welnnes center, no massage,  jacuzzi, no beauty, no spa
## Entertainment: medium family service, low games room, medium hiking, low cycling, low walking tours

#### Grpup #4 ########
### Average price(tend to be low), average score
## Enviroment: medium infrastructure, bad furniture, bad view
## Bedroom  : no private bathroom, kitchen, no cleaning,no ref, bad amenities
## Facilities: some private pool, no other pools,no family rooms,no private entrance,no security, no room service, no banquets, no housekeeping, no restaurant, no bar, good english
## Relax: no fitness, some sauna, no welnnes center, no massage,  some jacuzzi, no beauty, no spa
## Entertainment: low family service, no games room, low hiking, low cycling, low walking tours

#### Group #5 #########
### average price, low score
## Enviroment: good infrastructure, bad furniture, medium view
## Bedroom  : no private bathroom, kitchen, cleaning, yes/no ref, average amenities
## Facilities: some private pool, no other pools,some family rooms, private entrance,some security, no room service, no banquets, some housekeeping, restaurant, some bar, bad english
## Relax: no fitness, some sauna, no welnnes center, no massage,  some jacuzzi, no beauty, no spa
## Entertainment: low family service, high games room, high hiking, high cycling, medium walking tours


# Create a function to generate stacked bar plots
create_stacked_bar_plot <- function(variable) {
  data_total %>%
    group_by(clusters, !!sym(variable)) %>%
    summarise(count = n()) %>%
    mutate(percentage = count/sum(count)) %>%
    ggplot(aes(x = clusters, y = percentage, fill = as.factor(!!sym(variable)))) +
    geom_bar(stat = "identity") +
    labs(title = paste("Stacked Bar Plot of Percentage by", variable),
         x = "Variable",
         y = "Percentage") +
    scale_fill_discrete(name = "Factorial Variable") +
    theme_minimal()
}

#### Enviromental Graphs
# Assuming your list of variables is named variable_list
variable_list <- c("View", "River.view", "Mountain.view", "Garden.view", "Outdoor.furniture", 
                   "Sun.terrace", "Patio", "Balcony", "Terrace", "Garden")


# Create and save plots for each variable
for (variable in variable_list) {
  plot_name <- paste("stacked_bar_plot_", variable, ".png", sep = "")
  g <- create_stacked_bar_plot(variable)
  ggsave(plot_name, g, width = 10, height = 6, units = "in")
}

### Room 
variable_list <- c("Private.bathroom", "Kitchen", "Cleaning.products", "Refrigerator", "Room.Amenities", "Pets Pets.are.allowed..Charges.may.be.applicable.")

# Create and save plots for each variable
for (variable in variable_list) {
  plot_name <- paste("stacked_bar_plot_", variable, ".png", sep = "")
  g <- create_stacked_bar_plot(variable)
  ggsave(plot_name, g, width = 10, height = 6, units = "in")
}


### facilities
variable_list <- c("Bar", "Restaurant", "Internet", "Daily.housekeeping", 
                   "Meeting.banquet.facilities", "Room.service", 
                   "CCTV.outside.property", "CCTV.in.common.areas", 
                   "Private.entrance", "Family.rooms", "Pool.1...indoor", 
                   "Pool.2...outdoor", "Private.pool")

# Create and save plots for each variable
for (variable in variable_list) {
  plot_name <- paste("stacked_bar_plot_", variable, ".png", sep = "")
  g <- create_stacked_bar_plot(variable)
  ggsave(plot_name, g, width = 10, height = 6, units = "in")
}

### Relaation Servie
variable_list <- c("Spa.lounge.relaxation.area", "Steam.room", "Spa.facilities",
                   "Beauty.Services", "Hammam", "Hot.tub.Jacuzzi", "Massage",
                   "Spa.and.wellness.centre", "Fitness.centre", "Sauna", "English")

# Create and save plots for each variable
for (variable in variable_list) {
  plot_name <- paste("stacked_bar_plot_", variable, ".png", sep = "")
  g <- create_stacked_bar_plot(variable)
  ggsave(plot_name, g, width = 10, height = 6, units = "in")
}

### Other Entertainment
variable_list <- c("Walking.tours", "Cycling", "Off.site", "Hiking", 
                   "Table.tennis", "Games.room", "Entertainment.and.family.services")

# Create and save plots for each variable
for (variable in variable_list) {
  plot_name <- paste("stacked_bar_plot_", variable, ".png", sep = "")
  g <- create_stacked_bar_plot(variable)
  ggsave(plot_name, g, width = 10, height = 6, units = "in")
}


#### Creatin of all the variables
library(fmsb)
library(scales)
library(RColorBrewer)



data_star1 = data.frame(Price = c(10,4,8),
                     Score = c(10,9,8),
                     Environment = c(8,4,8),
                     Bedroom = c(8,6,8),
                     Facilities = c(8,6,5),
                     Relax = c(7,4,5),
                     Entertainment = c(9,7,6)
                     )
rownames(data_star1) <- paste("Group" , letters[1:3] , sep="-")

data_star1 = rbind(rep(10,3) , rep(1,3) , data_star1)



colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

radarchart( data_star1  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

legend(x=0.7, y=1.3, legend = rownames(data_star1[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)



###

data_star2= data.frame(Price = c(5,6),
                        Score = c(6,4),
                        Environment = c(4,6),
                        Bedroom = c(4,6),
                        Facilities = c(3,6),
                        Relax = c(5,5),
                        Entertainment = c(5,8)
)
rownames(data_star2) <- paste("Group" , letters[4:5] , sep="-")

data_star2 = rbind(rep(10,2) , rep(1,3) , data_star2)



coul <- brewer.pal(3, "BuPu")
colors_border <- coul

colors_in <- alpha(coul,0.5)

radarchart( data_star2  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

legend(x=0.7, y=1.3, legend = rownames(data_star2[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)



