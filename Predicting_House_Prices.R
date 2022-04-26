
library(dplyr) # for glimpse etc.
library(funModeling) # for plot_num, df_status etc.
options(scipen = 999) 
setwd("D:/")

housing_sales_data <- read.csv("./House_Prices.csv", header = T)

head(housing_sales_data)

str(housing_sales_data)

#Let's see the glimpse of the housing sales data
glimpse(housing_sales_data)

table(housing_sales_data$view)
table(housing_sales_data$condition)
table(housing_sales_data$grade)
table(housing_sales_data$bedrooms)

# health status of housing sales data
df_status(housing_sales_data)

# Let's understand the distributions more with the help of histograms
plot_num(housing_sales_data)

############################
#Q1
############################

#linear regression model to see regression line between various 
#features and price

plot1 <- ggplot(housing_sales_data, aes(bedrooms,price)) + geom_point(alpha=0.5) + geom_smooth(method = "lm",se= FALSE)
plot1
model1 <- lm(price ~ bedrooms, data = housing_sales_data)
summary(model1)
#Residual standard error: 323500 on 997 degrees of freedom
#Multiple R-squared:  0.09418,	Adjusted R-squared:  0.09327 
#F-statistic: 103.7 on 1 and 997 DF,  p-value: < 2.2e-16

#Bedrooms is not an important feature as R-square value is very less


plot2 <- ggplot(housing_sales_data, aes(bathrooms,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot2
model2 <- lm(price ~ bathrooms, data = housing_sales_data)
summary(model2)
#Residual standard error: 296800 on 997 degrees of freedom
#Multiple R-squared:  0.2375,	Adjusted R-squared:  0.2368 
#F-statistic: 310.6 on 1 and 997 DF,  p-value: < 2.2e-16

#Bathrooms is slightly important feature as R-square value is moderately fine

plot3 <- ggplot(housing_sales_data, aes(sqft_living,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot3
model3 <- lm(price ~ sqft_living, data = housing_sales_data)
summary(model3)

#Residual standard error: 241100 on 997 degrees of freedom
#Multiple R-squared:  0.497,	Adjusted R-squared:  0.4964 
#F-statistic: 984.9 on 1 and 997 DF,  p-value: < 2.2e-16

# sqft_living is very important as R-square value is 49.7%

plot4 <- ggplot(housing_sales_data, aes(sqft_lot,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot4
model4 <- lm(price ~ sqft_lot, data = housing_sales_data)
summary(model4)
#Residual standard error: 336200 on 997 degrees of freedom
#Multiple R-squared:  0.02152,	Adjusted R-squared:  0.02053 
#F-statistic: 21.92 on 1 and 997 DF,  p-value: 3.232e-06

#sqft_lot is not an important feature as R-square value is very less

plot5 <- ggplot(housing_sales_data, aes(floors,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot5
model5 <- lm(price ~ floors, data = housing_sales_data)
summary(model5)
#Residual standard error: 330000 on 997 degrees of freedom
#Multiple R-squared:  0.05736,	Adjusted R-squared:  0.05641 
#F-statistic: 60.67 on 1 and 997 DF,  p-value: 1.69e-14

#floors is not an important feature as R-square value is very less

plot6 <- ggplot(housing_sales_data, aes(waterfront,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot6
model6 <- lm(price ~ waterfront, data = housing_sales_data)
summary(model6)
#Residual standard error: 322400 on 997 degrees of freedom
#Multiple R-squared:  0.1007,	Adjusted R-squared:  0.09978 
#F-statistic: 111.6 on 1 and 997 DF,  p-value: < 2.2e-16

#waterfront is slightly important feature as R-square value comparatively less

plot7 <- ggplot(housing_sales_data, aes(view,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot7
model7 <- lm(price ~ view, data = housing_sales_data)
summary(model7)
#Residual standard error: 304400 on 997 degrees of freedom
#Multiple R-squared:  0.1982,	Adjusted R-squared:  0.1974 
#F-statistic: 246.4 on 1 and 997 DF,  p-value: < 2.2e-16

#view is slightly important feature as R-square value is moderately fine

plot8 <- ggplot(housing_sales_data, aes(condition,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot8
model8 <- lm(price ~ condition, data = housing_sales_data)
summary(model8)
#Residual standard error: 339000 on 997 degrees of freedom
#Multiple R-squared:  0.00539,	Adjusted R-squared:  0.004392 
#F-statistic: 5.402 on 1 and 997 DF,  p-value: 0.02031

#condition is not an important feature as R-square value is very less

plot9 <- ggplot(housing_sales_data, aes(grade,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot9
model9 <- lm(price ~ grade, data = housing_sales_data)
summary(model9)
#Residual standard error: 258900 on 997 degrees of freedom
#Multiple R-squared:  0.4197,	Adjusted R-squared:  0.4191 
#F-statistic: 721.1 on 1 and 997 DF,  p-value: < 2.2e-16

# grade is very important as R-square value is 41.97%

plot10 <- ggplot(housing_sales_data, aes(sqft_above,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot10
model10 <- lm(price ~ sqft_above, data = housing_sales_data)
summary(model10)
#Residual standard error: 276200 on 997 degrees of freedom
#Multiple R-squared:  0.3397,	Adjusted R-squared:  0.339 
#F-statistic: 512.8 on 1 and 997 DF,  p-value: < 2.2e-16

# sqft_above is important as R-square value is 33.97%

plot11 <- ggplot(housing_sales_data, aes(sqft_basement,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot11
model11 <- lm(price ~ sqft_basement, data = housing_sales_data)
summary(model11)

#Residual standard error: 316200 on 997 degrees of freedom
#Multiple R-squared:  0.1347,	Adjusted R-squared:  0.1339 
#F-statistic: 155.2 on 1 and 997 DF,  p-value: < 2.2e-16

#sqft_basement is slightly important feature as R-square value is comparatively less

plot12 <- ggplot(housing_sales_data, aes(yr_built,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot12
model12 <- lm(price ~ yr_built, data = housing_sales_data)
summary(model12)
#Residual standard error: 339900 on 997 degrees of freedom
#Multiple R-squared:  0.0002786,	Adjusted R-squared:  -0.0007242 
#F-statistic: 0.2778 on 1 and 997 DF,  p-value: 0.5983

#yr_built is not an important feature as R-square value is very less

plot13 <- ggplot(housing_sales_data, aes(yr_renovated,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot13
model13 <- lm(price ~ yr_renovated, data = housing_sales_data)
summary(model13)
#Residual standard error: 336300 on 997 degrees of freedom
#Multiple R-squared:  0.02139,	Adjusted R-squared:  0.02041 
#F-statistic: 21.79 on 1 and 997 DF,  p-value: 3.456e-06

#yr_renovated is not an important feature as R-square value is very less

plot14 <- ggplot(housing_sales_data, aes(zipcode,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot14
model14 <- lm(price ~ zipcode, data = housing_sales_data)
summary(model14)
#Residual standard error: 339200 on 997 degrees of freedom
#Multiple R-squared:  0.004384,	Adjusted R-squared:  0.003385 
#F-statistic:  4.39 on 1 and 997 DF,  p-value: 0.0364

#zipcode is not an important feature as R-square value is very less

plot15 <- ggplot(housing_sales_data, aes(lat,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot15
model15 <- lm(price ~ lat, data = housing_sales_data)
summary(model15)
#Residual standard error: 316400 on 997 degrees of freedom
#Multiple R-squared:  0.1333,	Adjusted R-squared:  0.1325 
#F-statistic: 153.4 on 1 and 997 DF,  p-value: < 2.2e-16

#lat is slightly important feature as R-square value is comparatively less

plot16 <- ggplot(housing_sales_data, aes(long,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot16
model16 <- lm(price ~ long, data = housing_sales_data)
summary(model16)
#Residual standard error: 339700 on 997 degrees of freedom
#Multiple R-squared:  0.001066,	Adjusted R-squared:  6.447e-05 
#F-statistic: 1.064 on 1 and 997 DF,  p-value: 0.3025

#long is not an important feature as R-square value is very less

plot17 <- ggplot(housing_sales_data, aes(sqft_living15,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot17
model17 <- lm(price ~ sqft_living15, data = housing_sales_data)
summary(model17)
#Residual standard error: 259500 on 997 degrees of freedom
#Multiple R-squared:  0.417,	Adjusted R-squared:  0.4164 
#F-statistic: 713.1 on 1 and 997 DF,  p-value: < 2.2e-16

#sqft_living15 is an important feature as R-square value is high

plot18 <- ggplot(housing_sales_data, aes(sqft_lot15,price)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm",se= FALSE)
plot18
model18 <- lm(price ~ sqft_lot15, data = housing_sales_data)
summary(model18)
#Residual standard error: 335400 on 997 degrees of freedom
#Multiple R-squared:  0.0262,	Adjusted R-squared:  0.02522 
#F-statistic: 26.82 on 1 and 997 DF,  p-value: 2.695e-07

#sqft_lot15 is not an important feature as R-square value is very less

###Conclusion###
## bathrooms, sqft_living, waterfront, view, grade, sqft_above, sqft_basement, lat
## and sqft_living15 are important features in predicting house prices


############################
#Q2
############################
#Correlation Matrix of identified variables
housing_sales_data_subset2 = subset(housing_sales_data, select = c(bathrooms, sqft_living, waterfront, view, grade, sqft_above,  
                                                                           sqft_basement, lat, sqft_living15))
head(housing_sales_data_subset2)
#cor(housing_sales_data_subset1)
cor(housing_sales_data_subset2)
## Let's visualize the correlation matrix created above
library(corrplot)
cor.vis <- round(cor(housing_sales_data_subset2),2)
#corrplot(cor.vis, method = "number")

#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cor.vis,hc.order = TRUE,
           type = "upper",
           lab = TRUE,
           lab_size = 2)

#lab_size is used to control size of coefficients in the plot

### Conclusion ###
# Looking at the visualization we can identify the following:

# view is correlated with waterfront
# sqft_above is correlated with bathrooms, grade and sqft_living and sqft_living15 
# sqft_living is correlated with sqft_basement, bathrooms, grade and sqft_living15
# sqft_living15 is correlated with bathrooms and grade
# grade is correlated with bathrooms



############################
#Q3
############################
# Let's build a multiple linear regression model using identified features
#housing_sales_data_subset3 = price + housing_sales_data_subset2
housing_sales_data_subset3 = subset(housing_sales_data, select = c(price, bathrooms, sqft_living, waterfront, view, grade, sqft_above,  
                                                                   sqft_basement, lat, sqft_living15))
multi_linreg_model3 <- lm(price ~ ., data = housing_sales_data_subset3)
summary(multi_linreg_model3)


#Residuals:
#  Min      1Q  Median      3Q     Max 
#-737460  -97361  -13801   67484 1487284 

#Coefficients: (1 not defined because of singularities)
#Estimate   Std. Error t value             Pr(>|t|)    
#(Intercept)   -31615195.41   2062983.85 -15.325 < 0.0000000000000002 ***
#  bathrooms        -19866.49     12525.74  -1.586              0.11305    
#  sqft_living         161.34        16.83   9.588 < 0.0000000000000002 ***
#  waterfront       733756.08     73625.46   9.966 < 0.0000000000000002 ***
#  view              73909.37      9373.59   7.885  0.00000000000000828 ***
#  grade             62588.43      9137.41   6.850  0.00000000001298103 ***
#  sqft_above          -17.35        16.51  -1.051              0.29363    
#  sqft_basement           NA           NA      NA                   NA    
#  lat              657810.11     43505.97  15.120 < 0.0000000000000002 ***
#  sqft_living15        49.23        16.51   2.981              0.00294 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 190000 on 990 degrees of freedom
#Multiple R-squared:  0.6899,	Adjusted R-squared:  0.6874 
#F-statistic: 275.3 on 8 and 990 DF,  p-value: < 0.00000000000000022

### Conclusion###
## Above p-values and R-squared value show that above features are important to
## predict house prices

############################
#Q4
############################
#Looking at the p-values from Q3, we can say that sqft_living, waterfront, view,
# grade, lat and sqft_living15 are more important factors to identify housing prices

#Let's build a multiple regression model using only important features identified 
#based on p-values from Q3:

housing_sales_data_subset4 = subset(housing_sales_data, select = c(price, sqft_living, waterfront, view, grade,  
                                                                   lat, sqft_living15))
multi_linreg_model4 <- lm(price ~ ., data = housing_sales_data_subset4)
summary(multi_linreg_model4)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-744295  -96037  -12060   69591 1483845 

#Coefficients:
#  Estimate   Std. Error t value             Pr(>|t|)    
#(Intercept)   -32177340.54   2040032.56 -15.773 < 0.0000000000000002 ***
#  sqft_living         142.94        12.85  11.122 < 0.0000000000000002 ***
#  waterfront       728548.82     73627.11   9.895 < 0.0000000000000002 ***
#  view              77987.86      9005.37   8.660 < 0.0000000000000002 ***
#  grade             57306.47      8692.47   6.593      0.0000000000701 ***
#  lat              669925.90     42991.67  15.583 < 0.0000000000000002 ***
#  sqft_living15        45.22        16.20   2.792              0.00535 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 190100 on 992 degrees of freedom
#Multiple R-squared:  0.6888,	Adjusted R-squared:  0.6869 
#F-statistic: 365.9 on 6 and 992 DF,  p-value: < 0.00000000000000022

### Conclusion###
##R-square values have not changed much after reducing the features (from 0.6899 to 0.6888) that means these selected features are able to explain the variance and left out features are not important.
##Residual error has not changed much after reducing the features (from 190000 to 190100) so we are good with these reduced number of features.
##F-Statistic has increased significantly i.e. from 275.3 to 365.9 which is very important

############################
#Q5
############################

## Now let's analyse sqft_living, waterfront, view, grade, lat and
## sqft_living15 for interaction effects as these all are 
## more important features in predicting house prices

#Let's add 5 new columns in dataframe to check interaction effects with sqft_living feature

#Let's check regression between price and these new variables one by one - 

housing_sales_data_interact <- housing_sales_data


housing_sales_data_interact$sqft_living_waterfront <- housing_sales_data$sqft_living * housing_sales_data$waterfront
#head(housing_sales_data_interact)

housing_sales_data_interact$sqft_living_view <- housing_sales_data$sqft_living * housing_sales_data$view
#head(housing_sales_data_interact)

housing_sales_data_interact$sqft_living_grade <- housing_sales_data$sqft_living * housing_sales_data$grade
#head(housing_sales_data_interact)

housing_sales_data_interact$sqft_living_lat <- housing_sales_data$sqft_living * housing_sales_data$lat
#head(housing_sales_data_interact)

housing_sales_data_interact$sqft_living_living15 <- housing_sales_data$sqft_living * housing_sales_data$sqft_living15
#head(housing_sales_data_interact)

#interaction effects (multiplication of values from original features)

plot36 <- ggplot(housing_sales_data_interact, aes(sqft_living_waterfront,price)) + geom_point(alpha=0.3) + geom_smooth(method = "lm",se= FALSE)
plot36
model36 <- lm(price ~ sqft_living_waterfront, data = housing_sales_data_interact)
summary(model36)

#Residual standard error: 313400 on 997 degrees of freedom
#Multiple R-squared:  0.1502,	Adjusted R-squared:  0.1494 
#F-statistic: 176.2 on 1 and 997 DF,  p-value: < 0.00000000000000022

#As the combined R-square value (0.1502) is lesser than individual 
#R-square value of sqft_living with price (0.497), we do not
#see interaction effect between sqft_living and grade

plot37 <- ggplot(housing_sales_data_interact, aes(sqft_living_view,price)) + geom_point(alpha=0.3) + geom_smooth(method = "lm",se= FALSE)
plot37
model37 <- lm(price ~ sqft_living_view, data = housing_sales_data_interact)
summary(model37)

#Residual standard error: 280200 on 997 degrees of freedom
#Multiple R-squared:  0.3206,	Adjusted R-squared:  0.3199 
#F-statistic: 470.5 on 1 and 997 DF,  p-value: < 0.00000000000000022

#As the combined R-square value (0.3206) is lesser than individual 
#R-square value of sqft_living with price (0.497), we do not
#see interaction effect between sqft_living and view


plot39 <- ggplot(housing_sales_data_interact, aes(sqft_living_grade,price)) + geom_point(alpha=0.3) + geom_smooth(method = "lm",se= FALSE)
plot39
model39 <- lm(price ~ sqft_living_grade, data = housing_sales_data_interact)
summary(model39)

#Residual standard error: 228800 on 997 degrees of freedom
#Multiple R-squared:  0.5469,	Adjusted R-squared:  0.5464 
#F-statistic:  1203 on 1 and 997 DF,  p-value: < 2.2e-16

#As the combined R-square value (0.5469) is greater than individual 
#R-square value of sqft_living with price (0.497), we do
#see interaction effect between sqft_living and grade


plot315 <- ggplot(housing_sales_data_interact, aes(sqft_living_lat,price)) + geom_point(alpha=0.3) + geom_smooth(method = "lm",se= FALSE)
plot315
model315 <- lm(price ~ sqft_living_lat, data = housing_sales_data_interact)
summary(model315)

#Residual standard error: 240500 on 997 degrees of freedom
#Multiple R-squared:  0.4996,	Adjusted R-squared:  0.4991 
#F-statistic: 995.3 on 1 and 997 DF,  p-value: < 0.00000000000000022

#As the combined R-square value (0.4996) is greater than individual 
#R-square value of sqft_living with price (0.497), we do
#see interaction effect between sqft_living and lat


plot317 <- ggplot(housing_sales_data_interact, aes(sqft_living_living15,price)) + geom_point(alpha=0.3) + geom_smooth(method = "lm",se= FALSE)
plot317
model317 <- lm(price ~ sqft_living_living15, data = housing_sales_data_interact)
summary(model317)

#Residual standard error: 229700 on 997 degrees of freedom
#Multiple R-squared:  0.5435,	Adjusted R-squared:  0.5431 
#F-statistic:  1187 on 1 and 997 DF,  p-value: < 2.2e-16

#As the combined R-square value (0.5435) is greater than individual 
#R-square value of sqft_living with price (0.497), we do
#see interaction effect between sqft_living and sqft_living15


### Conclusion ###
## We have seen interaction effects in:
#1) sqft_living and sqft_grade
#2) sqft_living and lat
#3) sqft_living and sqft_living15

