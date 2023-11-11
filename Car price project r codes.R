
getwd()
setwd("C:/Users/sanni/OneDrive/Documents/My_R_files/My_College_Projects/Cars Price Prediction")
Cars = read.csv("car price.csv")
head(Cars)
dim(Cars)
str(Cars)
summary(Cars)

#Checking if i have missing observations or not 
sum(is.na(Cars))

##Dataset is clean and no substitution of Null values is required

#CLEANING DATA 
#1. Separate the CarName variable to two columns : CompanyName and CarModel

#install.packages("tidyr")
library(tidyr)
#install.packages("rlang")

Cars <- separate(Cars, name, into = c("CompanyName", "CarModel"), sep = " ")
## in our analysis the two columns : ID and CarModel is unnecessary so we drop them now .
Cars$ID = NULL
Cars$CarModel = NULL

##Let's see in CompanyNames column, are there any repetitive values?
unique(Cars$CompanyName)

#In reviewing the above data, we found that few company names were identical but misspelled, such as:
##'maxda' Et 'mazda' ================> mazda
##'porsche' Et 'porcshce' ===========> porsche
##'toyota' Et 'toyouta' =============> toyota
##'vokswagen' Et 'volkswagen','vw' ==> volkswagen
##'Nissan' Et 'nissan' ==============> nissan
#So we have to adjust things by replacing the values with one identical variable:

Cars$CompanyName <- gsub("maxda", "mazda", Cars$CompanyName)
Cars$CompanyName <- gsub("porcshce", "porsche", Cars$CompanyName)
Cars$CompanyName <- gsub("toyouta", "toyota", Cars$CompanyName)
Cars$CompanyName <- gsub("vokswagen", "volkswagen", Cars$CompanyName)
Cars$CompanyName <- gsub("vw", "volkswagen", Cars$CompanyName)
Cars$CompanyName <- gsub("Nissan", "nissan", Cars$CompanyName) 

#2. Exploratory Data Analysis
##Since the independent variable (i.e Price) is continuous numerical variable, and there is many dependat variables, we we will use Multiple linear regression.
##Dependent variable visualization: Price

library(ggplot2)
ggplot(data=Cars, aes(x="", y=price)) + 
  geom_boxplot(fill="navyblue", color="black") +
  labs(title="Car Price Spread", y="Price") 
summary(Cars$price)

##According to the boxplot, the price field has an average around 13K and a median around 10k with the most expensive car values at 45k and the cheapest cars at 5k.

##Since we have mean > median, then our distribution is positively asymmetric, as we can see in the following histogram:

ggplot(data=Cars,aes(x=price))+geom_histogram(fill="lightblue",color="white")+  labs(title="Histogram of Price", x="Price")

###Distribution plot
ggplot(data=Cars, aes(x=price)) + geom_density(fill="lightgreen", color="lightgreen") +labs(title="Car Price Distribution Plot", x="Price") 

##Conclusion

##Which means that most of the prices offered by this company are low.

##As seen below, we have 75% prices are around 16k, or 25% between 17k and 45k.

#Visualization of independent variables :
#         ***Numerical***
##A. Checking the linear relationship between the dependent variable "Price" and the numerical independent variables
### we check this by drawing  scatterplot 
###A1. Price vs wheelbase

ggplot(data=Cars,aes(x=wheelbase,y=price))+geom_point(colour="royalblue") + geom_smooth(fill=NA)

cor(Cars$wheelbase,Cars$price)    # positively correlated as shown in the figure

###A2. Price vs Curbweight

ggplot(data=Cars,aes(x=curbweight,y=price))+geom_point(colour="royalblue") + geom_smooth(fill=NA)

cor(Cars$curbweight,Cars$price)

###A3. Price Vs Boreratio

ggplot(data=Cars,aes(x=boreratio,y=price))+geom_point(colour="royalblue") + geom_smooth(fill=NA)

cor(Cars$boreratio,Cars$price)    

##At first glance, the 3 variables are positively correlated but spread at higher values.

##We can make sure of this by looking at the Coefficient of Correlation. calculated above.

###A4. Price vs Carlength

ggplot(data=Cars,aes(x=carlength,y=price))+geom_point(colour="royalblue") + geom_smooth(fill=NA)

cor(Cars$carlength,Cars$price)

###A5. price vs Carwidth 

ggplot(data=Cars,aes(x=carwidth,y=price))+geom_point(colour="royalblue") + geom_smooth(fill=NA)

cor(Cars$carwidth,Cars$price)

###A6. price vs Carheight

ggplot(data=Cars,aes(x=carheight,y=price))+geom_point(colour="royalblue") + geom_smooth(fill=NA)

cor(Cars$carheight,Cars$price)

##Carlength and Carwidth are more correlated than carheight which is more spread out but positive.

##We can make sure of this by looking at the Coefficient of Correlation

###A7. price vs Enginesize 

ggplot(data=Cars,aes(x=enginesize,y=price))+geom_point(colour="royalblue") + geom_smooth(fill=NA)

cor(Cars$enginesize,Cars$price)

###A8. price vs  Horsepower

ggplot(data=Cars,aes(x=horsepower,y=price))+geom_point(colour="royalblue") + geom_smooth(fill=NA)

cor(Cars$horsepower,Cars$price)

###A9. price vs Stroke

ggplot(data=Cars,aes(x=stroke,y=price))+geom_point(colour="royalblue") + geom_smooth(fill=NA)

cor(Cars$stroke,Cars$price)

##Enginesize and Horsepower are positively correlated, but Stroke is more spread out (may not be related).

##We can make sure of this by looking at the Coefficient of Correlation

###A10. price vs compressionratio 

ggplot(data=Cars,aes(x=compressionratio,y=price))+geom_point(colour="royalblue") + geom_smooth(fill=NA)

cor(Cars$compressionratio,Cars$price)

###A11. price vs  peakrpm 

ggplot(data=Cars,aes(x=peakrpm,y=price))+geom_point(colour="royalblue") + geom_smooth(fill=NA)

cor(Cars$peakrpm,Cars$price)


###A12. price vs symboling

ggplot(data=Cars,aes(x=symboling,y=price))+geom_point(colour="royalblue") + geom_smooth(fill=NA)

cor(Cars$symboling,Cars$price)

##Compressionratio, Peakrpm and symboling are not correlated.

## We can make sure of this by looking at the Coefficient of Correlation

###A13. price vs citympg 

ggplot(data=Cars,aes(x=citympg,y=price))+geom_point(colour="royalblue") + geom_smooth(fill=NA)

cor(Cars$citympg,Cars$price)

###A14. price vs highwaympg

ggplot(data=Cars,aes(x=highwaympg,y=price))+geom_point(colour="royalblue") + geom_smooth(fill=NA)

cor(Cars$highwaympg,Cars$price)

##Citympg & Highwaympg are negatively correlated.
##The more prices get lower, the higher the distances get, which means that the cheapest cars have better mileage than expensive cars.

##We can make sure of this by looking at the Coefficient of Correlation

## Conclusion

#(+) positively correlated variables with Price: wheelbase, carlenght, carwidth, curbweight, enginesize, boreratio, horesepower

#(-) negatively correlated variables with Price: citympg, highwaympg

#These variables should be kept for a better model, and the other variables should be ignored as they are not correlated with Price

# Checking the multicollinearity between the correlated independent variables above and Price

#install.packages("reshape2")
library(reshape2)

# Calculate the correlation matrix
corr_matrix <- cor(Cars[,c("wheelbase", "carlength", "carwidth", "carheight","curbweight","enginesize","boreratio","stroke","compressionratio",
              "horsepower","peakrpm","citympg","highwaympg","price" )])

# Melt the correlation matrix into a long format
corr_melted <- melt(corr_matrix)

# Create the heatmap
ggplot(corr_melted, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="seagreen") +
  labs(title="Heatmap of Correlation Matrix I", x="", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label=round(value,2)), color="black", size=3)

 
#a. Examination of the correlation between the variables specific to the dimensions of a car 
#i.e. weight, height etc

dimension_col_list = c('wheelbase', 'carlength', 'carwidth','carheight','curbweight')
Cars_matrix = cor(Cars[, dimension_col_list])
Cars_melted = melt(Cars_matrix)

ggplot(Cars_melted, aes(x=Var1, y=Var2)) +
  geom_tile(aes(fill=value), colour="white") +
  scale_fill_gradient(low="white", high="seagreen") +
  labs(title="Heatmap of Correlation Matrix II", x="", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label=round(value,2)), color="black", size=3)


##Wheelbase , carlength, carwidth et curbweight [ 0.80 - 0.88 ] are very correlated and we have to keep only one between them.

#b. Examination of the correlation between the variables specific to the performance of a car

dimension_col_list1 = c('enginesize','boreratio','horsepower')
data_matrix = cor(Cars[, dimension_col_list1])
data_melted = melt(data_matrix)

ggplot(data_melted, aes(x=Var1, y=Var2)) +
  geom_tile(aes(fill=value), colour="white") +
  scale_fill_gradient(low="white", high="seagreen") +
  labs(title="Heatmap of Correlation Matrix III", x="", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label=round(value,2)), color="black", size=3)

#Horsepower and enginesize are highly correlated and we need to keep only one.
#c. Examining the correlation between citympg and highwaympg

dimension_col_list2 = c('citympg','highwaympg')
data_matrix1 = cor(Cars[, dimension_col_list2])
data_melted1 = melt(data_matrix1)

ggplot(data_melted1, aes(x=Var1, y=Var2)) +
  geom_tile(aes(fill=value), colour="white") +
  scale_fill_gradient(low="white", high="seagreen") +
  labs(title="Heatmap of Correlation Matrix IV", x="", y="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label=round(value,2)), color="black", size=3)

#citympg and highwaympg are highly correlated and we need to keep one of them.

#         *** Categorical ***

## Price VS CompanyName

ggplot(Cars, aes(x=CompanyName, y=price , fill=CompanyName)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))


ggplot(Cars, aes(x=CompanyName,fill= CompanyName)) +
  geom_bar() +
  coord_flip() +
  theme_minimal()

#Looking at the above histogram, Toyota seems to be very popular, followed by Nissan and Mazda.

## Price VS fueltype

ggplot(data=Cars,aes(x=fueltypes,y=price,color=fueltypes)) + geom_jitter() + geom_boxplot(size=1.2,alpha=0.5)

ggplot(data=Cars,aes(x=fueltypes,fill=fueltypes)) + geom_bar() +  ggtitle("Fuel Type Histogram")

##The average price of a diesel car is higher than that of gas cars, which explains, according to the histogram, why the company sold more gas cars than diesel cars.

#Price VS aspiration

ggplot(data=Cars,aes(x=aspiration,y=price,color=aspiration)) + geom_jitter() + geom_boxplot(size=1.2,alpha=0.5)

ggplot(data=Cars,aes(x=aspiration,fill=aspiration)) + geom_bar() +  ggtitle("Aspiration Histogram")

##The average price of cars with turbo aspiration is higher than that of standard aspiration, which explains, according to the histogram, why the company sells cars with standard
##aspiration more than of cars with turbo aspiration.

#Price VS doornumber

ggplot(data=Cars,aes(x=doornumbers,y=price,color=doornumbers)) + geom_jitter() + geom_boxplot(size=1.2,alpha=0.5)

ggplot(data=Cars,aes(x=doornumbers,fill=doornumbers)) + geom_bar() +  ggtitle("Doornumber Histogram")

#doornumber values are pretty close, which means the price is not affected by doornumber

#Price VS enginelocation


ggplot(data=Cars,aes(x=enginelocation,y=price,color=enginelocation)) + geom_jitter() + geom_boxplot(size=1.2,alpha=0.5)

ggplot(data=Cars,aes(x=enginelocation,fill=enginelocation)) + geom_bar() +  ggtitle("Enginelocation Histogram")

#It is clear that rear cars are very expensive, which is why the company sold more cars with front rear.

#Price VS carbody

ggplot(data=Cars,aes(x=carbody,y=price,color=carbody)) + geom_jitter() + geom_boxplot(size=1.2,alpha=0.5)

ggplot(data=Cars,aes(x=carbody,fill=carbody)) + geom_bar() +  ggtitle("Carbody Histogram")

#It seems that sedan is the most favored.hardtop has the highest average price.

#Price VS fuelsystem

ggplot(data=Cars,aes(x=fuelsystem,y=price,color=fuelsystem)) + geom_jitter() + geom_boxplot(size=1.2,alpha=0.5)

ggplot(data=Cars,aes(x=fuelsystem,fill=fuelsystem)) + geom_bar() +  ggtitle("Carbody Histogram")

#mpfi is the most favored type of fuelsystem , even though it has the highest average price.

#Price VS enginetype


ggplot(data=Cars,aes(x=enginetype,y=price,color=enginetype)) + geom_jitter() + geom_boxplot(size=1.2,alpha=0.5)

ggplot(data=Cars,aes(x=enginetype,fill=enginetype)) + geom_bar() +  ggtitle("Enginetype Histogram")

#ohc is the most favored engine type.

#Price VS cylindernumber

ggplot(data=Cars,aes(x=cylindernumber,y=price,color=cylindernumber)) + geom_jitter() + geom_boxplot(size=1.2,alpha=0.5)

ggplot(data=Cars,aes(x=cylindernumber,fill=cylindernumber)) + geom_bar() +  ggtitle("Cylinder Number Histogram")

#The four-cylinder seems to be the most favored.
# We can see that expensive cars have eight-cylinder , and four-cylinder are the cheapest.

#Price VS drivewheel

ggplot(data=Cars,aes(x=drivewheels,y=price,color=drivewheels)) + geom_jitter() + geom_boxplot(size=1.2,alpha=0.5)

ggplot(data=Cars,aes(x=drivewheels,fill=drivewheels)) + geom_bar() +  ggtitle("Drivewheel Histogram")

#FWD is the most favored, followed by RWD , and 4WD is the least favored even though it is cheaper than RWD .

#Price VS symboling

ggplot(data = Cars) +
  geom_boxplot(aes(x = symboling, y = price, fill = factor(symboling))) +
  labs(title = "Symboling vs Price", x = "Symboling", y = "Price") +
  scale_fill_manual(values = c("coral", "lightblue", "lightgreen", "orange", "purple" , "pink"))

ggplot(data = Cars) +
  geom_histogram(aes(x = symboling, fill = factor(symboling)), color = "black") +
  labs(title = "Symboling Histogram", x = "Symboling", y = "Frequency") +
  scale_fill_manual(values = c("coral", "lightblue", "lightgreen", "orange", "purple" , "pink"))

#It seems that symboling 0 and 1 are the most favored.

#Cars with symboling -1 and -2 are the most expensive, which is logical because it means that the car is more secure.

#Now we are going to subset our variables according to our use 

Cars1 =  subset(Cars, select = -c(carheight, stroke, compressionratio, peakrpm, carlength, carwidth, curbweight, enginesize, highwaympg, citympg,symboling,horsepower,boreratio,wheelbase))
head(Cars1)

Cars2 =  subset(Cars, select = c(carheight, stroke, compressionratio, peakrpm, carlength, carwidth, curbweight, enginesize, highwaympg, citympg,symboling,horsepower,boreratio,wheelbase , price))
head(Cars2)

Cars4 <- Cars[, !(names(Cars) %in% c('carheight', 'stroke', 'compressionratio', 'peakrpm', 'carlength', 'carwidth', 'curbweight', 'enginesize', 'highwaympg'))]
head(Cars4)



