##ANOVA FOR CAR PRICE DATA 

#ANOVA MODEL 1 
head(Cars1)
# to check which model can be used in , i count the number of observations in each cells
table(Cars1$CompanyName , Cars1$fueltypes)

#NOTE : it is clear that two way anova unequal observations per cell is suitable .

library(car)
Model1 = aov(price~CompanyName*fueltypes , data=Cars1)

Anova(Model1,type="III",singular.ok = TRUE)

###Based on the ANOVA table , i can reject the null hypothesis for CompanyName and CompanyName:fueltypes since their p-values 
###are less than 0.05. This means that there is a significant difference between at least two of the levels of these factors.

##However, i cannot reject the null hypothesis for fueltypes since its F value is empty. This means that there is not enough
##variation in the data for this factor to perform an ANOVA test.

#==================================================================================================================================

#ANOVA MODEL 2 
head(Cars1)
# to check which model can be used in , i count the number of observations in each cells
table(Cars1$CompanyName , Cars1$carbody)

#NOTE : it is clear that two way anova unequal observations per cell is suitable .

library(car)
Model2 = aov(price~CompanyName*carbody , data=Cars1)

Anova(Model2,type="III",singular.ok = TRUE)

###I can reject the null hypothesis for CompanyName since its p-value is less than 0.05 .
###This means that there is a significant difference between at least two of the levels of CompanyName.

##However, you cannot reject the null hypothesis for carbody or CompanyName:carbody since
##their p-values are greater than 0.05. This means that there is not enough evidence to suggest 
##that there is a significant difference between at least two of the levels of these factors.

##Pairwise t test 

pairwise.t.test(Cars1$price,Cars1$CompanyName,p.adjust.method = "BH")

#=================================================================================================================

#Multiple Regression

mat_a = subset(Cars2,select = -c(price))
numeric = mat_a[sapply(mat_a,is.numeric)]
descrcor=cor(numeric)
descrcor

library(caret)
highlyCorrelated=findCorrelation(descrcor,cutoff = 0.7)
highlycorcol = colnames(numeric)[highlyCorrelated]
highlycorcol
#we can find which columns are highly correlated and removing them from the model
Cars3 = Cars2[ , -which(colnames(Cars2) %in% highlycorcol)]
dim(Cars3)

#Model3
library(car)
Model3= lm(price~. , data=Cars3)
summary(Model3)

#Model4
Model4 = lm(price~. , data=Cars2)
summary(Model4)

#Model5
Model5 = lm(price~wheelbase + boreratio + horsepower + citympg + symboling , data=Cars4)
summary(Model5)

#Regression diagnostics 

# Plot the residuals vs fitted values
plot(Model5$fitted.values, Model5$residuals)

# Plot the normal Q-Q plot
qqnorm(Model4$residuals)
qqline(Model4$residuals)

# Plot the scale-location plot
plot(Model5$fitted.values, sqrt(abs(Model5$residuals)))



# Plot all the diagnostic plots together
par(mfrow = c(2, 2))
plot(Model4)