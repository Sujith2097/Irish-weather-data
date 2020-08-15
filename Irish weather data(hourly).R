# Reading the dataset
data.file <- read.csv("hourly_irish_weather.csv")

# Viewing the dataset
View(data.file)
head(data.file)
str(data.file)

# checking the number of rows
nrow(data.file)

# Data type of the file
class(data.file)

# Structure of the data
str(data.file)

# Dealing with the missing data 
# lets check the count of null values in the dataset
sum(is.na(data.file))

# As the data file contains 4,218,054 entries it is hard to analyze the data and time taking process  
# i would like to subset the cork county
# And check the missing data in the county
attach(data.file)
# subsetting the cork county
cork_county <- subset(data.file, county=="Cork")
View(cork_county)

# checking the structure of cork county
str(cork_county)

# Checking the NA values in each column of the data frame
lapply(cork_county,function(x) { length(which(is.na(x)))})

# As there are many NA values in some of the column 
# Now plotiing a graph to analyze the missing values       
library(VIM)
# Plotting a graph to choose the best consideration for the missing entries 
missing_values <- aggr(cork_county, prop= FALSE, numbers = TRUE)
summary(missing_values)

# Now checking the strongly correlated variables
# Ignoring the columns x, station, county, date, longitude, latitude as we cant correlate between them
# So taking only the numerical variables
numerical_variables <- subset(cork_county, select = -c(X, station, county, date, longitude, latitude))
str(numerical_variables)

#---------------------correlation plot----------------------------------#
# Plotting the correlation plot by using the library corrplot
library(corrplot)
corrplot(corr = cor(numerical_variables, use = "na.or.complete"), tl.col = "Black")

# after checking the correlation plot  selecting the variables that are highly correlated
cork_new <- subset(cork_county, select = c(station, county, date, temp, wetb, dewpt, vappr, rhum))
str(cork_new)

# Checking the NA values in each column
sapply(cork_new, function(x)sum(length(which(is.na(x)))))

# Checking the stations present in county cork 
unique(cork_new$station)

# Putting the columns which has the NA values into list_na to take the median average
list_na <- colnames(cork_new)[apply(cork_new, 2, anyNA) ]
list_na


# Filling the NA values with median imputation for each station
# Averaging the NA values in cork_airport station
average_missing_cork <- apply(cork_new[cork_new$station == "Cork_Airport",colnames(cork_new) %in% list_na],
                              2,
                              median,
                              na.rm =  TRUE)
average_missing_cork

# Averaging the NA values in Moore_park station
average_missing_moore <- apply(cork_new[cork_new$station == "Moore_Park",colnames(cork_new) %in% list_na],
                               2,
                               median,
                               na.rm =  TRUE)

average_missing_moore


# Averaging the NA values in Roches_Point station
average_missing_Roches <- apply(cork_new[cork_new$station == "Roches_Point",colnames(cork_new) %in% list_na],
                                2,
                                median,
                                na.rm =  TRUE)
average_missing_Roches

# Averaging the NA values in sherkinIsland station
average_missing_Sherkin <- apply(cork_new[cork_new$station == "SherkinIsland",colnames(cork_new) %in% list_na],
                                 2,
                                 median,
                                 na.rm =  TRUE)

average_missing_Sherkin

library(dplyr)
# Mutating the new median values in the dataframe cork_replace_cork
cork_replace_cork <- cork_new[cork_new$station=="Cork_Airport",] %>%
  mutate(median_temp  = ifelse(is.na(temp), average_missing_cork[1], temp),
         median_wetb = ifelse(is.na(wetb), average_missing_cork[2], wetb),
         median_dewpt  = ifelse(is.na(dewpt), average_missing_cork[3], dewpt),
         median_vappr = ifelse(is.na(vappr), average_missing_cork[4], vappr),
         median_rhum = ifelse(is.na(rhum), average_missing_cork[5], rhum))

# Mutating the new median values in the dataframe cork_replace_Moore
cork_replace_Moore <- cork_new[cork_new$station=="Moore_Park",] %>%
  mutate(median_temp  = ifelse(is.na(temp), average_missing_moore[1], temp),
         median_wetb = ifelse(is.na(wetb), average_missing_moore[2], wetb),
         median_dewpt  = ifelse(is.na(dewpt), average_missing_moore[3], dewpt),
         median_vappr = ifelse(is.na(vappr), average_missing_moore[4], vappr),
         median_rhum = ifelse(is.na(rhum), average_missing_cork[5], rhum))

# Mutating the new median values in the dataframe cork_replace_Roches
cork_replace_Roches <- cork_new[cork_new$station=="Roches_Point",] %>%
  mutate(median_temp  = ifelse(is.na(temp), average_missing_Roches[1], temp),
         median_wetb = ifelse(is.na(wetb), average_missing_Roches[2], wetb),
         median_dewpt  = ifelse(is.na(dewpt), average_missing_Roches[3], dewpt),
         median_vappr = ifelse(is.na(vappr), average_missing_Roches[4], vappr),
         median_rhum = ifelse(is.na(rhum), average_missing_cork[5], rhum))

# Mutating the new median values in the dataframe cork_replace_SherkinIsland
cork_replace_Sherkin <- cork_new[cork_new$station=="SherkinIsland",] %>%
  mutate(median_temp  = ifelse(is.na(temp), average_missing_Sherkin[1], temp),
         median_wetb = ifelse(is.na(wetb), average_missing_Sherkin[2], wetb),
         median_dewpt  = ifelse(is.na(dewpt), average_missing_Sherkin[3], dewpt),
         median_vappr = ifelse(is.na(vappr), average_missing_Sherkin[4], vappr),
         median_rhum = ifelse(is.na(rhum), average_missing_cork[5], rhum))


# Combing all the above dataframes in row wise
cork_modified = rbind(cork_replace_cork, cork_replace_Moore, cork_replace_Roches, cork_replace_Sherkin)

# Dropping the previous set of rows and keeping the new rows in the dataframe
cork_modified <- subset(cork_modified, select = -c(temp,vappr,dewpt,wetb,rhum))
View(cork_modified)
str(cork_modified)


#-------------------------- PCA---------------------------#
# Converting all the variables in cork_modified into numeric 
data_numerical_variables <- sapply(cork_modified, is.numeric)
data_numerical_variables

# Saving the numerical values into the new data frame 
pca_variables <- cork_modified[,data_numerical_variables]
pca_variables

# Storing the pca variables 
library("FactoMineR")
pca <- PCA(pca_variables, graph = FALSE)
summary(pca)
str(pca)
pca

# Eigen values and Eigen vectors
library("factoextra")
pca_eig_values <- get_eigenvalue(pca)
pca_eig_values

fviz_eig(pca, addlabels = TRUE, ylab = c(0,50))

fviz_pca_var(pca, col.var = "black")

# Correlation plot for pca varaiables
library("corrplot")
corrplot(pca$var$cos2, is.corr = TRUE)


#---------------------Normality testing------------------------------------#
# p-value tells us the chances that the sample comes from a normal distribution
# selecting random sample for doing shapiro test as it allows till 5000 samples
set.seed(100)
random_cork_modified <- cork_modified[sample(1:nrow(cork_modified), 5000, replace = FALSE),]
str(random_cork_modified)

# shapiro test tells if sample is normally distributed or not
normality_test <- shapiro.test(random_cork_modified$median_temp)
normality_test$p.value

normality_test <- shapiro.test(random_cork_modified$median_wetb)
normality_test$p.value

# checking distribution for every variable
with(cork_modified, sapply(random_cork_modified[,4:8], shapiro.test))
# Plotting to check distribution
#install.packages("viridis")

library(ggplot2)
# Plotting the histogram for the temp variable
hist(random_cork_modified$median_temp, 
     breaks = 'Sturges',
     xlab = 'temp in c',
     main = 'Bar plot for temp',
     col = 3)

# Plotting the Q-Q plot to check the normality of the variable
# Quantile-quantile plot allows us to tell whether data is distributed normally
qqnorm(random_cork_modified$median_temp, main = 'Q-Q plot for temp')
qqline(random_cork_modified$median_temp, col = 3, lwd = 2)

hist(cork_modified$median_wetb, 
     breaks = 'Sturges',
     xlab = 'wetb in c',
     main = 'Bar plot for wetb',
     col = 3)

# qqnorm function plots your sample 
# against a normal distribution
qqnorm(random_cork_modified$median_wetb, main = 'Q-Q plot for wetb')
qqline(random_cork_modified$median_wetb, col = 3, lwd = 2)

#---------------------------Hypothesis testing--------------------#
# In normality test the p-values is less than 0.05 so the null hypothesis is rejected
# As it is not normally distribution and temperature being independent variable and dewpt being dependent variable 
# we are using the spearman correlation test
# spearman correlation test
c_test <- cor.test(random_cork_modified$median_temp, random_cork_modified$median_wetb, 
                   method = "spearman")

c_test



# Power test for sample size determination
# Loading pwr library
library(pwr)
# giving the test name and testing the r value 
size <- cohen.ES(test = "r", size = "large")
size
# Computing the power of test by giving the r value
samplesize <- pwr.r.test(r = 0.5, sig.level = 0.01, power = 0.90, alternative = "two.sided")
samplesize
# Plotting the power of test
plot(samplesize)
