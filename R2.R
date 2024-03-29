# A. Conditional Statements and Loops – Using R
sum5 <- function(num){
  if(num < 0){
    print("error")
  } else {
    sum = 0
    while(num > 0){
      sum = sum + num
      num = num - 1
    }
    print(sum)
  }
}
sum5(10)

# B.Data Management – Using R
library(readxl)
library(dplyr)
setwd("~/Desktop/work_prep")
data <- read_excel("data/cars.xlsm")
data <- na.omit(data)

outlier_detect <- function(column_name){
  column_data <- data[[column_name]]
  data_iqr <- IQR(column_data)
  q1 <- quantile(column_data, .25)
  q2 <- quantile(column_data, .75)
  top_cutoff <- q2 + data_iqr*1.5
  bottom_cutoff <- q1 - data_iqr*1.5
  outliers <- column_data[(column_data > top_cutoff | column_data < bottom_cutoff)]
  print(paste(column_name, "Outliers:"))
  print(outliers)
  return(outliers)
}

replace_outliers <- function(column_names){
  for(column_name in column_names){
    if(is.numeric(data[[column_name]])){ 
      outliers <- outlier_detect(column_name)
      outlier_frame <- data[data[[column_name]] %in% outliers,]
      print.data.frame(outlier_frame)
      data[data[[column_name]] %in% outliers, column_name] <- mean(data[[column_name]])
    } else { # unsure of how to calculate outliers of categorical 
      # Detect outliers of categorical data
    }
  }
  return(data)
}
data <- replace_outliers(names(data))

#   3)
#     To be frank, I am not sure what advantage creating dummies has over using R's default factors. 
#     I read that factors are internally coded as dummy variables in R, so I am not entirely sure
#     of the differences. One of the few advtanges that I can imagine is that you could rename
#     dummy columns to whatever you wish, where you are a bit trapped in naming using default factors.
#     I will continue to research this. 

#     A dummy variable trap is when variables are multicollinear, yet separate variables are created
#     for each category regardless. There is essentially a duplicate category, when in reality there should
#     be some baseline that you do not manually add in the model. I.E. introducing both a male and female
#     column to represent gender would be problematic, as these are considered mutually exclusive and are
#     multicollinear. Instead, a single "female" category could be implemented with either a 0 to indicate
#     that the record is a man or a 1 to represent that the record is a female. Only n-1 variables are required
#     for a variable with n options.
#     
#     We use dummy variables on nominal data most often to incorporate them into regression analysis.
#     Consider a dataset with information about recent graduates, their majors, and their starting salaries.
#     If want to quantify the difference in salary between business and art majors, we cannot simply assign
#     these major names as business = 1, art = 2, etc., for they do not have a comparable relationship where
#     business is less than art and art is twice as much as business. Regression analysis depends on a directional
#     relationship of variables, and this is ignored with this crude technique. Instead, we can asign each of these majors
#     as either a 0 or a 1 to indicate its presence and measure the presence of that variable in studying it's
#     relationship to salary. In this way, each option to this nominal variable has an ordered relationship
#     where 0 represents the absence of that option and 1 represents it's presence. 

# C. User Defined Functions – Using R
#   4)
math_func <- function(vector){
  return(vector[1] + vector[2] * vector[3] - vector[4] / vector[5])
}
math_func(c(1,2,3,4,5))

#   5)
#   PROVIDED DATA DIFFERS IN NUMBER OF ROWS, ADDED NA TO END OF AGE COLUMN
sampledata <- data.frame(Marks = c(80,75,85,65,55,NA,NA,89,NA), 
                         age =   c(21,23,27,25,26,26,23,NA, NA) )
#     a)
impute_data <- function(column){
  replace(column, which(is.na(column)), mean(column, na.rm=TRUE))
}

#     b)
sampledata <- apply(sampledata, 2, impute_data)

#
# D. Descriptive Statistics
#   6)
#     a)
library(MASS)
library(dplyr)
# Unsure what the numbers under the data are -- emailed Prakash to confirm,
# but assumed that these are not important. initially thought they were
# frequencies but unsure
data <- c(23.20, 24.44, 25.29, 26.27, 28.11, 29.32, 30.36, 35.14, 36.19, 36.25,
          40.29, 42.27, 43.11, 44.20, 46.44, 46.36, 46.14, 48.19, 48.12, 48.26, 
          48.25, 49.20, 49.32, 49.36, 50.26, 53.28, 53.19, 70.14, 71.22, 85.10)
data_range <- range(data) 
breaks <- seq(floor(data_range[1]), ceiling(data_range[2]), by=3) 
interval <- cut(data, breaks, right=FALSE)
data_table <- table(interval)
data_table <- data_table %>% transform(Rel_Freq = prop.table(Freq), Per_Freq = prop.table(Freq) * 100, Cum_Freq = cumsum(Freq))
write.csv(data_table, file = "~/Desktop/data2.csv")
#     b)
#       ExcelHisto2.png attached
#     c)
#       The histogram appears to be moderately skewed right due to some extreme values on the upper end.
#     
#     d) Unsure what the numbers next to the question are. Wasn't sure if these are to be added to the dataset.
data_sd <- sd(data)
data_iqr <- IQR(data) 
#     e)
q1 <- quantile(data, .25)
q2 <- quantile(data, .75)
top_cutoff <- q2 + data_iqr*1.5
bottom_cutoff <- q1 - data_iqr*1.5
outliers <- data[(data > top_cutoff | data < bottom_cutoff)]
#     f)
#       The most appropriate central tendency measure for this data would be median, as it is skewed
#       and using mean would be influenced by these outliers.
#     g)
#       A logarithmic transformation would likely be most appropriate, as there are not many zeroes.
#       Using log(x+1) would be appropriate if any zeroes are in the dataset. 
# 7)
values <- c(23.2,24.4,25.2,26.2,28.1,29.3,30.3,35.1,36.1, 36.2, 
                                40.2, 42.2, 43.1, 44.2, 46.4, 46.3, 46.1, 48.1, 48.1,
                                48.2, NA, NA, 48.2, 49.2, 49.3, 49.3, 50.2, 53.2, 53.1,
                                70.1, 71.2, 85.1)
values <- na.omit(values)
values_range <- range(values) 
by <- 6
value_breaks <- seq(floor(values_range[1]), ceiling(values_range[2]) + by - ceiling(values_range[2]) %% by,
                     by=by) 
value_intervals <- cut(values, value_breaks, right=FALSE)
value_table <- table(value_intervals)
value_table <- value_table %>% transform(Rel_Freq = prop.table(Freq), Cum_Freq = cumsum(Freq))
value_table$Percent_Frequency <- value_table$Rel_Freq * 100
# MIDPOINTS FUNCTION CODE FROM https://www.r-bloggers.com/finding-the-midpoint-when-creating-intervals/
midpoints <- function(x, dp=2){
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(round(lower+(upper-lower)/2, dp))
}
value_table$Midpoint <- sort(midpoints(value_table$value_intervals))
value_table <- value_table[,c(1, 6, 2, 3, 5, 4)]
colnames(value_table) <- c("Interval", "Midpoint", "Absolute Frequency", "Relative Frequency",
                           "Percent Frequency", "Cumulative Frequency")

#    b)
mean <- mean(values)
median <- median(values)
quartile1 <- quantile(values, 0.25)
iqr <- IQR(values)
stand_dev <- sd(values)

#    c)
boxplot <- boxplot(values, ylab = "value",
                   main = "Value Boxplot")
#    d)
simple_table <- as.data.frame(values)
library(ggplot2)
histo <- ggplot(simple_table, aes(x=values)) +
  geom_histogram(color="black", fill="white", binwidth = 1) + ggtitle("Value Histogram") +
  theme(plot.title = element_text(hjust = 0.5))

# E. Probability Theory
# Probability that they are in semis - P(S)
# Both in group 1/2 but not immediate matchup P(G1)
# Both in group 3/4 but not immediate matchup P(G2)
# P(G1) = 1/2 * 2/7 (both in groups 1/2, diff matchup) = 1/7
# P(G2) = 1/2 * 2/7 (both in groups 3/4, diff matchup) = 1/7
# P(S | G1) requires both players to win .5 matchups
# P(S | G1) = 1/7 * 1/2 * 1/2 = 1/28
# +
# P(S | G2) = 1/7 * 1/2 * 1/2 = 1/28
# = 2/28 = 1/14

# F. Sampling Theory
# z(95%) = 1.96
# σ = 2.5
# m = 1
# m = z∗σ/√n
# n = (z∗σ/m)^2
# n = (1.96 * 2.5 / 1) ^2 =
ceiling(1.96*2.5 / 1)^2
# = 25 people

# G. Statistical Distributions – I and II
#   This problem follows a binomial distribution where n = 10, p = .1
#   P(X > 3) = 1 - (P(X = 0) + P(X = 1) + P(X = 2) + P(X = 3))
1 - (dbinom(0, 10, .1) + dbinom(1, 10, .1) + dbinom(2, 10, .1) + dbinom(3, 10, .1))

# H. Plots/Charts – Using R  **SAME AS IN R1.R FILE, COPY AND PASTED**
options(scipen=10000)
#   a) I was unsure of how to distinguish and label cities due to the high volume of cities, so I chose to remove the legend
#      entirely. Labeling each involved excessive overlap and showing a color legend took up extreme space. 
grouped_data <- txhousing %>% group_by(city, year) %>% summarise(sum = sum(volume, na.rm = TRUE))
ggplot(data=grouped_data, aes(x=year, y=sum, group=city, color = city)) +
  scale_y_continuous(labels = scales::comma) + geom_line() + xlab("Date") +
  ylab("Volume (Dollars)") +ggtitle("Texas Annual Volume by City") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 


#   b)
ggplot(data = grouped_data, aes(x = year, y = sum,fill=city)) +
  geom_bar(stat='identity', color = "white", size = .1) +
  scale_x_continuous(breaks = seq(min(grouped_data$year), max(grouped_data$year)))+ 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) + coord_flip() + ylab("Volume (Dollars)") + 
  ggtitle("Texas Annual Volume by City Bar") +
  xlab("Year") + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

#   c)
#     i)
library(zoo)
top_cities <- txhousing %>% group_by(city) %>% summarise(sum = sum(volume, na.rm = TRUE)) %>% 
  top_n(5, sum)
top_cities_monthly <- txhousing %>% group_by(city) %>% filter(city %in% top_cities$city)
top_cities_monthly$chrono_month <- as.yearmon(with(top_cities_monthly, paste(year, month, sep="-")), "%Y-%m")

ggplot(data=top_cities_monthly, aes(x=chrono_month, y=volume, group=city, color = city)) +
  scale_y_continuous(labels = scales::comma) + geom_line() +
  scale_x_continuous(breaks = seq(2000,2015,2)) +
  xlab("Date") + ylab("Volume (dollars)") + 
  ggtitle("Top 5 Texas City Monthly Volume") + 
  theme(plot.title = element_text(hjust = 0.5))

#     ii)
ggplot(data=top_cities_monthly, aes(x=chrono_month, y=volume, color = city)) +
  scale_y_continuous(labels = scales::comma) + geom_line() +
  scale_x_continuous(breaks = seq(2000,2015,2)) + facet_wrap(~city) + theme(legend.position = "none") +
  xlab("Date") + ylab("Volume (dollars)") + 
  ggtitle("Top 5 Texas City Monthly Volume") + 
  theme(plot.title = element_text(hjust = 0.5))

#     iii)
top_2015 <- top_cities_monthly %>% filter(year==2015) %>% group_by(city) %>% summarise(total_sales = sum(sales))
july_2015 <- top_cities_monthly %>% filter(year==2015) %>% filter(month==6) 
ggplot() + geom_point(data =top_2015, aes(city, total_sales, color = "dark green")) +
  geom_point(data = july_2015, aes(city, sales, color = "purple")) +
  scale_color_discrete(labels = c("Total 2015 Sales", "July 2015 Sales")) + 
  xlab("City") + ylab("Total Sales (dollars)") + 
  ggtitle("Top 5 Texas City 2015 Housing Sales") + 
  theme(plot.title = element_text(hjust = 0.5))

#     iv)
ggplot(top_cities_monthly, aes(x=city, y=sales)) + 
  xlab("City") + ylab("Number of Sales") + 
  ggtitle("Top 5 Texas City Monthly Sales") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot()

#     v)
library(reshape2)
city_means <- top_cities_monthly %>% summarise(sales_mean = mean(sales), volume_mean = mean(volume))

ggplot() + geom_point(data= top_cities_monthly, aes(sales, volume, color = city), size=.5) + scale_y_continuous(labels = scales::comma) +
  geom_point(data=city_means, mapping=aes(x = sales_mean, y = volume_mean, color = city), size = 3, shape = 10, stroke =2) + 
  geom_point(data=city_means, mapping=aes(x = sales_mean, y = volume_mean), size = 4, shape = 10, color = "black", stroke = .5) +
  xlab("Number of Sales") + ylab("Volume (dollars)") + 
  ggtitle("Top 5 Texas City Monthly Housing Sales vs. Volume") + 
  theme(plot.title = element_text(hjust = 0.5))

# I. Testing Of Hypothesis
#   a)
#     i.  H0 : b2 = 0 ; H1 : b2 < 0
#         t = (3.1 - 0)/1.70 = 1.824
#         Reject if t < -1.66
#         Accept null hypothesis

#     ii. H0 : b2 = 0 ; H1 : b2 ≠ 0
#         t = (3.1 - 0)/1.70 = 1.824
#         Reject if |t| > 1.99
#         Accept null hypothesis
#
#     iii. H0 : b2 = 0 ; H1 : b2 > 0
#         t = (3.1 - 0)/1.70 = 1.824
#         Reject if t > 1.66
#         Reject null hypothesis
#
#     iv. H0 : b2 = 0 ; H1 : b2 ≠ 1
#         t = (3.1 - 1)/1.70 = 1.235
#         Reject if |t| > 1.99
#         Accept null hypothesis
#   b)
#     iii. p ~ .035735 SMALLEST (also only one to reject null)

#   c) b2 +- t * SE
#     3.1 +- 1.99 * 1.7
#     3.1 +- 3.383
#     [-0.283, 6.483]
