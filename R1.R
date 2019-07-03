# A. Conditional Statements and Loops – Using R
a <- c(1:8)
b <- c("meep", "foo", "meep", "mod", "foo", "mod", "meep", "foo")
d <- cbind(a, b)
for(i in 1:nrow(d)) {
  if(d[i,2] == "meep"){
    print("oops")
  } else if (d[i,2] == "foo") {
    print("ooh")
  } else {
    print("yay")
  }
}

# B. Data Management – Using R
Manager <- function(n){
  
}

# C. User Defined Functions – Using R
#    4)
x <- c(1, 3, 5, 7, 10)
y <- c(1, 5, 10, 12, 14)
setdiff(x, y)
# One could alternatively loop through x, checking if val is not contained in y
x[!is.element(x,y)]

#    5)
#     a)
data_try <- data.frame(salary = c(20000,30000,50000,40000,60000,NA,-
                                       4000,25000),
                          age = c(21,23,27,25,26,26,23,NA) )

impute_data <- function(column){
  replace(column, which(column < 0 | is.na(column)), median(column, na.rm=TRUE))

}
#     b)
data_try <- apply(data_try, 2, impute_data)

# D. Descriptive Statistics
 # 6)
  # a)
library(MASS)
library(dplyr)
data <- c(42.52, 43.83, 37.14, 43.22, 40.30, 34.11, 42.88, 45.37, 43.33, 43.96,
          40.94, 37.20, 40.70, 42.83, 48.36, 38.74, 42.25, 41.53, 43.96, 50.99,
          43.65, 48.21, 38.22, 43.20, 31.90, 45.63, 41.55, 49.03, 39.69, 42.02)
data_range <- range(data) 
breaks <- seq(floor(data_range[1]), ceiling(data_range[2]), by=1) 
interval <- cut(data, breaks, right=FALSE)
data_table <- table(interval)
data_table <- data_table %>% transform(Rel_Freq = prop.table(Freq), Per_Freq = prop.table(Freq) * 100, Cum_Freq = cumsum(Freq))
write.csv(data_table, file = "~/Desktop/data2.csv")
  # b)
  #TODO: import photo
  # c)
  #    The histogram appears to indicate a symmetric, bell-shaped distribution, as the majority
  #    of values fall between the 40-45 range, although there is a slightly longer left tail, 
  #    causing somewhat of a left skew.
  # d)
  #    While the data is somewhat close to a symmetric distribution and calculating mean would be a reasonable choice,
  #    the median is likely a bit more accuarte, due to the presence of more left outliers than right.
  # e)
data_sd <- sd(data)
data_iqr <- IQR(data)
  # f)
q1 <- quantile(data, .25)
q2 <- quantile(data, .75)
top_cutoff <- q2 + data_iqr*1.5
bottom_cutoff <- q1 - data_iqr*1.5
outliers <- data[(data > top_cutoff | data < bottom_cutoff)]
#   g)
#    One could use a square/cube root transformation or add some constant and use a logarithmic transformation.
# 7)
values <- c(42.52,40.94,NA,39.69,43.83,37.2,NA,42.02,37.14,40.7,43.65,43.22,42.83,48.21,40.3,48.36,38.22,34.11,
            38.74,43.2,42.88,42.25,31.9,45.37,41.53,45.63,43.33,43.96,41.55,43.96,50.99,49.0)
values <- na.omit(values)
values_range <- range(values) 
value_breaks <- seq(floor(values_range[1]), ceiling(values_range[2]), by=1) 
value_intervals <- cut(values, value_breaks, right=FALSE)
value_table <- table(value_intervals)
value_table <- value_table %>% transform(Rel_Freq = prop.table(Freq), Cum_Freq = cumsum(Freq))

# MIDPOINTS FUNCTION CODE FROM https://www.r-bloggers.com/finding-the-midpoint-when-creating-intervals/
midpoints <- function(x, dp=2){
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(round(lower+(upper-lower)/2, dp))
}
value_table$Midpoint <- sort(midpoints(value_table$value_intervals))
value_table <- value_table[,c(1, 5, 2, 3, 4)]
colnames(value_table) <- c("Interval", "Midpoint", "Absolute Frequency", "Relative Frequency", "Cumulative Frequency")

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
# Unsure how to run completely in R, struggling to find information online
# Total umber of outcomes of sample space: (10 choose 3) 
possible_outcomes <- choose(10, 3)
# Each outcome in sample space holds an equally likely probability of being filled
# 8 outcomes where there are 3 consecutive empty spots
8 / possible_outcomes



# F. Sampling Theory
library("PracTools")
output <- strAlloc(n.tot = 36, Nh = c(10000,10000), Sh = c(10.27, 6.67), alloc = "neyman")
num_boys <- output$nh[1]

# G. Statistical Distributions
#   a)
#     i)
pnorm(25, mean = 25.8, sd = .5, lower.tail = TRUE)
#     ii)
pnorm(26.5, mean = 25.8, sd = .5, lower.tail=TRUE) - 
  pnorm(25.5, mean = 25.8, sd = .5, lower.tail=TRUE)

# H. Plots/Charts – Using R
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

#  I)
#     a. null, Ho: There is no significant difference between the mean Verbal SAT scores of first year
#              psychology majors compared to the population
#        alternative, Ha: There is a signifcant difference between the mean Verbal SAT scores of first year
#              psychology majors compared to the population
#     b. Reject null if p-value < .05
#     c. μ = 520, σ^2 = σ * σ = 95*95 = 9025, M = 548, N = 36
#        Z = (M - μ) / √(σ^2 / N)
#        Z = (548 - 520) / √(9025 / 36)
#        Z = 28 / 15.83333
#        Z = 1.76842  => α(.05) =>  p = .07672
#     d. p = .07672 > .05, fail to reject Ho
#     e. There is no significant difference between the mean score and the scores of first year psych majors
