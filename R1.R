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
