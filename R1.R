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
library(MASS)
library(dplyr)
data <- c(42.52, 43.83, 37.14, 43.22, 40.30, 34.11, 42.88, 45.37, 43.33, 43.96,
          40.94, 37.20, 40.70, 42.83, 48.36, 38.74, 42.25, 41.53, 43.96, 50.99,
          43.65, 48.21, 38.22, 43.20, 31.90, 45.63, 41.55, 49.03, 39.69, 42.02)
data_table <- table(data)
data_table <- data_table %>% transform(Rel_Freq = prop.table(Freq), Cum_Freq = cumsum(Freq))

write.csv(data_table, file = "~/Desktop/data.csv")
#TODO: import photo


# E. Probability Theor
