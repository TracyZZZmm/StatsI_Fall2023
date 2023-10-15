# Problem Set 2
# (a)Calculate the χ2 test statistic by hand/manually (even better if you can do ”by hand” in R).
# H0 = officers may not demand bribes from drivers, depending on their class
# H1 = officers may demand bribes from drivers, depending on their class
data <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)
data
#       [,1] [,2] [,3]
# [1,]   14    6    7
# [2,]    7    7    1
row_sums <- rowSums(data) # 27 15
col_sums <- colSums(data) # 21 13  8
total_sum <- sum(data) # 42

expected <- matrix(0, nrow = 2, ncol = 3)
for (i in 1:2) {
  for (j in 1:3) {
    expected[i, j] <- (row_sums[i] * col_sums[j]) / total_sum
  }
}
expected
#             Not stopped  Bribe requested Stopped/given warning
# Upper class     13.5        8.357143           5.142857
# Lower class       7.5        4.642857           2.857143
\chi^2_test_statistic <- sum((data - expected)^2 / expected)
\chi^2_test_statistic # 3.791168




# (b) Now calculate the p-value from the test statistic you just created (in R).
df <- (nrow(data) - 1) * (ncol(data) - 1)
df # 2
pchisq(χ2_test_statistic, df = 2, lower.tail = FALSE)
p_value # 0.1502306
p_value > α # 0.1502306 > 0.1
# we are failed to reject the null hypothesis, officers may not demand bribes from drivers, depending on their class.


# 2 What do you conclude if α = 0.1?
# calculate Quantile for the Chi-Squared Distribution
qchisq(0.1, df = 2, lower.tail = FALSE)
# 4.60517 > 3.791168, we are failed to reject the H0.




# (c) Calculate the standardized residuals for each cell and put them in the table below.
expected_1 <- 13.5
expected_2 <- 8.357143
expected_3 <- 5.142857
expected_4 <- 7.5
expected_5 <- 4.642857
expectedz_6 <- 2.857143
SR_1 <- (14-13.5)/sqrt(13.5*(1-27/42)*(1-21/42))
SR_2 <- (6-8.357143)/sqrt(8.357143*(1-27/42)*(1-13/42))
SR_3 <- (7-5.142857)/sqrt(5.142857*(1-27/42)*(1-8/42))
SR_4 <- (7-7.5)/sqrt(7.5*(1-15/42)*(1-21/42))
SR_5 <- (7-4.642857)/sqrt(4.642857*(1-15/42)*(1-13/42))
SR_6 <- (1-2.857143)/sqrt(2.857143*(1-15/42)*(1-8/42))
SR_1 # 0.3220306
SR_2 # -1.641957
SR_3 # 1.523026
SR_4 # -0.3220306
SR_5 # 1.641957
SR_6 # -1.523026
SR_table <- matrix(c(SR_1, SR_2, SR_3, SR_4, SR_5, SR_6), nrow = 2, byrow = TRUE)
standardized_residuals_table
rownames(SR_table) <- c("Upper class", "Lower class")
colnames(SR_table) <- c("Not stopped", "Bribe requested", "Stopped/given warning")
SR_table
#             Not stopped    Bribe requested    Stopped/given warning
# Upper class   0.3220306       -1.641957              1.523026
# Lower class  -0.3220306        1.641957             -1.523026



#(d) How might the standardized residuals help you interpret the results?
# The maximum absolute value of standardized residuals is less than 1.96(95%), 
# So we are failed to reject the null hypothesis,
# officers may not demand bribes from drivers, depending on their class.




# Question 2: Economics
# (a) State a null and alternative (two-tailed) hypothesis.
install.packages("readr") 
library(readr)
url <- "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"
data <- read_csv(url)
H0 = the reservation policy has no effect on the number of new or repaired drinking water facilities in the villages.
H1 = the reservation policy has an effect on the number of new or repaired drinking water facilities in the villages.

# qnorm(c(0.025, 0.975)) # critical value [-1.959964  1.959964]
t.test(data$water ~ data$reserved, mu = 0, confint = 0.95, alternative = "two.sided")
# t = -1.8141, t-value is inside of the critical value, we are falied reject the null hypothesis.

# (b) Run a bivariate regression to test this hypothesis in R (include your code!).
model <- lm(water ~ reserved, data=data)
summary(model)
p_value <- summary(model)$coefficients["reserved", "Pr(>|t|)"]
p_value
plot(data$reserved, data$water, main = "Scatterplot of water vs. reserved", 
     xlab = "Reserved", ylab = "Water")


# (c) Interpret the coefficient estimate for reservation policy.
table (data$reserved)
#   0   1 
#  214 108
# reserved: binary variable ( 0 and 1 )
coefficients <- coef(model)
print(coefficients)
# (Intercept)    reserved 
# 14.738318    9.252423 
# yi = Intercept + slope * xi
# water_i = 14.738318 + 9.252423 * reserved_i

water_0 <- 14.738318 + 9.252423 * 0  # 14.73832
water_1 <- 14.738318 + 9.252423 * 1  # 23.99074
# the coefficient estimate is 14.73832, it means the GP was reserved for women leaders.
# the coefficient estimate is 23.99074, it means the GP was not reserved for women leaders.



