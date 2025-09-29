# Set the working directory.

setwd("C:/BMGF2025")

# Read the data into R from a CSV file.

spiders <- read.csv("Arachnophobia.csv",
                    header = TRUE,
                    sep = ",")

# Browse the data.

str(spiders)

View(spiders)

# Let y be the GSR measurement and let x be the size.

y <- spiders$GSR

x <- spiders$Size

# Calculate 5-number summaries and means for y and x.

summary(y)

summary(x)

# Draw box plots for the GSR measurements and for 
# the sizes of the spiders.

boxplot(y, 
        horizontal = TRUE,
        main = "Box plot for the GSR measurements",
        xlab = "GSR measurement",
        border = "firebrick",
        col = "orange")

boxplot(x, 
        horizontal = TRUE,
        main = "Box plot for the sizes of the spiders",
        xlab = "Size (cm)",
        border = "navy",
        col = "cornflowerblue")

# Draw a scatterplot of the GSR measurements against
# the sizes of the spiders.

plot(x, y,
     main = "Scatterplot of the GSR measurements against the sizes of the spiders",
     xlab = "Size (cm)", 
     ylab = "GSR measurement",
     pch = 19,
     cex = 2,
     col = "coral")

# Calculate the mean and the median size of the spiders.

mean(x)

median(x)

# Calculate the standard deviation and the interquartile
# range for the sizes of the spiders.

sd(x)

IQR(x)

# Calculate the mean and the median GSR measurement.

mean(y)

median(y)

# Calculate the standard deviation and the interquartile
# range for the GSR measurements.

sd(y)

IQR(y)

# Test whether the mean spider size is significantly
# different from 10 cm.

t.test(x,
       alternative = "two.sided",
       mu = 10)

# Test whether the mean GSR measurement is significantly
# more than 25.

t.test(y,
       alternative = "greater",
       mu = 25)

# Calculate the correlation coefficient between the
# GSR measurements and the sizes of the spiders.

r <- cor(y, x)

# Fit a simple linear regression model in which the GSR
# measurements are explained by the sizes of the spiders.

lrm <- lm(y ~ x, data = spiders)

summary(lrm)

# Draw again a scatterplot of the GSR measurements
# against the sizes of the spiders. Add the fitted
# regression line to the graph. 

plot(x, y,
     main = "Scatterplot of the GSR measurements against the sizes of the spiders",
     xlab = "Size (cm)", 
     ylab = "GSR measurement",
     pch = 19,
     cex = 2,
     col = "coral")
abline(lrm,
       lwd = 2,
       col = "blueviolet")

# Predict the GSR measurement for Nosnow Cannotski
# who had to interact with a spider of 13 cm and
# calculate the corresponding residual. 

# Observed x values:

x

# Observed y values:

y

# Predicted y values:

lrm$fitted.values

# Residuals:

lrm$residuals

# For Nosnow Cannotski:

x[5]

y[5]

lrm$fitted.values[5]

lrm$residuals[5]

plot(x, y,
     main = "Scatterplot of the GSR measurements against the sizes of the spiders",
     xlab = "Size (cm)", 
     ylab = "GSR measurement",
     pch = 19,
     cex = 2,
     col = "coral")
abline(lrm,
       lwd = 2,
       col = "blueviolet")
points(x[5],
       lrm$fitted.values[5],
       pch = 15,
       cex = 2,
       col = "black")