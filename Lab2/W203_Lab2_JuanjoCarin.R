##    W203-3 Lab2       ##
##    Juan Jose Carin   ##


## @knitr Libraries
library(psych)
library(pastecs)
library(ggplot2)
library(car)


## @knitr 1-DataImport
   # Import data
load("GSS.Rdata")

## @knitr 1-Descriptive
   # Not used in the Report
dim(GSS)
head(GSS)
summary(GSS)

## @knitr 1a-Examine1
   # Descriptive statistics and histogram
summary(GSS$agewed)
round(stat.desc(GSS$agewed, desc = F), digits = 2)
agewed.plots <- ggplot(GSS, aes(agewed))
agewed.plots + geom_histogram(colour = "black", fill = "white") +
   labs(x = "Age when married", y = "Number of cases")

## @knitr 1a-Examine2
   # Box-plot
agewed.plots + geom_boxplot(aes(y = agewed)) + 
   labs(x = "", y = "Age when married")  + scale_x_discrete(breaks = NULL)

## @knitr 1ai-agewed0
   # Check number of cases of agewed=0 and associated marital status
sum(GSS$agewed == 0)
table(GSS$marital, GSS$agewed==0)[, 2]

## @knitr 1ai-agewed99
   # Check number of cases of agewed>90, the exact value for all those cases
   # and associated marital status
sum(GSS$agewed > 90)
GSS[GSS$agewed > 90, "agewed"]
table(GSS$marital, GSS$agewed == 99)[, 2]

## @knitr 1b-Recode
GSS$agewed[GSS$agewed == 0 | GSS$agewed == 99] <- NA

## @knitr 1b-Histogram
   # New histogram of agewed density (plus normal density curve)
agewed.plots2 <- ggplot(GSS, aes(agewed))
agewed.plots2 + 
   geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
   labs(x = "Age when married", y = "Density of cases") + 
   stat_function(fun = dnorm, 
                 args = list (mean = mean(GSS$agewed, na.rm = TRUE), 
                              sd = sd(GSS$agewed, na.rm = TRUE)), 
                 colour="red", size=1)

## @knitr 1b-Descriptive
   # Not used in the Report
   # The values are mentioned in the report using the corresponding formulas
summary(GSS$agewed)
round(stat.desc(GSS$agewed, desc = F), digits = 2)

## @knitr 1bi-Mean
   # New (and valid) mean
agewed.mean <- mean(GSS$agewed, na.rm = TRUE)
print(paste("The mean of the agewed variable is", 
            format(agewed.mean, digits=4), "years"), sep=" ")


## @knitr 2a-QQplot
agewed.plots3 <- qplot(sample = GSS$agewed[!is.na(GSS$agewed)], stat = "qq")
agewed.plots3 + stat_qq(distribution = qnorm, size = .25) + 
   labs(x = "Theoretical quantiles", y = "Sample quantiles")

## @knitr 2a-QQplot-standardized
   # QQplot of the standardized variable (plus the straight line of
   # theoretical standard normal distribution)
agewed.plots4 <- qplot(sample = scale(GSS$agewed[!is.na(GSS$agewed)]), 
                       stat = "qq")
agewed.plots4 + geom_abline(colour = "red", size = .5) + 
   stat_qq(distribution = qnorm, size = .25) + 
   labs(x = "Theoretical quantiles", y = "(Standardized) Sample quantiles") + 
   coord_fixed(ratio = 1) +
   scale_y_continuous(breaks = seq(-2, 6, 2))

## @knitr 2b-Shapiro-WilkTest
shapiro.test(GSS$agewed)
shapiro.test(GSS$agewed)$p.value

## @knitr 2b-Skew&Kurtosis
stat.desc(GSS$agewed, basic = FALSE, desc = FALSE, norm = TRUE)

## @knitr 2c-Variances
by(GSS$agewed, GSS$sex, var, na.rm = TRUE)

## @knitr 2c-Variances-hist
agewed.plots5 <- ggplot(GSS, aes(agewed))
agewed.plots5 + 
   geom_histogram(colour = "black", fill = "white") +
   labs(x = "Age when married", y = "Number of cases") + 
   facet_grid( ~ sex)

## @knitr 2c-Variances-more
   # Not used in the Report
   # Comparison of means and variances (total and by sex)
mean(GSS$agewed, na.rm = TRUE)
by(GSS$agewed, GSS$sex, mean, na.rm = TRUE)
   # As expected, the total mean lies inbetween
var(GSS$agewed, na.rm = TRUE)
by(GSS$agewed, GSS$sex, var, na.rm = TRUE)
   # As expected, the total variance is greater than by sex
   # Number and frequency of males and females
cbind(Freq = table(GSS$sex), Cum = cumsum(table(GSS$sex)), 
Rel = round(100*prop.table(table(GSS$sex)), 1))
   # Number and frequency of males and females considering only when agewed is
   # known
cbind(Freq = table(GSS[!is.na(GSS$agewed), "sex"]), 
      Cum = cumsum(table(GSS[!is.na(GSS$agewed), "sex"])), 
      Rel = round(100*prop.table(table(GSS[!is.na(GSS$agewed), "sex"])), 1))

## @knitr 2d-LeveneTest
leveneTest(GSS$agewed, GSS$sex)


## @knitr 3a-zTest
mu0 <- 23
SD <- 5
N <- sum(!is.na(GSS$agewed))
SE <- SD/sqrt(N)
sample.mean <- mean(GSS$agewed, na.rm = TRUE)
z <- (sample.mean - mu0) / SE
z
p.value <- (1 - pnorm(abs(z)))*2
p.value

## @knitr 3a-zTest-installBSDA
install.packages("BSDA", repos = "http://cran.cnr.Berkeley.edu")

## @knitr 3a-zTest2
library(BSDA)
z.test(GSS$agewed[!is.na(GSS$agewed)], alternative = "two.sided", mu = mu0, 
       sigma.x = SD, conf.level = 0.95)
