##    W203-3 Lab3       ##
##    Juan Jose Carin   ##


## @knitr Part1-5
   # A simulation to prove my answer
beer <- as.data.frame(matrix(0, 100, 1e4))
plaid <- beer
beer <- apply(beer, 2, function(x){rbinom(100,1,.6)})
plaid <- apply(plaid, 2, function(x){rbinom(100,1,.7)})
both <- apply(beer*plaid, 2, sum)
mean(both)


## @knitr Libraries
library(psych)
library(pastecs)
library(ggplot2)
library(car)
library(gmodels)
library(compute.es)


## @knitr DataImport
load("GSS.Rdata")

## @knitr Descriptive
   # Not used in the Report
dim(GSS)
head(GSS)
summary(GSS)
colnames(GSS)


## @knitr Part2-1intro
   # Data cleansing and recoding
levels(GSS$income91)
levels(GSS$visitart)
GSS$income91[GSS$income91 == "NAP" | GSS$income91 == "Refused" | 
                GSS$income91 == "DK" | GSS$income91 == "NA"] <- NA
GSS$visitart[GSS$visitart == "NAP" | GSS$visitart == "DK" | 
                GSS$visitart == "NA"] <- NA
GSS$income91 <- factor(GSS$income91)
GSS$visitart <- factor(GSS$visitart)
GSS$visitart[is.na(GSS$income91)] <- NA
GSS$income91[is.na(GSS$visitart)] <- NA
income91 <- subset(GSS[!is.na(GSS$income91), ], sex == "Male", 
                   select = "income91")[, 1]
visitart <- subset(GSS[!is.na(GSS$visitart), ], sex == "Male", 
                   select = "visitart")[, 1]

## @knitr Part2-1
CrossTable(income91, visitart, fisher = FALSE, chisq = FALSE, expected = FALSE, 
           prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE, sresid = FALSE, 
           format = "SPSS")

## @knitr Part2-2
line <- ggplot(GSS[GSS$country != "NA", ], aes(country, age))
line + stat_summary(fun.y = mean, geom = "line", size = 1, aes(group=1), 
                    colour = "#FF6633") + 
   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, 
                size = 0.75, colour = "#990000") +  
   stat_summary(fun.y = mean, geom = "point", size = 4, colour = "#FF6633") + 
   labs(x = "Opinion of Country Music", y = "Age") + 
   theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))

## @knitr Part2-3
GSS$sibs[GSS$sibs == 98 | GSS$sibs == 99] <- NA
GSS$catholic <- GSS$relig == "Catholic"
GSS$catholic <- factor(GSS$catholic, levels = c(TRUE, FALSE), 
                       labels = c("Catholic", "Non-catholic"))

by(GSS$sibs, GSS[, c("sex", "catholic")], shapiro.test)
leveneTest(GSS$sibs[GSS$sex == "Male"], GSS$catholic[GSS$sex == "Male"])
leveneTest(GSS$sibs[GSS$sex == "Female"], GSS$catholic[GSS$sex == "Female"])

bar <- ggplot(GSS, aes(catholic, sibs))
bar + stat_summary(fun.y = mean, geom = "bar", position = "dodge", 
                   fill = "White", colour = "Black") + 
   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, 
                position = position_dodge(width = 0.90), width = 0.2) +  
   labs(x = "Religion", y = "Siblings") + facet_wrap( ~ sex) + 
   theme(legend.position = "none")

# t.test(GSS$sibs[GSS$catholic == "Catholic" & GSS$sex == "Male"], 
#        GSS$sibs[GSS$catholic == "Non-catholic" & GSS$sex == "Male"],
#        alternative = "greater")
# t.test(GSS$sibs[GSS$catholic == "Catholic" & GSS$sex == "Female"], 
#        GSS$sibs[GSS$catholic == "Non-catholic" & GSS$sex == "Female"],
#        alternative = "greater")

## @knitr Part2-4
GSS$educ[GSS$educ == 98 | GSS$educ == 99] <- NA
GSS$tvhours[GSS$tvhours == 98 | GSS$tvhours == 99] <- NA
scatter <- ggplot(GSS, aes(tvhours, educ))
scatter + geom_point() + labs(x = "TV hours", y = "Years of education") + 
   geom_smooth(method = "lm")

## @knitr Part2-5
# levels(GSS$politics)
# levels(GSS$news)
GSS$educ[GSS$educ == 98 | GSS$educ == 99] <- NA


## @knitr Part3-1-intro1
   # Descriptive statistics, data cleansing and re-coding
summary(GSS$marital)
summary(GSS$politics)
Part3.1 <- GSS[GSS$marital != "NA" & !is.na(GSS$politics), c("marital", "politics")]
Part3.1$marital <- factor(Part3.1$marital)
summary(Part3.1)

## @knitr Part3-1-intro2
   # Chi-square test
table(Part3.1$marital, Part3.1$politics)
(cs = chisq.test(Part3.1$marital, Part3.1$politics))
CrossTable(Part3.1$marital, Part3.1$politics, fisher = FALSE, chisq = TRUE,
           expected = TRUE, asresid = TRUE, prop.t = FALSE, format = "SPSS")

## @knitr Part3-1-B1
cs

## @knitr Part3-1-B2
cs$expected

## @knitr Part3-1-C
   # Cramer's V
cramers_v = function(cs) {
   cv = sqrt(cs$statistic / (sum(cs$observed) * (min(dim(cs$observed))-1)))
   print.noquote(paste("Cramer's V:", round(as.numeric(cv), 4)))
}

cramers_v(cs)


## @knitr Part3-2-intro1
   # Descriptive statistics, data cleansing and re-coding
GSS$agewed[GSS$agewed == 0 | GSS$agewed == 98 | GSS$agewed == 99] <- NA
GSS$tvhours[GSS$tvhours == 98 | GSS$tvhours == 99] <- NA
Part3.2 <- GSS[!is.na(GSS$agewed) & !is.na(GSS$tvhours), c("agewed", "tvhours")]
summary(Part3.2)

## @knitr Part3-2-intro2
   # Plots and assumptions
shapiro.test(Part3.2$agewed)
shapiro.test(Part3.2$tvhours)

scatter <- ggplot(Part3.2, aes(agewed, tvhours))
scatter + geom_point() + labs(x = "Age when married", y = "TV hours") + 
   geom_smooth(method = "lm")

## @knitr Part3-2-intro3
   # Pearson correlation
(correlation <- cor.test(Part3.2$agewed, Part3.2$tvhours))

## @knitr Part3-2-B
correlation$statistic
correlation$p.value


## @knitr Part3-3-intro
summary(GSS$marital)
GSS$married <- factor(ifelse(GSS$marital == "married", 1, 0))
(Part3.3 <- subset(GSS, age == 23, select = c("childs", "married")))

## @knitr Part3-3-A
(married23_mean <- sum(Part3.3$married==1)/length(Part3.3$married))

## @knitr Part3-3-C
table(Part3.3$childs, Part3.3$married)
(w <- wilcox.test(childs ~ married, Part3.3))

## @knitr Part3-3-D
rFromWilcox <- function(wilcoxModel, N){
   z <- qnorm(wilcoxModel$p.value/2)
   r <- z/sqrt(N)
   cat(wilcoxModel$data.name, "Effect size, r = ", r)
}
rFromWilcox(w, length(Part3.3$married))


## @knitr Part3-4-intro1
summary(GSS$relig)
GSS$relig[GSS$relig == "NA" | GSS$relig == "DK"] <- NA
GSS$relig <- factor(GSS$relig)
Part3.4 <- GSS[!is.na(GSS$relig) & !is.na(GSS$agewed), c("relig", "agewed")]

## @knitr Part3-4-intro2
by(Part3.4$agewed, Part3.4$relig, shapiro.test)
leveneTest(Part3.4$agewed, Part3.4$relig)
bar <- ggplot(Part3.4, aes(relig, agewed))
bar + stat_summary(fun.y = mean, geom = "bar", position = "dodge", 
                   fill = "White", colour = "Black") + 
   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, 
                position = position_dodge(width = 0.90), width = 0.2) +  
   labs(x = "Religion", y = "Age when married")

## @knitr Part3-4-intro3
aovm <- aov(agewed ~ relig, Part3.4)
summary(aovm)

## @knitr Part3-4-C-1
sig_stars = function(p){
   stars = symnum(p, na = F, cutpoints = c(0, .001, .01, .05, .1, 1), 
                  symbols=c("***","**", "*", ".", " "))
   return( paste(round(p, 3), stars) )
}

tt_bonferroni <- pairwise.t.test(Part3.4$agewed, Part3.4$relig, 
                                p.adjust.method = "bonferroni")
t_table_bonferroni <- apply(tt_bonferroni$p.value, c(1,2), sig_stars)
t_table_bonferroni <- noquote(t_table_bonferroni)
t_table_bonferroni[upper.tri(t_table_bonferroni)] <- ""
t_table_bonferroni

## @knitr Part3-4-C-2
tt_holm <- pairwise.t.test(Part3.4$agewed, Part3.4$relig, p.adjust.method = "holm")
t_table_holm <- apply(tt_holm$p.value, c(1,2), sig_stars)
t_table_holm <- noquote(t_table_holm)
t_table_holm[upper.tri(t_table_holm)] <- ""
t_table_holm

## @knitr Part3-4-C-3
tt_BH <- pairwise.t.test(Part3.4$agewed, Part3.4$relig, p.adjust.method = "BH")
t_table_BH <- apply(tt_BH$p.value, c(1,2), sig_stars)
t_table_BH <- noquote(t_table_BH)
t_table_BH[upper.tri(t_table_BH)] <- ""
summary(t_table_BH)

## @knitr Part3-4-D-1
   # Definition and use of omega-squared function (also calculates R-squared)
      # to estimate the effect size of ANOVA analysis
omega <- function(aov.summary){
   dfm <- aov.summary[[1]][1,1]
   SSm <- aov.summary[[1]][1,2]
   SSr <- aov.summary[[1]][2,2]
   SSt <- SSm + SSr
   MSr <- aov.summary[[1]][2,3]
   omega2 <- (SSm-(dfm*MSr))/(SSt+MSr)
   print.noquote(paste("Omega-squared:", round(as.numeric(omega2), 2)))
   R2 <- SSm/SSt
   print.noquote(paste("R-squared:", round(as.numeric(R2), 2)))
}

omega(summary(aovm))

## @knitr Part3-4-D-2
   # Using mes function (part of calculate.es package) to estimate "d" effect
      # size of the post-hoc tests that were statistically significant
mes(mean(Part3.4$agewed[Part3.4$relig == "Protestant"]), 
    mean(Part3.4$agewed[Part3.4$relig == "Catholic"]), 
    sd(Part3.4$agewed[Part3.4$relig == "Protestant"]), 
    sd(Part3.4$agewed[Part3.4$relig == "Catholic"]), 
    length(Part3.4$agewed[Part3.4$relig == "Protestant"]), 
    length(Part3.4$agewed[Part3.4$relig == "Catholic"]))

mes(mean(Part3.4$agewed[Part3.4$relig == "Protestant"]), 
    mean(Part3.4$agewed[Part3.4$relig == "Jewish"]), 
    sd(Part3.4$agewed[Part3.4$relig == "Protestant"]), 
    sd(Part3.4$agewed[Part3.4$relig == "Jewish"]), 
    length(Part3.4$agewed[Part3.4$relig == "Protestant"]), 
    length(Part3.4$agewed[Part3.4$relig == "Jewish"]))

mes(mean(Part3.4$agewed[Part3.4$relig == "Protestant"]), 
    mean(Part3.4$agewed[Part3.4$relig == "Other"]), 
    sd(Part3.4$agewed[Part3.4$relig == "Other"]), 
    sd(Part3.4$agewed[Part3.4$relig == "Other"]), 
    length(Part3.4$agewed[Part3.4$relig == "Protestant"]), 
    length(Part3.4$agewed[Part3.4$relig == "Other"]))

