##    W203-3 FINAL      ##
##    Juan Jose Carin   ##


## @knitr Part1-8
   # A simulation to prove my answer
false_positives <- rep(0, 100)
for(i in 1:100){
   x <- data.frame(matrix(NA, nrow = 50, ncol = 100))
   y <- x
   x <- apply(x, 2, function(x){rnorm(50, 180, 5)})
   y <- apply(x, 2, function(x){rnorm(50, 170, 5)})
   xydif <- x - y
   xydif_avg <- apply(xydif, 2, mean)
   xydif_prob <- pnorm(xydif_avg, 10, 1)
   false_positives[i] <- length(xydif_prob[abs(xydif_prob) > .975])
}
false_positives
length(false_positives[false_positives > 5])


## @knitr Libraries
library(psych)
library(pastecs)
library(ggplot2)
library(car)
library(gmodels)
library(compute.es)
library(mlogit)
library(WRS)
library(lmtest)
library(sandwich)
library(QuantPsyc)


## @knitr DataImport
Dating <- read.csv("Dating.csv", header = TRUE)

## @knitr Descriptive
   # Not used in the Report
dim(Dating)
head(Dating)
summary(Dating)
colnames(Dating)


## @knitr Part2-1-intro
summary(Dating$marital_status)
summary(Dating$use_reddit)

## @knitr Part2-1
Dating_sample1 <- Dating[(Dating$use_reddit == "Yes" | 
                             Dating$use_reddit == "No") & 
                            Dating$marital_status != "Refused",
                         c("marital_status", "use_reddit")]
CrossTable(Dating_sample1$marital_status, Dating_sample1$use_reddit, 
           fisher = FALSE, chisq = TRUE, expected = TRUE, prop.c = TRUE, 
           prop.t = TRUE, prop.chisq = FALSE, sresid = FALSE, format = "SPSS")


## @knitr Part2-2-intro
summary(Dating$region)
summary(Dating$life_quality)

## @knitr Part2-2
Dating_sample2 <- Dating[Dating$life_quality != "Don't know" & 
                            Dating$life_quality != "Refused", 
                         c("life_quality", "region")]
Dating_sample2$life_quality <- as.integer(factor(Dating_sample2$life_quality))

Dating_sample2 <- mlogit.data(Dating_sample2, choice = "region", shape = "wide")
model1 <- mlogit(region ~ 1 | life_quality, Dating_sample2)
summary(model1)
data.frame(exp(model1$coefficients))
exp(confint(model1))


## @knitr Part2-3-intro
summary(Dating$flirted_online)
summaryclass(Dating$years_in_relationship)

## @knitr Part2-3
Dating_sample3 <- Dating[Dating$flirted_online == "Yes" | 
                             Dating$flirted_online == "No", 
                          c("flirted_online", "years_in_relationship")]
Dating_sample3$flirted_online <- factor(Dating_sample3$flirted_online)
Dating_sample3$years_in_relationship <- 
   as.numeric(as.character(Dating_sample3$years_in_relationship))
Dating_sample3 <- Dating_sample3[!is.na(Dating_sample3$years_in_relationship), ]

by(Dating_sample3$years_in_relationship, Dating_sample3[, "flirted_online"], 
   shapiro.test)
leveneTest(Dating_sample3$years_in_relationship, Dating_sample3$flirted_online)

# t.test(years_in_relationship ~ flirted_online, Dating_sample3)

model <- lm(years_in_relationship ~ flirted_online, Dating_sample3)
summary(model)

bar <- ggplot(Dating_sample3, aes(flirted_online, years_in_relationship))
bar + stat_summary(fun.y = mean, geom = "bar", position = "dodge", 
                   fill = "White", colour = "Black") + 
   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, 
                position = position_dodge(width = 0.90), width = 0.2) +  
   labs(x = "Flirted Online", y = "Years in Relationship") + 
   theme(legend.position = "none", axis.title.x = element_text(size = 10), 
         axis.title.y = element_text(size = 10))


## @knitr Part2-4-intro
summary(Dating$lgbt)
summary(Dating$adults_in_household)

## @knitr Part2-4
Dating_sample4 <- Dating[(Dating$lgbt != "Don't know" & 
                             Dating$lgbt != "Refused") & 
                            (Dating$adults_in_household != "Don't know" & 
                                Dating$adults_in_household != "Refused"), 
                         c("lgbt", "adults_in_household")]
Dating_sample4$lgbt <- factor(Dating_sample4$lgbt)
Dating_sample4$lgbt <- factor(Dating_sample4$lgbt, 
                              levels = levels(Dating_sample4$lgbt)[c(1,2,4,3)])
Dating_sample4$adults_in_household <- factor(Dating_sample4$adults_in_household)
Dating_sample4$adults_in_household <- 
   as.numeric(Dating_sample4$adults_in_household)

by(Dating_sample4$adults_in_household, Dating_sample4$lgbt, mean)

# CrossTable(Dating_sample4$lgbt, Dating_sample4$adults_in_household, 
#            fisher = FALSE, chisq = TRUE, expected = TRUE, prop.c = TRUE, 
#            prop.t = TRUE, prop.chisq = FALSE, sresid = FALSE, format = "SPSS")

model2 <- aov(adults_in_household ~ lgbt, Dating_sample4)
summary(model2)

model3 <- pairwise.t.test(Dating_sample4$adults_in_household, 
                          Dating_sample4$lgbt, paired = FALSE, 
                          p.adjust.method = "bonferroni")
model3

by(Dating_sample4$adults_in_household, Dating_sample4[, "lgbt"], 
   shapiro.test)
leveneTest(Dating_sample4$adults_in_household, Dating_sample4$lgbt, 
           center = median)

Dating_sample5 <- unstack(Dating_sample4, adults_in_household ~ lgbt)
mcppb20(Dating_sample5, tr = .2, nboot = 2000)

line <- ggplot(Dating_sample4, aes(lgbt, adults_in_household))
line + stat_summary(fun.y = mean, geom = "line", size = 1, aes(group=1), 
                    colour = "#FF6633") + 
   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, 
                size = 0.75, colour = "#990000") +  
   stat_summary(fun.y = mean, geom = "point", size = 4, colour = "#FF6633") + 
   labs(x = "Sexual orientation", 
        y = "Mean of Adults in the househod") + 
   theme(axis.title.x = element_text(size = 10), 
         axis.title.y = element_text(size = 10))


## @knitr Part2-5-intro
summary(Dating$age)
summary(Dating$children0_5)
summary(Dating$children6_11)
summary(Dating$children12_17)

table(Dating$children0_5)
table(Dating$children6_11)
table(Dating$children12_17)


## @knitr Part2-5
Dating_sample6 <- Dating[Dating$age != 99 & Dating$children0_5 != 99 & 
                            Dating$children6_11 != 98 & 
                            Dating$children6_11 != 99 & 
                            Dating$children12_17 != 98 & 
                            Dating$children12_17 != 98, 
                         c("age", "children0_5", "children6_11", 
                           "children12_17")]
Dating_sample6 <- Dating_sample6[complete.cases(Dating_sample6), ]
Dating_sample6$children0_17 <- apply(Dating_sample6[, c(2:4)], 1, sum)
Dating_sample6 <- Dating_sample6[, c(1,5)]

cor.test(Dating_sample6$age, Dating_sample6$children0_17)

scatter <- ggplot(Dating_sample6, aes(age, children0_17))
scatter + geom_hex() + 
   geom_smooth(method = "lm", colour = "Red", alpha = 0.1, fill = "Red") + 
   labs(x = "Age", y = "Number of Children") + 
   theme(axis.title.x = element_text(size = 10), 
         axis.title.y = element_text(size = 10))


## @knitr Part2-6
Dating_sample7 <- Dating[Dating$age != 99 & Dating$children0_5 != 99 & 
                            Dating$children6_11 != 98 & 
                            Dating$children6_11 != 99 & 
                            Dating$children12_17 != 98 & 
                            Dating$children12_17 != 98, 
                         c("sex", "age", "children0_5", "children6_11", 
                           "children12_17")]
Dating_sample7 <- Dating_sample7[complete.cases(Dating_sample7), ]
Dating_sample7$children0_17 <- apply(Dating_sample7[, c(3:5)], 1, sum)
Dating_sample7 <- Dating_sample7[, c(1, 2, 6)]

Dating_sample7 <- subset(Dating_sample7, age == 31)

# t.test(children0_17 ~ sex, Dating_sample7, alternative = "less")

by(Dating_sample7$children0_17, Dating_sample7[, "sex"], 
   shapiro.test)
leveneTest(Dating_sample7$children0_17, Dating_sample7$sex)

by(Dating_sample7$children0_17, Dating_sample7[, "sex"], median)

wilcox.test(children0_17 ~ sex, Dating_sample7, exact = FALSE, correct = FALSE)

box <- ggplot(Dating_sample7, aes(sex, children0_17))
box + geom_boxplot() + 
   labs(x = "Sex", y = "Number of Children") + 
   theme(legend.position = "none", axis.title.x = element_text(size = 10), 
         axis.title.y = element_text(size = 10))


## @knitr Part3-1-a-intro
summary(Dating$life_quality)
levels(Dating$life_quality)
head(Dating$life_quality)
dim(Dating)[1]

## @knitr Part3-1-a-1
   # The followind code also works but I prefer the one that I've finally used
# life_quality <- as.character(Dating$life_quality)
# life_quality <- ifelse(life_quality == "5", "now1", life_quality)
# life_quality <- ifelse(life_quality == "4", "now2", life_quality)
# life_quality <- ifelse(life_quality == "2", "now4", life_quality)
# life_quality <- ifelse(life_quality == "1", "now5", life_quality)
# life_quality <- ifelse(life_quality == "now1", "1", life_quality)
# life_quality <- ifelse(life_quality == "now2", "2", life_quality)
# life_quality <- ifelse(life_quality == "now4", "4", life_quality)
# life_quality <- ifelse(life_quality == "now5", "5", life_quality)
# life_quality <- factor(life_quality)
# life_quality[life_quality == "Refused" | life_quality == "Don't know"] <- NA
# life_quality <- factor(life_quality)

life_quality <- Dating$life_quality
life_quality[life_quality == "Refused" | life_quality == "Don't know"] <- NA
life_quality <- factor(life_quality)
life_quality <- factor(life_quality, levels=rev(levels(life_quality)))
levels(life_quality) <- 
   as.character((4*as.numeric(levels(life_quality))) %% 5+1)

## @knitr Part3-1-a-2
head(Dating$life_quality)
as.numeric(Dating$life_quality[1]) + as.numeric(Dating$life_quality[2])
head(life_quality)
as.numeric(life_quality[1]) + as.numeric(life_quality[2])

## @knitr Part3-1-a-3
mean(as.numeric(life_quality), na.rm = TRUE)


## @knitr Part3-1-b-intro
summary(Dating$years_in_relationship)
levels(Dating$years_in_relationship)
head(Dating$years_in_relationship)

## @knitr Part3-1-b
years_in_relationship <- Dating$years_in_relationship
years_in_relationship <- as.numeric(as.character(years_in_relationship))

mean(years_in_relationship, na.rm = TRUE)


## @knitr Part3-1-c-intro
summary(Dating$use_internet)
levels(Dating$use_internet)
head(Dating$use_internet)

## @knitr Part3-1-c-1
dim(Dating)[1]
length(life_quality[!is.na(life_quality)])
length(years_in_relationship[!is.na(years_in_relationship)])

## @knitr Part3-1-c-2
use_internet <- Dating$use_internet
use_internet[use_internet == "Refused" | use_internet == "Don't know" | 
                use_internet == " "] <- NA
use_internet <- factor(use_internet)
length(use_internet[!is.na(use_internet)])

## @knitr Part3-1-c-3
Dating_P3_preliminar <- data.frame(life_quality, years_in_relationship, 
                                    use_internet)
Dating_P3 <- Dating_P3_preliminar[complete.cases(Dating_P3_preliminar), ]
dim(Dating_P3)[1]
Dating_P3$life_quality <- as.numeric(Dating_P3$life_quality)

## @knitr Part3-1-c-4
Dating_P3_preliminar$age <- Dating$age
subset(Dating_P3_preliminar, 
       age -10 < years_in_relationship)
Dating_P3_preliminar <- Dating_P3_preliminar[Dating_P3_preliminar$age > 
                                                10 + years_in_relationship, ]
Dating_P3_rev <- Dating_P3_preliminar[complete.cases(Dating_P3_preliminar), ]
dim(Dating_P3_rev)[1]

## @knitr Part3-1-d-1
(round(stat.desc(Dating_P3$life_quality, basic = FALSE), 3))
round(stat.desc(Dating_P3$years_in_relationship, basic = FALSE), 3)

## @knitr Part3-1-d-2
scatter <- ggplot(Dating_P3, aes(years_in_relationship, life_quality))
scatter + geom_hex(bins=50) + 
   geom_smooth(method = "lm", colour = "Red", alpha = 0.1, fill = "Red") + 
   labs(x = "Years in Relationship", y = "Life Quality") + 
   theme(axis.title.x = element_text(size = 10), 
         axis.title.y = element_text(size = 10))

## @knitr Part3-1-d-3
scatterplot(Dating_P3$years_in_relationship, Dating_P3$life_quality, 
            xlab = "Years in Relationship", ylab = "Life Quality")

## @knitr Part3-1-d-4
lq5 <- dim(Dating_P3[Dating_P3$life_quality == 5, ])[1]
lq4 <- dim(Dating_P3[Dating_P3$life_quality == 4, ])[1]
lq3 <- dim(Dating_P3[Dating_P3$life_quality == 3, ])[1]
lq2 <- dim(Dating_P3[Dating_P3$life_quality == 2, ])[1]
lq1 <- dim(Dating_P3[Dating_P3$life_quality == 1, ])[1]
(lq3 + lq4) / (lq1 + lq2 + lq3 + lq4 + lq5)

dim(Dating_P3[Dating_P3$years_in_relationship == 0, ])[1] / dim(Dating_P3)[1]

## @knitr Part3-1-d-5
model1 <- lm(life_quality ~ years_in_relationship, Dating_P3)
(summary_model1 <- summary(model1))
confint(model1)

## @knitr Part3-1-d-6
lm.beta(model1)

## @knitr Part3-1-d-7
model0 <- lm(life_quality ~ 1, Dating_P3)

## @knitr Part3-1-d-8
AIC(model0); AIC(model1)
BIC(model0); BIC(model1)


## @knitr Part3-1-e-1
summary(Dating_P3$use_internet)
t.test(life_quality ~ use_internet, Dating_P3)
scatter <- ggplot(Dating_P3, aes(years_in_relationship, life_quality, colour = use_internet))
scatter + geom_point() + 
   geom_smooth(method = "lm", alpha = 0.1, aes(fill = use_internet)) + 
   labs(x = "Years in Relationship", y = "Life Quality") + 
   theme(axis.title.x = element_text(size = 10), 
         axis.title.y = element_text(size = 10))

## @knitr Part3-1-e-2
model2 <- update(model1, .~.+use_internet)
(summary_model2 <- summary(model2))
confint(model2)

## @knitr Part3-1-f
anova(model1, model2)

AIC(model1)
AIC(model2)


## @knitr Part3-2-a-intro
summary(Dating$flirted_online)
levels(Dating$flirted_online)
head(Dating$flirted_online)

## @knitr Part3-2-a
flirted_online <- Dating$flirted_online
flirted_online[flirted_online == "Refused" | flirted_online == "Don't know" | 
                  flirted_online == " "] <- NA
flirted_online <- factor(flirted_online)

(odds <- 
    length(flirted_online[!is.na(flirted_online) & flirted_online == "Yes"]) / 
    length(flirted_online[!is.na(flirted_online) & flirted_online == "No"]))

## @knitr Part3-2-b-intro
summary(Dating$usr)
levels(Dating$usr)
head(Dating$usr)

## @knitr Part3-2-b-1
usr <- Dating$usr
usr[usr == " "] <- NA
usr <- factor(usr)

Dating_P4_preliminar <- data.frame(flirted_online, usr)
Dating_P4 <- Dating_P4_preliminar[complete.cases(Dating_P4_preliminar), ]

table(Dating_P4$flirted_online, Dating_P4$usr)

bar <- ggplot(Dating_P4, aes(usr, fill = flirted_online))
bar + stat_bin(position = "dodge", ) + 
   labs(x = "Area", y = "Number of respondents") + 
   theme(axis.title.x = element_text(size = 10), 
         axis.title.y = element_text(size = 10)) + 
   scale_fill_discrete(guide = guide_legend(title = "Flirted online"))



## @knitr Part3-2-b
log_model = glm(flirted_online ~ usr, data = Dating_P4, family=binomial())
summary(log_model)

## @knitr Part3-2-b-2
anova(log_model, test="Chisq")


## @knitr Part3-2-c

exp(log_model$coefficients)
exp(confint(log_model))
