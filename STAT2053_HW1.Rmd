---
title: "STAT253 - Homework#1"
author: "Ahmet Elburuz Gürbüz - 150116024"
date: "Monday, March 23, 2020"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r chunkLabel}
Hw1_Dataset = read.csv("dataset.csv")
manSYSBP = Hw1_Dataset[Hw1_Dataset$GENDER == 0, "SYSBP"]
womenSYSBP = Hw1_Dataset[Hw1_Dataset$GENDER == 1, "SYSBP"]
manDIASBP = Hw1_Dataset[Hw1_Dataset$GENDER == 0, "DIASBP"]
womenDIASBP = Hw1_Dataset[Hw1_Dataset$GENDER == 1, "DIASBP"]
#a-----------------------------------------------
manMean1 = mean(manSYSBP)
manMean1
womenMean1 = mean(womenSYSBP)
womenMean1
manMean2 = mean(manDIASBP)
manMean2
womenMean2 = mean(womenDIASBP)
womenMean2
#b-----------------------------------------------
manVar1 = var(manSYSBP)
manVar1
manVar2 = var(manDIASBP)
manVar2
womenVar1 = var(womenSYSBP)
womenVar1
womenVar2 = var(womenDIASBP)
womenVar2
#c-----------------------------------------------
mansd1 = sd(manSYSBP)
mansd1
mansd2 = sd(manDIASBP)
mansd2
womensd1 = sd(womenSYSBP)
womensd1
womensd2 = sd(womenDIASBP)
womensd2
#d-----------------------------------------------
manQuantile1=quantile(manSYSBP,0.25)
manQuantile1
manqQuantile2=quantile(manSYSBP,0.75)
manqQuantile2
manQuantile3=quantile(manDIASBP,0.25)
manQuantile3
manQuantile4=quantile(manDIASBP,0.75)
manQuantile4

womenQuantile1=quantile(womenSYSBP,0.25)
womenQuantile1
womenQuantile2=quantile(womenSYSBP,0.75)
womenQuantile2
womenQuantile3=quantile(womenDIASBP,0.25)
womenQuantile3
womenQuantile4=quantile(womenDIASBP,0.75)
womenQuantile4
#e-----------------------------------------------
menmin1 = min(womenSYSBP)
menmin1
menmax1 = max(womenSYSBP)
menmax1
menmin2 = min(womenDIASBP)
menmin2
menmax2 = max(womenDIASBP)
menmax2

womenmin1 = min(womenSYSBP)
womenmin1
womenmax1 = max(womenSYSBP)
womenmax1
womenmin2 = min(womenDIASBP)
womenmin2
womenmax2 = max(womenDIASBP)
womenmax2
#f-----------------------------------------------
menRange1 = range(manSYSBP)
menRange1
menRange2 = range(manDIASBP)
menRange2

womenRange1 = range(womenSYSBP)
womenRange1
womenRange2 = range(womenDIASBP)
womenRange2
#g-----------------------------------------------
menRate1 = menRange1/mansd1 
menRate1
menRate2 = menRange2/mansd2 
menRate2

womenRate1 = womenRange1/womensd1
womenRate1
womenRate2 = womenRange2/womensd2
womenRate2
#h-----------------------------------------------
menMedian1 = median(manSYSBP)
menMedian1
menMedian2 = median(manDIASBP)
menMedian2

womenMedian1 = median(womenSYSBP)
womenMedian1
womenMedian2 = median(womenDIASBP)
womenMedian2
#i-----------------------------------------------
menIQR1 = IQR(manSYSBP)
menIQR1
menIQR2 = IQR(manDIASBP)
menIQR2

womenIQR1 = IQR(womenSYSBP)
womenIQR1
womenIQR2 = IQR(womenDIASBP)
womenIQR2
#j-----------------------------------------------
menSummary1 = fivenum(manSYSBP)
menSummary1
menSummary2 = fivenum(manDIASBP)
menSummary2

womenSummary1 = fivenum(womenSYSBP)
womenSummary1
womenSummary2 = fivenum(womenDIASBP)
womenSummary2
#k-----------------------------------------------
boxplot(Hw1_Dataset$SYSBP ~ Hw1_Dataset$GENDER, 
main = "Boxplot of SYSBP",
xlab="Gender", 
ylab="SYSBP")

boxplot(Hw1_Dataset$DIASBP ~ Hw1_Dataset$GENDER,
main = "Boxplot of DIASBP",
xlab="Gender",
ylab="DIASBP")

#l-----------------------------------------------
stem(Hw1_Dataset$SYSBP[Hw1_Dataset$GENDER==0])
stem(Hw1_Dataset$DIASBP[Hw1_Dataset$GENDER==0])
stem(Hw1_Dataset$SYSBP[Hw1_Dataset$GENDER==1])
stem(Hw1_Dataset$DIASBP[Hw1_Dataset$GENDER==1])
#m-----------------------------------------------
hist(Hw1_Dataset$SYSBP[Hw1_Dataset$GENDER==0],
main = "Histogram of Men SYSBP with Breaks = 5",
xlab= "SYSBP",
breaks = 5)

hist(Hw1_Dataset$SYSBP[Hw1_Dataset$GENDER==0],
main = "Histogram of Men SYSBP with Breaks = 10",
xlab= "SYSBP",
breaks = 10)

hist(Hw1_Dataset$DIASBP[Hw1_Dataset$GENDER==0],
main = "Histogram of Men DIASBP with Breaks = 5",
xlab= "DIASBP",
breaks = 5)

hist(Hw1_Dataset$DIASBP[Hw1_Dataset$GENDER==0],
main = "Histogram of Men DIASBP with Breaks = 10",
xlab= "DIASBP",
breaks = 10)

hist(Hw1_Dataset$SYSBP[Hw1_Dataset$GENDER==1],
main = "Histogram of Women DIASBP with Breaks = 5",
xlab= "SYSBP",
breaks = 5)

hist(Hw1_Dataset$SYSBP[Hw1_Dataset$GENDER==1],
main = "Histogram of Women DIASBP with Breaks = 10",
xlab= "SYSBP",
breaks = 10)

hist(Hw1_Dataset$DIASBP[Hw1_Dataset$GENDER==1],
main = "Histogram of Women SYSBP with Breaks = 5",
xlab= "DIASBP",
breaks = 5)

hist(Hw1_Dataset$DIASBP[Hw1_Dataset$GENDER==1],
main = "Histogram of Women SYSBP with Breaks = 10",
xlab= "DIASBP",
breaks = 10)
#n-----------------------------------------------
dotchart(Hw1_Dataset$SYSBP[Hw1_Dataset$GENDER==0],
color = "green",
main = "Dotplot of Men SYSBP",
xlab= "SYSBP")

dotchart(Hw1_Dataset$DIASBP[Hw1_Dataset$GENDER==0],
color = "blue",
main = "Dotplot of Men DIASBP",
xlab= "DIASBP")

dotchart(Hw1_Dataset$SYSBP[Hw1_Dataset$GENDER==1],
color = "red",
main = "Dotplot of Women SYSBP",
xlab= "SYSBP")

dotchart(Hw1_Dataset$DIASBP[Hw1_Dataset$GENDER==1],
color = "brown",
main = "Dotplot of Women DIASBP",
xlab= "DIASBP")

#q-----------------------------------------------
plot(Hw1_Dataset$SYSBP[Hw1_Dataset$GENDER==0],Hw1_Dataset$DIASBP[Hw1_Dataset$GENDER==0],
col=c("blue","red"),
main = "Comparasion of Men SYSBP and DIASBP",
xlab= "SYSBP",
ylab = "DIASBP")

plot(Hw1_Dataset$SYSBP[Hw1_Dataset$GENDER==1],Hw1_Dataset$DIASBP[Hw1_Dataset$GENDER==1],
col=c("blue","red"),
main = "Comparasion of Women SYSBP and DIASBP",
xlab= "SYSBP",
ylab = "DIASBP")

#r-----------------------------------------------
library(HistogramTools)

PlotRelativeFrequency(hist(Hw1_Dataset$SYSBP[Hw1_Dataset$GENDER==0],plot = F),
main = "Histogram of Relative Frequency for Men SYSBP",
xlab= "SYSBP")

PlotRelativeFrequency(hist(Hw1_Dataset$DIASBP[Hw1_Dataset$GENDER==0],plot = F),
main = "Histogram of Relative Frequency for Men DIASBP",
xlab= "DIASBP")

PlotRelativeFrequency(hist(Hw1_Dataset$SYSBP[Hw1_Dataset$GENDER==1],plot = F),
main = "Histogram of Relative Frequency for Women SYSBP",
xlab= "SYSBP")
PlotRelativeFrequency(hist(Hw1_Dataset$DIASBP[Hw1_Dataset$GENDER==1],plot = F),
main = "Histogram of Relative Frequency for Women DIASBP",
xlab= "DIASBP")

#s-----------------------------------------------
zScoreMenLargerstSYSBP = (menmax1-manMean1)/mansd1
zScoreMenLargerstSYSBP
zScoreMenLargestDIASBP = (menmax2-manMean2)/mansd2
zScoreMenLargestDIASBP
zScoreMenSmallestSYSBP = (menmin1-manMean1)/mansd1
zScoreMenSmallestSYSBP
zScoreMenSmallestDIASBP = (menmin2-manMean2)/mansd2
zScoreMenSmallestDIASBP

zScoreWomenLargestSYSBP = (womenmax1-womenMean1)/womensd1
zScoreWomenLargestSYSBP
zScoreWomenLargestDIASBP = (womenmax2-womenMean2)/womensd2
zScoreWomenLargestDIASBP
zScoreWomenSmallestSYSBP = (womenmin1-womenMean1)/womensd1
zScoreWomenSmallestSYSBP
zScorWomenSmallestDIASBP = (womenmin2-womenMean2)/womensd2
zScorWomenSmallestDIASBP
# R code
```
