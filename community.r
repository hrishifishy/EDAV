---
title: "Community Contribution"
author: 'Hrishikesh Telang(hnt2107)'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE,
                      cache = TRUE)
```

```{r}
library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)
library(choroplethrZip)
library(choroplethr)
library(choroplethrMaps)
data("df_pop_state")
head(df_pop_state)

state_choropleth(df_pop_state)

state_choropleth(df_pop_state,num_colors=2)

state_choropleth(df_pop_state,num_colors=1)
```


```{r}

demo<-read_csv('/Users/hrishikeshtelang/Downloads/Demo.csv')
names(demo)[1]<-"region"

part<-select(demo,region,"COUNT PARTICIPANTS")
names(part)[2]<-"value"
part$region <- as.character(part$region)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
zip_choropleth(part,county_zoom=nyc_fips)
```


```{r}
hisp<-select(demo,region,"PERCENT HISPANIC LATINO")
names(hisp)[2]<-"value"
hisp$region <- as.character(hisp$region)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
zip_choropleth(hisp,num_colors= 1,county_zoom=nyc_fips)

bl<-select(demo,region,"PERCENT BLACK NON HISPANIC")
names(bl)[2]<-"value"
bl$region <- as.character(bl$region)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
zip_choropleth(bl,num_colors= 1,county_zoom=nyc_fips)

wh<-select(demo,region,"PERCENT WHITE NON HISPANIC")
names(wh)[2]<-"value"
wh$region <- as.character(wh$region)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
zip_choropleth(wh,num_colors= 1,county_zoom=nyc_fips)


alien<-select(demo,region,"PERCENT PERMANENT RESIDENT ALIEN")
names(alien)[2]<-"value"
alien$region <- as.character(alien$region)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
zip_choropleth(alien,num_colors= 1,county_zoom=nyc_fips)
```

```{r}
decr<-demo%>%select(region,'COUNT PARTICIPANTS')
names(decr)[2]<-"count"
decr<-decr%>%arrange(desc(count))
decr[1:10,]

insights<-demo%>%filter(region==10467|region==11224|region==11230|region==10468|region==11219|region==10466|region==10463|region==11234|region==11218|region==11223)


b<-insights%>%select(region,'PERCENT RECEIVES PUBLIC ASSISTANCE')%>%mutate(per=(insights$'PERCENT RECEIVES PUBLIC ASSISTANCE')*100)
p<-ggplot(data=b, aes(x=as.character(region), y=per)) +geom_bar(stat="identity")
p+xlab("Zipcode")+ylab("% getting Public Assistance")

b$'PERCENT RECEIVES PUBLIC ASSISTANCE'<-NULL
names(b)[2]<-"value"
b$region <- as.character(b$region)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
zip_choropleth(b,num_colors= 1,county_zoom=nyc_fips)
```


```{r}
scatter<-insights%>%select(region,'COUNT FEMALE','COUNT MALE','COUNT RECEIVES PUBLIC ASSISTANCE')
names(scatter)[2]<-"Female"
names(scatter)[3]<-"Male"

newsc<-scatter%>%gather(key="Gender",value="Count",'Female':'Male')

ggplot(newsc, aes(x=as.character(region), y=Count, fill =Gender)) + geom_bar(stat="identity",position="dodge")+xlab("Zipcode")
```

```{r}
library(GGally)
library(extracat)
corr<-insights%>%select('COUNT HISPANIC LATINO','COUNT ASIAN NON HISPANIC','COUNT WHITE NON HISPANIC','COUNT RECEIVES PUBLIC ASSISTANCE',region)
ggpairs(corr,echo=FALSE)
```

