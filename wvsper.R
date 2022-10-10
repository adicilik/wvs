############## Persiapan Data World Values Survey (WVS) #############
##### Script ini adalah  alur kerja untuk memeriksa data WVS,   #####  
##### dari awal data mentah, pemilihan variabel yang relevan    #####
##### dan inspeksi data secara umum dan keseluruhan             #####  
#####################################################################

## 1. Setup working directory  in local desktop and github

setwd("D:/acp/datasekunder/WVS")

## 2. Mengimport Data dari local

library(haven)
wvs7idn <- read_dta("WVS7IDN.dta")

## 3. Inspeksi Data (1) - Mengecek data secara keseluruhan

dim(wvs7idn)
View(wvs7idn)
names(wvs7idn)
head(wvs7idn, n = 2)
tail(wvs7idn, n = 3)
str(wvs7idn)

## 4. Membuat data frame untuk analisis yang kita butuhkan 

library(tidyverse)

wvs7idn <- wvs7idn %>% 
  select(G_TOWNSIZE, G_TOWNSIZE2, H_URBRURAL, Q46, Q47, Q48, Q49, 
         Q57, Q58, Q59, Q60, Q61, Q62, Q63, Q69, Q71, Q73, Q75, Q112, Q260, Q262, Q273) %>% 
  rename ("townsize"="G_TOWNSIZE", 
          "townsize2"="G_TOWNSIZE2", 
          "urbrural"="H_URBRURAL", 
          "happy"="Q46", 
          "health"="Q47", 
          "freedom"="Q48",
          "lifesat"="Q49", 
          "trust"="Q57", 
          "trfam"="Q58",  
          "trng"="Q59", 
          "trknow"="Q60", 
          "trmeet"="Q61", 
          "trareg"="Q62", 
          "tranat"="Q63",
          "corrupt"="Q112",
          "gender"="Q260",
          "age"="Q262",
          "marital"="Q273",
          "trpolis"="Q69", 
          "trpempus"="Q71", 
          "trparlem"="Q73", 
          "truniv"="Q75" 
  )

## 5. Inspeksi Data (2) - Setelah pemilihan variabel

dim(wvs7idn)
View(wvs7idn)
names(wvs7idn)
head(wvs7idn, n = 2)
tail(wvs7idn, n = 2)
str(wvs7idn)

## 6. Inspeksi Data (3) - Lebih dalam memahami variabel 

library(labelled)
look_for(wvs7idn)
look_for(wvs7idn, "townsize")
look_for(wvs7idn, "happy")

wvs7idn <- as.data.frame(apply(wvs7idn, 2, function(x) as.numeric(x)))

look_for(wvs7idn)
look_for(wvs7idn, "townsize")
look_for(wvs7idn, "happy")


## 1. Recode Data

### Recode happy Indonesia
wvs7idn <- mutate(wvs7idn, happy.x = case_when(
  happy  == 1 ~ 4,
  happy  == 2 ~ 3,
  happy  == 3 ~ 2,
  happy  == 4 ~ 1,
  TRUE ~ NA_real_
))

###Recode health
wvs7idn <- mutate(wvs7idn, health.x = case_when(
  health  == 1 ~ 5,
  health  == 2 ~ 4,
  health  == 3 ~ 3,
  health  == 4 ~ 2,
  health  == 5 ~ 1,
  TRUE ~ NA_real_
))

wvs7idn <- mutate(wvs7idn, trpolis.x = case_when(
  trpolis  == 1 ~ 4,
  trpolis  == 2 ~ 3,
  trpolis  == 3 ~ 2,
  trpolis  == 4 ~ 1,
  TRUE ~ NA_real_
))


wvs7idn <- mutate(wvs7idn, trpempus.x = case_when(
  trpempus  == 1 ~ 4,
  trpempus  == 2 ~ 3,
  trpempus  == 3 ~ 2,
  trpempus  == 4 ~ 1,
  TRUE ~ NA_real_
))

wvs7idn <- mutate(wvs7idn, trparlem.x = case_when(
  trparlem  == 1 ~ 4,
  trparlem  == 2 ~ 3,
  trparlem  == 3 ~ 2,
  trparlem  == 4 ~ 1,
  TRUE ~ NA_real_
))

wvs7idn <- mutate(wvs7idn, truniv.x = case_when(
  truniv  == 1 ~ 4,
  truniv  == 2 ~ 3,
  truniv  == 3 ~ 2,
  truniv  == 4 ~ 1,
  TRUE ~ NA_real_
))





### 2. Membuat dummy variable

###2a. Dummy Urban Rural

library(labelled)
look_for(wvs7idn, "urbrural")
wvs7idn$urban <- ifelse(wvs7idn$urbrural==1,1,0)
wvs7idn$rural <- ifelse(wvs7idn$urbrural==2,1,0)

## 2b. Dummy Gender
look_for(wvs7idn, "gender")
wvs7idn$female <- ifelse(wvs7idn$gender==2,1,0)
wvs7idn$male <- ifelse(wvs7idn$gender==1,1,0)


## 2c. Dummy Marital
look_for(wvs7idn, "marital")
wvs7idn$married <- ifelse(wvs7idn$marital==1,1,0)
wvs7idn$livtog <- ifelse(wvs7idn$marital==2,1,0)
wvs7idn$divorced <- ifelse(wvs7idn$marital==3,1,0)
wvs7idn$separated <- ifelse(wvs7idn$marital==4,1,0)
wvs7idn$widowed <- ifelse(wvs7idn$marital==5,1,0)
wvs7idn$single <- ifelse(wvs7idn$marital==6,1,0)




