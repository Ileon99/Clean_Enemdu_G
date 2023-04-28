#First we are loading ENEMDU data from 2007 to 2021


library(haven)
library(readr)
library(dplyr)
library(ineq)
library(feather)


Enemdu_200712 <- read_csv2("ENEMDU_PERSONAS_2007_12_hom.csv")
Enemdu_200812 <- read_csv2("ENEMDU_PERSONAS_2008_12_hom.csv")
Enemdu_200912 <- read_csv2("ENEMDU_PERSONAS_2009_12_hom.csv")
Enemdu_201012 <- read_csv2("ENEMDU_PERSONAS_2010_12_hom.csv")
Enemdu_201112 <- read_csv2("ENEMDU_PERSONAS_2011_12_hom.csv")
Enemdu_201212 <- read_csv2("ENEMDU_PERSONAS_2012_12_hom.csv")
Enemdu_201312 <- read_csv2("ENEMDU_PERSONAS_2013_12_hom.csv")
Enemdu_201412 <- read_csv2("201412_EnemduBDD_15anios.csv")
Enemdu_201512 <- read_csv2("ENEMDU_PERSONAS_2015_12_hom.csv")
Enemdu_201612 <- read_csv2("ENEMDU_PERSONAS_2016_12_hom.csv")
Enemdu_201712 <- read_csv2("ENEMDU_PERSONAS_2017_12_hom.csv")
Enemdu_201812 <- read_csv2("ENEMDU_PERSONAS_2018_12_hom.csv")
Enemdu_201912 <- read_csv2("enemdu_persona_201912.csv")
Enemdu_202012 <- read_csv2("enemdu_persona_2020_12.csv")
Enemdu_202112 <- read_csv2("enemdu_persona_2021_12.csv")
province_codes <- read_csv2("province_codes.csv")



#First we are going equalize the format in all the bases

#2014 have some diferent variable names that we are going to equalize
#Create a variable periodo with the date

Enemdu_201412$periodo <- rep(201412,length(Enemdu_201412$pean))

#Change desem by desempleo

Enemdu_201412$desempleo <- Enemdu_201412$desem

#create id_hogar

Enemdu_201412$index <- as.numeric(row.names(Enemdu_201412))
Enemdu_201412$id_hogar <- Enemdu_201412$index - Enemdu_201412$p01

Enemdu_201412$id_hogar

#Now we replace our old databases by the cleaned ones

Enemdu_200712 <- Clean_df(Enemdu_200712)
Enemdu_200812 <- Clean_df(Enemdu_200812)
Enemdu_200912 <- Clean_df(Enemdu_200912)
Enemdu_201012 <- Clean_df(Enemdu_201012)
Enemdu_201112 <- Clean_df(Enemdu_201112)
Enemdu_201212 <- Clean_df(Enemdu_201212)
Enemdu_201312 <- Clean_df(Enemdu_201312)
Enemdu_201412 <- Clean_df(Enemdu_201412)
Enemdu_201512 <- Clean_df(Enemdu_201512)
Enemdu_201612 <- Clean_df(Enemdu_201612)
Enemdu_201712 <- Clean_df(Enemdu_201712)
Enemdu_201812 <- Clean_df(Enemdu_201812)
Enemdu_201912 <- Clean_df_2019(Enemdu_201912)
Enemdu_202012 <- Clean_df_2019(Enemdu_202012)
Enemdu_202112 <- Clean_df_2019(Enemdu_202112)


#We joint all data bases in the same one


Enemdu <- rbind(Enemdu_200712,
                Enemdu_200812,
                Enemdu_200912,
                Enemdu_201012,
                Enemdu_201112,
                Enemdu_201212,
                Enemdu_201312,
                Enemdu_201412,
                Enemdu_201512,
                Enemdu_201612,
                Enemdu_201712,
                Enemdu_201812,
                Enemdu_201912,
                Enemdu_202012,
                Enemdu_202112
)



################## Fixing formal per house #################################

#### The variable get how many formal employees a household has

#This variable is used in order to correct for endogeneity created by this variable

#When there is one formal in the household there is an insurance effect 

#However, now the variable count also if the main individual is formal or not, which can bias this insurance effect

#What we will do is to modify this variable : 

#Formal per house without counting on the main individual : 

table(Enemdu$formal_per_house)

Enemdu$formal_per_house0 <- Enemdu$formal_per_house

Enemdu$formal_per_house <- ifelse(Enemdu$formal == 1, Enemdu$formal_per_house0 - 1, Enemdu$formal_per_house0)

table(Enemdu$formal_per_house)

table(Enemdu$province,Enemdu$period)

write_feather(Enemdu, "Enemdu2.feather")

viejoEnemdu <- read_feather("Enemdu.feather")


