#Function for 2019 and more (for migration)

Clean_df_2019<- function(df){
  
  
  #The variable informal is 1 if the employment is informal and 0 if not
  #We consider as informal employment all employment that is not registered in the IESS and that works in a company without RUC
  
  #to construct the informal dummies we are going to see if the individual is an employee or am employer
  
  #An employer is whether an employer or a self-employed
  df$employer <- rep(0, length(df$p42))
  df$employer[df$p42 == "5" | df$p42 =="6"]=1
  
  
  df$employee <- rep(0, length(df$p42))
  df$employee[df$p42 == "1" | df$p42 == "2" | df$p42 == "3" | df$p42 == "4" | df$p42 == "10" ]=1
  
  #We are going to create a variable equal to 1 if the indivual works and equal to 0 if not
  
  df$employment <- df$employer + df$employee
  
  #Informal2 not registered to the social security
  #Question 5 ask if the person is affiliated to the social security
  
  df$p05a <- as.numeric(df$p05a)
  df$p05b <- as.numeric(df$p05b)
  
  df$informal <- rep(0,length(df$p48))
  df$informal[df$p05a>4 & df$p05b>4 & df$employment == 1]=1
  
  #Lower tier of informal employment not registered to social security
  df$informal_lt <- rep(0,length(df$p48))
  df$informal_lt[df$p05a>4 & df$p05b>4 & df$employee == 1 ]=1
  
  #Upper tier of informal employment employer that are not registered to social security
  df$informal_up <- rep(0,length(df$p48))
  df$informal_up[df$p05a>4 & df$p05b>4 & df$employer == 1]=1
  
  df$formal <- df$employment - df$informal
  
  
  #Create a variable year 
  df$period <-  floor(df$periodo/100)
  
  
  #Create a variable City 
  df$city <- as.numeric(df$ciudad)
  
  #Rural 1 if the individual lives in the rural area 0 otherwise
  df$rural <- rep(0, length(df$area))
  df$rural[df$area ==2]=1
  
  #active population
  
  df$pean <- rep(0,length(df$employment))
  df$pean[df$employment==1 | df$desempleo == 1]=1
  
  
  #unemployment 
  
  df$unemp <- rep(0,length(df$desempleo))
  df$unemp[df$desempleo == 1 ]=1
  
  #let replace the na of employment and inactivity by 0
  df$pean[is.na(df$pean)]=0
  df$unemp[is.na(df$unemp)]=0
  
  #Calculate the total labour income
  #We have Wage = Monetary first job wage (66 + 67 taxes paid) + wage first job in kind (68b)
  #+ Monetary second job wage + wage second job in kind
  #+ Business income
  df$mincome <- rep(0,length(df$pean))
  
  #Market income = business income (p64b) + wages (66 + 69) + capital income (71b) + private transfers (73b + 74b)
  
  df$p63 <- as.numeric(df$p63)
  df$p63[is.na(df$p63)]=0
  df$p63 <- ifelse(df$p63 == 999999, 0, df$p63)
  
  df$p64b <- as.numeric(df$p64b)
  df$p64b[is.na(df$p64b)]=0
  df$p64b <- ifelse(df$p64b == 999999, 0, df$p64b)
  
  df$p65 <- as.numeric(df$p65)
  df$p65[is.na(df$p65)]=0
  df$p65 <- ifelse(df$p65 == 999999, 0, df$p65)
  
  df$p66 <- as.numeric(df$p66)
  df$p66[is.na(df$p66)]=0
  df$p66 <- ifelse(df$p66 == 999999, 0, df$p66)
  
  df$p67 <- as.numeric(df$p67)
  df$p67[is.na(df$p67)]=0
  df$p67 <- ifelse(df$p67 == 999999, 0, df$p67)
  
  df$p68b <- as.numeric(df$p68b)
  df$p68b[is.na(df$p68b)]=0
  df$p68b <- ifelse(df$p68b == 999999, 0, df$p68b)
  
  df$p69 <- as.numeric(df$p69)
  df$p69[is.na(df$p69)]=0
  df$p69 <- ifelse(df$p69 == 999999, 0, df$p69)
  
  df$p70b <- as.numeric(df$p70b)
  df$p70b[is.na(df$p70b)]=0
  df$p70b <- ifelse(df$p70b == 999999,0,df$p70b)
  
  df$p71b <- as.numeric(df$p71b)
  df$p71b[is.na(df$p71b)]=0
  df$p71b <- ifelse(df$p71b == 999999,0,df$p71b)
  
  df$p72b <- as.numeric(df$p72b)
  df$p72b[is.na(df$p72b)]=0
  df$p72b <- ifelse(df$p72b == 999999,0,df$p72b)
  
  df$p73b <- as.numeric(df$p73b)
  df$p73b[is.na(df$p73b)]=0
  df$p73b <- ifelse(df$p73b == 999999,0,df$p73b)
  
  df$p74b <- as.numeric(df$p74b)
  df$p74b[is.na(df$p74b)]=0
  df$p74b <- ifelse(df$p74b == 999999,0,df$p74b)
  
  df$p76 <- as.numeric(df$p76)
  df$p76[is.na(df$p76)]=0
  df$p76 <- ifelse(df$p76 == 999999,0,df$p76)
  
  
  #labour taxes
  
  df$taxes <- df$p67
  
  #I add taxes because question 66 is how much did you receive and question 67 is how much was deducted from your salary.
  
  #Monetary First Wage Job
  
  df$wage_m_1 <- df$p66 + df$taxes
  
  #First Job Wage in Kind
  
  df$wage_inkind_1 <- df$p68b
  
  #Total First Job Wage
  
  df$wage_1 <- df$wage_m_1 + df$wage_inkind_1
  
  #Monetary Second Job Wage
  
  df$wage_m_2 <-  df$p69
  
  #Second Job Wage in Kind
  
  df$wage_inkind_2 <- df$p70b
  
  #Total Second Job Wage 
  
  df$wage_2 <- df$wage_m_2 + df$wage_inkind_2
  
  #Total wage
  
  df$tot_wage <- df$wage_1 + df$wage_2 
  
  #Business income: Income from their own income 
  # Business income = Sales Revenue - Costs
  
  df$sales_revenue <- df$p63 + df$p64b
  
  df$business_costs <- df$p65
  
  df$business <- df$sales_revenue - df$business_costs
  
  df$business <- ifelse(df$business <0, 0, df$business)
  
  df$tot_labour_income <- df$tot_wage + df$business
  
  
  #capital is income from capital such as shares
  
  df$capital <- df$p71b
  
  #social tranfers are p72b (retirement, divorce, employment insurance, etc)
  # plus p76 Bono de desarrollo humano
  
  df$stransfers <- df$p72b + df$p76 
  
  #Donations : gifts, donations from people or institutions from inside of the country
  
  df$donations <- df$p73b
  
  #Remittances : transfers from friends or family that live in other country
  
  df$remittances <- df$p74b
  
  
  #Redistribution = Social transfers - taxes
  
  df$redist <- df$stransfers - df$taxes
  
  #Total Net Income = Total labour income + capital income + social transfers + donations + remittances - taxes
  
  df$tot_net_income <- df$tot_labour_income + df$capital + df$donations + df$remittances + df$stransfers - df$taxes
  
  #household market income, disposable income, size of the housegold, informal and formal per household
  
  df$nhousehold <- rep(1,length(df$panelm))
  
  household_income <- df %>%
    group_by(id_hogar) %>%
    summarise_at(vars(tot_labour_income, tot_net_income, nhousehold, informal, formal),
                 sum) %>%
    ungroup()
  
  household_income$h_tot_labour_income_pc <- household_income$tot_labour_income / household_income$nhousehold
  household_income$h_tot_net_income_pc <- household_income$tot_net_income / household_income$nhousehold
  
  names(household_income) <- c("id_hogar", "h_tot_labour_income","h_tot_net_income","nhousehold","informal_per_house","formal_per_house" ,"h_tot_labour_income_pc","h_tot_net_income_pc")
  
  df <- merge(df,household_income, by = "id_hogar")
  
  #We are going to create a variable that count the number of kids per house (less than 5 years, between 5 years and 10 years and 10-17 years)
  
  #Create a dummy for each 
  
  #Less than 5 years
  
  df$little_kid <- rep(0,length(df$p03))
  df$little_kid[df$p03 <= 5]=1
  
  #5 - 10 years
  
  df$kid <- rep(0,length(df$p03))
  df$kid[df$p03 > 5 & df$p03 < 11]=1
  
  #10 - 17  years
  
  df$teen <- rep(0,length(df$p03))
  df$teen[df$p03 > 10 & df$p03 < 18]=1
  
  #more a variable of the three of them
  
  df$young <- df$little_kid + df$kid + df$teen
  
  df$adult <- rep(0, length(df$p03))
  df$adult[df$p03 > 18]=1
  
  #Now we create a new variable that counts the number of little kids, kids, and teens in the household
  
  household_kids <- df %>%
    group_by(id_hogar) %>%
    summarise_at(vars(little_kid,kid,teen, young, adult),
                 sum) %>%
    ungroup()
  
  names(household_kids) <- c("id_hogar", "h_little_kids","h_kid","h_teen","h_young", "h_adult")
  
  df <- merge(df,household_kids, by = "id_hogar")
  
  
  #Education level
  
  df$education <- rep(0, length(df$p10a))
  df$education <- as.numeric(df$p10a)
  df$education[is.na(df$education)]=0
  
  #Primary School
  
  df$school <- rep(0, length(df$p10a))
  df$school[df$education >= 4]=1
  
  #High School
  
  df$highschool <- rep(0, length(df$p10a))
  df$highschool[df$education >= 6]=1
  
  #University
  
  df$university <- rep(0, length(df$p10a))
  df$university[df$education >= 9]=1
  
  
  #"año aprobado"
  
  df$ap <- as.numeric(df$p10b)
  
  #Years of education
  df$education_y <- rep(0, length(df$p10a))
  df$education_y[df$education == 1]=0
  df$education_y[df$education == 2]= df$ap[df$education == 2]
  df$education_y[df$education == 3]= 1
  df$education_y[df$education == 4]= df$ap[df$education == 4]+1
  df$education_y[df$education == 5]= df$ap[df$education == 5]
  df$education_y[df$education == 6]= df$ap[df$education == 6]+7
  df$education_y[df$education == 7]= df$ap[df$education == 7]+10
  df$education_y[df$education == 8]= df$ap[df$education == 8]+13
  df$education_y[df$education == 9]= df$ap[df$education == 9]+13
  df$education_y[df$education == 10]= df$ap[df$education == 10]+17
  
  df$tool1 <- rep(1,length(df$panelm))
  
  education_b <- df %>%
    group_by(id_hogar) %>%
    summarise_at(vars(education_y,tool1),
                 sum) %>%
    ungroup()
  
  education_b$education_y <- education_b$education_y / education_b$tool1
  
  names(education_b) <- c("id_hogar", "educ_house","number")
  
  df <- merge(df,education_b, by = "id_hogar")
  
  #illiterate people
  df$illiterate <- rep(0,length(df$p11))
  df$illiterate[df$p11 == "2"]=1
  
  #Assist_class
  
  df$assist_c <- rep(0, length(df$p07))
  df$assist_c[df$p07 == 1]=1
  
  #Studying and working
  
  df$student_employment<- rep(0, length(df$p07))
  df$student_employment[df$p07 == 1 & df$employment == 1]=1
  
  #Age 
  df$age <- as.numeric(df$p03)
  
  #Sector
  df$sector <- as.numeric(df$p42)
  df$sector[is.na(df$sector)]=0
  
  df$rama2 = rep(0,length(df$p42))
  df$rama2[df$rama1 == "1"]= "Agriculture"
  df$rama2[df$rama1 == "2"|df$rama1 == "3" | df$rama1 == "4"|df$rama1 == "5"]= "Manufacturing"
  df$rama2[df$rama1 == "6"]= "Construction"
  df$rama2[df$rama1 == "7"|df$rama1 == "8" | df$rama1 == "9"]= "Wholesale and retail trade"
  df$rama2[df$rama1 == "10"]= "Information and communication"
  df$rama2[df$rama1 == "11"]= "Financial and insurance activities"
  df$rama2[df$rama1 == "12"]= "Real estate activities"
  df$rama2[df$rama1 == "13"|df$rama1 == "14"]= "Professional and scientific services"
  df$rama2[df$rama1 == "15"| df$rama1 == "16"|df$rama1 == "17"]= "Public administration, education and health services"
  df$rama2[df$rama1 == "18" | df$rama1 == "19" | df$rama1 == "20" | df$rama1 == "21"]= "Other Services"
  
  #Dummy per sector from the ONU 
  
  #"agriculture", "manufacturing","construction", "retail", "information", "financial", "realestate","scientific", "public", "other_services"
  
  df$agriculture <- rep(0, length(df$rama2))
  df$agriculture[df$rama2 == "Agriculture"]=1
  
  df$manufacturing <- rep(0, length(df$rama2))
  df$manufacturing[df$rama2 == "Manufacturing"]=1
  
  df$construction <- rep(0, length(df$rama2))
  df$construction[df$rama2 == "Construction" ]=1
  
  df$retail <- rep(0,length(df$rama2))
  df$retail[df$rama2 == "Wholesale and retail trade"]=1
  
  df$information <- rep(0,length(df$rama2))
  df$information[df$rama2 == "Information and communication"]=1
  
  df$financial <- rep(0,length(df$rama2))
  df$financial[df$rama2 == "Financial and insurance activities"]=1
  
  df$realestate <- rep(0,length(df$rama2))
  df$realestate[df$rama2 == "Real estate activities"]=1
  
  df$scientific <- rep(0,length(df$rama2))
  df$scientific[df$rama2 == "Professional and scientific services"]=1
  
  df$public <- rep(0,length(df$rama2))
  df$public[df$rama2 == "Public administration, education and health services"]=1
  
  df$other_services <- rep(0,length(df$rama2))
  df$other_services[df$rama2 == "Other Services"]=1
  
  #Occupations
  #We use the International Standard Classification of Occupations ISCO-08 to get the occupations
  #We keep only the 10 major groups
  
  #In the Enemdu we get the majour group by dividing question 41 by 1000 and getting the round value
  
  df$p41 <- as.numeric(df$p41)
  df$occupation <- floor(df$p41/1000)
  
  #Now we create a dummy for each occupation
  
  df$managers <- rep(0,length(df$p41))
  df$managers[df$occupation == 1]=1
  
  df$professionals <- rep(0, length(df$p41))
  df$professionals[df$occupation == 2]=1
  
  df$technicians <- rep(0,length(df$p41))
  df$technicians[df$occupation == 3]=1
  
  df$clerical_support <- rep(0,length(df$p41))
  df$clerical_support[df$occupation == 4]=1
  
  df$services_and_sales <- rep(0,length(df$p41))
  df$services_and_sales[df$occupation == 5]=1
  
  df$skilled_agricultural <- rep(0,length(df$p41))
  df$skilled_agricultural[df$occupation == 6]=1
  
  df$craft_related_trades <- rep(0,length(df$p41))
  df$craft_related_trades[df$occupation == 7]= 1
  
  df$plant_machine_operators <- rep(0,length(df$p41))
  df$plant_machine_operators[df$occupation == 8]=1
  
  df$elementary_occupations <- rep(0,length(df$p41))
  df$elementary_occupations[df$occupation == 9]=1
  
  df$armed_forces <- rep(0,length(df$p41))
  df$armed_forces[df$occupation == 0]=1
  
  
  
  #Ethny (categorical)
  df$ethny <- as.numeric(df$p15)
  
  #Indigenous (dummy)
  df$indigenous <-  rep(0, length(df$ethny))
  df$indigenous[df$ethny == 1]=1
  
  #Afro (dummy)
  df$afro <-  rep(0, length(df$ethny))
  df$afro[df$ethny == 2 | df$ethny == 3 ]=1
  
  #Otro Mestizo 
  
  df$mestizo<- rep(1,length(df$ethny))
  df$mestizo<- df$mestizo - df$afro - df$indigenous
  
  #Noindigenous
  df$noindigenous <- df$mestizo + df$afro
  
  
  #Sex dummy equal to 1 if women 0 if men
  df$sex <- as.numeric(df$p02)
  df$sex[df$sex == 1]=0
  df$sex[df$sex == 2]=1
  
  #province
  df$province_c <- floor(df$city/10000)
  df <- merge(df, province_codes, by = "province_c")
  
  #unemployment rate
  
  df$unempx <- rep(0,length(df$city))
  
  df$peanx <- rep(0,length(df$city))
  
  df$unempr <- rep(0,length(df$city))
  
  for (i in 1:24 ) {
    
    df$unempx[df$province_c == i] <- sum(df$unemp[df$province_c == i])
    df$peanx[df$province_c == i] <- sum(df$pean[df$province_c == i])
    
    
    
  }
  
  df$unempr <- df$unempx / df$peanx
  
  #Informal employment rate per province
  for (i in 1:24 ) {
    
    df$informal_rate[df$province_c == i] <- sum(df$informal[df$province_c == i])
    
  }
  
  df$informal_rate <- df$informal_rate / df$peanx
  
  #Horizontal inequalities market income per province 
  
  
  for (i in 1:24 ) {
    
    pi <- sum(df$indigenous[df$province_c == i])/nrow(df[df$province_c == i,])
    
    pni <- 1 - pi
    
    yi <- mean(df$tot_labour_income[df$indigenous==1 & df$province_c == i])
    
    yni <- mean(df$tot_labour_income[df$indigenous==0 & df$province_c == i])
    
    y <- mean(df$tot_labour_income[df$province_c == i])
    
    
    df$GGini[df$province_c == i] <- (1/(2*y))*(pi*pni*abs(yi-yni)+pi*pni*abs(yni-yi))
    
    df$GTheil[df$province_c == i] <- pi*(yi/y)*log(yi/y)+pni*(yni/y)*log(yni/y)
    
  }
  
  #Horizontal inequalities household market income per province 
  
  
  for (i in 1:24 ) {
    
    pi <- sum(df$indigenous[df$province_c == i])/nrow(df[df$province_c == i,])
    
    pni <- 1 - pi
    
    yi <- mean(df$h_tot_labour_income[df$indigenous==1 & df$province_c == i])
    
    yni <- mean(df$h_tot_labour_income[df$indigenous==0 & df$province_c == i])
    
    y <- mean(df$h_tot_labour_income[df$province_c == i])
    
    
    df$GGini_h[df$province_c == i] <- (1/(2*y))*(pi*pni*abs(yi-yni)+pi*pni*abs(yni-yi))
    
    df$GTheil_h[df$province_c == i] <- pi*(yi/y)*log(yi/y)+pni*(yni/y)*log(yni/y)
    
  }
  
  #Horizontal inequalities household market income per capita per province 
  
  
  for (i in 1:24 ) {
    
    pi <- sum(df$indigenous[df$province_c == i])/nrow(df[df$province_c == i,])
    
    pni <- 1 - pi
    
    yi <- mean(df$h_tot_labour_income_pc[df$indigenous==1 & df$province_c == i])
    
    yni <- mean(df$h_tot_labour_income_pc[df$indigenous==0 & df$province_c == i])
    
    y <- mean(df$h_tot_labour_income_pc[df$province_c == i])
    
    
    df$GGini_hpc[df$province_c == i] <- (1/(2*y))*(pi*pni*abs(yi-yni)+pi*pni*abs(yni-yi))
    
    df$GTheil_hpc[df$province_c == i] <- pi*(yi/y)*log(yi/y)+pni*(yni/y)*log(yni/y)
    
  }
  
  #Vertical inequalities with the market income
  for (i in 1:24 ) {
    
    df$gini[df$province_c == i] <- Gini(df$tot_labour_income[df$province_c == i])
    
  }
  
  #Vertical inequalities with the household market income
  for (i in 1:24 ) {
    
    df$gini_h[df$province_c == i] <- Gini(df$h_tot_labour_income[df$province_c == i])
    
  }
  
  #Vertical inequalities with the household market income
  
  for (i in 1:24 ) {
    
    df$gini_hpc[df$province_c == i] <- Gini(df$h_tot_labour_income_pc[df$province_c == i])
    
  }
  
  
  #We are going to compute the quintil of each individual with the household market income per capita (first at a National level)
  
  
  df$quintil <- rep(0, length(df$tot_labour_income))
  q=0
  l=0
  
  for (u in unname(quantile(df$h_tot_labour_income_pc, probs = seq(0,1, 1/5)))) {
    df$quintil[df$h_tot_labour_income_pc >= l &  df$h_tot_labour_income_pc < u]=q
    q=q+1
    l=u
  }
  
  #In order to measure the effect of the Senescyt we create statistics per cohort of 17-19 years 
  
  #First we create a dummy for people between 17 - 19 years
  
  df$cohort_17_19 <- rep(0,length(df$p03))
  df$cohort_17_19[df$p03 > 16 & df$p03 < 20]= 1
  
  df$cohort <- rep(0, length(df$p03))
  df$cohort[df$p03 < 14]= "0-13"
  df$cohort[df$p03 > 13 & df$p03 < 17]= "14-16"
  df$cohort[df$p03 > 16 & df$p03 < 20]= "17-19"
  df$cohort[df$p03 > 19 & df$p03 < 23]= "20-22"
  df$cohort[df$p03 > 22 & df$p03 < 26]= "23-25"
  df$cohort[df$p03 > 25 & df$p03 < 29]= "26-28"
  df$cohort[df$p03 > 28 & df$p03 < 32]= "29-31"
  df$cohort[df$p03 > 31 & df$p03 < 35]= "32-34"
  df$cohort[df$p03 > 34 & df$p03 < 38]= "35-37"
  df$cohort[df$p03 > 37 & df$p03 < 41]= "38-40"
  df$cohort[df$p03 > 40 & df$p03 < 44]= "41-43"
  df$cohort[df$p03 > 43 & df$p03 < 47]= "44-46"
  df$cohort[df$p03 > 46 & df$p03 < 50]= "47-49"
  df$cohort[df$p03 > 49 & df$p03 < 53]= "50-52"
  df$cohort[df$p03 > 52 & df$p03 < 56]= "53-55"
  df$cohort[df$p03 > 55 & df$p03 < 59]= "56-58"
  df$cohort[df$p03 > 58 & df$p03 < 62]= "59-61"
  df$cohort[df$p03 > 61 & df$p03 < 65]= "62-64"
  df$cohort[df$p03 > 64]= "65 or more"
  
  #Migrant
  df$migrant<- rep(0,length(df$p15aa))
  df$migrant[df$p15aa == 2  | df$p15aa == 3 ]=1
  
  print("si se puede")
  
  myvars <- df[,c("employment",     "employer",       "employee",       "informal",       "informal_lt",   
                  "informal_up","informal_per_house" , "formal", "formal_per_house",  "period",         "city",           "pean",           "unemp",          "tot_labour_income", "h_tot_labour_income_pc","h_tot_net_income_pc" ,    
                  "wage_m_1",    "wage_inkind_1",   "wage_1", "wage_m_2",   "wage_inkind_2",   "wage_2",  "tot_wage", "business",
                  "capital",        "taxes",                     "stransfers",    "donations", "remittances",       
                  "tot_net_income",        "redist",    "education",      "school",         "highschool",    
                  "university",     "ap",             "education_y",    "illiterate",     "age", "little_kid", "kid", "teen",
                  "id_hogar", "h_little_kids","h_kid","h_teen","h_young", "young", "h_adult",
                  "rama2",         
                  "agriculture",   "manufacturing",  "construction",   "retail",         "information",    "financial",    
                  "realestate",     "scientific",     "public",         "other_services",
                  "occupation",   "managers",  "professionals", "technicians", "clerical_support", "services_and_sales", 
                  "skilled_agricultural", "craft_related_trades", "plant_machine_operators", "elementary_occupations", "armed_forces",
                  "ethny",          "indigenous",    
                  "afro",           "mestizo",       "noindigenous",   "sex",            "province",       "unempx",      
                  "peanx",          "unempr",         "informal_rate",  "GGini",          "GTheil",         "GGini_h",       
                  "GTheil_h",       "GGini_hpc",      "GTheil_hpc",     "gini" ,          "gini_h",         "gini_hpc",      
                  "quintil","id_hogar","h_tot_labour_income", "h_tot_net_income",      "nhousehold.y", "cohort_17_19", "cohort", "migrant","student_employment","assist_c", "rural" ,"educ_house")]
  
  print("si se pudo")
  
  return(myvars)
  
  
}
