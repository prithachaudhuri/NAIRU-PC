## Clean CPS-MORG 1979-2019

library(pacman) # package to install and load packages with one command
p_load(tidyverse,lubridate,xml2,rvest,ggplot2,foreign)
set.seed(123)

#----------Clean MORG files-------------------------------------------------

## 1979-1982
# esr, grateat, class, occ70
group1 <- paste0("morg", c(79:82))
list1 <- list()
for (i in group1) {
  list1[[i]] <- read.dta(paste0("E:/MP shocks/R Data/data/",i,".dta"),
                         convert.dates = T,
                         convert.factors = F,
                         missing.type = F,
                         convert.underscore = F, 
                         warn.missing.labels = T) %>% 
    filter(age %in% c(25:65)) %>% 
    mutate(empl = case_when(esr %in% c(1,2) ~ "E", 
                            esr %in% c(4:7) ~ "NILF",
                            TRUE ~ "U"),
           skill = case_when(gradeat > 16 ~ "Bachelors",
                             gradeat %in% c(13:16) ~ "Somecoll",
                             gradeat == 12 ~ "HSgrad",
                             gradeat < 12 ~ "LessHS"),
           worker = case_when(class %in% c(5,6) ~ "self",
                              TRUE ~ "paid"),
           race = case_when(race == 1 ~ "White",
                            race == 2 ~ "Black",
                            TRUE ~ "Other"))%>% 
    select("year", "intmonth", "age", "sex", "race", "weight",
           "earnwt" , "skill", "earnwke", "earnhre", "paidhre",
           "worker", "empl", "uhourse", "hourslw", "occ"="occ70")
  cat(i, "\n")
}

## 1983-1988
# esr, gradeat, class, occ80
group2 <- paste0("morg",c(83:88))
list2 <- list()
for (i in group2) {
  list2[[i]] <- read.dta(paste0("E:/MP shocks/R Data/data/",i,".dta"),
                         convert.dates = TRUE, 
                         convert.factors = FALSE,
                         missing.type = FALSE,
                         convert.underscore = FALSE, 
                         warn.missing.labels = TRUE) %>%
    filter(age %in% c(25:65)) %>% 
    mutate(empl = case_when(esr %in% c(1,2) ~ "E", 
                            esr %in% c(4:7) ~ "NILF",
                            TRUE ~ "U"),
           skill = case_when(gradeat > 16 ~ "Bachelors",
                             gradeat %in% c(13:16) ~ "Somecoll",
                             gradeat == 12 ~ "HSgrad",
                             gradeat < 12 ~ "LessHS"),
           worker = case_when(class %in% c(5,6) ~ "self",
                              TRUE ~ "paid"),
           race = case_when(race == 1 ~ "White",
                            race == 2 ~ "Black",
                            TRUE ~ "Other")) %>% 
    select("year", "intmonth", "age", "sex", "race", "weight",
           "earnwt" , "skill", "earnwke", "earnhre", "paidhre",
           "worker", "empl", "uhourse", "hourslw", "occ"="occ80")
  cat(i, "\n")
}

## 1989-1991
# lfsr89, gradeat, class, occ80
group3 <- paste0("morg",c(89:91))
list3 <- list()
for (i in group3) {
  list3[[i]] <- read.dta(paste0("E:/MP shocks/R Data/data/",i,".dta"), 
                         convert.dates = TRUE, 
                         convert.factors = FALSE,
                         missing.type = FALSE,
                         convert.underscore = FALSE, 
                         warn.missing.labels = TRUE) %>% 
    filter(age %in% c(25:65)) %>% 
    mutate(empl = case_when(lfsr89 %in% c(1,2) ~ "E", 
                            lfsr89 %in% c(3:4) ~ "U",
                            TRUE ~ "NILF"),
           skill = case_when(gradeat > 16 ~ "Bachelors",
                             gradeat %in% c(13:16) ~ "Somecoll",
                             gradeat == 12 ~ "HSgrad",
                             gradeat < 12 ~ "LessHS"),
           worker = case_when(class %in% c(5,6) ~ "self",
                              TRUE ~ "paid"),
           race = case_when(race == 1 ~ "White", 
                            race == 2 ~ "Black", 
                            race == 3 ~ "Native",
                            race == 4 ~ "Asian/Pacific",
                            TRUE ~ "Other")) %>% 
    select("year", "intmonth", "age", "sex", "race", "weight",
           "earnwt" , "skill", "earnwke", "earnhre", "paidhre",
           "worker", "empl", "uhourse", "hourslw", "occ"="occ80")
  cat(i, "\n")
}

## 1992-93
# lfsr89, grade92, class, occ80
group4 <- paste0("morg",c(92:93))
list4 <- list()
for (i in group4) {
  list4[[i]] <- read.dta(paste0("E:/MP shocks/R Data/data/",i,".dta"), 
                         convert.dates = TRUE, 
                         convert.factors = FALSE,
                         missing.type = FALSE,
                         convert.underscore = FALSE, 
                         warn.missing.labels = TRUE) %>% 
    filter(age %in% c(25:65)) %>% 
    mutate(empl = case_when(lfsr89 %in% c(1,2) ~ "E", 
                            lfsr89 %in% c(3:4) ~ "U",
                            TRUE ~ "NILF"),
           skill = case_when(grade92 >= 43 ~ "Bachelors",
                             grade92 %in% c(40:42) ~ "Somecoll",
                             grade92 == 39 ~ "HSgrad",
                             grade92 < 39 ~ "LessHS"),
           worker = case_when(class %in% c(5,6) ~ "self",
                              TRUE ~ "paid"), 
           race = case_when(race == 1 ~ "White", 
                            race == 2 ~ "Black", 
                            race == 3 ~ "Native",
                            race == 4 ~ "Asian/Pacific",
                            TRUE ~ "Other")) %>% 
    select("year", "intmonth", "age", "sex", "race", "weight",
           "earnwt" , "skill", "earnwke", "earnhre", "paidhre",
           "worker", "empl", "uhourse", "hourslw", "occ"="occ80")
  cat(i, "\n")
}

## 1994-1999
# lfsr94, grade92, class94, occ80 
group5 <- paste0("morg",c(94:99))
list5 <- list()
for (i in group5) {
  list5[[i]] <- read.dta(paste0("E:/MP shocks/R Data/data/",i,".dta"), 
                         convert.dates = TRUE, 
                         convert.factors = FALSE,
                         missing.type = FALSE,
                         convert.underscore = FALSE, 
                         warn.missing.labels = TRUE) %>% 
    filter(age %in% c(25:65)) %>% 
    mutate(empl = case_when(lfsr94 %in% c(1,2) ~ "E", 
                            lfsr94 %in% c(3:4) ~ "U",
                            TRUE ~ "NILF"),
           skill = case_when(grade92 >= 43 ~ "Bachelors",
                             grade92 %in% c(40:42) ~ "Somecoll",
                             grade92 == 39 ~ "HSgrad",
                             grade92 < 39 ~ "LessHS"),
           worker = case_when(class94 %in% c(6,7) ~ "self",
                              TRUE ~ "paid"),
           race = case_when(race == 1 ~ "White", 
                            race == 2 ~ "Black", 
                            race == 3 ~ "Native",
                            race == 4 ~ "Asian/Pacific",
                            TRUE ~ "Other")) %>% 
    select("year", "intmonth", "age", "sex", "race", "weight",
           "earnwt" , "skill", "earnwke", "earnhre", "paidhre",
           "worker", "empl", "uhourse", "hourslw", "occ"="occ80")
  cat(i, "\n")
}

## 2000-2018
# lfsr94, grade92, class94, docc00
group6 <- paste0("morg",c(0:18))
list6 <- list()
for (i in group6) {
  list6[[i]] <- read.dta(paste0("E:/MP shocks/R Data/data/",i,".dta"), 
                         convert.dates = TRUE, 
                         convert.factors = FALSE,
                         missing.type = FALSE,
                         convert.underscore = FALSE, 
                         warn.missing.labels = TRUE) %>%
    filter(age %in% c(25:65)) %>% 
    mutate(empl = case_when(lfsr94 %in% c(1,2) ~ "E", 
                            lfsr94 %in% c(3:4) ~ "U",
                            TRUE ~ "NILF"),
           skill = case_when(grade92 >= 43 ~ "Bachelors",
                             grade92 %in% c(40:42) ~ "Somecoll",
                             grade92 == 39 ~ "HSgrad",
                             grade92 < 39 ~ "LessHS"),
           worker = case_when(class94 %in% c(6,7) ~ "self",
                              TRUE ~ "paid"),
           race = case_when(race == 1 ~ "White", 
                            race == 2 ~ "Black", 
                            race == 3 ~ "Native", 
                            race %in% c(4:5) ~ "Asian/Pacific",
                            TRUE ~ "Other")) %>% 
    select("year", "intmonth", "age", "sex", "race", "weight",
           "earnwt" , "skill", "earnwke", "earnhre", "paidhre",
           "worker", "empl", "uhourse", "hourslw", "occ"="docc00")
  cat(i, "\n")
}

cpsdata_pc <- bind_rows(bind_rows(list1), bind_rows(list2), 
                        bind_rows(list3), bind_rows(list4), 
                        bind_rows(list5), bind_rows(list6)) %>% 
  mutate(quarter = case_when(intmonth %in% c(1:3) ~ 1,
                             intmonth %in% c(4:6) ~ 2,
                             intmonth %in% c(7:9) ~ 3,
                             intmonth %in% c(10:12) ~ 4))

write.csv(cpsdata_pc, file = "data/cps1979_2018.csv")
 

#----------Education levels-------------------------------------------------
# Calculate unemployment levels and fraction of each type in population
# Quarterly averages

cpseducation <- cpsdata_pc %>% 
  filter(empl!= "NILF") %>% 
  group_by(skill, empl, year, intmonth) %>% 
  summarize(n = sum(weight)) %>% 
  group_by(skill, year, intmonth) %>% 
  mutate(unemp = 100*n/sum(n)) %>% 
  filter(empl == "U") %>% 
  select(skill, year, intmonth, unemp) %>% 
  ungroup() %>% 
  mutate(skill = case_when(skill == "Bachelors" ~ "uBachelors",
                           skill == "Somecoll" ~ "uSomecoll",
                           skill == "HSgrad" ~ "uHSgrad",
                           skill == "LessHS" ~ "uLessHS")) %>% 
  spread(skill, unemp) %>% 
  left_join(cpsdata_pc %>%
              group_by(skill, year, intmonth) %>% 
              summarise(n = n()) %>% 
              group_by(year, intmonth) %>% 
              mutate(pop = 100*n/sum(n)) %>%
              select(skill, year, intmonth, pop) %>% 
              ungroup() %>% 
              mutate(skill = case_when(skill == "Bachelors" ~ "popBachelors",
                                       skill == "Somecoll" ~ "popSomecoll",
                                       skill == "HSgrad" ~ "popHSgrad",
                                       skill == "LessHS" ~ "popLessHS")) %>% 
              spread(skill, pop)) %>% 
  mutate(quarter = case_when(intmonth %in% c(1:3) ~ 1,
                             intmonth %in% c(4:6) ~ 2,
                             intmonth %in% c(7:9) ~ 3,
                             intmonth %in% c(10:12) ~ 4)) %>% 
  group_by(year, quarter) %>% 
  summarise(qBachelors = mean(uBachelors), 
            qSomecoll = mean(uSomecoll), 
            qHSgrad = mean(uHSgrad), 
            qLessHS = mean(uLessHS),
            qpopBachelors = mean(popBachelors), 
            qpopSomecoll = mean(popSomecoll), 
            qpopHSgrad = mean(popHSgrad), 
            qpopLessHS = mean(popLessHS)) 

write.csv(cpseducation, file = "cspedu_quarterly_1979_2018.csv")

# test <- cpsdata_pc %>% 
#   group_by(skill, year, intmonth) %>% 
#   summarise(n = n()) %>% 
#   group_by(year, intmonth) %>% 
#   mutate(pop = 100*n/sum(n)) %>%
#   select(skill, year, intmonth, pop) %>% 
#   ungroup() %>% 
#   mutate(skill = case_when(skill == "Bachelors" ~ "popBachelors",
#                            skill == "Somecoll" ~ "popSomecoll",
#                            skill == "HSgrad" ~ "popHSgrad",
#                            skill == "LessHS" ~ "popLessHS")) %>% 
#   spread(skill, pop)
# 
# test %>%
#   filter(year %in% c(1989:2018)) %>% 
#   mutate(ym = sprintf("%d-%02d", year, intmonth)) %>%
#   ggplot(aes(ym, pop, group = race, color = race)) +
#   geom_line() +
#   theme_pri()

#----------Gender----------------------------------------------------------

cpsgender <- cpsdata_pc %>% 
  filter(empl != "NILF") %>% 
  group_by(sex, empl, year, intmonth) %>% 
  summarise(n = sum(weight)) %>% 
  group_by(sex, year, intmonth) %>% 
  mutate(unemp = 100*n/sum(n)) %>% 
  filter(empl == "U") %>% 
  select(sex, year, intmonth, unemp) %>% 
  ungroup() %>% 
  mutate(sex = case_when(sex == 1 ~ "uMen",
                         sex == 2 ~ "uWomen")) %>% 
  spread(sex, unemp) %>% 
  left_join(cpsdata_pc %>% 
              group_by(sex, year, intmonth) %>% 
              summarise(n = n()) %>% 
              group_by(year, intmonth) %>% 
              mutate(pop = 100*n/sum(n)) %>% 
              select(sex, year, intmonth, pop) %>% 
              ungroup() %>% 
              mutate(sex = case_when(sex == 1 ~ "popMen",
                                     sex == 2 ~ "popWomen")) %>% 
              spread(sex, pop)) %>% 
  mutate(quarter = case_when(intmonth %in% c(1:3) ~ 1,
                             intmonth %in% c(4:6) ~ 2,
                             intmonth %in% c(7:9) ~ 3,
                             intmonth %in% c(10:12) ~ 4)) %>% 
  group_by(year, quarter) %>% 
  summarise(qMen = mean(uMen), 
            qWomen = mean(uWomen),
            qpopMen = mean(popMen), 
            qpopWomen = mean(popWomen)) 

write.csv(cpsgender, file = "cpsgender_quarterly_1979_2018.csv")

# test <- cpsdata_pc %>% 
#   group_by(sex, year, intmonth) %>% 
#   summarise(n = n()) %>% 
#   group_by(year, intmonth) %>% 
#   mutate(pop = 100*n/sum(n)) %>% 
#   select(sex, year, intmonth, pop) %>% 
#   ungroup() %>% 
#   mutate(sex = case_when(sex == 1 ~ "popMen",
#                          sex == 2 ~ "popWomen")) %>% 
#   spread(sex, pop)

#----------Race------------------------------------------------------------

cpsrace <- cpsdata_pc %>% 
  filter(empl != "NILF") %>% 
  group_by(race, empl, year, intmonth) %>% 
  summarise(n = sum(weight)) %>% 
  group_by(race, year, intmonth) %>% 
  mutate(unemp = 100*n/sum(n)) %>% 
  filter(empl == "U") %>% 
  select(race, year, intmonth, unemp) %>% 
  ungroup() %>% 
  mutate(race = case_when(race == "White" ~ "uWhite",
                          race == "Black" ~ "uBlack",
                          race == "Native" ~ "uNative",
                          race == "Asian/Pacific" ~ "uAsianPacific",
                          race == "Other" ~ "uOther")) %>%
  spread(race, unemp) %>% 
  left_join(cpsdata_pc %>% 
              group_by(race, year, intmonth) %>% 
              summarise(n = n()) %>% 
              group_by(year, intmonth) %>% 
              mutate(pop = 100*n/sum(n)) %>% 
              select(race, year, intmonth, pop) %>% 
              ungroup() %>% 
              mutate(race = case_when(race == "White" ~ "popWhite",
                                      race == "Black" ~ "popBlack",
                                      race == "Native" ~ "popNative",
                                      race == "Asian/Pacific" ~ "popAsianPacific",
                                      race == "Other" ~ "popOther")) %>%
              spread(race, pop)) %>% 
  mutate(quarter = case_when(intmonth %in% c(1:3) ~ 1,
                             intmonth %in% c(4:6) ~ 2,
                             intmonth %in% c(7:9) ~ 3,
                             intmonth %in% c(10:12) ~ 4)) %>% 
  group_by(year, quarter) %>% 
  summarise(qWhite = mean(uWhite), 
            qBlack = mean(uBlack),
            qNative = mean(uNative),
            qAsianPacific = mean(uAsianPacific),
            qOther = mean(uOther),
            qpopWhite = mean(popWhite), 
            qpopBlack = mean(popBlack),
            qpopNative = mean(popNative),
            qpopAsianPacific = mean(popAsianPacific),
            qpopOther = mean(popOther)) 

write.csv(cpsrace, file = "cpsrace_quarterly_1979_2018.csv")

# test <- cpsdata_pc %>% 
#   group_by(race, year, intmonth) %>% 
#   summarise(n = n()) %>% 
#   group_by(year, intmonth) %>% 
#   mutate(pop = 100*n/sum(n)) %>% 
#   select(race, year, intmonth, pop) %>% 
#   ungroup() %>% 
#   mutate(race = case_when(race == "White" ~ "popWhite",
#                           race == "Black" ~ "popBlack",
#                           race == "Native" ~ "popNative",
#                           race == "Asian/Pacific" ~ "popAsian/Pacific",
#                           race == "Other" ~ "popOther")) %>%
#   spread(race, pop)
  
