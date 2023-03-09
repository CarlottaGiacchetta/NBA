library(dplyr)

#
st.21.22 = read.csv("Player_21_22.csv", sep=",", dec = ".",
                        stringsAsFactors=TRUE, na.strings=c("NA","NaN", " ", " NA's "))
st.21.22$Player.additional = NULL
st.21.22$Rk = NULL

sa.22.23 = read.csv("Salary_22_23.csv", sep=",", dec = ".",
                    stringsAsFactors=TRUE, na.strings=c("NA","NaN", " ", " NA's ")) 
sa.22.23$Rk = NULL
sa.22.23$Tm = NULL
sa.22.23$X2023.24 = NULL
sa.22.23$X2024.25 = NULL
sa.22.23$X2025.26 = NULL
sa.22.23$X2026.27 = NULL
sa.22.23$X2027.28 = NULL
sa.22.23$Guaranteed = NULL
sa.22.23$X.9999 = NULL

st.21.22 = inner_join(st.21.22, sa.22.23, by = "Player")
st.21.22$Year = rep("2021-22", nrow(st.21.22))
colnames(st.21.22)[30] = "Sal"
st.21.22$Sal = as.integer(st.21.22$Sal)
st.21.22$Player = as.character(st.21.22$Player)

#

st.20.21 = read.csv("Player_20_21.csv", sep=",", dec = ".",
                    stringsAsFactors=TRUE, na.strings=c("NA","NaN", " ", " NA's "))
st.20.21$Player.additional = NULL
st.20.21$Rk = NULL

sa.21.22 = read.csv("Salary_21_22.csv", sep=",", dec = ".",
                    stringsAsFactors=TRUE, na.strings=c("NA","NaN", " ", " NA's ")) 
sa.21.22$del = NULL

st.20.21 = inner_join(st.20.21, sa.21.22, by = "Player")
st.20.21$Year = rep("2020-21", nrow(st.20.21))
colnames(st.20.21)[30] = "Sal"
st.20.21$Player = as.character(st.20.21$Player)

players = rbind(st.21.22, st.20.21)

#
st.19.20 = read.csv("Player_19_20.csv", sep=",", dec = ".",
                    stringsAsFactors=TRUE, na.strings=c("NA","NaN", " ", " NA's "))
st.19.20$Player.additional = NULL
st.19.20$Rk = NULL

sa.20.21 = read.csv("Salary_20_21.csv", sep=",", dec = ".",
                    stringsAsFactors=TRUE, na.strings=c("NA","NaN", " ", " NA's ")) 
sa.20.21$del = NULL
sa.20.21$n = NULL

st.19.20 = inner_join(st.19.20, sa.20.21, by = "Player")
st.19.20$Year = rep("2019-20", nrow(st.19.20))
colnames(st.19.20)[30] = "Sal"
st.19.20$Player = as.character(st.19.20$Player)

players = rbind(players, st.19.20)
#
#
st.18.19 = read.csv("Player_18_19.csv", sep=",", dec = ".",
                    stringsAsFactors=TRUE, na.strings=c("NA","NaN", " ", " NA's "))
st.18.19$Player.additional = NULL
st.18.19$Rk = NULL

sa.19.20 = read.csv("Salary_19_20.csv", sep=",", dec = ".",
                    stringsAsFactors=TRUE, na.strings=c("NA","NaN", " ", " NA's ")) 
sa.19.20$del = NULL
sa.19.20$n = NULL

st.18.19 = inner_join(st.18.19, sa.19.20, by = "Player")
st.18.19$Year = rep("2018-19", nrow(st.18.19))
colnames(st.18.19)[30] = "Sal"
st.18.19$Player = as.character(st.18.19$Player)

players = rbind(players, st.18.19)
#
#
st.17.18 = read.csv("Player_17_18.csv", sep=",", dec = ".",
                    stringsAsFactors=TRUE, na.strings=c("NA","NaN", " ", " NA's "))
st.17.18$Player.additional = NULL
st.17.18$Rk = NULL

sa.18.19 = read.csv("Salary_18_19.csv", sep=",", dec = ".",
                    stringsAsFactors=TRUE, na.strings=c("NA","NaN", " ", " NA's ")) 
sa.18.19$del = NULL
sa.18.19$n = NULL

st.17.18 = inner_join(st.17.18, sa.18.19, by = "Player")
st.17.18$Year = rep("2017-18", nrow(st.17.18))
colnames(st.17.18)[30] = "Sal"
st.17.18$Player = as.character(st.17.18$Player)

players = rbind(players, st.17.18)

players$Year = as.factor(players$Year)
#
save(players, file = "players.RData")

