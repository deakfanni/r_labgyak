# Libraryk
library(tidyverse)
library(ggplot2)
library(see)
library(dplyr)
library(maps)
library(tidyr)
library(ggthemes)

#Beolvasás
# betoltom az SCSS adathalmaz kulonbozo reszeit a dplace-rol:
# a konkret adatok: milyen kulturalis kozosseg / torzs milyen tipusu tulajdonsaga milyen erteket vesz fol
data = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/SCCS/data.csv')
# a kulturalis kozossegek / torzsek reszletes adatai
societies = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/SCCS/societies.csv')
# a tulajdonsagok reszletes ismertetoi
variables = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/SCCS/variables.csv')
# a tulajdonsagok ertekeinek a reszletes ismertetoi
codes = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/SCCS/codes.csv')

# az, hogy ki hol lakik, egy masik fajlban van.
locations = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/legacy/society_locations.csv')

# klíma adatok
eco = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/ecoClimate/data.csv')

#Oszlopok szűrése
data = data %>% 
  select(soc_id,var_id,code)

societies = societies %>% 
  select(id,pref_name_for_society,glottocode,Lat,Long)

variables = variables %>% 
  select(id,category,title,definition)  

locations = locations %>% 
  select(dataset,soc_id,region) #megtartom a dataset-et is, hogy később tudjam "javítani" a soc_id oszlopot

eco$eco_id <- eco$var_id # átnevezem a var_id-t, hogy a későbbiekben ne keveredjen a másik var_id-val
eco$eco_code <- eco$code #ugyanez a code-dal
eco = eco %>% 
  select(soc_id,eco_id, eco_code) # csak az új eco_id-t és eco_code-ot tartom meg


# Oszlopok átnevezése
societies = societies %>% 
  rename(soc_id = id) # a data soc_id oszlopa ugyanaz, mint a societies id oszlopa

variables = variables %>% 
  rename(var_id = id) # a data var_id oszlopa ugyanaz, mint a variables id oszlopa

# Összegyúrás egy nagy táblázatba, kombináció kettesével 
# data és variables
d_long = left_join(data,variables)

# új adattábla + societies
d_long = left_join(d_long,societies)

# d_long + codes
d_long = left_join(d_long,codes)

# locations hozzáadása
# a locations esetében újra kell definiálni a soc_id oszlopot, mert a dataset-ben van, 
# hogy SCSS, a soc_id-nál pedig csak a szám, így ezeket mergelem
locations$soc_id <- paste(locations$dataset, locations$soc_id, sep = "")
locations = locations %>% 
  select(soc_id,region) # itt már kivehetjük belőle a dataset oszlopot
d_long = left_join(d_long,locations)

# klíma adatok hozzáadása
d_long = left_join(d_long,eco) 

# 705: settlement type, 767: konfliktus gyakorisága local communityn belül, starvation: 1263, 1723: number of poor
# Sorokra szűrés
d_long = d_long %>% 
  filter(
    var_id %in% c('SCCS767', 'SCCS705', 'SCCS1263', 'SCCS1723'),
    eco_id == 'PrecipitationPredictability'
  )

# Kiíratás
write_tsv(d_long, 'd_long.tsv')

# NA szűrése
d_filtered <- d_long[!is.na(d_long$code), ]

# Típusok átalakítása
d_filtered$code <- as.character(d_filtered$code)


# Ábrákhoz további filterezés
# függvény definiálása
filtering <- function(column, value) {
  d_filtered %>%
    filter({{ column }} == value)
}

starvation <- filtering(title, "Occurrence of Seasonal Starvation")
settlement <- filtering(title, "Settlement Type")
poor <- filtering(title, "Number of Poor")
conflict <- filtering(title, "(No) Conflict (Social or Political) in the Local Community")


# A worldmapekhez a skálákat egységesítettem, ahol több szint volt, ott csináltam négyet a hasonlók/kevés elemszámúak összevonásával
poor$code[poor$code == 10] <- 4
poor$code[poor$code == 20] <- 3
poor$code[poor$code == 21] <- 2
poor$code[poor$code == 22] <- 1

starvation$code[starvation$code == 1] <- 4
starvation$code[starvation$code == 2] <- 3
starvation$code[starvation$code == 3] <- 2 
starvation$code[starvation$code == 4] <- 2 # magas változó nagyon kevés volt, ezért vontam egybe
starvation$code[starvation$code == 5] <- 1

settlement$code[settlement$code == 6] <- 4
settlement$code[settlement$code == 5] <- 4
settlement$code[settlement$code == 4] <- 3 #a felső és középső értékeket egybevontam