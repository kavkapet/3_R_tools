library(readxl)
library(writexl)
library(dplyr)
library(purrr)
library(tidyr)

library(data.table)
library(ggplot2)

setwd("D:/2_granty_projekty/2_Bezici/2024_LIFE/01_reseni_projektu/04_final_agristructure_prvky2/")


#### PRVNI PULKA O ZDALENSOTI HEX
#xx = read.csv("runoff_sediment_intervals_20240925_en.csv",  sep = ";")
# případně nainstaluj:
# 1) Načtení CSV tabulky
# uprav si název souboru a oddělovač podle potřeby
tab <- read.csv("ZB_hex_Elim_e.csv", stringsAsFactors = FALSE)
#readjoinORP <- read.csv("KU_ORP_ExportTable.csv", sep = ";", stringsAsFactors = FALSE)
#readKU_ORP <- read.csv("KU_ORP2.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "Windows-1250")
#readORP <- read.csv("ORP2.csv", sep = ";", stringsAsFactors = FALSE)

# 2) Přepsání NA ve sloupci "Okoli" na 0
cols_to_zero <- c(
  "Urban",
  "Silnice_vedlejsi",
  "Silnice_hlavni",
  "Cesta_nezpevnena",
  "Vege_lin",
  "Voda",
  "Okoli",
  "Les"
)

# 3) Přepsání NA na 0 ve vybraných sloupcích
tab <- tab %>%
  mutate(across(all_of(cols_to_zero), ~ ifelse(is.na(.), 0, .)))

# 3) Uložení upravené tabulky jako .rds
saveRDS(tab, "tabulka_okoli.rds")

tab1 = readRDS("tabulka_okoli.rds")
# 4) Sumární tabulka podle "KOD_KU" a "Okoli" – součet SHAPE_area
sumar <- tab1 %>%
  group_by(KOD_KU, Okoli) %>%
  summarise(SHAPE_Area = sum(SHAPE_Area, na.rm = TRUE), .groups = "drop")

sumcr <- tab1 %>%
  group_by(Okoli) %>%
  summarise(SHAPE_Area = sum(SHAPE_Area, na.rm = TRUE), .groups = "drop")

# 5) (volitelné) Uložení výsledku do CSV
write.csv(sumcr, "sumcr.csv", row.names = FALSE)


# pro jistotu vezmeme jen potřebné sloupce
readjoinORP_sub <- readjoinORP %>%
  select(KOD_KU, ORP_ja_KOD) %>%
  distinct()   # kdyby tam byly duplicitní KOD_KU

# připojení k hlavní tabulce "table"
tab1 <- tab1 %>%
  left_join(readjoinORP_sub, by = "KOD_KU")

tab1 <- tab1 %>%
  mutate(
    skupina = case_when(
      Silnice_hlavni > 0 | Urban > 0 ~ "nepřírodní",
      Silnice_vedlejsi > 0          ~ "polopřírodní",
      TRUE                          ~ "přírodní"
    )
  )
fwrite(tab1, "sumar_okoli_KOD_KU.csv", row.names = FALSE)


# 1) Nejdřív sečteme SHAPE_Area pro kombinace KOD_KU × Okoli
tab1_ORP <- tab1 %>%
  group_by(ORP_ja_KOD, Okoli) %>%
  summarise(SHAPE_Area = sum(SHAPE_Area, na.rm = TRUE), .groups = "drop")

tab1_KU <- tab1 %>%
  group_by(KOD_KU, Okoli) %>%
  summarise(SHAPE_Area = sum(SHAPE_Area, na.rm = TRUE), .groups = "drop")

tab1_ORP_main_group <- tab1 %>%
  group_by(ORP_ja_KOD, skupina) %>%
  summarise(SHAPE_Area = sum(SHAPE_Area, na.rm = TRUE), .groups = "drop")

tab1_KU_main_group <- tab1 %>%
  group_by(KOD_KU, skupina) %>%
  summarise(SHAPE_Area = sum(SHAPE_Area, na.rm = TRUE), .groups = "drop")



tab1_ORP_main_group_wide <- tab1_ORP_main_group %>%
  pivot_wider(
    id_cols    = ORP_ja_KOD,          # podle čeho má být 1 řádek
    names_from = skupina,             # z čeho se stanou názvy sloupců
    values_from = -c(ORP_ja_KOD, skupina),  # všechno ostatní zachovej jako hodnoty
    values_fill = 0                   # prázdné kombinace doplň nulou (můžeš klidně vynechat)
  )

tab1_ORP_wide <- tab1_ORP %>%
  pivot_wider(
    id_cols    = ORP_ja_KOD,          # podle čeho má být 1 řádek
    names_from = Okoli,             # z čeho se stanou názvy sloupců
    values_from = -c(ORP_ja_KOD, Okoli),  # všechno ostatní zachovej jako hodnoty
    values_fill = 0                   # prázdné kombinace doplň nulou (můžeš klidně vynechat)
  )



tab1_KU_main_group_wide <- tab1_KU_main_group %>%
  pivot_wider(
    id_cols    = KOD_KU,          # podle čeho má být 1 řádek
    names_from = skupina,             # z čeho se stanou názvy sloupců
    values_from = -c(KOD_KU, skupina),  # všechno ostatní zachovej jako hodnoty
    values_fill = 0                   # prázdné kombinace doplň nulou (můžeš klidně vynechat)
  )

tab1_KU_wide <- tab1_KU %>%
  pivot_wider(
    id_cols    = KOD_KU,          # podle čeho má být 1 řádek
    names_from = Okoli,             # z čeho se stanou názvy sloupců
    values_from = -c(KOD_KU, Okoli),  # všechno ostatní zachovej jako hodnoty
    values_fill = 0                   # prázdné kombinace doplň nulou (můžeš klidně vynechat)
  )








fwrite(tab1_ORP_wide, "HEXtab1_ORP_okoli.csv", row.names = FALSE)
fwrite(tab1_KU_wide, "HEXtab1_KU_Okoli.csv", row.names = FALSE)
fwrite(tab1_ORP_main_group_wide, "HEXtab1_ORP_okoli_main_group.csv", row.names = FALSE)
fwrite(tab1_KU_main_group_wide, "HEXtab1_KU_main_group.csv", row.names = FALSE)
write.table(tab1_KU_wide, "pokus.txt")

# 2) Sloupcový graf: X = KOD_KU, Y = SHAPE_Area, barvy podle Okoli
ggplot(tab1_sum, aes(x = factor(ORP_ja_KOD),
                     y = SHAPE_Area,
                     fill = factor(Okoli))) +
  geom_col() +
  labs(x = "ORP",
       y = "SHAPE_Area",
       fill = "Okoli") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 3) Nejdřív sečteme SHAPE_Area pro kombinace KOD_KU × Okoli


tab2_sum <- tab1 %>%
  group_by(ORP_ja_KOD, skupina) %>%
  summarise(SHAPE_Area = sum(SHAPE_Area, na.rm = TRUE), .groups = "drop")

tab2_sumKU <- tab1 %>%
  group_by(KOD_KU, skupina) %>%
  summarise(SHAPE_Area = sum(SHAPE_Area, na.rm = TRUE), .groups = "drop")


# 2) Sloupcový graf: X = KOD_KU, Y = SHAPE_Area, barvy podle Okoli
ggplot(tab2_sum, aes(x = factor(ORP_ja_KOD),
                     y = SHAPE_Area,
                     fill = factor(skupina))) +
  geom_col() +
  labs(x = "ORP",
       y = "SHAPE_Area",
       fill = "Skupina") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
write.csv(tab2_sum, "sumar_okoli_ORP.csv", row.names = FALSE)
write.csv(tab2_sumKU, "sumar_okoli_KU.csv", row.names = FALSE)



#### DRUHA PULKA - summrry KU ORP




KU_ORP_les <- read.csv("KU_ORP_Les2.csv", sep = ",", stringsAsFactors = FALSE, , fileEncoding = "UTF-8")
KU_ORP_Lpis <- read.csv("KU_ORP_Lpis.csv", sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
KU_ORP_Orna <- read.csv("KU_ORP_Orna.csv", sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
KU_ORP_ZB_erase <- read.csv("KU_ORP_ZB_erase.csv", sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
KU_ORP_Urban <- read.csv("KU_ORP_Urban.csv", sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
KU_ORP_bio <- read.csv("KU_ORP_ZB_bio.csv", sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

layers <- c(
  "KU_ORP_les",
  "KU_ORP_Lpis",
  "KU_ORP_Orna",
  "KU_ORP_ZB_erase",
  "KU_ORP_Urban",
  "KU_ORP_bio"
)


summ_list <- list()


#toto udela sumarry podle KOD_ORP a ploch do listu
for (nm in layers) {
  df <- get(nm)
  
  # součet SHAPE_Area podle KOD_ORP
  s <- aggregate(
    SHAPE_Area ~ KOD_ORP,
    data = df,
    FUN = sum,
    na.rm = TRUE
  )
  
  # přejmenujeme sloupec SHAPE_Area na název vrstvy
  names(s)[names(s) == "SHAPE_Area"] <- nm
  
  summ_list[[nm]] <- s
}

tabORP <- Reduce(function(x, y) merge(x, y, by = "KOD_ORP", all = TRUE),
                 summ_list)




orp_total <- readKU_ORP %>%
  group_by(KOD_ORP) %>%
  summarise(SHAPE_Area = sum(SHAPE_Area, na.rm = TRUE), .groups = "drop")

tabORP_ratio <- orp_total %>%
  left_join(tabORP, by = "KOD_ORP") %>%
  mutate(
    across(
      starts_with("KU_ORP_"),
      ~ .x / SHAPE_Area,
      .names = "{.col}_ratio"
    )
  )


#toto udela sumarry pro katastry KU_ORP a ploch do listu
summ_listKU <- list()


for (nm in layers) {
  df <- get(nm)
  
  # součet SHAPE_Area podle KOD_ORP
  s <- aggregate(
    SHAPE_Area ~ KOD_KU,
    data = df,
    FUN = sum,
    na.rm = TRUE
  )
  
  # přejmenujeme sloupec SHAPE_Area na název vrstvy
  names(s)[names(s) == "SHAPE_Area"] <- nm
  
  summ_listKU[[nm]] <- s
}

tabKU_ORP <- Reduce(function(x, y) merge(x, y, by = "KOD_KU", all = TRUE),
                 summ_listKU)

ku_total <- readKU_ORP %>%
  select(KOD_KU, SHAPE_Area)

# připojit k tabKU_ORP
tabKU_ORP_ratio <- ku_total %>%
  left_join(tabKU_ORP, by = "KOD_KU") %>%
  # spočítat poměr pro všechna pole KU_ORP_* vůči SHAPE_Area
  mutate(
    across(
      starts_with("KU_ORP_"),
      ~ .x / SHAPE_Area,
      .names = "{.col}_ratio"
    )
  )

#Suppary pro mapovani biotopu

# zdrojová vrstva
df <- KU_ORP_bio

# najít sloupec s plochou (Shape_Area / SHAPE_area / apod.)
shape_col <- grep("shape.*area", names(df), ignore.case = TRUE, value = TRUE)[1]

# summary podle KOD_ORP a FSB
bio_summary <- aggregate(
  x  = df[[shape_col]],
  by = list(KOD_ORP = df$KOD_ORP,
            FSB     = df$FSB),
  FUN = sum,
  na.rm = TRUE
)

bio_summary_wide <- bio_summary %>%
  pivot_wider(
    id_cols    = KOD_ORP,          # podle čeho má být 1 řádek
    names_from = FSB,             # z čeho se stanou názvy sloupců
    values_from = -c(KOD_ORP, FSB),  # všechno ostatní zachovej jako hodnoty
    values_fill = 0                   # prázdné kombinace doplň nulou (můžeš klidně vynechat)
  )

# přejmenujeme sum-sloupec na název vrstvy
names(bio_summary)[3] <- "KU_ORP_bio"

# summary podle KU a FSB
bio_summaryKU <- aggregate(
  x  = df[[shape_col]],
  by = list(KOD_KU = df$KOD_KU,
            FSB     = df$FSB),
  FUN = sum,
  na.rm = TRUE
)

bio_summaryKU_wide <- bio_summaryKU %>%
  pivot_wider(
    id_cols    = KOD_KU,          # podle čeho má být 1 řádek
    names_from = FSB,             # z čeho se stanou názvy sloupců
    values_from = -c(KOD_KU, FSB),  # všechno ostatní zachovej jako hodnoty
    values_fill = 0                   # prázdné kombinace doplň nulou (můžeš klidně vynechat)
  )

# přejmenujeme sum-sloupec na název vrstvy
names(bio_summaryKU)[3] <- "KU_ORP_bio"

fwrite(tabORP_ratio, 'ORP_plochy.csv')
fwrite(tabKU_ORP_ratio, 'KU_ORP_plochy.csv')
fwrite(bio_summary_wide, 'map_bio_prunik_summaryORP.csv')
fwrite(bio_summaryKU_wide, 'map_bio_prunik_summaryKU.csv')

