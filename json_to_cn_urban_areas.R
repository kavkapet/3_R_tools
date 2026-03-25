library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(tibble)
library(ggplot2)

# nastav kořenový adresář
root_dir <- "d:/2_granty_projekty/2_Bezici/2023_SrUrb/01_reseni_projektu/999_to_run_LU/987_stat/out"

# najdi všechny ndjson soubory
files_ndjson <- list.files(
  path = root_dir,
  pattern = "\\.ndjson$",
  recursive = TRUE,
  full.names = TRUE
)

cat("Počet nalezených souborů:", length(files_ndjson), "\n")

# výstupní soubor
out_file <- file("ALL_MERGED.ndjson", open = "w")

for (f in files_ndjson) {
  cat("Zpracovávám:", f, "\n")
  
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  
  writeLines(lines, out_file)
}

close(out_file)

cat("Hotovo – vytvořen soubor ALL_MERGED.ndjson\n")




`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

#--------------------------------------------
# 1) Načtení NDJSON po řádcích se zachováním listů
#--------------------------------------------
#setwd("s:/_PROJEKTY/2019_StrazkyII/01_reseni_projektu/02_povodi/5km_povodi/_R")
setwd("d:/2_granty_projekty/2_Bezici/2023_SrUrb/01_reseni_projektu/999_to_run_LU/987_stat/")


#--------------------------------------------
# 1) Načtení NDJSON
#--------------------------------------------
# uprav si cestu k souboru
lines <- readLines("ALL_MERGED.ndjson", encoding = "UTF-8")
data_list <- lapply(lines, function(x) {
  jsonlite::fromJSON(x, simplifyVector = FALSE)
})

# základní přehledová tabulka
df <- tibble(
  StrUAID = map_dbl(data_list, ~ as.numeric(.x$StrUAID %||% NA)),
  ua_area = map_dbl(data_list, ~ as.numeric(.x$ua_area %||% NA)),
  CN_mean = map_dbl(data_list, ~ as.numeric(.x$CN_mean %||% NA)),
  CN_runoff_height = map_dbl(data_list, ~ as.numeric(.x$CN_runoff_height %||% NA)),
  watershed_brut_area = map_dbl(data_list, ~ as.numeric(.x$watershed_brut_area %||% NA)),
  fpg_number = map_dbl(data_list, ~ as.numeric(.x$fpg_number %||% NA)),
  valid_fpg_number = map_dbl(data_list, ~ as.numeric(.x$valid_fpg_number %||% NA)),
  valid_fpg_properties = map(data_list, ~ .x$valid_fpg_properties %||% NULL)
)

#--------------------------------------------
# 2) Funkce pro bezpečný vážený průměr
#--------------------------------------------
wmean_safe <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  
  ok <- !(is.na(x) | is.na(w))
  x <- x[ok]
  w <- w[ok]
  
  if (length(x) == 0 || sum(w) == 0) {
    return(NA_real_)
  }
  
  sum(x * w) / sum(w)
}

#--------------------------------------------
# 3) Kontrola počtu FPG v valid_fpg_properties
#--------------------------------------------
df_check <- df %>%
  mutate(
    valid_fpg_count_real = map_int(valid_fpg_properties, ~ {
      if (is.null(.x) || length(.x) == 0) 0 else length(.x)
    }),
    valid_fpg_match = valid_fpg_count_real == valid_fpg_number
  )

table(df_check$valid_fpg_match, useNA = "ifany")

problems <- df_check %>%
  filter(!valid_fpg_match) %>%
  select(StrUAID, valid_fpg_number, valid_fpg_count_real)

print(problems)

#--------------------------------------------
# 4) Rozbalení valid_fpg_properties do long formátu
#--------------------------------------------
fpg_long <- df %>%
  select(StrUAID, valid_fpg_properties) %>%
  mutate(
    fpg_tbl = map(valid_fpg_properties, function(x) {
      if (is.null(x) || length(x) == 0) {
        return(tibble(
          fpg_id = character(),
          sa_area = numeric(),
          CN_mean_fpg = numeric(),
          CN_runoff_height_fpg = numeric()
        ))
      }
      
      tibble(
        fpg_id = names(x),
        sa_area = map_dbl(x, ~ as.numeric(.x$sa_area %||% NA)),
        CN_mean_fpg = map_dbl(x, ~ as.numeric(.x$CN_mean %||% NA)),
        CN_runoff_height_fpg = map_dbl(x, ~ as.numeric(.x$CN_runoff_height %||% NA))
      )
    })
  ) %>%
  select(StrUAID, fpg_tbl) %>%
  unnest(fpg_tbl)

#--------------------------------------------
# 5) Sumarizace
#--------------------------------------------
fpg_summary <- fpg_long %>%
  group_by(StrUAID) %>%
  summarise(
    sa_area_sum = sum(sa_area, na.rm = TRUE),
    
    CN_mean_weighted_sa_area =
      wmean_safe(CN_mean_fpg, sa_area),
    
    CN_runoff_height_weighted_sa_area =
      wmean_safe(CN_runoff_height_fpg, sa_area),
    
    .groups = "drop"
  )

#--------------------------------------------
# 6) Finální tabulka
#--------------------------------------------
result_table <- df %>%
  select(
    StrUAID,
    ua_area,
    CN_mean,
    CN_runoff_height,
    watershed_brut_area,
    fpg_number,
    valid_fpg_number
  ) %>%
  left_join(fpg_summary, by = "StrUAID")

print(result_table)

result_table$area_ratio = result_table$ua_area / result_table$watershed_brut_area
result_table$areaIN_ratio = result_table$sa_area_sum / result_table$watershed_brut_area
result_table$volume_in = result_table$CN_runoff_height_weighted_sa_area * result_table$sa_area_sum
result_table$volume_out_only = result_table$CN_runoff_height * result_table$ua_area

result_table <- result_table %>%
  mutate(
    ua_area_km2 = ua_area / 1e6,
    ua_size_category = case_when(
      ua_area_km2 < 1   ~ "very_small",
      ua_area_km2 < 3   ~ "small",
      ua_area_km2 < 5   ~ "medium",
      ua_area_km2 < 10  ~ "large",
      ua_area_km2 >= 10 ~ "very_large",
      TRUE ~ NA_character_
    ),
    ua_size_category = factor(
      ua_size_category,
      levels = c("very_small", "small", "medium", "large", "very_large")
    )
  )

result_table %>%
  count(ua_size_category, sort = FALSE)

ggplot(result_table, aes(x = ua_size_category, y = area_ratio)) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    x = "Velikostní třída obce",
    y = "UA area / watershed area",
    title = "Area ratio podle velikosti obce"
  ) +
  theme_bw()
ggplot(result_table, aes(x = ua_size_category, y = areaIN_ratio)) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    x = "Velikostní třída obce",
    y = "FPG area / watershed area",
    title = "AreaIN ratio podle velikosti obce"
  ) +
  theme_bw()

ggplot(result_table, aes(x = ua_size_category, y = CN_runoff_height_weighted_sa_area)) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    x = "Velikostní třída obce",
    y = "CN_runoff_height_weighted_sa_area",
    title = "Volume IN podle velikosti obce"
  ) +
  theme_bw()

ggplot(result_table, aes(x = ua_size_category, y = CN_runoff_height)) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    x = "Velikostní třída obce",
    y = "runoff_height",
    title = "Volume OUT podle velikosti obce"
  ) +
  theme_bw()

#--------------------------------------------
# 7) Export
#--------------------------------------------
write_csv(result_table, "result_table_summary.csv")
