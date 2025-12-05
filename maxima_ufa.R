library(readxl)
library(dplyr)
library(ggplot2)
library(fst)
library(tidyr)
library(readr)   # read_csv / read_delim
library(dplyr)
library(purrr)


# Define the file path
setwd("d:/2_granty_projekty/2_Bezici/2023_SrUrb/01_reseni_projektu/08_blokova_maxima/")
#d:/2_granty_projekty/2_Bezici/2023_SrUrb/01_reseni_projektu/08_blokova_maxima/
file_path <- "Gridovana_Maxima_CR/Blokova_Maxima_1hod.xlsx"
folder_path <- "Gridovana_Maxima_CR"

data_spat = read.csv("gridy_ufa_id_jed_upv.csv", header = TRUE, sep = ",", dec = ".", encoding = "UTF-8")
# Read the first sheet "Pixely" and keep columns A-G
#pixely_data <- read_excel(file_path, sheet = "Pixely", range = "A1:G82738")

# Initialize a dataframe with the "Pixely" data
#final_data <- pixely_data

# Define the folder path containing the files

# List all files with "Blokova_Maxima" in their name and .xlsx extension
file_list <- list.files(path = folder_path, pattern = "Blokova_Maxima.*\\.xlsx$", full.names = TRUE)

# Initialize an empty dataframe for the final data
final_data <- NULL

# Loop through each file
for (file_path in file_list) {
  # Extract the file-specific suffix (everything after the last underscore in the file name)
  file_suffix <- tools::file_path_sans_ext(basename(file_path))
  file_suffix <- sub(".*_(.*)$", "\\1", file_suffix)
  
  # Read the first sheet "Pixely" and keep columns A-G
  pixely_data <- read_excel(file_path, sheet = "Pixely", range = "A1:G82738")
  
  # If final_data is NULL, initialize it with pixely_data
  if (is.null(final_data)) {
    final_data <- pixely_data
  }
  
  # Get the names of all sheets in the current file
  sheet_names <- excel_sheets(file_path)
  
  # Loop through all sheets except the first one
  for (sheet in sheet_names[-1]) {
    # Read the data from the current sheet, keeping columns A-U
    sheet_data <- read_excel(file_path, sheet = sheet, range = "A1:U82738")
    
    # Modify column names to include both the sheet name and file-specific suffix
    colnames(sheet_data)[-1] <- paste0(colnames(sheet_data)[-1], "_", sheet, "_", file_suffix)
    
    # Merge the data based on the "ID" column
    final_data <- merge(final_data, sheet_data, by = "ID", all.x = TRUE)
  }
}

# View the merged dataset
#head(final_data)

all_data <- merge(data_spat, final_data, by.x = "TARGET_FID",by.y = "ID", all = FALSE)
write.fst(all_data, "all_data.fst")

all_data = read.fst("all_data.fst")
all_data_kat = all_data
all_data_upov = all_data
# Optional: Save the final dataset to a new Excel file
# write.xlsx(final_data, "Merged_Data_All_Files.xlsx")

# Function to extract and match suffix patterns from column names
extract_suffix <- function(colnames, delimiter) {
  sapply(strsplit(colnames, delimiter), function(x) paste(tail(x, 2), collapse = "_"))
}

# Extract suffix patterns from column names
suffix_patterns <- extract_suffix(colnames(all_data), "_")

# Initialize a new dataframe to store the calculated means

#usuffix_patterns = unique(suffix_patterns[20:307])
#usuffix_patterns = colnames(all_data[20:307])
usuffix_patterns = unique(suffix_patterns[20:length(suffix_patterns)])


#katastry
all_data_kat <- all_data_kat %>%
  filter(KOD_KU != "")
averages_kat <- data.frame("KOD_KU" = unique(all_data$KOD_KU))
#UPOVS
all_data_upov <- all_data_upov %>%
  filter(UPOV_ID != "")
averages_upov <- data.frame("UPOV_ID" = unique(all_data$UPOV_ID))
#kraje
all_data <- all_data %>%
  filter(NAZ_CZNUTS3 != "")
averages <- data.frame("NAZ_CZNUTS3" = unique(all_data$NAZ_CZNUTS3))
# Loop through unique suffix patterns and calculate means
for (suffix in usuffix_patterns) {
  # Identify columns with matching suffix
  split_text <- strsplit(suffix, "_")[[1]]
  suffix <- paste(tail(split_text, 2), collapse = "_")
  matching_cols <- grep(suffix, names(all_data), value = TRUE)
  #print(matching_cols[1])
  print(suffix)
  #split_text <- strsplit(matching_cols, "_")[[1]]
  #finame <- paste(tail(split_text, 2), collapse = "_")
  
  # Calculate mean for matching columns
  #colname_to_extract <- suffix
  finame <- suffix #sub(".*_(.*_.*)_.*$", "\\1", colname_to_extract)
  avg_finame = paste0("avg_", finame)
  max5_finame = paste0("max5_", finame)
  max_finame = paste0("max_", finame)
  
  avg_data <- all_data %>%
    select("NAZ_CZNUTS3", all_of(matching_cols)) %>%
    group_by(NAZ_CZNUTS3) %>%
    ungroup()
  avg_data[[avg_finame]] = rowMeans(avg_data[, 2:ncol(avg_data)], na.rm = TRUE)
  avg_data <- avg_data %>%
    group_by(NAZ_CZNUTS3) %>%
    summarise(across(-1, ~ mean(.x, na.rm = TRUE)))
  
  max5_data <- all_data %>%
    select("NAZ_CZNUTS3", all_of(matching_cols)[1:5]) %>%
    group_by(NAZ_CZNUTS3) %>%
    ungroup()
  max5_data[[max5_finame]] = rowMeans(max5_data[, 2:6], na.rm = TRUE)
  max5_data <- max5_data %>%
    group_by(NAZ_CZNUTS3) %>%
    summarise(across(-1, ~ max(.x, na.rm = TRUE)))
  
  max_data <- max5_data[,1:2]
  #max_data[[max_finame]] = avg_data[, 2]
  colnames(max_data)[2] = max_finame 
  
  
  # Rename columns based on suffix pattern

  # Merge with the averages dataframe
  averages <- merge(averages, avg_data[,c(1,ncol(avg_data))], by.x = "NAZ_CZNUTS3", by.y = "NAZ_CZNUTS3")
  averages <- merge(averages, max5_data[,c(1,ncol(max5_data))], by.x = "NAZ_CZNUTS3", by.y = "NAZ_CZNUTS3")
  averages <- merge(averages, max_data[,c(1,ncol(max_data))], by.x = "NAZ_CZNUTS3", by.y = "NAZ_CZNUTS3")
}
###UPOVs
for (suffix in usuffix_patterns) {
  # Identify columns with matching suffix
  split_text <- strsplit(suffix, "_")[[1]]
  suffix <- paste(tail(split_text, 2), collapse = "_")
  matching_cols <- grep(suffix, names(all_data), value = TRUE)
  #print(matching_cols[1])
  print(suffix)
  #split_text <- strsplit(matching_cols, "_")[[1]]
  #finame <- paste(tail(split_text, 2), collapse = "_")
  
  # Calculate mean for matching columns
  #colname_to_extract <- suffix
  finame <- suffix #sub(".*_(.*_.*)_.*$", "\\1", colname_to_extract)
  avg_finame = paste0("avg_", finame)
  max5_finame = paste0("max5_", finame)
  max_finame = paste0("max_", finame)
  
  avg_data <- all_data %>%
    select("UPOV_ID", all_of(matching_cols)) %>%
    group_by(UPOV_ID) %>%
    ungroup()
  avg_data[[avg_finame]] = rowMeans(avg_data[, 2:ncol(avg_data)], na.rm = TRUE)
  avg_data <- avg_data %>%
    group_by(UPOV_ID) %>%
    summarise(across(-1, ~ mean(.x, na.rm = TRUE)))
  
  max5_data <- all_data %>%
    select("UPOV_ID", all_of(matching_cols)[1:5]) %>%
    group_by(UPOV_ID) %>%
    ungroup()
  max5_data[[max5_finame]] = rowMeans(max5_data[, 2:6], na.rm = TRUE)
  max5_data <- max5_data %>%
    group_by(UPOV_ID) %>%
    summarise(across(-1, ~ max(.x, na.rm = TRUE)))
  
  max_data <- max5_data[,1:2]
  #max_data[[max_finame]] = avg_data[, 2]
  colnames(max_data)[2] = max_finame 
  
  
  # Rename columns based on suffix pattern
  
  # Merge with the averages dataframe
  averages_upov <- merge(averages_upov, avg_data[,c(1,ncol(avg_data))], by.x = "UPOV_ID", by.y = "UPOV_ID")
  averages_upov <- merge(averages_upov, max5_data[,c(1,ncol(max5_data))], by.x = "UPOV_ID", by.y = "UPOV_ID")
  averages_upov <- merge(averages_upov, max_data[,c(1,ncol(max_data))], by.x = "UPOV_ID", by.y = "UPOV_ID")
}

#katastry
for (suffix in usuffix_patterns) {
  # Identify columns with matching suffix
  split_text <- strsplit(suffix, "_")[[1]]
  suffix <- paste(tail(split_text, 2), collapse = "_")
  matching_cols <- grep(suffix, names(all_data), value = TRUE)
  #print(matching_cols[1])
  print(suffix)
  #split_text <- strsplit(matching_cols, "_")[[1]]
  #finame <- paste(tail(split_text, 2), collapse = "_")
  
  # Calculate mean for matching columns
  #colname_to_extract <- suffix
  finame <- suffix #sub(".*_(.*_.*)_.*$", "\\1", colname_to_extract)
  avg_finame = paste0("avg_", finame)
  max5_finame = paste0("max5_", finame)
  max_finame = paste0("max_", finame)
  
  avg_data <- all_data %>%
    select("KOD_KU", all_of(matching_cols)) %>%
    group_by(KOD_KU) %>%
    ungroup()
  avg_data[[avg_finame]] = rowMeans(avg_data[, 2:ncol(avg_data)], na.rm = TRUE)
  avg_data <- avg_data %>%
    group_by(KOD_KU) %>%
    summarise(across(-1, ~ mean(.x, na.rm = TRUE)))
  
  max5_data <- all_data %>%
    select("KOD_KU", all_of(matching_cols)[1:5]) %>%
    group_by(KOD_KU) %>%
    ungroup()
  max5_data[[max5_finame]] = rowMeans(max5_data[, 2:6], na.rm = TRUE)
  max5_data <- max5_data %>%
    group_by(KOD_KU) %>%
    summarise(across(-1, ~ max(.x, na.rm = TRUE)))
  
  max_data <- max5_data[,1:2]
  #max_data[[max_finame]] = avg_data[, 2]
  colnames(max_data)[2] = max_finame 
  
  
  # Rename columns based on suffix pattern
  
  # Merge with the averages dataframe
  averages_kat <- merge(averages_kat, avg_data[,c(1,ncol(avg_data))], by.x = "KOD_KU", by.y = "KOD_KU")
  averages_kat <- merge(averages_kat, max5_data[,c(1,ncol(max5_data))], by.x = "KOD_KU", by.y = "KOD_KU")
  averages_kat <- merge(averages_kat, max_data[,c(1,ncol(max_data))], by.x = "KOD_KU", by.y = "KOD_KU")
}

write.csv(averages, "averages_kraje.csv")
write.csv(averages_upov, "averages_upov.csv")
write.csv(averages_kat, "averages_kat.csv")


#ffffffffff - statistika bodových maxim

# df = tvůj původní data.frame
# df <- readRDS("...") nebo jak ho načítáš

# --- 1) Vyber sloupce s blokovými maximy a rozbij názvy ---

## ------------------------------------------------------------
## 0) Data
## ------------------------------------------------------------
# tady dosaď svůj objekt s daty:
# d má mít sloupce typu BM1_10min_rok_10min, BM2_..., M1_..., M2_...
# d <- datmax

## ------------------------------------------------------------
## 1) Metainformace o sloupcích s maximy
## ------------------------------------------------------------


rok_idx <- grep("rok", names(all_data))
d_rok <- subset(all_data, select = c(id_pixel, rok_idx))

all_names <- names(d_rok)

# sloupce typu BM1_..., BM2_..., M1_..., M2_...
is_BM    <- grepl("^(BM|M)[0-9]+_", all_names)
bm_names <- all_names[is_BM]

# suffix = část za prvním "_" (např. "10min_rok_10min")
suffix <- sub("^[^_]+_", "", bm_names)

# pořadí maxima 1..20 z prefixu BM1 / M1
ord <- as.integer(gsub("^(BM|M)", "", sub("_.*", "", bm_names)))

meta <- data.frame(
  col    = bm_names,
  suffix = suffix,
  ord    = ord,
  stringsAsFactors = FALSE
)

## ------------------------------------------------------------
## 2) Funkce pro log-lineární trend (lm) a Gumbel
## ------------------------------------------------------------

# log-lineární trend: y = a * ln(x) + b, x = 1..n
fit_llin <- function(y) {
  y <- as.numeric(y)
  x <- seq_along(y)
  mod <- lm(y ~ log(x))
  s   <- summary(mod)
  c(intercept = coef(mod)[1],
    slope     = coef(mod)[2],
    R2        = s$r.squared)
}

# odhad Gumbela z průměru a sd (metoda momentů)
gumbel_fit <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) < 3) return(list(mu = NA_real_, beta = NA_real_))
  gamma <- 0.5772156649               # Eulerova konstanta
  s     <- sd(x)
  m     <- mean(x)
  beta  <- s * sqrt(6) / pi           # scale
  mu    <- m - gamma * beta           # location
  list(mu = mu, beta = beta)
}

# návratové hodnoty pro zadaná T (v letech)
gumbel_return <- function(T, mu, beta) {
  if (is.na(mu) || is.na(beta)) return(rep(NA_real_, length(T)))
  zT <- -log(-log(1 - 1/T))           # redukovaná proměnná
  mu + beta * zT
}

T_vals <- c(2, 5, 10, 20, 50, 100)       # návratová období

## ------------------------------------------------------------
## 3) Smyčka přes suffix (typ maxima) a stanice
## ------------------------------------------------------------

## 3) Smyčka přes suffix (typ maxima) a stanice + tisk názvu stanice

# název sloupce s identifikátorem stanice:
station_names <- d_rok$id_pixel   # pokud je to jiný sloupec, např. d$stanice, změň tady

res_list <- lapply(split(meta, meta$suffix), function(mg) {
  cols <- mg$col[order(mg$ord)]                # BM1..BM20 ve správném pořadí
  Y    <- as.matrix(d_rok[, cols, drop = FALSE])   # řádky = stanice, sloupce = maxima
  
  out_rows <- lapply(seq_len(nrow(Y)), function(i) {
    
    # >>> tady proběhne tisk názvu stanice do konzole <<<
    cat("Zpracovávám stanici:", station_names[i],
        " (suffix:", mg$suffix[1], ")\n")
    
    # log-lineární trend
    ll <- fit_llin(Y[i, ])
    
    # Gumbelovy kvantily
    fit_g <- gumbel_fit(Y[i, ])
    q_vec <- gumbel_return(T_vals, fit_g$mu, fit_g$beta)
    names(q_vec) <- paste0("Q", T_vals, "_let")
    
    data.frame(
      station   = station_names[i],
      code      = mg$suffix[1],
      intercept = ll["intercept"],
      slope     = ll["slope"],
      R2        = ll["R2"],
      as.list(q_vec),
      row.names = NULL
    )
  })
  
  do.call(rbind, out_rows)
})

# výsledný dlouhý data.frame
res_long <- do.call(rbind, res_list)

res_wide <- res_long %>%
  # vše kromě station a code přelijeme do dlouhého tvaru
  pivot_longer(
    cols = -c(station, code),
    names_to = "param",
    values_to = "value"
  ) %>%
  # složíme název výsledného sloupce: code_param
  mutate(col_name = paste(code, param, sep = "_")) %>%
  select(station, col_name, value) %>%
  # a teď zpátky do wide: 1 řádek = station, sloupce = code_param
  pivot_wider(
    names_from  = col_name,
    values_from = value
  )

idxm <- grepl("rok", names(d_rok)) & grepl("M1_", names(d_rok))

# subset maxim v daném pixelu
cols <- c("id_pixel", names(d_rok)[idxm])
d_rok_maxima <- d_rok[, cols, drop = FALSE]
res_wide$id_pixel = res_wide$station

res_join <- right_join(res_wide, d_rok_maxima, by = "id_pixel")
write.csv(res_join, "maxima_body_i_gumbel.csv")
res_join = read.csv("maxima_body_i_gumbel.csv")

plot(res_join$`10min_rok_10min_Q10_let`, res_join$BM1_10min_rok_10min)
abline(1,1, col = "red")

plot(res_join$`30min_rok_30min_Q100_let`, res_join$BM1_30min_rok_30min)
abline(1,1, col = "blue")

plot(res_join$`6hod_rok_6hod_Q100_let`, res_join$BM1_6hod_rok_6hod)
abline(1,1, col = "green")

plot(res_join$`1hod_rok_1hod_Q20_let`, res_join$`3hod_rok_3hod_Q20_let`)
plot(res_join$`1hod_rok_1hod_Q20_let`, res_join$`30min_rok_30min_Q20_let`)
abline(1,1, col = "brown")


#### nacteni stanic chmi a jejich IDF

# balíčky

# kořenový adresář, kde máš ty .csv soubory

setwd("d:/2_granty_projekty/2_Bezici/2023_Srurb/ZZ_prijata_data/perun_idf")

ifiles <- list.files(pattern = ".csv")
data_list <- lapply(ifiles, function(f) read.csv(f, sep = ";", dec = ".", header = TRUE, fileEncoding = "Windows-1250"))



# Spojení všech dat
data_all <- do.call(rbind, data_list)

data_allW = pivot_wider(data_all)

data_allW = pivot_wider(
  data_all, 
  names_from = "TRVANI..min.",
  values_from = c(4:9))

# Seznam sloupců pro regresi
cols <- grep("^SRA_", names(data_all), value = TRUE)

# Funkce pro výpočet mocninné regrese + R²
fit_power <- function(x, y) {
  model <- try(nls(y ~ a * x^b, start = list(a = 1, b = 0.5)), silent = TRUE)
  if (inherits(model, "try-error")) return(NULL)
  
  # Parametry
  coefs <- coef(model)
  
  # Výpočet R²
  y_pred <- predict(model)
  ss_res <- sum((y - y_pred)^2)
  ss_tot <- sum((y - mean(y))^2)
  r2 <- 1 - ss_res / ss_tot
  
  return(c(a = coefs[1], b = coefs[2], R2 = r2))
}

# Výsledky
results <- data.frame()

stations <- unique(data_all$ID)

for (st in stations) {
  subset_data <- subset(data_all, ID == st)
  x <- subset_data$TRVANI
  
  for (col in cols) {
    y <- subset_data[[col]]
    params <- fit_power(x, y)
    if (!is.null(params)) {
      results <- rbind(results, data.frame(
        Stanice = st,
        Sloupec = col,
        a = params[1],
        b = params[2],
        R2 = params[3]
      ))
    }
  }
}

# Výpis výsledků
print(results)

setwd("d:/2_granty_projekty/2_Bezici/2023_SrUrb/01_reseni_projektu/08_blokova_maxima/")

resultsW = pivot_wider(
  results, 
  names_from = "Sloupec",
  values_from = c(3:5))





# Uložení do CSV
write.csv(results, "vysledky_regrese.csv", row.names = FALSE)

stationsCHMI = read.csv("stationsCHMI.csv")
resultsW$ID = resultsW$Stanice
resultsW$ID = as.numeric(resultsW$ID)
stationsCHMIlj = left_join(stationsCHMI, resultsW, by = "ID")
write.csv(stationsCHMIlj, "vysledky_regrese_join.csv", row.names = FALSE)

xx = right_join(stationsCHMIlj, data_allW, by = "ID")

allinone_stations = left_join(xx, d_rok, by = "id_pixel")

allinone_stations = left_join(allinone_stations, res_join, by = "id_pixel")

colnames(allinone_stations)[1] = "Stanice"
colnames(allinone_stations)[2] = "ID"


# 1) najdu názvy sloupců končící na "_1hod_rok_1hod"
cols_1hod <- grep("_1hod_rok_1hod$", names(allinone_stations), value = TRUE)

# 2) funkce, která spočítá statistiky pro jeden vektor
stat_fun <- function(x, p = 0.95) {
  x <- as.numeric(x)
  c(
    mean   = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    q25    = quantile(x, 0.25, na.rm = TRUE),
    q75    = quantile(x, 0.75, na.rm = TRUE),
    perc   = quantile(x, p,    na.rm = TRUE)  # např. 95. percentil
  )
}

get_row_stats_for_pattern <- function(df, pattern) {
  # najdu sloupce, které sedí na daný pattern
  cols <- grep(pattern, names(df), value = TRUE)
  if (length(cols) == 0) return(NULL)  # nic nenašel, vrátím NULL
  
  # hodnoty pro výpočet statistik
  x <- df[cols]
  
  # základ: ID + statistiky po řádcích
  row_stats <- data.frame(
    ID = df$ID,
    t(apply(x, 1, stat_fun)),
    check.names = FALSE
  )
  
  # zjistím "label" pro příponu (1hod, 3hod, 6hod ...) z názvu prvního sloupce
  # např. BM3_1hod_rok_1hod -> vytáhne "1hod"
  lab <- sub(".*_(\\d+hod).*", "\\1", cols[1])
  
  # přejmenuju sloupce statistik kromě ID
  names(row_stats)[-1] <- paste0(names(row_stats)[-1], "_", lab)
  
  row_stats
}

patterns <- c(
  "_10min_rok_10min",
  "_1hod_rok_1hod$",
  "_2hod_rok_2hod$",
  "_3hod_rok_3hod$",
  "_6hod_rok_6hod$"
)

# spočítat statistiky pro každý pattern
stats_list <- lapply(patterns, function(pat) {
  get_row_stats_for_pattern(allinone_stations, pat)
})

# vyhodit NULL (pro případ, že nějaký pattern nic nenašel)
stats_list <- stats_list[!sapply(stats_list, is.null)]


# když máš víc než jednu skupinu, spojíme podle ID
if (length(stats_list) > 1) {
  row_stats_all <- Reduce(function(x, y) merge(x, y, by = "ID"), stats_list)
} else {
  row_stats_all <- stats_list[[1]]
}

allinone_stations <- merge(
  allinone_stations,
  row_stats_all,
  by = "ID"
)

write.csv(allinone_stations, "all_data_stanice.csv", row.names = FALSE)

#co je co
#SRA_10lete..mm._60 - návrhová srážka 10 leté opakování 60 minut
#X1hod_rok_1hod_Q10_let - srážka spočítaná z dvaceti maxim tady Rkem - NENI TO UFA/CHMI
#a_SRA_10lete..mm. - a, b a R2 - paremetry a spolehlivost mocninné funkce IDF ve stanicích
#BM8_3hod_rok_3hod - blokové maximum BM8 - osme maximum, 3hod délka maxima, roční. Máme to i po měsících, ale to je asi por nás na nic



plot(allinone_stations$SRA_10lete..mm._60, allinone_stations$`perc.95%_1hod.x`)
abline(1,1)

points(allinone_stations$SRA_10lete..mm._60, allinone_stations$X1hod_rok_1hod_Q20_let, col = "red")

ggplot()
