library(dplyr)
library(ggplot2)
library(fst)
texttodatetime <- function(str)
{
  str[nchar(str) == 9] = paste('0',str[nchar(str) == 9],sep = '')
  ye = substr(str, start = 1, stop = 2)
  mo = substr(str, start = 3, stop = 4)
  da = substr(str, start = 5, stop = 6)
  ho = substr(str, start = 7, stop = 8)
  mi = substr(str, start = 9, stop = 10)
  data = paste(ye, mo, da, sep = '-')
  time = paste(ho,  mi, sep = ':')
  dt = paste(data, time)
  return(dt)
}

readpixel <- function(dir_)
{
  ll = list.files(path = dir_, pattern = '*.dat', full.names = T)
  P = read.table(ll[1], colClasses = c('character', 'numeric'))
  print (ll[1])
  jmeno = (substr(basename(ll[1]), start = 7, stop = 11))
  tmp = texttodatetime(P$V1)
  P$TIMESTAMP = as.POSIXct(strptime(tmp, "%y-%m-%d %H:%M"))
  P$V2[P$V2 == -999] =NA
  P = P[,c(3,2)]
  colnames(P)[2] <- jmeno
  for (i in ll[2:length(ll)])
  {
    p1 = read.table(i, colClasses = c('character', 'numeric'))
    print (i)
    jmeno = (substr(basename(i), start = 7, stop = 11))
    tmp = texttodatetime(p1$V1)
    p1$TIMESTAMP = as.POSIXct(strptime(tmp, "%y-%m-%d %H:%M"))
    p1$V2[p1$V2 == -999] =NA
    p1 = p1[,c(3,2)]
    colnames(p1)[2] <- jmeno
    P = cbind(P, p1[,2])
    colnames(P)[length(P[1,])] = jmeno
    # P = merge(P, p1, by = 'TIMESTAMP')
  }
  P = P[!is.na(P$TIMESTAMP),]
  # colnames(P[,-1]) <- 'mm'
  return(P)
}

setwd('d:/2_granty_projekty/2_Bezici/2020_PPZ_DPZ/01_reseni_projektu/lokality_DPZ/07_radary_monitoring/')

#d:/2_granty_projekty/2_Bezici/2019_NAZV_SrazkyII/01_reseni_projektu/01_srazky/01_radary/pixely_nase_stanice/bykovice/




#pizel4 = readpixel('d:/2_granty_projekty/2_Bezici/2019_NAZV_SrazkyII/01_reseni_projektu/01_srazky/01_radary/pixely_nase_stanice/bykovice/')
pizel4 = readpixel('d:/2_granty_projekty/2_Bezici/2020_PPZ_DPZ/01_reseni_projektu/lokality_DPZ/07_radary_monitoring/radar_all/')

# uloz
fwrite(pizel4, 'zmeny2.csv')
saveRDS(pizel4, 'zmeny.rds')
write_fst(pizel4, 'zmeny.fst', compress = 100)

#nacti

pizel4x = readRDS('zmeny.rds')
dDatumy = read.csv("dates.csv", sep = ";")
list_stanic = read.csv(('d:/2_granty_projekty/2_Bezici/2020_PPZ_DPZ/01_reseni_projektu/lokality_DPZ/radar_id.csv'))
jenstanice = list_stanic[,c("OID_", "EUID")]

#vyberstanic = pivot_longer(jenstanice, cols = c("OID_","EUID"), names_to = "EUID")


#najdi vyber

dDatumy$time = as.POSIXct((strptime(dDatumy$date, "%d.%m.%Y")))
 
je_EUID = unique(list_stanic$EUID)
cochci = 531

for (no in 13:length(dDatumy[,2])){
      cochci = dDatumy[no,2]
      print(cochci)
      dDateSel = dDatumy[dDatumy$id == cochci,]
      aktual = list_stanic[list_stanic$EUID == cochci,]
      csel = as.character(aktual$OID_)
      csel =  append(csel,"TIMESTAMP", after = 0)
      pizel4 = pizel4x[,csel]
      
      pizel4 = pizel4[!is.na(pizel4[,2]),]
      pizel4$mean_mm = apply(pizel4[,-1], 1, mean)
      pizel4$max_mm = apply(pizel4[,-1], 1, max)
      pizel4$min_mm = apply(pizel4[,-1], 1, min)
      pizel4$sd_mm = apply(pizel4[,-1], 1, sd)
      
      
      
      myApi <-function(x, pocet, cislo)
        #x = sloupec
        #pocet = pocet radek, dke se to ma pocitat
        #cislo = exponent k funkce
        #ccc = myApi(x = xoo[,2], pocet = 24*6,cislo = -0.223144)
      
      {
          xx = x
          pocet = pocet
          zapis = 0
          cislo = cislo
          dt = 1/pocet
          apiex1 = 0
          sem = c()
          dt1 = 0
          rolo =  length(xx)
          for (cl in pocet:rolo){
            
            #print (cl)
            dt1 = 0
            apiex1 = 0
            for (ro in 1:pocet){
              #print(ro)
              dt1 = dt1+dt
              ##print(dt1)
              ex = cislo*dt1
              #print(ex)
              apiex = exp(ex)*xx[cl-ro+1]
              #print(apiex)
              apiex1 = apiex1+apiex
              #print(apiex1)
              sem[cl] = apiex1
            
            }
          }
          output = sem
          return(output)
        }
      xoo = pizel4[,1:2]
      xoo[,3] = myApi(x = xoo[,2], pocet = 24*6,cislo = -0.223144)
      
      getApi <- function(x,k=0.9,n=5,finite=TRUE) {
        
        l <- length(x)
        y <- rep(NA,times=l)
        
        if(finite) {
          
          if(length(k)==1) {
            kn <- rep(NA,times=n)
            for(i in 1:n) kn[i] <- k^(n-i)
          } else {
            n <- length(k)
            kn <- sort(k)
          }
          
          for(i in (n+1):l) {
            y[i] <- t(kn)%*%x[(i-n):(i-1)]
          }
          
        } else {
          
          k <- max(k)
          y[2] <- x[1]
          for(i in 3:l) {y[i] <- k*y[i-1]+x[i-1]}
          
        }
        
        return(y)
        
      }
      
      #xoo = pizel4[10000:20000,1:2]
      xDate.time.POSIX = strptime(xoo$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
      xdays = format(xDate.time.POSIX, "%y-%m-%d")
      xapix.daily = aggregate(xoo[,2], by = list(xdays), FUN = max)
      xapix1.daily = getApi(xapix.daily$x, k=0.8,n=1,finite=TRUE)
      for (i in 1:71){xapix.daily[i,3]=i*24*6}
      #plot(sem, col = "red", xlim = c(1200,5800))
      #points(x = xxoo$V2, y = xxoo$xapix1.daily, col = "orange")
      #points(x = xapix.daily$V3, y =xapix.daily$x, col = "blue")
      #points(x = x, col = "green")
      
      
      # 10 min data
      #apix5 = getApi(pizel4$mean_mm, k=0.9,n=(6*24*5),finite=TRUE)
      #apix1 = getApi(pizel4$mean_mm, k=0.9,n=(6*24),finite=TRUE)
      
      apix5 = myApi(x = pizel4$mean_mm, pocet = 24*6*5,cislo = -0.223144)
      apix1 = myApi(x = pizel4$mean_mm, pocet = 24*6,cislo = -0.223144)
      
      apix = cbind(pizel4, apix5)
      apix = cbind(apix, apix1)
      #write.csv(apix, file = "d:/2_granty_projekty/2_Bezici/2019_NAZV_SrazkyII/01_reseni_projektu/01_srazky/01_radary/pixely_nase_stanice/bykovice/apix.csv")
      
      
      Date.time.POSIX = strptime(apix$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
      hours = format(Date.time.POSIX, "%y-%m-%d %H")
      days = format(Date.time.POSIX, "%y-%m-%d")
      memes = format(Date.time.POSIX, "%y-%m")
      
      #max hour data
      
      apix.hour = aggregate(apix$max_mm, by = list(hours), FUN = sum)
      apix.hourmean = aggregate(apix$mean_mm, by = list(hours), FUN = sum)
      apix.hour[,3] = apix.hourmean[,2]
      Hour.time.POSIX = strptime(apix.hour$Group.1, format = "%Y-%m-%d %H")
      daysH = format(Hour.time.POSIX, "%y-%m-%d")
      #sum daily data
      
      apix.daily = data.frame()
      apix.daily = aggregate(apix$mean_mm, by = list(days), FUN = sum)
      
      Day.time.POSIX = strptime(apix.daily$Group.1, format = "%Y-%m-%d")
      
      nmes = as.numeric(format(Day.time.POSIX, "%m"))
      rok = as.numeric(format(Day.time.POSIX, "%y"))
      apix.daily$date_form = as.Date(apix.daily$Group.1, format = "%y-%m-%d")
      
      apix.daily$daymax = aggregate(apix$max_mm, by = list(days), FUN = sum)[,2]
      apix.daily$max10_mm = aggregate(apix$max_mm, by = list(days), FUN = max)[,2]
      apix.daily$mean10_mm = aggregate(apix$max_mm, by = list(days), FUN = max)[,2]
      apix.daily$max60_mm = aggregate(apix.hour$x, by = list(daysH), FUN = max)[,2]
      apix.daily$mean60_mm = aggregate(apix.hour$V3, by = list(daysH), FUN = max)[,2]
      
      
      apix.daily$dapi1mean = aggregate(apix$apix1, by = list(days), FUN = mean)[,2]
      apix.daily$dapi1max = aggregate(apix$apix1, by = list(days), FUN = max)[,2]
      apix.daily$dapi5mean = aggregate(apix$apix5, by = list(days), FUN = mean)[,2]
      apix.daily$dapi5max = aggregate(apix$apix5, by = list(days), FUN = max)[,2]
      
      
      
      Day.time.POSIX = strptime(apix.daily$Group.1, format = "%Y-%m-%d")
      ndays = as.numeric(format(Day.time.POSIX, "%d"))
      nmes = as.numeric(format(Day.time.POSIX, "%m"))
      rok = as.numeric(format(Day.time.POSIX, "%Y"))
      
      apix5.daily = getApi(apix.daily$x, k=0.9,n=(5),finite=TRUE)
      apix1.daily = getApi(apix.daily$x, k=0.9,n=(1),finite=TRUE)
      tmp = cbind(apix.daily, apix5.daily)
      dapixallall = cbind(tmp, apix1.daily)
      dapixallall$ratio = apix1.daily/apix5.daily
      dapixallall = cbind(dapixallall, rok)
      dapixallall[,"rok"] = dapixallall[,"rok"]+2000
      dapixallall = cbind(dapixallall, nmes)
      dapixallall = cbind(dapixallall, ndays)
      colnames(dapixallall) = c("ddate", "day_meanRain", "date_form","day_maxRain", "max10MinRain", "mean10MinRain",  "max60MinRain", "mean60MinRain", "dapi1mean", "dapi1max", "dapi5mean", "dapi5max","API5", "API1", "APIratio", "year", "mes", "day")
      
      #ddate - datum divnoforma
      #day_meanRain - průměrná denní srážka z pixelů v oblasti
      #date_form - datum v R formatu
      #day_maxRain - maximální denní srážka z maxim pixelů v oblasti
      
      #max10MinRain - maximální desetiminutovka v daném dni ze všech pixelů
      #mean10MinRain - maximální průměrná desetiminutovka v daném dni ze všech pixelů
      #max60MinRain - maximální hodinovka v daném dni ze maxim ze všech pixelůa jejich desetiminutovek
      #mean60MinRain - maximální průměrná desetiminutovka v daném dni ze všech pixelů
      
      #dapi1mean - denní API - průměrné z desetiminutovek
      #dapi1mean - denní API - z maximpixelů z desetiminutovek
      #dapi5mean - pětidenní API - průměrné z desetiminutovek
      #dapi5mean - pětidenní API - z maximpixelů z desetiminutovek
      #"API5" - 5dní API počítané z denních hodnot
      # API1" 1dní API počítané z denních hodnot
      # ratio - poměr mezi dapi1mean/dapi5mean
      #rain5dnu - pedidenní srážka (suma z day_meanRain)
      
      dapixallall = dapixallall[!is.na(dapixallall$APIratio),]
      dapixallall$rain5dnu = NA
      for(i in 5:length(dapixallall$ddate)){
        dapixallall$rain5dnu[i] = sum(dapixallall$day_meanRain[(i-4):i])
          }
      
      
      
      #write.csv(dapixallall, file = "d:/2_granty_projekty/2_Bezici/2019_NAZV_SrazkyII/01_reseni_projektu/01_srazky/01_radary/pixely_nase_stanice/bykovice/dapixallall.csv")
      
      
      maxdaySubset = subset(dapixallall, dapixallall$max60MinRain > 30 | 
                              dapixallall$max10MinRain > 10| dapixallall$day_meanRain > 60| 
                              dapixallall$rain5dnu > 120)
      maxdaySubset$deltarain = 0
      for(no in 2:nrow(maxdaySubset)){maxdaySubset[no,ncol(maxdaySubset)] = difftime(maxdaySubset[no,3], maxdaySubset[(no-1),3], units = "days")}
      maxdaySubset = maxdaySubset[!maxdaySubset$deltarain == 1, ]
      erosion_date = dapixallall[dapixallall$date_form == as.Date(dDateSel$time, tz = Sys.timezone()),]
      
      
      
      plotplot = ggplot(data = maxdaySubset, aes(x = mean60MinRain,  y = mean10MinRain, color = dapi1max, size = day_meanRain, label = date_form)) +#, fill = rain5dnu))  +
        geom_point() +
        geom_text(hjust=0, vjust=2, size = 2, color = "black") +
        scale_color_gradient(low="blue", high="green")
      plot(plotplot)
      
      
      plotmes =  plotplot + facet_grid( ~ maxdaySubset$mes)
      plot(plotmes)  
      
      plotplot = plotplot + 
        geom_point(data = erosion_date, aes(x = mean60MinRain,  y = mean10MinRain) , color = "red", size = 6, shape=10) +
        labs(title = paste("Situation", cochci, erosion_date$date_form, "day", erosion_date$day_maxRain, "hour", erosion_date$max60MinRain, "ten min", erosion_date$max10MinRain,"dapi1max",  erosion_date$dapi1max))
      plot(plotplot)
      
      sname = paste("Erosion_situation_mean", cochci, erosion_date$date_form, sep = "_")
      #sname = "Alldata"
      maxName = paste("maxDay",sname, ".csv")
      allName = paste("allDay", sname, ".csv")
      write.csv(maxdaySubset, paste("maxDay",sname, ".csv"))
      write.csv(dapixallall, paste("allDay", sname, ".csv"))
      ggsave(paste(sname,".png"))
      }

rSWI =  read.csv("Bykovice_PK.csv", sep = ";")
rSWI$time = strptime(rSWI$datum, format = "%d.%m.%Y")
rSWIcas = as.Date(rSWI$time, tz = Sys.timezone())
rSWI$date_form = as.Date(rSWI$time, tz = Sys.timezone())
radarSEL = dapixallall[dapixallall$date_form %in% rSWIcas, ]
rALL = merge(radarSEL, rSWI, by = "date_form")

plotplot = ggplot(data = rALL, aes(x = SSM_,  y = dapi5mean, color = dapi1mean, size = rain5dnu, label = date_form)) +#, fill = rain5dnu))  +
  geom_point() +
#  geom_text(hjust=0, vjust=2, size = 2, color = "black") +
  scale_color_gradient(low="blue", high="green")
plot(plotplot)
plotmes =  plotplot + facet_grid( ~ rALL$mes)
plot(plotmes)


boxplot(rALL$SSM_)


xxx = order(rALL)




