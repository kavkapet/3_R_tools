## load libraries
#install.packages("devtools")
#devtools::install_github("davidsjoberg/ggsankey")

library(dplyr)
library(networkD3)
library(alluvial)
library(tidyr)
library(ggplot2)
library(ggsankey)
library(clusterCrit)

# read in EU referendum results dataset
setwd("d:/2_granty_projekty/2_Bezici/2019_NAZV_SrazkyII/01_reseni_projektu/02_povodi/5km_povodi/_R/csv_vyber_a_simplify/")

snakdata <- read.csv("clustersR.txt")
slinks = snakdata[,15:21]
slinks2 = slinks

slinks2$F2_Clusters[slinks2$F2_Clusters == 1] <- 17 #B
slinks2$F2_Clusters[slinks2$F2_Clusters == 2] <- 13 #A

slinks2$F3_Clusters[slinks2$F3_Clusters == 3] <- 11 #A1
slinks2$F3_Clusters[slinks2$F3_Clusters == 2] <- 17 #B
slinks2$F3_Clusters[slinks2$F3_Clusters == 1] <- 13 #A2

slinks2$F4_Clusters[slinks2$F4_Clusters == 4] <- 11 #A1
slinks2$F4_Clusters[slinks2$F4_Clusters == 2] <- 13 #A2
slinks2$F4_Clusters[slinks2$F4_Clusters == 1] <- 17 #B2
slinks2$F4_Clusters[slinks2$F4_Clusters == 3] <- 15 #B1

slinks2$F5_Clusters[slinks2$F5_Clusters == 4] <- 11 #A11
slinks2$F5_Clusters[slinks2$F5_Clusters == 3] <- 12 #A12
slinks2$F5_Clusters[slinks2$F5_Clusters == 5] <- 13 #A2
slinks2$F5_Clusters[slinks2$F5_Clusters == 1] <- 15 #B1
slinks2$F5_Clusters[slinks2$F5_Clusters == 2] <- 17 #B2

slinks2$F6_Clusters[slinks2$F6_Clusters == 3] <- 11 #A11
slinks2$F6_Clusters[slinks2$F6_Clusters == 1] <- 12 #A12
slinks2$F6_Clusters[slinks2$F6_Clusters == 5] <- 13 #A2
slinks2$F6_Clusters[slinks2$F6_Clusters == 4] <- 15 #B1
slinks2$F6_Clusters[slinks2$F6_Clusters == 6] <- 17 #B2
slinks2$F6_Clusters[slinks2$F6_Clusters == 2] <- 18 #C

slinks2$F7_Clusters[slinks2$F7_Clusters == 5] <- 13 #A2
slinks2$F7_Clusters[slinks2$F7_Clusters == 1] <- 18 #C
slinks2$F7_Clusters[slinks2$F7_Clusters == 4] <- 11 #A11
slinks2$F7_Clusters[slinks2$F7_Clusters == 2] <- 12 #A12
slinks2$F7_Clusters[slinks2$F7_Clusters == 3] <- 17 #B3
slinks2$F7_Clusters[slinks2$F7_Clusters == 6] <- 16 #B2A2
slinks2$F7_Clusters[slinks2$F7_Clusters == 7] <- 15 #B1

slinks2$F8_Clusters[slinks2$F8_Clusters == 5] <- 13 #A2
slinks2$F8_Clusters[slinks2$F8_Clusters == 7] <- 18 #C
slinks2$F8_Clusters[slinks2$F8_Clusters == 1] <- 11 #A11
slinks2$F8_Clusters[slinks2$F8_Clusters == 2] <- 12 #A12
slinks2$F8_Clusters[slinks2$F8_Clusters == 3] <- 14 #A2B2
slinks2$F8_Clusters[slinks2$F8_Clusters == 4] <- 17 #B3
slinks2$F8_Clusters[slinks2$F8_Clusters == 6] <- 15 #B1
slinks2$F8_Clusters[slinks2$F8_Clusters == 8] <- 16 #B2A2

slong = make_long(slinks2, F2_Clusters, F3_Clusters, F4_Clusters, F5_Clusters, F6_Clusters, F7_Clusters, F8_Clusters)
ggplot(slong, aes(x = x, 
                  next_x = next_x, 
                  node = node, 
                  next_node = next_node,
                  fill = factor(node))) +
  geom_sankey()



slinks3 = slinks

slinks3$F2_Clusters[slinks3$F2_Clusters == 1] <- "B"
slinks3$F2_Clusters[slinks3$F2_Clusters == 2] <- "A"

slinks3$F3_Clusters[slinks3$F3_Clusters == 3] <- "A1"
slinks3$F3_Clusters[slinks3$F3_Clusters == 2] <- "B"
slinks3$F3_Clusters[slinks3$F3_Clusters == 1] <- "A2"

slinks3$F4_Clusters[slinks3$F4_Clusters == 4] <- "A1"
slinks3$F4_Clusters[slinks3$F4_Clusters == 2] <- "A2"
slinks3$F4_Clusters[slinks3$F4_Clusters == 1] <- "B2"
slinks3$F4_Clusters[slinks3$F4_Clusters == 3] <- "B1"

slinks3$F5_Clusters[slinks3$F5_Clusters == 4] <- "A11"
slinks3$F5_Clusters[slinks3$F5_Clusters == 3] <- "A12"
slinks3$F5_Clusters[slinks3$F5_Clusters == 5] <- "A2"
slinks3$F5_Clusters[slinks3$F5_Clusters == 1] <- "B1"
slinks3$F5_Clusters[slinks3$F5_Clusters == 2] <- "B2"

slinks3$F6_Clusters[slinks3$F6_Clusters == 3] <- "A11"
slinks3$F6_Clusters[slinks3$F6_Clusters == 1] <- "A12"
slinks3$F6_Clusters[slinks3$F6_Clusters == 5] <- "A2"
slinks3$F6_Clusters[slinks3$F6_Clusters == 4] <- "B1"
slinks3$F6_Clusters[slinks3$F6_Clusters == 6] <- "B2"
slinks3$F6_Clusters[slinks3$F6_Clusters == 2] <- "D"

slinks3$F7_Clusters[slinks3$F7_Clusters == 5] <- "A2"
slinks3$F7_Clusters[slinks3$F7_Clusters == 1] <- "D"
slinks3$F7_Clusters[slinks3$F7_Clusters == 4] <- "A11"
slinks3$F7_Clusters[slinks3$F7_Clusters == 2] <- "A12"
slinks3$F7_Clusters[slinks3$F7_Clusters == 3] <- "B3"
slinks3$F7_Clusters[slinks3$F7_Clusters == 6] <- "C"
slinks3$F7_Clusters[slinks3$F7_Clusters == 7] <- "B1"

slinks3$F8_Clusters[slinks3$F8_Clusters == 5] <- "A2"
slinks3$F8_Clusters[slinks3$F8_Clusters == 7] <- "D"
slinks3$F8_Clusters[slinks3$F8_Clusters == 1] <- "A11"
slinks3$F8_Clusters[slinks3$F8_Clusters == 2] <- "A12"
slinks3$F8_Clusters[slinks3$F8_Clusters == 3] <- "C1"
slinks3$F8_Clusters[slinks3$F8_Clusters == 4] <- "B3"
slinks3$F8_Clusters[slinks3$F8_Clusters == 6] <- "B1"
slinks3$F8_Clusters[slinks3$F8_Clusters == 8] <- "C2"


snakdataexport = snakdata
snakdataexport$F2_Clusters[snakdataexport$F2_Clusters == 1] <- "B"
snakdataexport$F2_Clusters[snakdataexport$F2_Clusters == 2] <- "A"

snakdataexport$F3_Clusters[snakdataexport$F3_Clusters == 3] <- "A1"
snakdataexport$F3_Clusters[snakdataexport$F3_Clusters == 2] <- "B"
snakdataexport$F3_Clusters[snakdataexport$F3_Clusters == 1] <- "A2"

snakdataexport$F4_Clusters[snakdataexport$F4_Clusters == 4] <- "A1"
snakdataexport$F4_Clusters[snakdataexport$F4_Clusters == 2] <- "A2"
snakdataexport$F4_Clusters[snakdataexport$F4_Clusters == 1] <- "B2"
snakdataexport$F4_Clusters[snakdataexport$F4_Clusters == 3] <- "B1"

snakdataexport$F5_Clusters[snakdataexport$F5_Clusters == 4] <- "A11"
snakdataexport$F5_Clusters[snakdataexport$F5_Clusters == 3] <- "A12"
snakdataexport$F5_Clusters[snakdataexport$F5_Clusters == 5] <- "A2"
snakdataexport$F5_Clusters[snakdataexport$F5_Clusters == 1] <- "B1"
snakdataexport$F5_Clusters[snakdataexport$F5_Clusters == 2] <- "B2"

snakdataexport$F6_Clusters[snakdataexport$F6_Clusters == 3] <- "A11"
snakdataexport$F6_Clusters[snakdataexport$F6_Clusters == 1] <- "A12"
snakdataexport$F6_Clusters[snakdataexport$F6_Clusters == 5] <- "A2"
snakdataexport$F6_Clusters[snakdataexport$F6_Clusters == 4] <- "B1"
snakdataexport$F6_Clusters[snakdataexport$F6_Clusters == 6] <- "B2"
snakdataexport$F6_Clusters[snakdataexport$F6_Clusters == 2] <- "D"

snakdataexport$F7_Clusters[snakdataexport$F7_Clusters == 5] <- "A2"
snakdataexport$F7_Clusters[snakdataexport$F7_Clusters == 1] <- "D"
snakdataexport$F7_Clusters[snakdataexport$F7_Clusters == 4] <- "A11"
snakdataexport$F7_Clusters[snakdataexport$F7_Clusters == 2] <- "A12"
snakdataexport$F7_Clusters[snakdataexport$F7_Clusters == 3] <- "B3"
snakdataexport$F7_Clusters[snakdataexport$F7_Clusters == 6] <- "C"
snakdataexport$F7_Clusters[snakdataexport$F7_Clusters == 7] <- "B1"

snakdataexport$F8_Clusters[snakdataexport$F8_Clusters == 5] <- "A2"
snakdataexport$F8_Clusters[snakdataexport$F8_Clusters == 7] <- "D"
snakdataexport$F8_Clusters[snakdataexport$F8_Clusters == 1] <- "A11"
snakdataexport$F8_Clusters[snakdataexport$F8_Clusters == 2] <- "A12"
snakdataexport$F8_Clusters[snakdataexport$F8_Clusters == 3] <- "C1"
snakdataexport$F8_Clusters[snakdataexport$F8_Clusters == 4] <- "B3"
snakdataexport$F8_Clusters[snakdataexport$F8_Clusters == 6] <- "B1"
snakdataexport$F8_Clusters[snakdataexport$F8_Clusters == 8] <- "C2"

write.csv(snakdataexport, "snakdataexport.csv")


slong = make_long(slinks3, F2_Clusters, F3_Clusters, F4_Clusters, F5_Clusters, F6_Clusters, F7_Clusters, F8_Clusters)
ggplot(slong, aes(x = x, 
                  next_x = next_x, 
                  node = node, 
                  next_node = next_node,
                  fill = factor(node), 
                  label = node)) +
  geom_sankey(flow.alpha = .6,
              node.color = "gray30") +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  theme_sankey(base_size = 18) +
  scale_fill_viridis_d()
  #scale_color_jcolors(palette = "pal2")

C2 = snakdataexport%>% group_by(F2_Clusters) %>% summarise(num = n(), hrs = mean(hrs*1000), Tlag = mean(Tlag), alfa = mean(alfa), CN_mean = mean(CN_mean), P_20yr_6h = mean(P_20yr_6h))
C3 = snakdataexport%>% group_by(F3_Clusters) %>% summarise(num = n(),hrs = mean(hrs*1000), Tlag = mean(Tlag), alfa = mean(alfa), CN_mean = mean(CN_mean), P_20yr_6h = mean(P_20yr_6h))
C4 = snakdataexport%>% group_by(F4_Clusters) %>% summarise(num = n(),hrs = mean(hrs*1000), Tlag = mean(Tlag), alfa = mean(alfa), CN_mean = mean(CN_mean), P_20yr_6h = mean(P_20yr_6h))
C5 = snakdataexport%>% group_by(F5_Clusters) %>% summarise(num = n(),hrs = mean(hrs*1000), Tlag = mean(Tlag), alfa = mean(alfa), CN_mean = mean(CN_mean), P_20yr_6h = mean(P_20yr_6h))
C6 = snakdataexport%>% group_by(F6_Clusters) %>% summarise(num = n(),hrs = mean(hrs*1000), Tlag = mean(Tlag), alfa = mean(alfa), CN_mean = mean(CN_mean), P_20yr_6h = mean(P_20yr_6h))
C7 = snakdataexport%>% group_by(F7_Clusters) %>% summarise(num = n(),hrs = mean(hrs*1000), Tlag = mean(Tlag), alfa = mean(alfa), CN_mean = mean(CN_mean), P_20yr_6h = mean(P_20yr_6h))
C8 = snakdataexport%>% group_by(F8_Clusters) %>% summarise(num = n(),hrs = mean(hrs*1000), Tlag = mean(Tlag), alfa = mean(alfa), CN_mean = mean(CN_mean), P_20yr_6h = mean(P_20yr_6h))
colnames(C2) = c("num", "name", "hrs", "Tlag", "alfa", "CN_mean", "P_20yr_6h")
colnames(C3) = c("num", "name", "hrs", "Tlag", "alfa", "CN_mean", "P_20yr_6h")
colnames(C4) = c("num", "name", "hrs", "Tlag", "alfa", "CN_mean", "P_20yr_6h")
colnames(C5) = c("num", "name", "hrs", "Tlag", "alfa", "CN_mean", "P_20yr_6h")
colnames(C6) = c("num", "name", "hrs", "Tlag", "alfa", "CN_mean", "P_20yr_6h")
colnames(C7) = c("num", "name", "hrs", "Tlag", "alfa", "CN_mean", "P_20yr_6h")
colnames(C8) = c("num", "name", "hrs", "Tlag", "alfa", "CN_mean", "P_20yr_6h")

C2$NoCL = 2
C3$NoCL = 3
C4$NoCL = 4
C5$NoCL = 5
C6$NoCL = 6
C7$NoCL = 7
C8$NoCL = 8




C = rbind(C2, C3, C4, C5, C6, C7, C8)
write.csv(C, "sanky_mean_in_groups2.csv")

allCmean = snakdataexport%>% summarise(num = n(), hrs = mean(hrs*1000), Tlag = mean(Tlag), alfa = mean(alfa), CN_mean = mean(CN_mean), P_20yr_6h = mean(P_20yr_6h))
allCmax = snakdataexport%>% summarise(num = n(), hrs = max (hrs*1000), Tlag = max (Tlag), alfa = max(alfa), CN_mean = max(CN_mean), P_20yr_6h = max(P_20yr_6h))
allCmin = snakdataexport%>% summarise(num = n(), hrs = min (hrs*1000), Tlag = min (Tlag), alfa = min(alfa), CN_mean = min(CN_mean), P_20yr_6h = min(P_20yr_6h))


##############################################
#####podobnost clusteru#################
obs =  as.matrix(snakdata[3:7])

obsscale = scale(obs)
crit2C = intCriteria(obs,snakdata$F2_Clusters,c("Calinski_Harabasz", "Davies_Bouldin", "Ratkowsky_Lance", "Scott_Symons"))
crit3C = intCriteria(obs,snakdata$F3_Clusters,c("Calinski_Harabasz", "Davies_Bouldin", "Ratkowsky_Lance", "Scott_Symons"))
crit4C = intCriteria(obs,snakdata$F4_Clusters,c("Calinski_Harabasz", "Davies_Bouldin", "Ratkowsky_Lance", "Scott_Symons"))
crit5C = intCriteria(obs,snakdata$F5_Clusters,c("Calinski_Harabasz", "Davies_Bouldin", "Ratkowsky_Lance", "Scott_Symons"))
crit6C = intCriteria(obs,snakdata$F6_Clusters,c("Calinski_Harabasz", "Davies_Bouldin", "Ratkowsky_Lance", "Scott_Symons"))
crit7C = intCriteria(obs,snakdata$F7_Clusters,c("Calinski_Harabasz", "Davies_Bouldin", "Ratkowsky_Lance", "Scott_Symons"))
crit8C = intCriteria(obs,snakdata$F8_Clusters,c("Calinski_Harabasz", "Davies_Bouldin", "Ratkowsky_Lance", "Scott_Symons"))


crit2Csc = intCriteria(obsscale,snakdata$F2_Clusters,c("Calinski_Harabasz", "Davies_Bouldin", "Ratkowsky_Lance", "Scott_Symons"))
crit3Csc = intCriteria(obsscale,snakdata$F3_Clusters,c("Calinski_Harabasz", "Davies_Bouldin", "Ratkowsky_Lance", "Scott_Symons"))
crit4Csc = intCriteria(obsscale,snakdata$F4_Clusters,c("Calinski_Harabasz", "Davies_Bouldin", "Ratkowsky_Lance", "Scott_Symons"))
crit5Csc = intCriteria(obsscale,snakdata$F5_Clusters,c("Calinski_Harabasz", "Davies_Bouldin", "Ratkowsky_Lance", "Scott_Symons"))
crit6Csc = intCriteria(obsscale,snakdata$F6_Clusters,c("Calinski_Harabasz", "Davies_Bouldin", "Ratkowsky_Lance", "Scott_Symons"))
crit7Csc = intCriteria(obsscale,snakdata$F7_Clusters,c("Calinski_Harabasz", "Davies_Bouldin", "Ratkowsky_Lance", "Scott_Symons"))
crit8Csc = intCriteria(obsscale,snakdata$F8_Clusters,c("Calinski_Harabasz", "Davies_Bouldin", "Ratkowsky_Lance", "Scott_Symons"))



prcprc = prcomp(obs, center = TRUE, scale. = TRUE)
summary(prcprc)

fviz_eig(prcprc)



fviz_pca_var(prcprc,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, 
             labelsize = 6,
             title = "nic"
             # Avoid text overlapping
)



Calinski = c(crit2C[1], crit3C[1], crit4C[1], crit5C[1], crit6C[1], crit7C[1], crit8C[1])
Davies = c(crit2C[2], crit3C[2], crit4C[2], crit5C[2], crit6C[2], crit7C[2], crit8C[2])
Ratkowsky = c(crit2C[3], crit3C[3], crit4C[3], crit5C[3], crit6C[3], crit7C[3], crit8C[3])
Scott = c(crit2C[4], crit3C[4], crit4C[4], crit5C[4], crit6C[4], crit7C[4], crit8C[4])

Calinskisc = c(crit2Csc[1], crit3Csc[1], crit4Csc[1], crit5Csc[1], crit6Csc[1], crit7Csc[1], crit8Csc[1])
Daviessc = c(crit2Csc[2], crit3Csc[2], crit4Csc[2], crit5Csc[2], crit6Csc[2], crit7Csc[2], crit8Csc[2])
Ratkowskysc = c(crit2Csc[3], crit3Csc[3], crit4Csc[3], crit5Csc[3], crit6Csc[3], crit7Csc[3], crit8Csc[3])
Scottsc = c(crit2Csc[4], crit3Csc[4], crit4Csc[4], crit5Csc[4], crit6Csc[4], crit7Csc[4], crit8Csc[4])

a = 1
b =  1
for (i in critalllist){
  print(i)
  critall[a,1] = i[b]
  critall[a,2] = i[b+1]
  print (a)
  print (b)
  a = a+1
  b = b+1
  
} 



selpca = select(uuu, SND, Fl_len_noStream_STD, Alfa)
prcprcsel = prcomp(selpca, center = TRUE, scale. = TRUE)
summary(prcprcsel)
prcprcsel

pccall = prcomp(uuu, center = TRUE, scale. = TRUE)
summary(pccall)
prccall

j =  1
total_sel =  snakdata[c(3:7, 19)]
max = ncol(total_sel)-1
ccc = colnames(total_sel)

pov_stat = (ggplot(data = total_sel, mapping = aes(y = total_sel$hrs, fill =  total_sel$F6_Clusters))
            +geom_boxplot()#outlier.shape = NA, coef = 0)
            +xlab("Cluster") + ylab("SND")
            +facet_grid("" ~ total_sel$F6_Clusters)
)
            #+ylim(0, xxx)
print (pov_stat)

pov_stat = (ggplot(data = total_sel, mapping = aes(y = total_sel$alfa, fill =  total_sel$F6_Clusters))
            +geom_boxplot()#outlier.shape = NA, coef = 0)
            +xlab("Cluster") + ylab("Alfa")
            +facet_grid("" ~ total_sel$F6_Clusters)
)
#+ylim(0, xxx)
print (pov_stat)

pov_stat = (ggplot(data = total_sel, mapping = aes(y = total_sel$CN_mean, fill =  total_sel$F6_Clusters))
            +geom_boxplot()#outlier.shape = NA, coef = 0)
            +xlab("Cluster") + ylab("CN_mean")
            +facet_grid("" ~ total_sel$F6_Clusters)
)
#+ylim(0, xxx)
print (pov_stat)

pov_stat = (ggplot(data = total_sel, mapping = aes(y = total_sel$CN_mean, fill =  total_sel$F6_Clusters))
            +geom_boxplot()#outlier.shape = NA, coef = 0)
            +xlab("Cluster") + ylab("CN_mean")
            +facet_grid("" ~ total_sel$F6_Clusters)
)
#+ylim(0, xxx)
print (pov_stat)

pov_stat = (ggplot(data = total_sel, mapping = aes(y = total_sel$CN_mean, fill =  total_sel$F6_Clusters))
            +geom_boxplot()#outlier.shape = NA, coef = 0)
            +xlab("Cluster") + ylab("CN_mean")
            +facet_grid("" ~ total_sel$F6_Clusters)
)
#+ylim(0, xxx)
print (pov_stat)





for (i in 1:max){
  
  
  
  
  print(ccc[i])
  #xxx = 5*sd(total_all050[,j])
  pov_stat = (ggplot(data = total_sel, mapping = aes(y = total_sel[j], fill =  total_sel$F6_Clusters ))
              +geom_boxplot()#outlier.shape = NA, coef = 0)
              +facet_grid("" ~ total_sel$F6_Clusters)
              +xlab("Cluster") + ylab(colnames(total_sel[j]))
              #+ylim(0, xxx)
              
              
  )
  j = j+1
  
  plot(pov_stat)
  ggsave(paste(ccc[i],"en.png"))
}
#hist
j =  1
max = ncol(total_sel)-2
max = total_sel$Tlag
for (i in 1:max){
  
  pov_hist =  (ggplot(data = total_sel, mapping = aes(x =total_sel[,i], y = (stat(ncount))))
               + geom_histogram()
               +geom_freqpoly(colour = "red")
               +xlab("Tøída povodí") + ylab(colnames(total_sel[j]))
               + facet_grid(factor(total_sel$size, levels=c("all", "5", "10", "20", "30", "40", "50")) ~ "" )
               + xlab(ccc[j])
               
               
  )
  plot(pov_hist)
  ggsave(paste(i,"enhist.png"))
  j=j+1
}

