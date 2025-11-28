##############################################
# Script quickly plots SMODERP2D model results 
##############################################
#
#
# Setting
# In setting set the location of your data
#
library('manipulate')
library("stringr")
library(ggplot2)
library(tidyr)
# install package is missing with: install.packages("manipulate")
#

root  <-  "./"
root  <-  "d:/2_granty_projekty/2_Bezici/2022_RAGO/01_reseni_projektu/01_lokality_ziva_krajina/05_hrozenskySK/out_clear"
# output dir

outdir <- c('N020_B_IC2_ZBGD/control_point/',
            'N020_C_IC2_ZBGD/control_point/')

outdir <- c('5Hro_S_n020_A_360m_IC1/control_point/','5Hro_S_n020_A_360m_IC1_ZK/control_point/','5Hro_S_n020_A_360m_IC2/control_point/','5Hro_S_n020_A_360m_IC2_ZK/control_point/','5Hro_S_n020_A_360m_IC3/control_point/','5Hro_S_n020_A_360m_IC3_ZK/control_point/','5Hro_S_n020_B_360m_IC1/control_point/','5Hro_S_n020_B_360m_IC1_ZK/control_point/','5Hro_S_n020_B_360m_IC2/control_point/','5Hro_S_n020_B_360m_IC2_ZK/control_point/','5Hro_S_n020_B_360m_IC3/control_point/','5Hro_S_n020_B_360m_IC3_ZK/control_point/','5Hro_S_n020_C_360m_IC1/control_point/','5Hro_S_n020_C_360m_IC1_ZK/control_point/','5Hro_S_n020_C_360m_IC2/control_point/','5Hro_S_n020_C_360m_IC2_ZK/control_point/','5Hro_S_n020_C_360m_IC3/control_point/','5Hro_S_n020_C_360m_IC3_ZK/control_point/','5Hro_S_n020_D_360m_IC1/control_point/','5Hro_S_n020_D_360m_IC1_ZK/control_point/','5Hro_S_n020_D_360m_IC2/control_point/','5Hro_S_n020_D_360m_IC2_ZK/control_point/','5Hro_S_n020_D_360m_IC3/control_point/','5Hro_S_n020_D_360m_IC3_ZK/control_point/','5Hro_S_n020_E_360m_IC1/control_point/','5Hro_S_n020_E_360m_IC1_ZK/control_point/','5Hro_S_n020_E_360m_IC2/control_point/','5Hro_S_n020_E_360m_IC2_ZK/control_point/','5Hro_S_n020_F_360m_IC1/control_point/','5Hro_S_n020_F_360m_IC1_ZK/control_point/','5Hro_S_n020_F_360m_IC2/control_point/','5Hro_S_n020_F_360m_IC2_ZK/control_point/','5Hro_S_n100_A_360m_IC1/control_point/','5Hro_S_n100_A_360m_IC1_ZK/control_point/','5Hro_S_n100_A_360m_IC2/control_point/','5Hro_S_n100_A_360m_IC2_ZK/control_point/','5Hro_S_n100_A_360m_IC3/control_point/','5Hro_S_n100_A_360m_IC3_ZK/control_point/','5Hro_S_n100_B_360m_IC1/control_point/','5Hro_S_n100_B_360m_IC1_ZK/control_point/','5Hro_S_n100_B_360m_IC2/control_point/','5Hro_S_n100_B_360m_IC2_ZK/control_point/','5Hro_S_n100_B_360m_IC3/control_point/','5Hro_S_n100_B_360m_IC3_ZK/control_point/','5Hro_S_n100_C_360m_IC1/control_point/','5Hro_S_n100_C_360m_IC1_ZK/control_point/','5Hro_S_n100_C_360m_IC2/control_point/','5Hro_S_n100_C_360m_IC2_ZK/control_point/'
)
outdir <- c("4Vel_S_n020_A_360m_IC1/control_point/", "4Vel_S_n020_A_360m_IC1_ZK/control_point/", "4Vel_S_n020_A_360m_IC2/control_point/", "4Vel_S_n020_A_360m_IC2_ZK/control_point/", "4Vel_S_n020_A_360m_IC3/control_point/", "4Vel_S_n020_A_360m_IC3_ZK/control_point/", "4Vel_S_n020_A_360m_IC4/control_point/", "4Vel_S_n020_A_360m_IC4_ZK/control_point/", "4Vel_S_n020_B_360m_IC1/control_point/", "4Vel_S_n020_B_360m_IC1_ZK/control_point/", "4Vel_S_n020_B_360m_IC2/control_point/", "4Vel_S_n020_B_360m_IC2_ZK/control_point/", "4Vel_S_n020_B_360m_IC3/control_point/", "4Vel_S_n020_B_360m_IC3_ZK/control_point/", "4Vel_S_n020_B_360m_IC4/control_point/", "4Vel_S_n020_B_360m_IC4_ZK/control_point/", "4Vel_S_n020_C_360m_IC1/control_point/", "4Vel_S_n020_C_360m_IC1_ZK/control_point/", "4Vel_S_n020_C_360m_IC2/control_point/", "4Vel_S_n020_C_360m_IC2_ZK/control_point/", "4Vel_S_n020_C_360m_IC3/control_point/", "4Vel_S_n020_C_360m_IC3_ZK/control_point/", "4Vel_S_n020_C_360m_IC4/control_point/", "4Vel_S_n020_C_360m_IC4_ZK/control_point/", "4Vel_S_n020_D_360m_IC1/control_point/", "4Vel_S_n020_D_360m_IC1_ZK/control_point/", "4Vel_S_n020_D_360m_IC2/control_point/", "4Vel_S_n020_D_360m_IC2_ZK/control_point/", "4Vel_S_n020_D_360m_IC3/control_point/", "4Vel_S_n020_D_360m_IC3_ZK/control_point/", "4Vel_S_n020_D_360m_IC4/control_point/", "4Vel_S_n020_D_360m_IC4ZK/control_point/", "4Vel_S_n020_E_360m_IC1/control_point/", "4Vel_S_n020_E_360m_IC1_ZK/control_point/", "4Vel_S_n020_E_360m_IC2/control_point/", "4Vel_S_n020_E_360m_IC2_ZK/control_point/", "4Vel_S_n020_E_360m_IC3/control_point/", "4Vel_S_n020_E_360m_IC3_ZK/control_point/", "4Vel_S_n020_E_360m_IC4/control_point/", "4Vel_S_n020_E_360m_IC4ZK/control_point/", "4Vel_S_n020_F_360m_IC1/control_point/", "4Vel_S_n020_F_360m_IC1_ZK/control_point/", "4Vel_S_n020_F_360m_IC2/control_point/", "4Vel_S_n020_F_360m_IC2_ZK/control_point/", "4Vel_S_n020_F_360m_IC3/control_point/", "4Vel_S_n020_F_360m_IC3_ZK/control_point/", "4Vel_S_n020_F_360m_IC4/control_point/", "4Vel_S_n020_F_360m_IC4ZK/control_point/", "4Vel_S_n100_A_360m_IC1/control_point/", "4Vel_S_n100_A_360m_IC1_ZK/control_point/", "4Vel_S_n100_A_360m_IC2/control_point/", "4Vel_S_n100_A_360m_IC2_ZK/control_point/", "4Vel_S_n100_A_360m_IC3/control_point/", "4Vel_S_n100_A_360m_IC3_ZK/control_point/", "4Vel_S_n100_E_360m_IC2/control_point/", "4Vel_S_n100_F_360m_IC2/control_point/", "4Vel_S_n100_F_360m_IC3/control_point/", "4Vel_S_n100_F_360m_IC4ZK/control_point/"
)


# 
# outdir <- c('reference_nuc/control_point/',
# 'implicit_new_nuc/control_point/')
# 


id1_ = 1:6
id2_ = id1_ + 6


# Do no edit rest of the scipt!
#
#
#
# Functions
#
nactibod = function(dir_,sep_  = ',', skip_ = 3, extension_ = '*.dat')
{
  files = list.files(dir_,pattern = '*.dat')
  pixel = read.table(paste(dir_,'/',files[1],sep = ''),skip=2,nrows = 1,comment.char = '')
  pixel = as.numeric(pixel[2])
  H = list()
  for (file_ in files) {
    name_ = substr(file_,1,8)
    H[[name_]] = read.table(paste(dir_,'/',file_,sep = ''),sep = sep_,header = TRUE,skip=skip_,comment.char = '')
  }
  return(H)
}

pp = function(t1,t2,sel,add_,sel2,od,do,stejny,titles)
  {
  dd1 = which(od < t1[,1] & t1[,1] < do)
  dd2 = which(od < t2[,1] & t2[,1] < do)
  if (stejny) {
    r1 = range(t1[[sel]][dd1],t2[[sel2]][dd2],na.rm = TRUE)
    r1 = range(t1[[sel]],t2[[sel2]],na.rm = TRUE)
    r2 = r1
  } else {
    r1 = range(t1[[sel]][dd1],na.rm = TRUE)
    r1 = range(t1[[sel]],na.rm = TRUE)
    r2 = range(t2[[sel2]][dd2],na.rm = TRUE)
    r2 = range(t2[[sel2]],na.rm = TRUE)
  }
  names1_ = names(t1)
  names2_ = names(t2)
  par(mar=c(4,4,4,4))
  plot(t1[,1],t1[[sel]],
       ylab = '',type = 'o',lwd=2,xlim = c(od,do),ylim=r1,cex=0.5)
  grid()
  # mtext(paste(basename(titles[1]),":",sel),side = 3,line = 0.8,adj = 0,cex = 1.5)
  mtext(paste(titles[1],":",sel),side = 3,line = 0.8,adj = 0,cex = 1.5)
  mtext(names1_[sel],side = 2,line = 3)
  if (add_) {
    par(new=TRUE)
    plot(t2[,1],t2[[sel2]],
         axes = FALSE, ylab = '',type = 'o',col=2,lwd=2,xlim = c(od,do),ylim=r2,cex=0.5)
    axis(4,col.ticks = 2, col = 2,col.axis=2)
    # mtext(paste(basename(titles[2]),":",sel2),side = 3,line = 2,adj = 1,cex = 1.5, col=2)
    mtext(paste(titles[2],":",sel2),side = 3,line = 2,adj = 1,cex = 1.5, col=2)
    mtext(names2_[sel2],side = 4,line = 3,col = 2)
  }
}

plot_ = function(id1,id2,title='')
  {
  t1 = H[[id1]]
  t2 = H[[id2]]
  titles = substr(names(H)[c(id1,id2)], 104, 160)
  names1_ = names(H[[id1]])
  names2_ = names(H[[id2]])
  # t1$X..time.s. = t1$time.s.
  n1 = length(t1[1,])
  m = length(t1[,1])
  maxCas = t1[,1][m]
  n2 = length(t2[1,])
  manipulate(pp(t1,t2,sel,add_,sel2,od,do,stejny,titles),
             # sel = slider(initial = 5,1,n1,label = 'spoupec v levem grafu'),
             sel = picker(as.list(names1_), initial = as.list(names1_)[[length(as.list(names1_))]]),
             add_= checkbox(TRUE,'pridat druhy graf'),
             stejny= checkbox(TRUE,'stejny meritka'),
             # sel2 = slider(initial = n2, 1,n2,label = 'spoupec v pravem grafu'),
             sel2 = picker(as.list(names2_), initial = as.list(names1_)[[length(as.list(names1_))]]),
             od = slider(initial = 0     ,0,maxCas,label = 'cas od'),
             do = slider(initial = maxCas,0,maxCas,label = 'cas do')
             )
}
#
# End Functions 
#
#

#
#
# Main
#
dir_ = paste(root, outdir, sep='/')
sep_  = ';'
skip_ = 0
extension_ = '*.csv'

files  = c()
for (idir_ in dir_) {
  files = c(files,list.files(idir_,pattern = '*.csv',full.names = TRUE))
}
###
pixel = read.table(paste(files[1],sep = ''),skip=1,nrows = 1,comment.char = '')
pixel = as.numeric(pixel[7])
H = list()
a = 1
b = 1
for (file_ in files) {
  print (file_)
  name_ = substr(file_,1,8)
  name_ = file_
  scenario <- str_extract(file_, "(?<=out_clear/)[^/]+")  # bacha tady je magie - je to adresář za kterým je scenar
  point_no <- str_extract(file_, "(?<=point)\\d+")
  skip_ = 1
  d = read.table(file_, sep = sep_, header = TRUE, skip=skip_, comment.char = '#')
  
  if (any(grepl('infiltration.m.', x = names(d)))){
    d$cumRainfall_m3 = cumsum(d$rainfall.m.*pixel)
    d$cumInfiltration_m3 = cumsum(d$infiltration.m.*pixel)
    d$cumSheetRunoff_m3 = cumsum(d$sheetVRunoff.m3.)
    max_values <- as.numeric(sapply(d, max, na.rm = TRUE))
    cnames = colnames(d)

    if (a == 1){max_surface_df <- as.data.frame(t(max_values), stringsAsFactors = FALSE)
              colnames(max_surface_df) = cnames
              a = 2
              max_surface_df$scenaio = scenario
              max_surface_df$point_no = point_no
    
      }
      else{tmpdf <- as.data.frame(t(max_values), stringsAsFactors = FALSE)
      
          colnames(tmpdf) = cnames
          tmpdf$scenaio = scenario
          tmpdf$point_no = point_no


          max_surface_df = rbind(max_surface_df, tmpdf)
      }
  } else {
    d$cumRainfall_m3 = cumsum(d$rainfall.m.*pixel)
    d$cumRunoff_m3 = cumsum(d$vRunoff.m3.)
    max_values <- as.numeric(sapply(d, max, na.rm = TRUE))
    cnames = colnames(d)

    if (b == 1){max_stream_df <- as.data.frame(t(max_values), stringsAsFactors = FALSE )
    colnames(max_stream_df) = cnames

    b = 2
    max_stream_df$scenaio = scenario
    max_stream_df$point_no = point_no
    }
    else{tmpdf <- as.data.frame(t(max_values), stringsAsFactors = FALSE)
    colnames(tmpdf) = cnames
    tmpdf$scenaio = scenario
    tmpdf$point_no = point_no
 

    max_stream_df = rbind(max_stream_df, tmpdf)}
  }
  H[[name_]] = d
}
if (length(id1_) == length(id2_)) {
  n = length(id1_)
  for (i in 1:n)
  {
    plot_(id1_[i],id2_[i])
  }
} else {
  print ('id1_ and id2_ needs to be same length')
}

names_H = as.character(names(H))

H1 <- H[grepl("point002", names(H))]
twins = length(H)/2-1
a = -1
for (i in 1:twins){
  b = i*2-1 
  plot2_(H1, b, b+1)}


plot2_ = function(Hx, id1,id2,title='')
{
  t1 = Hx[[id1]]
  t2 = Hx[[id2]]
  titles = substr(names(Hx)[c(id1,id2)], 104, 160)
  names1_ = names(Hx[[id1]])
  names2_ = names(Hx[[id2]])
  # t1$X..time.s. = t1$time.s.
  n1 = length(t1[1,])
  m = length(t1[,1])
  maxCas = t1[,1][m]
  n2 = length(t2[1,])
  manipulate(pp(t1,t2,sel,add_,sel2,od,do,stejny,titles),
             # sel = slider(initial = 5,1,n1,label = 'spoupec v levem grafu'),
             sel = picker(as.list(names1_), initial = as.list(names1_)[[length(as.list(names1_))]]),
             add_= checkbox(TRUE,'pridat druhy graf'),
             stejny= checkbox(TRUE,'stejny meritka'),
             # sel2 = slider(initial = n2, 1,n2,label = 'spoupec v pravem grafu'),
             sel2 = picker(as.list(names2_), initial = as.list(names1_)[[length(as.list(names1_))]]),
             od = slider(initial = 0     ,0,maxCas,label = 'cas od'),
             do = slider(initial = maxCas,0,maxCas,label = 'cas do')
  )
}
##plot from streams
#sDTA = max_stream_df[max_stream_df$point_no == "001",]

sDTA = max_stream_df
sDTA$code = paste(sDTA$scenaio, sDTA$point_no, sep = "_")
sDTA <- sDTA %>%
  separate(scenaio, into = c("Catchmet", "Smoderp", "rain_rep", "rain_shape", "rain_len", "initial_condition", "ZK"), sep = "_")
sDTA$simulation = paste(sDTA$rain_rep, sDTA$rain_shape,sDTA$initial_condition, sep = "_")
sDTA$ZK = replace_na(sDTA$ZK, "BEZ")


plott = ggplot(data = sDTA, mapping = (aes(x = sDTA$simulation, y = sDTA$cumRunoff_m3, color =  sDTA$ZK, shape = sDTA$rain_shape))) +
  geom_jitter(size = 3) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
plot(plott)
plott2 = plott + facet_grid(  sDTA$point_no ~ .)
plot(plott2)

##plot from surface
sDTA = max_surface_df[max_surface_df$point_no == "003",]
sDTA = max_surface_df
sDTA$code = paste(sDTA$scenaio, sDTA$point_no, sep = "_")
sDTA <- sDTA %>%
  separate(scenaio, into = c("Catchmet", "Smoderp", "rain_rep", "rain_shape", "rain_len", "initial_condition", "ZK"), sep = "_")
sDTA$simulation = paste(sDTA$rain_rep, sDTA$rain_shape,sDTA$initial_condition, sep = "_")
sDTA$ZK = replace_na(sDTA$ZK, "BEZ")

plott = ggplot(data = sDTA, mapping = (aes(x = sDTA$simulation, y = sDTA$surfaceFlow.m3.s., color =  sDTA$ZK, shape = sDTA$rain_shape))) +
  geom_jitter(size = 3) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
plot(plott)
plott2 = plott + facet_grid(sDTA$rain_rep ~ . )
plot(plott2)



