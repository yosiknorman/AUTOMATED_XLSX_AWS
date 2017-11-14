#!/usr/bin/Rscript

# Created By Yosik Norman, 2017
# Tittle: Discontinuity String Decomposition of AWS's Datasets

# Data : 10 minutes Maros AAWS Rainfall Accumulation data (COORDINATE POINT : xxx;yyy )

library(xlsx) 

# START PROGRAM

rm(list = ls())

f = list.files("../data/",pattern = "Download")
f = f[length(f)]
f = f[1]
dat = read.delim2( sprintf("../data/%s",f),header = T)
id = as.character(unique(dat$ID.Station))[1]
hujan = (as.vector(as.numeric(as.character(dat$RR))))
akhir_okt = which(dat$Date.Time == "2017-10-31 23:50:00")   # input menit akhir bulan verifikasi
dat1 = data.frame(id=id,time = as.character(dat$Date.Time),RR = hujan)
dat1 = dat1[1:akhir_okt,]
hujan[is.na(hujan)] = 0

hour = substr(dat1$time,1,13)
n_hour <- length(unique(substr(dat1$time,1,13)))-1
i_hour <- unique(substr(dat1$time,1,13))[1:n_hour]

hujan_jam = c()
for(i in 1:n_hour){
  hujan_jam[i] =mean(dat1$RR[hour == i_hour[i]])    # Rata-rata per-jam
}

hasil = data.frame(jam=i_hour,hourly_rainfall=hujan_jam)

plot(hujan_jam,type = "l")

# ================  Decomposition of time =================
start <- as.POSIXct("2017-10-01",tz = "UTC")
interval <- 60
end <- start + as.difftime(31, units="days")
jam_ori = seq(from=start, by=interval*60, to=end)
length(jam_ori)
jam_ori=substr(jam_ori,1,13)[1:(length(jam_ori)-1)]
hasil$hourly_rainfall
i_hilang =which(!(jam_ori %in% hasil$jam))
i_ada =which((jam_ori %in% hasil$jam))

hasil_full=rep(NA,length(jam_ori))
for(i in 1:length(i_ada)){
  hasil_full[i_ada[i]] = hasil$hourly_rainfall[i]
}


hasil_full = data.frame(time=jam_ori,hourly_rainfall = hasil_full)

# ++++++++++++++++++++++ SESUAIKAN FORMAT VERIFIKASI ++++++++++++++++++++++++
nrow = length(hasil_full$hourly_rainfall)/24
ncol = length(hasil_full$hourly_rainfall)/ncol

matHas  = matrix(hasil_full$hourly_rainfall,ncol = ncol,byrow = T)
row_date = substr(hasil$jam,1,10)
row_date = unique(row_date)
rownames(matHas) = row_date
jaam = as.character(0:23)
colnames(matHas) = jaam
matHas = data.frame(matHas)
nameee = unique(substr(row_date,1,7))
jamee = substr(hasil$jam,12,13)
write.xlsx(x = matHas, file = sprintf("../output/%s.xlsx",nameee),
           sheetName = nameee, row.names = T)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
write.xlsx(x = "Selamat datang", file = "../output/verifikasi_aws.xlsx",
           sheetName = "sss", row.names = FALSE)
wb = loadWorkbook(file = "../output/verifikasi_aws.xlsx")  

sheet = createSheet(wb,sheetName = row_date[1])
addDataFrame(matHas[1,], sheet=sheet, startColumn=2, row.names=T,characterNA = "",col.names = T)
saveWorkbook(wb, "../output/verifikasi_aws.xlsx")

wb = loadWorkbook(file = "../output/verifikasi_aws.xlsx")  
sheet = list()
for(i in 2:length(row_date)){
  sheet[[i]] = createSheet(wb,sheetName = row_date[i])
  addDataFrame(matHas[i,], sheet=sheet[[i]], startColumn=1, row.names=T,characterNA = "",col.names = T)
  saveWorkbook(wb, "../output/verifikasi_aws.xlsx")
}


# END PROGRAM

