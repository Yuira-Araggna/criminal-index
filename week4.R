#install.packages("zoo")
#install.packages("mvn")
#install.packages("MVN")
#install.packages("ggpubr")

#Load library
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)

#Baca data dari file Excel
crime_index = read_xlsx("_Index_Crimes_by_County_and_Agency__Beginning_1990.xlsx", sheet=1)

#Pastikan data berbentuk dataframe
crime_index = as.data.frame(crime_index)

#Ganti missing values atau NA pada dataframe dengan mean pada kolomnya
for(i in 4:(ncol(crime_index)-1)){
  crime_index[is.na(crime_index[,i]), i] <- mean(as.double(crime_index[,i]), na.rm = TRUE)
}

#Hapus baris region total dan county total
new_index = subset(crime_index, crime_index$County!="Region Total" & crime_index$Agency!="County Total")



#======================================================================================================
#Cari jumlah tiap kategori kejahatan berdasarkan county dan tahun
murder_year <- aggregate(as.numeric(new_index$Murder),
                         by=list(Year=new_index$Year,Country=new_index$County),
                         FUN=sum)
rape_year <- aggregate(as.numeric(new_index$Rape),
                       by=list(Year=new_index$Year,Country=new_index$County),
                       FUN=sum)
robbery_year <- aggregate(as.numeric(new_index$Robbery),
                          by=list(Year=new_index$Year,Country=new_index$County),
                          FUN=sum)
theft_year <- aggregate(as.numeric(new_index$`Motor Vehicle Theft`),
                        by=list(Year=new_index$Year,Country=new_index$County),
                        FUN=sum)


aggravated_assault = aggregate(as.numeric(new_index$`Aggravated Assault`),
                               by=list(Year=new_index$Year,Country=new_index$County),
                               FUN=sum)


property = aggregate(as.numeric(new_index$`Property Total`),
                     by=list(Year=new_index$Year,Country=new_index$County),
                     FUN=sum)


burglary = aggregate(as.numeric(new_index$Burglary),
                     by=list(Year=new_index$Year,Country=new_index$County),
                     FUN=sum)



larceny = aggregate(as.numeric(new_index$Larceny),
                     by=list(Year=new_index$Year,Country=new_index$County),
                     FUN=sum)


#Visualisasikan data dalam bentuk plot line
ggplot(murder_year, aes(Year, x)) +
  geom_line(aes(color=Country)) +
  labs(title="Murder Index in each Country by Years", x = "Years", y = "Murder Case")


ggplot(rape_year, aes(Year, x)) +
  geom_line(aes(color=Country)) +
  labs(title="Rape Index in each Country by Years", x = "Years", y = "Rape Case")

ggplot(robbery_year, aes(Year, x)) +
  geom_line(aes(color=Country)) +
  labs(title="Robbery Index in each Country by Years", x = "Years", y = "Robbery Case")

ggplot(theft_year, aes(Year, x)) +
  geom_line(aes(color=Country)) +
  labs(title="Motor Theft Index in each Country by Years", x = "Years", y = "Motor Theft Case")




ggplot(aggravated_assault, aes(Year, x)) +
  geom_line(aes(color=Country)) +
  labs(title="aggravated assault Index in each Country by Years", x = "Years", y = "Murder Case")




ggplot(property, aes(Year, x)) +
  geom_line(aes(color=Country)) +
  labs(title="Property stolen Index in each Country by Years", x = "Years", y = "Murder Case")



ggplot(burglary, aes(Year, x)) +
  geom_line(aes(color=Country)) +
  labs(title="Burglary Index in each Country by Years", x = "Years", y = "Murder Case")




ggplot(larceny, aes(Year, x)) +
  geom_line(aes(color=Country)) +
  labs(title="Larceny Index in each Country by Years", x = "Years", y = "Murder Case")


#======================================================================================================

#normal density plot untuk semua kategori kejahatan
ggqqplot(murder_year$x) + labs(title="Murder Index")
ggqqplot(rape_year$x) + labs(title="Rape Index")
ggqqplot(robbery_year$x) + labs(title="Robbery Index")
ggqqplot(theft_year$x) + labs(title="Motor Theft Index")
ggqqplot(aggravated_assault$x) + labs(title="Aggravated Assault Index")
ggqqplot(property$x) + labs(title="Property stolen Index")
ggqqplot(burglary$x) + labs(title="Burglary Index")
ggqqplot(larceny$x) + labs(title="Larceny Index")


ggdensity(murder_year$x) + labs(title="Murder Index")
ggdensity(rape_year$x)+ labs(title="Rape Index")
ggdensity(robbery_year$x) + labs(title="Robbery Index")
ggdensity(theft_year$x) + labs(title="Motor Theft Index")
ggdensity(aggravated_assault$x) + labs(title="Aggravated Assault Index")
ggdensity(property$x) + labs(title="Property stolen Index")
ggdensity(burglary$x) + labs(title="Burglary Index")
ggdensity(larceny$x) + labs(title="Larceny Index")
