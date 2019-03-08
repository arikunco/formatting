# Title       : Formatting_Data_Online_Retail.R 
# Description : This is an online retail script and its formatting process.
# Objective   : To format data (after cleaning)
# Data source : https://archive.ics.uci.edu/ml/datasets/online+retail 

# This is a transnational data set which contains all the transactions occurring between 01/12/2010 and 09/12/2011 for a UK-based and registered non-store online retail.The company mainly sells unique all-occasion gifts. Many customers of the company are wholesalers.

# load library
library(tidyverse)
library(lubridate)
library(DataExplorer)

# load data
onlineretail <- read.csv("dataset/OnlineRetail.csv",stringsAsFactors = F)

# load data
onlineretail <- read.csv("dataset/online_retail/OnlineRetail.csv",stringsAsFactors = F)

# preview first 6 rows of data
head(onlineretail)

# preview last 6 rows of data
tail(onlineretail)

# summary of the data 
summary(onlineretail)

plot_missing(onlineretail)

# sampling data 
ol_sample <- onlineretail[sample(1:nrow(onlineretail), size=10000),]
summary(ol_sample)

# We are going to clean ol_sample data. 

# checking variable data
str(ol_sample)
plot_str(ol_sample)

# From the output, are the variables in the right format? 

# Check out the InvoiceDate, it is in character type format, it should be in Date datatype. You have to convert it to date friendly format. Find out the documentation of lubridate package on Help! 

# Convert the data type for InvoiceDate variable from chr to Date (POSIXct)
ol_sample$InvoiceDate <- dmy_hm(ol_sample$InvoiceDate)

# Are there any missing data? 
plot_missing(ol_sample)

# Customer ID have 25% missing value. What are you going to do with that? Please ask your data owner! (in this case, we think that those empty data because the customer has no customer ID or bought as a guest)

# if you want to drop, simply use this command: 
ol_sample_drop <- ol_sample[!is.na(ol_sample$CustomerID),]

# if you want to keep rows with empty customerID, simply use this command, replace with unique value that did not yet input. 
max_CustID <- max(ol_sample[!is.na(ol_sample$CustomerID),]$CustomerID)
ol_sample_imput <- ol_sample
ol_sample_imput$CustomerID[is.na(ol_sample_imput$CustomerID)] <- sample(max_CustID+10000:max_CustID+10000+length(ol_sample), size=sum(is.na(ol_sample_imput$CustomerID)), replace=F)
length(ol_sample[is.na(ol_sample$CustomerID),])

# check missing data again 
plot_missing(ol_sample_drop)
plot_missing(ol_sample_imput)

# let's assume we use the drop data (only complete customer id). We would like to change contry column from United Kingdom into Inggris for example, we can use gsub.

# select, filter, mutate, and group_by in one shot! :)  
profil_pelanggan <- ol_sample_drop %>% 
  select(CustomerID,Country) %>% 
  mutate(Province=
           ifelse(Country=='United Kingdom','DKI Jakarta',
                  ifelse(Country=='Germany','Jawa Barat', 
                         ifelse(Country=='EIRE','Banten',ifelse(
                           Country=='Spain','Jawa Tengah',
                           ifelse(Country=='France','DI Yogyakarta',
                                  ifelse(Country=='Switzerland','Jawa Timur','Luar Jawa'))))))) %>% 
  select(CustomerID, Province) %>% unique() %>% na.omit()

produk <- ol_sample_drop %>% select(StockCode,Description,UnitPrice) %>% distinct() %>% mutate(UnitPrice2=20000*UnitPrice) %>% select(StockCode,Description,UnitPrice2) %>% group_by(StockCode) %>% filter(row_number()==1)

transaksi <- ol_sample_drop %>% select(InvoiceDate,InvoiceDate,StockCode,Quantity,CustomerID) %>% distinct() %>% filter(CustomerID>10000)

write.csv(transaksi,"transaksi.csv",row.names = F)
write.table(produk,"produk.csv",row.names = F,sep = "|",quote = F)
write.csv(profil_pelanggan,"profile_pelanggan.csv",row.names = F,quote=F)

# Exercise of tidyr (changing profil_pelanggan from long to wide)
profil_pelanggan$jumlah <- 1
profil_pelanggan_to_wide <- spread(profil_pelanggan,Province,jumlah)

# Exercise of tidyr (changing profil_pelanggan from wide to long)
profil_pelanggan_to_long <- gather(profil_pelanggan_to_wide,Province,Jumlah,-CustomerID,na.rm = T)
head(profil_pelanggan_to_long)

# Quiz      : Please make a tidy table from produk, transaksi, and profil_pelanggan table, thus contain the following variables: 
# CustomerID | Recency | Frequency | Amount 
# Recency   : jumlah hari ini s.d. terakhir bertransaksi (dalam hari)
# Frequency : jumlah transaksi yang terjadi dalam 6 bulan terakhir 
# Monetary  : jumlah rupiah yang dibelanjakan oleh Customer ID unik

rfm_df <- profil_pelanggan %>% 
  left_join(transaksi, by=CustomerID) %>% left_join() 
