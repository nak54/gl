#md&a 24_25 from https://github.com/john-friedman https://github.com/john-friedman/Every-10-K-MDA-01-01-1993-12-21-2025.
#create mda industry data for 24 25 for corpus of TM work
pacman::p_load(data.table,readxl,stringr,arrow)

b_dir <- "C:\\Users\\d1ttb\\Downloads\\fy26_wdir\\nlp\\sec_industry_mda\\"
f_p1 <- paste0(b_dir,"p_sec\\2024\\sec_ein_mapping2024.xlsx")
f_p2 <- paste0(b_dir,"p_sec\\2025\\sec_ein_mapping2025.xlsx")
master_cik_sic <-  read_excel(f_p1)
master_cik_sic

setDT(master_cik_sic)
print(nrow(master_cik_sic))

tmp <- read_excel(f_p2); setDT(tmp);tmp
print(nrow(tmp))
master_cik_sic <- rbindlist(list(master_cik_sic,tmp),use.names = TRUE, fill = T)
print(nrow(master_cik_sic))
rm(tmp)
master_cik_sic[,rn:=NULL]

a_dir <- "C:\\Users\\d1ttb\\Downloads\\fy26_wdir\\sec_industry_mda\\sec_mda_24_25\\2024_pq\\"
c_dir <- "C:\\Users\\d1ttb\\Downloads\\fy26_wdir\\sec_industry_mda\\sec_mda_24_25\\2025_pq\\"

dt0 <- read_parquet(paste0(a_dir,"batch_0.parquet"));  setDT(dt0);
dt1 <- read_parquet(paste0(a_dir,"batch_1.parquet"));  setDT(dt1);
dt2 <- read_parquet(paste0(a_dir,"batch_2.parquet"));  setDT(dt2);
dt3 <- read_parquet(paste0(a_dir,"batch_3.parquet"));  setDT(dt3);
dt4 <- read_parquet(paste0(a_dir,"batch_4.parquet"));  setDT(dt4);
dt5 <- read_parquet(paste0(a_dir,"batch_5.parquet"));  setDT(dt5);
dt6 <- read_parquet(paste0(a_dir,"batch_6.parquet"));  setDT(dt6);
print(nrow(dt0)+nrow(dt1)+nrow(dt2)+nrow(dt3)+nrow(dt4)+nrow(dt5)+nrow(dt6))
master_mda <- rbindlist(list(dt0,dt1,dt2,dt3,dt4,dt5,dt6),use.names = TRUE, fill = T); print(nrow(master_mda))
rm(dt0);rm(dt1);rm(dt2);rm(dt3);rm(dt4);rm(dt5);rm(dt6)

dt0 <- read_parquet(paste0(c_dir,"batch_0.parquet"));  setDT(dt0);
dt1 <- read_parquet(paste0(c_dir,"batch_1.parquet"));  setDT(dt1);
dt2 <- read_parquet(paste0(c_dir,"batch_2.parquet"));  setDT(dt2);
dt3 <- read_parquet(paste0(c_dir,"batch_3.parquet"));  setDT(dt3);
dt4 <- read_parquet(paste0(c_dir,"batch_4.parquet"));  setDT(dt4);
dt5 <- read_parquet(paste0(c_dir,"batch_5.parquet"));  setDT(dt5);
dt6 <- read_parquet(paste0(c_dir,"batch_6.parquet"));  setDT(dt6);
print(nrow(dt0)+nrow(dt1)+nrow(dt2)+nrow(dt3)+nrow(dt4)+nrow(dt5)+nrow(dt6))
tmp <- rbindlist(list(dt0,dt1,dt2,dt3,dt4,dt5,dt6),use.names = TRUE, fill = T); print(nrow(tmp))
print(nrow(tmp)+nrow(master_mda));
master_mda <- rbindlist(list(master_mda,tmp));print(nrow(master_mda))
rm(tmp);rm(dt0);rm(dt1);rm(dt2);rm(dt3);rm(dt4);rm(dt5);rm(dt6)
head(master_mda[,.SD,.SDcols = c("accession","year","negative_ratio")])
colnames(master_mda)
master_mda[,accession:=as.character(accession)]
master_mda[,accession:=str_pad(accession,18,side ="left",pad="0")]
head(master_mda[,.SD,.SDcols = c("accession","year","negative_ratio")])
master_mda[,cik:=str_sub(accession,1,10)]
head(master_mda[,.SD,.SDcols = c("accession","cik","year","negative_ratio")])

#rem dup

master_mda <- unique(master_mda ,by=c("accession","year","cik")) #11912,11912
master_cik_sic <- unique(master_cik_sic,by=c("cik","ein","sic","ftyp","acesnum")) #16305,16276

#drop rows without cik
master_mda <-master_mda[!is.na(cik) & cik != ""]
master_cik_sic <- master_cik_sic[!is.na(cik) & cik != ""]

merged_mda_cik <- merge.data.table(master_mda,master_cik_sic,by="cik")
colnames(merged_mda_cik)

merged_mda_cik[,.N,by="sic"]
merged_mda_cik[,.N,by=c("year","sic")]

#loaded sic_naics workspace
#tmp <- sic_to_naics3[,.N, by="naics17Title"] #2204 -> 1057 & 464 (N>1)
#2204 -- 2201
sic_to_naics3 <- sic_to_naics3[!is.na(sic) & sic != ""]
sic_to_naics3 <- sic_to_naics3[!is.na(naics3) & naics3 != ""]

#merged_mda_cik 4615 rows prior to sic crosswalk
colnames(merged_mda_cik)
# [1] "cik"            "accession"      "year"           "mda_text"       "negative_ratio" "ein"            "name"          
# [8] "sic"  
#re run this point on 260410
tmp <- merge.data.table(sic_to_naics3,naics17,by="naics17Title")
tmp <- tmp[!is.na(sic) & sic != ""]
tmp
setkeyv(tmp,c("sic"))
tmp
setkeyv(merged_mda_cik,c("sic"))
head(merged_mda_cik[,.SD,.SDcols = c("cik","ein","sic","accession")])
Dt2_first <- tmp[,.SD[1L],by=c("sic")]
Dt2_first
merged_mda_cik_naics <- Dt2_first[merged_mda_cik]

merged_mda_cik[!is.na(sic) & sic != "",.N] 

merged_mda_cik_naics <- merged_mda_cik_naics[!is.na(sic) & sic != ""]
merged_mda_cik_naics <- merged_mda_cik_naics[!is.na(naics3) & naics3 != ""]
head(merged_mda_cik_naics[,.SD,.SDcols = c("cik","ein","sic","accession")]) #3163 rows

#which rows get dropped
tmp2 <- merged_mda_cik_naics[,.SD,.SDcols = c("cik","ein","sic","name","naics3")]
tmp3 <- merged_mda_cik[,.SD,.SDcols = c("cik","ein","sic","name")]
tmp4 <- tmp3[!tmp2, on=c("cik","ein","sic","name")]
View(tmp2[tmp4[!is.na(sic) & sic != ""],on = "cik"])
#these sic dont have naics 17?
tmp5 <- sic_to_naics3[tmp4[!is.na(sic) & sic != ""],on = "sic"]

# > save.image("C:/Users/d1ttb/Downloads/fy26_wdir/sec_industry_mda/mda_cik_industry_24_25_2.RData")
# > merged_mda_cik[!is.na(sic) & sic != "",.N] 
# [1] 4125
# > merged_mda_cik[!is.na(naics3) & naics3 != "",.N]
# Error in .checkTypos(e, names_x) : 
#   Object 'naics3' not found amongst [cik, accession, year, mda_text, negative_ratio, ein, name, sic]
# > tmp2 <- merged_mda_cik_naics[,.SD,.SDcols = c("cik","ein","sic","name")]
# > tmp3 <- merged_mda_cik[,.SD,.SDcols = c("cik","ein","sic","name")]
# > tmp3[!tmp2, on=c("cik","ein","sic","name")]
# Key: <sic>
#              cik       ein    sic                   name
#           <char>    <char> <char>                 <char>
#    1: 0000017313 751072796   <NA> CAPITAL SOUTHWEST CORP
#    2: 0000017313 751072796   <NA> CAPITAL SOUTHWEST CORP
#    3: 0001143513 542040781   <NA> GLADSTONE CAPITAL CORP
#    4: 0001143513 542040781   <NA> GLADSTONE CAPITAL CORP
#    5: 0001280784 743113410   <NA> Hercules Capital, Inc.
#   ---                                                   
# 1448: 0001364954 203237489   8200             CHEGG, INC
# 1449: 0001607939 271779864   8200            Udemy, Inc.
# 1450: 0001607939 271779864   8200            Udemy, Inc.
# 1451: 0001819404 981499860   8200             Nerdy Inc.
# 1452: 0001819404 981499860   8200             Nerdy Inc.
# > #which rows get dropped
# > tmp2 <- merged_mda_cik_naics[,.SD,.SDcols = c("cik","ein","sic","name","naics3")]
# > tmp3 <- merged_mda_cik[,.SD,.SDcols = c("cik","ein","sic","name")]
# > tmp4 <- tmp3[!tmp2, on=c("cik","ein","sic","name")]
# > View(tmp4)
# > tmp2[tmp4[!is.na(sic)],on = cik]
# Error in eval(onsub, parent.frame(2L), parent.frame(2L)) : 
#   object 'cik' not found
# > tmp2[tmp4[!is.na(sic)],on = "cik"]
#              cik    ein    sic   name naics3     i.ein  i.sic                          i.name
#           <char> <char> <char> <char> <char>    <char> <char>                          <char>
#    1: 0001020242   <NA>   <NA>   <NA>   <NA> 133347003        DEUTSCHE BANK NATIONAL TRUST Co
#    2: 0001020242   <NA>   <NA>   <NA>   <NA> 133347003        DEUTSCHE BANK NATIONAL TRUST Co
#    3: 0001020242   <NA>   <NA>   <NA>   <NA> 133347003        DEUTSCHE BANK NATIONAL TRUST Co
#    4: 0001020242   <NA>   <NA>   <NA>   <NA> 133347003        DEUTSCHE BANK NATIONAL TRUST Co
#    5: 0001020242   <NA>   <NA>   <NA>   <NA> 133347003        DEUTSCHE BANK NATIONAL TRUST Co
#   ---                                                                                        
# 1355: 0001364954   <NA>   <NA>   <NA>   <NA> 203237489   8200                      CHEGG, INC
# 1356: 0001607939   <NA>   <NA>   <NA>   <NA> 271779864   8200                     Udemy, Inc.
# 1357: 0001607939   <NA>   <NA>   <NA>   <NA> 271779864   8200                     Udemy, Inc.
# 1358: 0001819404   <NA>   <NA>   <NA>   <NA> 981499860   8200                      Nerdy Inc.
# 1359: 0001819404   <NA>   <NA>   <NA>   <NA> 981499860   8200                      Nerdy Inc.
# > View(tmp4[!is.na(sic)])
# > View(tmp2[tmp4[!is.na(sic) & sic != ""],on = "cik"])
# > View(sic_to_naics3[tmp4[!is.na(sic) & sic != ""],on = "sic"])
# > #these sic dont have naics 17?
# > tmp5 <- sic_to_naics3[tmp4[!is.na(sic) & sic != ""],on = "sic"]
# > unique(tmp5$sic) 128 sic dont have map to naics
#  "0100" "0700" "0900" "1000" "1040" "1090" "1220" "1400" "1540" "1600" "1700" "2000" "2030" "2040" "2060" "2070" "2080" "2090"
#  "2300" "2320" "2330" "2340" "2400" "2430" "2510" "2520" "2650" "2670" "2750" "2780" "2800" "2810" "2820" "2860" "2870" "2890"
#  "2990" "3050" "3060" "3100" "3140" "3290" "3310" "3350" "3360" "3390" "3420" "3430" "3440" "3460" "3470" "3480" "3490" "3510"
#  "3530" "3540" "3550" "3560" "3570" "3576" "3580" "3590" "3600" "3620" "3630" "3640" "3670" "3690" "3720" "3730" "3760" "3790"
#  "3910" "3990" "4210" "4400" "4610" "4700" "4900" "4955" "4991" "5000" "5010" "5030" "5040" "5070" "5080" "5090" "5140" "5150"
# "5160" "5190" "5200" "5400" "5500" "5600" "5700" "5810" "5900" "5940" "5990" "6189" "6199" "6200" "6500" "6510" "6770" "6795"
#  "7000" "7200" "7310" "7320" "7330" "7340" "7350" "7370" "7380" "7500" "7510" "7600" "7830" "7900" "7990" "8000" "8050" "8060"
#  "8090" "8200"

library(openxlsx)
write.xlsx(merged_mda_cik_naics, "mda_cik_industry_24_25.xlsx_2") #50 warnings() Number of characters exeed the limit of 32767.
library(arrow)
write_parquet(merged_mda_cik_naics,"mda_cik_industry_24_25_2.parquet") #260410
