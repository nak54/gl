library(data.table)
library(readxl)
library(stringr)
#https://www2.census.gov/library/reference/naics/technical-documentation/concordance/
sic87_naics02 <- read_excel("1987_SIC_to_2002_NAICS.xls") #data frame 2168 rows 4 columns includes header
naics02_07 <- read_excel("2002_to_2007_NAICS.xls",skip = 2) #data frame 1200 rows 4 columns includes header
naics07_12 <- read_excel("2007_to_2012_NAICS.xls",skip = 2) #data frame 1184 rows 4 columns includes header
naics12_17 <- read_excel("2012_to_2017_NAICS.xlsx",skip = 2)#data frame 1069 rows 4 columns includes header

naics17 <- read_excel("NAICS6-digit_2017_Codes.xlsx") #1057x2
setDT(naics17)
naics17 <- naics17[,1:2]
colnames(naics17) <- c("naics17","naics17Title")
naics17[,naics17:=as.character(naics17)]

s87_n02_cols <- c("SIC87","SIC87Title","NAICS2002","NAICS2002Title")
n02_07_cols <- c("NAICS2002","NAICS2002Title","NAICS2007","NAICS2002Title")
n07_12_cols <- c("NAICS2007","NAICS2007Title","NAICS2012","NAICS2012Title")
n12_17_cols <- c("NAICS2012","NAICS2012Title","NAICS2017","NAICS2017Title")

colnames(sic87_naics02) <- s87_n02_cols
colnames(naics02_07) <- n02_07_cols
colnames(naics07_12) <- n07_12_cols
colnames(naics12_17) <- n12_17_cols

dt1 <- as.data.table(sic87_naics02)
dt2 <- as.data.table(naics02_07)
dt3 <- as.data.table(naics07_12)
dt4 <- as.data.table(naics12_17)
dt3 <- dt3[,1:4] #
dt4 <- dt4[,1:4] #rem empty col

# Keep only the 2 key columns in each table and rename to a standard schema
dt1 <- dt1[, .(sic = get("SIC87"),  naics02 = get("NAICS2002"))]
dt1
dt2 <- dt2[, .(naics02 = get("NAICS2002"), naics07 = get("NAICS2007"))]
dt3 <- dt3[, .(naics07 = get("NAICS2007"), naics12 = get("NAICS2012"))]
dt4 <- dt4[, .(naics12 = get("NAICS2012"), naics17 = get("NAICS2017"))]

# Normalize to strings (preserve leading zeros)

dt1[, sic := str_pad(as.character(sic),4,side ="left",pad="0")]
dt1[, naics02 := as.character(naics02)]
dt1
dt2[, `:=`(naics02 = as.character(naics02), naics07 = as.character(naics07))]
dt3[, `:=`(naics07 = as.character(naics07), naics12 = as.character(naics12))]
dt4[, `:=`(naics12 = as.character(naics12), naics17 = as.character(naics17))]


# Drop blanks
dt1 <- dt1[!is.na(sic) & sic != "" & !is.na(naics02) & naics02 != ""]
dt2 <- dt2[!is.na(naics02) & naics02 != "" & !is.na(naics07) & naics07 != ""]
dt3 <- dt3[!is.na(naics07) & naics07 != "" & !is.na(naics12) & naics12 != ""]
dt4 <- dt4[!is.na(naics12) & naics12 != "" & !is.na(naics17) & naics17 != ""]
dt1h <- head(dt1);dt2h <-head(dt2);dt3h <-head(dt3);dt4h <-head(dt4)
# Many-to-many chain (allow.cartesian is expected for real concordances)
setkey(dt1, naics02); setkey(dt2, naics02)
x <- dt1[dt2, allow.cartesian = TRUE, nomatch = 0][, .(sic, naics07)]

setkey(x, naics07); setkey(dt3, naics07)
x <- x[dt3, allow.cartesian = TRUE, nomatch = 0][, .(sic, naics12)]

setkey(x, naics12); setkey(dt4, naics12)
x <- x[dt4, allow.cartesian = TRUE, nomatch = 0][, .(sic, naics17)]

# Deduplicate final SIC -> NAICS2017 matches
x <- unique(x)[order(sic, naics17)]

# Roll up to NAICS 2- and 3-digit
x[, `:=`(naics2 = substr(naics17, 1, 2), naics3 = substr(naics17, 1, 3))]

merge_sic_naics <- merge.data.table(x,naics17,by="naics17",all.y = T)

sic_to_naics2 <- unique(merge_sic_naics[, .(sic, naics2,naics17Title)])[order(sic, naics2)]
sic_to_naics3 <- unique(merge_sic_naics[, .(sic, naics3,naics17Title)])[order(sic, naics3)]

# Ambiguity stats
sic_naics2_stats <- sic_to_naics2[, .(
  naics2_count = uniqueN(naics2),
  is_ambiguous_naics2 = uniqueN(naics2) > 1
), by = sic]

sic_naics3_stats <- sic_to_naics3[, .(
  naics3_count = uniqueN(naics3),
  is_ambiguous_naics3 = uniqueN(naics3) > 1
), by = sic]

# Attach stats (optional)
sic_to_naics2 <- sic_to_naics2[sic_naics2_stats, on = "sic"]
sic_to_naics3 <- sic_to_naics3[sic_naics3_stats, on = "sic"]

# library(openxlsx)
# write.xlsx(dt,file)
