#https://www.kaggle.com/datasets/ahmetfurkandemr/superlig-sezonlari?resource=download
#https://www.tff.org/default.aspx?pageID=1628
#https://www.tff.org/default.aspx?pageID=1648

library(tidyverse)


#File Path
dir_nm <- paste0(getwd(), "/Futbol Sezonlar/TFF Data")

#Write all subfolers and seasons.csv files into a tibble
seasons<-
dir(path = dir_nm) |> 
  tibble() |> 
  rename_with(~"folder_nm") |> 
  mutate(csv_filename = str_c(dir_nm, "/",folder_nm ,"/", folder_nm, ".csv")) 

#The function to read csv file and create a new column named "Sezon" with the name of the folder which includes .csv file
read_csv_season <- function(folder_nm, csv_filename){
  read_csv(csv_filename, show_col_types = FALSE) |> 
    mutate(Sezon = folder_nm)  |> 
    arrange(desc(Puan)) |> 
    mutate(Rank = row_number(), .before =Takim_adi )
}


pattern1 <- "ATAKAŞ|AYTEMİZ|VESTEL|YILPORT|YUKATEL|HANGİKREDİ|GZT|YİMPAŞ|CORENDON|BITEXEN|MEDICANA|MEDİPOL|MEDİCAL|EMS YAPI"     

pattern2 <- "TELESET MOBİLYA|JETPA|SİLTAŞ YAPI|SANİCA BORU|RAMS|MONDİHOME|HELENEX|HES KABLO|SUAT ALTIN İNŞAAT|TORKU|TÜMOSAN"

pattern3 <- "BTC TURK|DEMİR GRUP|VAVACARS|FRAPORT TAV|FRAPORT-TAV|EVKUR|OFTAŞSPOR|İTTİFAK HOLDİNG|İSTİKBAL MOBİLYA|ÖZNUR KABLO"

pattern4 <- "ATİKER|ARABAM.COM"

pattern5 <- paste(pattern1, pattern2, pattern3, pattern4, sep = "|")



col_names_lookup <- c("Position", "Club", "Played", "Won", "Drawn", "Lost", "Goals_For", "Goals_Against", "Goals_Difference", "Points", "Season" )

#bind all seasons together from 1990 to 2024
map2(seasons$folder_nm, seasons$csv_filename, read_csv_season) |> 
  bind_rows() |> 
  arrange(Sezon, Rank) |> 
  mutate(Takim_adi = str_remove(Takim_adi, pattern = pattern5)) |> #remove sponsor company names
  mutate(Takim_adi = str_remove(Takim_adi, regex("A\\.Ş$| AŞ$|AŞ\\.$|A\\.Ş\\.$", ignore_case = TRUE))) |> #remove A.Ş
  mutate(Takim_adi = str_trim(Takim_adi)) |> #remove space
  mutate(
    Takim_adi = case_when(
      str_detect(Takim_adi, regex("KARDEMİR|KARABÜKSPOR", ignore_case = TRUE)) ~ "KARDEMİR KARABÜKSPOR",
      str_detect(Takim_adi, regex("DEMİRSPOR|DEMIRSPOR", ignore_case = TRUE)) ~ "ADANADEMİRSPOR",
      str_detect(Takim_adi, "MERSİN") ~ "MERSİNİDMANYURDU",
      str_detect(Takim_adi, "PENDİKSPOR") ~ "PENDİKSPOR",
      str_detect(Takim_adi, "ANKARASPOR") ~ "ANKARASPOR",
      str_detect(Takim_adi, "BAŞAKŞEHİR") ~ "BAŞAKŞEHİR",
      str_detect(Takim_adi, "ANTEP") ~ "GAZİANTESPOR",
      str_detect(Takim_adi, regex("NBURNUSPOR|ZEYTİNBURNUSPOR", ignore_case = TRUE)) ~ "ZEYTİNBURNUSPOR",
      str_detect(Takim_adi, "ERZURUMSPOR") ~ "ERZURUMSPOR",
      str_detect(Takim_adi, "AKHİSAR") ~ "AKHİSARSPOR",
      str_detect(Takim_adi, "KAYSERİSPOR") ~ "KAYSERİSPOR",
      str_detect(Takim_adi, regex("MKE\\sANKARA", ignore_case = TRUE)) ~ "MKE ANKARAGÜCÜ",
      str_detect(Takim_adi, "DARDANELSPOR") ~ "ÇANAKKALE DARDANELSPOR",
      str_detect(Takim_adi, regex("BÜYÜKŞEHİR\\sBLD|İSTANBUL\\sBBSK", ignore_case = TRUE)) ~ "İSTANBUL BBSK",
      TRUE ~ Takim_adi # Keep original if no match
                        )
       ) |> 
    rename_with(~col_names_lookup)
  
  


