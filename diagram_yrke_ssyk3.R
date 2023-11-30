# ==============================================================================
#
# Skapar diagram över yrken på SSYK3-nivå. Följane diagram finns:
#                    1. Hur ett eller flera yrken har utvecklats över tid (alla år hämtas) 
#                    2. Hur stor andel av arbetsmarknaden ett (eller flera) yrke(n) utgör i vald kommun/region. 
#                    3. Hur stor andel av arbetsmarknaden utgör ett yrke jämfört med andra kommuner (endast kommuner).
#
# Data kommer från SCB:s öppna statistikdatabas, Arbetmarknad - Yrkesregistret med yrkesstatistik. För att välja yrken i en dialogruta kan 
# skriptet dlg_valj_yrken köras
#
# Parametrar: 
#              - vald_geografi    = kommunkoder, det är inga län med i datasetet, vill man ha med län får man köra alla kommuner och
#                                   gruppera dem med gruppering_namn = "Dalarna" tex. 
#
#              - valt_ar          = vektor med de år man vill ha med, NA = senaste året, startår är år 2002
#
# ==============================================================================

library(pxweb)
library(httr)
library(tidyverse)
library(svDialogs)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)


diag_yrke <- function(vald_geografi = "20",           # kan vara en eller flera geografier, skicka med vektor om flera, ex: c("20", "25") 
                      valt_ar = NA,                                                # om NA väljs senaste möjliga år, väljs ett år som ligger efter tillgängliga år så väljs istället senaste möjliga år
                      jmfr_ar = NA, 
                      valt_yrke_ssyk2012 = NA,
                      valt_yrke_ssyk96 = NA,
                      namn_yrkesgrupp = NA,
                      jmfr_region = NA,               # om NA så används samtliga regioner när man jämför, om länskoder jämförs bara län, om kommunkoder jämförs bara kommuner, om det finns båda så jämförs båda
                      lagg_ihop_yrken = TRUE,         # TRUE så läggs alla valda yrken ihop till en grupp, annars används de en och en
                      manual_color = NA,              # om man vill skicka med en egen färgpalett till diagram 1
                      output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
                      diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Peter Möller, Region Dalarna",
                      lagg_till_logga = TRUE,
                      logga_sokvag = NA,
                      skapa_fil = TRUE,
                      diagram_utv_over_tid = TRUE,
                      diagram_jmfr_alla_kommuner = FALSE,     # diagram med andel av valda yrken jämfört med alla kommuner - TAR LÅNG TID!
                      diagram_andel_av_alla_yrken = TRUE     # 
) {
  
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG58N",   # Anställda 16-64 år med arbetsplats i regionen (dagbef) efter region, yrke (3-siffrig SSYK 2012) och kön. Ny tidsserie. År 2019 - 2020
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG58",    # Anställda 16-64 år med arbetsplats i regionen (dagbef) efter region, yrke (3-siffrig SSYK 2012) och kön. År 2014 - 2018
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG17")    # Anställda 16-64 år med arbetsplats i regionen (dagbef) efter region, yrke (3-siffrig SSYK 96) och kön. År 2001 - 2013
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  
  # hämta bara de regioner som man behöver
  giltig_region <- map(url_list, ~ hamta_giltiga_varden_fran_tabell(.x, "region")) %>% unlist() %>% unique()
  
  if (all(is.na(jmfr_region))) {
    if(all(nchar(vald_geografi) ==2)) jmfr_region <- giltig_region[nchar(giltig_region) == 2 & !giltig_region %in% "99"]
    if(all(nchar(vald_geografi) ==4)) jmfr_region <- giltig_region[nchar(giltig_region) == 4 & !giltig_region %in% "9999"]
    if(any(nchar(vald_geografi) == 2) & any(nchar(vald_geografi) == 4)) jmfr_region <- giltig_region[!giltig_region %in% c("99", "9999")]
  }
  region_var <- if (diagram_jmfr_alla_kommuner) c(vald_geografi, jmfr_region) %>% unique() else vald_geografi
  
  
  giltig_tid <- map(url_list, ~ hamta_giltiga_varden_fran_tabell(.x, "tid")) %>% unlist() %>% sort()
  
  if (is.na(valt_ar)) valt_ar <- max(giltig_tid)        # tilldela valt_ar senaste år om inget annat är valt
  if (is.na(jmfr_ar)) jmfr_ar <- min(giltig_tid)        # tilldela jmfr_ar tidigaste år om inget annat är valt
  
  tid_var <- if(diagram_utv_over_tid) giltig_tid else if(diagram_andel_av_alla_yrken) c(valt_ar, jmfr_ar) else if (diagram_jmfr_alla_kommuner) valt_ar
  
  
  # ================== hämta data ===================================
  
  varlista <- list(
    Region = region_var,
    Yrke2012 = valt_yrke_ssyk2012,
    Kon = c("1", "2"),
    ContentsCode = '*',
    Tid = tid_var
  )
  
  # Här hämtar vi namn till geografierna vi har med i uttaget - till diagramtitel och för att döpa diagramfilen
  geo_df <- hamtaregion_kod_namn(vald_geografi)
  geo_namn <- geo_df$region
  
  # =============================================== API-uttag ===============================================
  
  px_df <- NULL
  
  for (url_tab in 1:length(url_list)){
    hamta_data <- TRUE
    
    giltig_tid_tab <- hamta_giltiga_varden_fran_tabell(url_list[url_tab], "tid")
    hamta_tid <- tid_var[tid_var %in% giltig_tid_tab]
    if (length(hamta_tid) < 1) hamta_data <- FALSE else varlista$Tid <- hamta_tid
    
    # giltig_region_tab <- hamta_giltiga_varden_fran_tabell(url_list[url_tab], "region")
    # hamta_region <- region_var[region_var %in% giltig_region_tab]
    # if (length(hamta_tid) < 1) hamta_data <- FALSE else varlista$Tid <- hamta_tid
    
    
    
    # ändra variabler beroende på databas ======================================================
    if (grepl("58", url_list[url_tab])) {
      names(varlista)[2] <- "Yrke2012"                                       # ändra namn på yrkesvariabeln i API-hämtningen, heter annorlunda i olika databaser
      varlista[[2]] <- valt_yrke_ssyk2012
      valda_yrken <- valt_yrke_ssyk2012
      if (is.na(valt_yrke_ssyk2012[1])) hamta_data <- FALSE                     # om man inte skickat med något värde för yrken i ssyk2012 så hämtar vi inga värden från den databasen
    } else {
      names(varlista)[2] <- "Yrke"                                           # ändra namn på yrkesvariabeln i API-hämtningen, heter annorlunda i olika databaser
      varlista[[2]] <- valt_yrke_ssyk96
      valda_yrken <- valt_yrke_ssyk96
      if (is.na(valt_yrke_ssyk96[1])) hamta_data <- FALSE                       # om man inte skickat med något värde för yrken i ssyk96 så hämtar vi inga värden från den databasen
    }
    
    # ta ut alla yrken om man vill ha andel
    if (diagram_andel_av_alla_yrken | diagram_jmfr_alla_kommuner) varlista[[2]] <- '*'
    
    # här görs själva uttaget =================================================================
    if (hamta_data){
      px_uttag <- pxweb_get(url = url_list[url_tab],
                            query = varlista
      ) 
      
      # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
      # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
      px_df_temp <- as.data.frame(px_uttag) %>% 
        cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
                select(Region, 2)) %>% rename(regionkod = regionkod.Region, yrke = 2, yrkeskod = 7) %>% 
        relocate(regionkod, .before = region) %>% relocate(yrkeskod, .before = yrke) 
      
      px_df_temp$valt_yrke <- 0
      px_df_temp <- px_df_temp %>% mutate(valt_yrke = replace(valt_yrke, yrkeskod %in% valda_yrken,1))
      
      px_df <- rbind(px_df, px_df_temp)
    } # slut if-sats hamta_data
  } # slut looop url_tab
  
  # kontrollera om användaren skickat med ett namn för yrkesgruppen, annars använd de yrkeskoder som skickats med
  
  if (is.na(namn_yrkesgrupp)) {
    yrkeslista <- px_df %>% select(yrkeskod, yrke) %>% unique(.)
    
    yrkesgrp <- yrkeslista[yrkeslista$yrkeskod %in% valt_yrke_ssyk2012]
    
    namn_yrkesgrupp <- yrkesgrp$yrke %>% list_komma_och()
    
  }
  
  
  # om man vill ha andelar av totala arbetsmarknaden för de yrken man tittar på så fixar vi till df här
  
  # lägg till mer information i caption om man grupperar ihop flera yrken
  if(lagg_ihop_yrken){
    if(!is.na(valt_yrke_ssyk96[1])) diagram_capt <- paste0(diagram_capt,
                                                           "\nGruppen ", namn_yrkesgrupp , " består av SSYK96-kod: ",
                                                           list_komma_och(valt_yrke_ssyk96))
    # om det också finns ssyk2012-koder
    if(!is.na(valt_yrke_ssyk2012[1])){
      
      if(!is.na(valt_yrke_ssyk96[1])) {
        # om det finns både ssyk96 och ssyk2012
        diagram_capt <- paste0(diagram_capt, ", samt SSYK2012-kod: ",
                               list_komma_och(valt_yrke_ssyk2012))
      } else {
        # om det bara finns ssyk2012-koder
        diagram_capt <- paste0(diagram_capt,
                               "\nGruppen ", namn_yrkesgrupp , " består av SSYK2012-kod: ",
                               list_komma_och(valt_yrke_ssyk2012))
      } # else 
    }  # if valt_yrke ssyk2012
    
  } # if lägg_ihop_yrken
  
  
  # beräkna andel per yrke - förbered dataset för diagram
  if (lagg_ihop_yrken){
    # beräkna andel för valda yrken grupperat
    px_andel <- px_df %>% 
      filter(år == valt_ar | år == jmfr_ar) %>% 
      group_by(år, regionkod, region, valt_yrke) %>% 
      summarise(sysselsatta = sum(`Anställda 16-64 år med arbetsplats i regionen (dagbef)`),
                valt_yrke = max(valt_yrke)) %>% 
      mutate(andel = (sysselsatta / sum(sysselsatta))*100) %>% 
      ungroup() %>% 
      filter(valt_yrke == 1)
  } else {
    # hämta yrkesbenämningar
    yrke_tab <- px_df[px_df$år == valt_ar,] %>% 
      select(yrkeskod, yrke) %>% 
      distinct()
    # bearbeta dataset + koppla på yrkesbenämningar
    px_andel <- px_df %>% 
      filter(år == valt_ar) %>% 
      group_by(år, regionkod, region, yrkeskod) %>% 
      summarise(sysselsatta = sum(`Anställda 16-64 år med arbetsplats i regionen (dagbef)`),
                valt_yrke = max(valt_yrke)) %>% 
      mutate((andel = sysselsatta / sum(sysselsatta))*100) %>% 
      ungroup() %>% 
      filter(valt_yrke == 1) %>% 
      left_join(yrke_tab, by = c("yrkeskod"))
  }
  
  
  # ========================= Skapa själva diagrammen ==============================================================
  
  
  # diagram 1: förändring under hela perioden - index startår = 100 =========================
  if (diagram_utv_over_tid) {  
    chart_df <- px_df %>%
      filter(regionkod %in% vald_geografi) %>% 
      filter(valt_yrke == 1) %>% 
      group_by(region, år) %>% 
      summarise(sysselsatta = sum(`Anställda 16-64 år med arbetsplats i regionen (dagbef)`))
    
    diagramtitel <- paste0("Utveckling av antal ", tolower(namn_yrkesgrupp), " år ", min(chart_df$år), "-", max(chart_df$år)) %>% 
      str_wrap()
    filnamn <- paste0("Forandr_", tolower(namn_yrkesgrupp), "_", min(chart_df$år), "-", max(chart_df$år),"_", paste(vald_geografi, collapse = "_"),".png")
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = chart_df, 
                                skickad_x_var = "år",
                                skickad_y_var = "sysselsatta",
                                skickad_x_grupp = "region",
                                diagram_titel = diagramtitel,
                                diagram_capt = diagram_capt,
                                output_mapp = output_mapp,
                                filnamn_diagram = filnamn,
                                manual_color = diagramfarger("rd_gron_tre_fokus"),
                                lagg_pa_logga = lagg_till_logga, 
                                logga_path = logga_sokvag,
                                #brew_palett = "Dark2",
                                berakna_index = TRUE,
                                stodlinjer_avrunda_fem = TRUE,
                                #manual_y_axis_title = "procent",
                                #manual_x_axis_text_hjust = 1,
                                #manual_x_axis_text_vjust = 1,
                                #y_axis_100proc = TRUE,
                                #lagg_pa_logga = ifelse(is.null(logga_path), FALSE, TRUE),
                                #logga_path = ifelse(is.null(logga_path), NA, logga_path),
                                skriv_till_diagramfil = skapa_fil
    )
    
    gg_list[[length(gg_list)+1]] <- gg_obj
    
  } # slut if-sats diagram_utv_over_tid
  
  # diagram 2: andel av valt yrke(n) i valda geografier ==========================
  
  if (diagram_andel_av_alla_yrken){
    
    px_andel_valda <- px_andel %>% 
      filter(regionkod %in% vald_geografi)
    
    diagramtitel <- paste0("Andel ", tolower(namn_yrkesgrupp), " av alla yrken år ", valt_ar) %>% 
      str_wrap()
    filnamn <- paste0("Andel_", tolower(namn_yrkesgrupp), "_", valt_ar, "_", paste(vald_geografi, collapse = "_"), ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_andel_valda, 
                                 skickad_x_var = "region",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = ifelse(lagg_ihop_yrken, "år", "yrke"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = filnamn,
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_color = diagramfarger("rd_gron_tre_fokus"),
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 lagg_pa_logga = lagg_till_logga, 
                                 logga_path = logga_sokvag,
                                 #y_axis_100proc = TRUE,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[length(gg_list)+1]] <- gg_obj         # fyll på returlista med nytt objekt
  } # slut if-sats (diagram_andel_av_alla_yrken)
  
  # diagram 3: De 15 kommuner som har högst andel av valt yrke(n) + i valda geografier ==========================
  if (diagram_jmfr_alla_kommuner){
    px_andel_rank <- px_andel %>% 
      filter(nchar(regionkod) == 4) %>%
      filter(år == valt_ar) %>% 
      arrange(desc(andel)) %>%
      mutate(rank = row_number()) %>% 
      filter(rank <= 15 | regionkod %in% vald_geografi) %>% 
      mutate(kommun_rank = paste0("(", rank, ")  ", region))
    
    px_andel_rank$fokus <- NULL  
    px_andel_rank$fokus <- 0
    px_andel_rank$fokus[px_andel_rank$regionkod %in% vald_geografi] <- 1
    
    diagramtitel <- paste0("Kommuner med högst andel ", tolower(namn_yrkesgrupp), " av alla yrken år ", valt_ar, " (ranking inom parantes)") %>% 
      str_wrap()
    filnamn <- paste0("Kommun_", tolower(namn_yrkesgrupp), "_", valt_ar, "_", paste(vald_geografi, collapse = "_"), ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_andel_rank, 
                                 skickad_x_var = "kommun_rank",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = NA,
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = filnamn,
                                 diagram_liggande = TRUE,
                                 x_axis_sort_value = TRUE,
                                 x_var_fokus = "fokus",
                                 x_axis_lutning = 0,
                                 manual_color = diagramfarger("rd_gron_tre_fokus"),
                                 manual_y_axis_title = "procent",
                                 stodlinjer_avrunda_fem = TRUE,
                                 lagg_pa_logga = lagg_till_logga, 
                                 logga_path = logga_sokvag,
                                 #manual_x_axis_text_hjust = 1,
                                 #manual_x_axis_text_vjust = 1,
                                 #y_axis_100proc = TRUE,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[length(gg_list)+1]] <- gg_obj         # fyll på returlista med nytt objekt
    
  }
  # sist av allt returnerar vi en lista med diagram
  if (length(gg_list) > 1) return(gg_list) else return(gg_list[[1]])
} # slut funktion diag_yrke


dlg_valj_yrken <- function(){
  
  # lista som vi skickar i retur sedan
  retur_lista <- list(valt_yrke_ssyk2012 = NULL, 
                      valt_yrke_ssyk96 = NULL,
                      namn_yrkesgrupp = NULL)
  
  url_list <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG58N",   # Anställda 16-64 år med arbetsplats i regionen (dagbef) efter region, yrke (3-siffrig SSYK 2012) och kön. Ny tidsserie. År 2019 - 2020
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG58",    # Anställda 16-64 år med arbetsplats i regionen (dagbef) efter region, yrke (3-siffrig SSYK 2012) och kön. År 2014 - 2018
                "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0208/AM0208D/YREG17")    # Anställda 16-64 år med arbetsplats i regionen (dagbef) efter region, yrke (3-siffrig SSYK 96) och kön. År 2001 - 2013
  
  # ================== hämta data ===================================
  
  varlista <- list(
    Region = "00",
    Yrke2012 = "*",
    #Kon = c("1", "2"),
    ContentsCode = '*',
    Tid = c('*')
  )
  
  # =============================================== API-uttag ===============================================
  
  yrken_list <- list(ssyk2012 = NULL, ssyk96 = NULL)
  
  for (url_tab in 1:length(url_list)){
    
    yrke_index <- str_which( tolower(pxvarlist(url_list[url_tab])$koder), "yrke")             # hitta vilken rad i metadata namnet på yrkesvariabeln finns
    names(varlista)[2] <- pxvarlist(url_list[url_tab])$koder[yrke_index]                      # döp om varlista med namnet på yrkesvariabeln som vi hämtar med hjälp av index ovan
    
    varlista$Tid <- "*"  # byt till alla år
    #senaste_ar <- hamta_senaste_tid_i_tabell(url_list[url_tab], "år", query_list = varlista)
    senaste_ar <- max(hamta_giltiga_varden_fran_tabell(url_list[url_tab], "tid"))
    varlista$Tid <- senaste_ar  # byt till senaste tillgängliga år för aktuell tabell
    
    px_uttag <- pxweb_get(url = url_list[url_tab], query = varlista) 
    
    px_df_temp <- as.data.frame(px_uttag) %>% 
      cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
              select(Region, 2)) %>% rename(regionkod = regionkod.Region, yrke = 2, yrkeskod = 6) %>% 
      relocate(regionkod, .before = region) %>% relocate(yrkeskod, .before = yrke) 
    
    if (grepl("58", url_list[url_tab])) { 
      yrken_list$ssyk2012 <- px_df_temp 
    } else { 
      yrken_list$ssyk96 <- px_df_temp
    }
    
  } # slut for-loop
  
  # plocka ut yrken för ssyk2012
  if (!is.null(yrken_list$ssyk2012)) {
    ssyk2012list <- paste0(yrken_list$ssyk2012$yrkeskod, " - ", yrken_list$ssyk2012$yrke)
    svar <- dlg_list(ssyk2012list, multiple = TRUE, title = "Välj vilka SSYK2012-yrken som ska ingå")$res
    valt_yrke_ssyk2012 <- sub(" .*", "", svar)
    retur_lista$valt_yrke_ssyk2012 <- valt_yrke_ssyk2012
    if (length(valt_yrke_ssyk2012) == 0) retur_lista$valt_yrke_ssyk2012 <- NA
  }
  
  # plocka ut yrken för ssyk96
  if (!is.null(yrken_list$ssyk96)) {
    ssyk96list <- paste0(yrken_list$ssyk96$yrkeskod, " - ", yrken_list$ssyk96$yrke)
    svar <- dlg_list(ssyk96list, multiple = TRUE, title = "Välj vilka SSYK96-yrken som ska ingå")$res
    valt_yrke_ssyk96 <- sub(" .*", "", svar)
    retur_lista$valt_yrke_ssyk96 <- valt_yrke_ssyk96
    if (length(valt_yrke_ssyk96) == 0) retur_lista$valt_yrke_ssyk96 <- NA
  }
  
  # skapa namn för yrkesgruppen
  if (!is.null(yrken_list$ssyk2012) | !is.null(yrken_list$ssyk96)) {
    namn_yrkesgrupp <- dlg_input(message = "Ge yrkesgruppen ett namn: ", title = "Döp yrkesgrupp")$res
    # Kör denna om det blir problem med svenska å, ä och ö
    namn_yrkesgrupp <- iconv(namn_yrkesgrupp, "UTF-8", "latin1")
    
    retur_lista$namn_yrkesgrupp <- namn_yrkesgrupp
    
  } 
  
  return(retur_lista)    
}
