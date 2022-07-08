## Rioolwatersurveillance

## auteur script: Pieter Seinen (GGD Gelderland Zuid) en Leonard Vanbrabant (GGD West-Brabant)
## datum aangemaakt: 31-03-2022
## datum aangepast:  08-07-2022 door LV
## laaste wijzigingen:
  # missings worden nu op rwzi niveau geimputeerd
  # landelijke trendlijn is obv de mediaan


library(shiny)
library(dplyr)     # dplyr syntax
library(lubridate) # for time and dates
library(openxlsx)  # load excel tabs
library(zoo)       # moving average PS
library(sf)        # simple featuers
library(tmap)      # maak kaartjes
library(imputeTS)  # impute time series data
library(gridExtra) # create grid for plots
library(ggplot2)   # for plots
library(data.table) # dealing with large datasets



# je hoeft enkel de gemeenten en de VR aan te passen ----------------------

# gemeenten vrmwb
gemeenten <- c("Alphen-Chaam", "Altena", "Baarle-Nassau", "Bergen op Zoom",
               "Breda", "Drimmelen", "Etten-Leur", "Geertruidenberg",
               "Halderberge", "Moerdijk", "Oosterhout", "Roosendaal",
               "Rucphen", "Steenbergen", "Woensdrecht", "Zundert",
               "Dongen", "Gilze en Rijen", "Goirle", "Loon op Zand",
               "Oisterwijk", "Tilburg", "Waalwijk", "Hilvarenbeek")

VR <- "VR20" # "Midden- en West-Brabant"





# laad data ---------------------------------------------------------------

# gegevens van RWZI ophalen & weekgemiddelde per RWZI uitrekenen
rioolwater <- read.csv("https://data.rivm.nl/covid-19/COVID-19_rioolwaterdata.csv", sep = ";") 
# eeste week start op maandag van 2022, dit kun je aanpassen indien gewenst
rioolwater <- subset(rioolwater, Date_measurement >= "2022-01-03")

# 
if (!file.exists("data/Inwoners_per_verzorgingsgebied.xlsx")) {
  download.file("https://www.cbs.nl/-/media/_excel/2021/39/20210930-aantal-inwoners-per-verzorgingsgebied-2021.xlsx",
                destfile = "Inwoners_per_verzorgingsgebied.xlsx",
                mode = "wb")
} else {
  inwoners_per_rwzi_gemeente_VR <- read.xlsx("data/Inwoners_per_verzorgingsgebied.xlsx", 
                                             sheet = "Tabel 1")
}


# gemeenten polygonen
if (!file.exists("data/gemeente.shp")) {
  gemeente_sf <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2022_gegeneraliseerd&outputFormat=json")
} else {
  gemeente_sf <- st_read("data/gemeente_sf.shp")
}

# behoud getal na de punt (.)
gemeente_sf$id <- sub(".*\\.", "", gemeente_sf$id)
# subset gemeenten 
gemeente_sf <- subset(gemeente_sf, statnaam %in% gemeenten)

# welke gemeente-naam hoort erbij een regio code
gz_codes_en_namen <- gemeente_sf[gemeente_sf$statnaam %in% gemeenten,] %>%
  as.data.frame()%>%
  mutate(regio_code =  statcode,
         naam = statnaam) %>%
  select(regio_code, naam) 



# shiny -------------------------------------------------------------------

ui <- fluidPage(

    # Application title
    titlePanel("Rioolwatersurveillance"),

    mainPanel(
      plotOutput("tmap", width = "768px", height = "768px"),
      tableOutput("table"),
      plotOutput("VR", width = "768px", height = "576px")
    )
)


server <- function(input, output) {

  # help functie: verwijder characters van string en maak hem numeriek
  # to_number <- function(codes) {
  #   return(as.numeric(sub("[a-zA-Z]+", "", codes)))
  # }
  
  #range(rioolwater$Date_measurement)

  # write.csv(rioolwater,paste0("rioolwater", Sys.Date(),".csv"))
  rioolwater_per_week <- rioolwater %>%
    # weekvariabele: startdag van week is maandag
    mutate(week = floor_date(as.Date(Date_measurement),
                             "week", week_start = getOption("lubridate.week.start", 1))) %>%
    group_by(week, RWZI_AWZI_code) %>%
    summarise(RNA_flow_per_100000 = mean(RNA_flow_per_100000, na.rm = TRUE), 
              A_metingen          = length(week)) %>%
    ungroup() %>%
    rename(rwzi_code = RWZI_AWZI_code) 
  
  
  # Het kan voorkomen dat er in een week geen metingen van een RWZI zijn geregistreerd.  
  # T/m juni 2021 zijn er erg veel missings. Hierna ontbreken slechts sporadisch gegevens. 
  # gegevens op rwzi-week niveau die ontbreken kunnen we imputeren.
  # Hiervoor is het nodig om rijen te hebben van iedere combinatie weeknummer en RWZI
  
  # Vector met alle weken in periode waar data van is maken: start is 3 januari 2022
  weken <- seq(min(rioolwater_per_week$week),
               max(rioolwater_per_week$week), by = "weeks" )
  
  rwzis <- unique(rioolwater_per_week$rwzi_code)
  
  rwzis_weken <- expand.grid(weken,rwzis)
  colnames(rwzis_weken) <- c("week", "rwzi_code")
  
  # Combinaties van week en RWZI die nog niet in de data bestaan aanvullen met rwzis_weken
  rioolwater_per_week <- rioolwater_per_week %>% 
    full_join(rwzis_weken, by = c("rwzi_code", "week")) %>%
    mutate(is_echt = !is.na(RNA_flow_per_100000)) %>%
    group_by(rwzi_code) %>%
    arrange(week) %>%
    # imputeer missings
    mutate(RNA_flow_per_100000 = na_ma(RNA_flow_per_100000, k = 2, weighting = "simple")) 
  
  
  
  # Gemeenteniveau ----------------------------------------------------------
  alle_VR <- unique(inwoners_per_rwzi_gemeente_VR$regio_naam[inwoners_per_rwzi_gemeente_VR$regio_type == "VR"])
  alle_VR <- alle_VR[!is.na(alle_VR)]
  
  # alle VRs
  inwoners_per_rwzi_vr <- subset(inwoners_per_rwzi_gemeente_VR, regio_naam %in% alle_VR &
                                 regio_type == "VR")
  # gemeenten vrmwb
  inwoners_per_rwzi_gemeente <- subset(inwoners_per_rwzi_gemeente_VR, regio_naam %in% gemeenten &
                                         regio_type == "GM")
  
  inwoners_per_rwzi_gemeente_VR <- rbind(inwoners_per_rwzi_vr, inwoners_per_rwzi_gemeente)
  
  
  ## zie Berekening cijfers rioolwatermetingen COVID (nieuwe methode)_V5.pdf voor de methode
  rna_per_gemeente_vr <- inwoners_per_rwzi_gemeente_VR %>%
    # als einddatum ingevuld is gaat het om een oude telling; verwijderen
    # als aandeel NA / 0  is; heeft het geen zin telling mee te nemen.
    filter(is.na(einddatum),
           !is.na(aandeel),
           aandeel > 0,
           rwzi_code != 49999 # Is schiphol; geen gegevens
    ) %>%
    # Hoeveel inwoners die gebruik maken  van specifieke installatie horen bij gemeente 
    # (inwoners gemeente * aandeel gemeente bij rwzi)
    mutate(inwoners_gemeente_per_rwzi = round(inwoners * aandeel),
           rwzi_code = as.integer(rwzi_code)) %>%
    # Merge met rioolwaterdata obv rwzi
    left_join(rioolwater_per_week, by = "rwzi_code") %>%
    # Rna per bewoner uitrekenen
    mutate(rna_per_bewoner = RNA_flow_per_100000 / 1e5) %>%
    # Hoeveel is dat per rwzi per gemeente (rna_per_bewoner * inwoners gemeente (die bij rwzi horen))
    mutate(rna_rwzi_gemeente = inwoners_gemeente_per_rwzi * rna_per_bewoner) %>%
    # Groeperen op regio zodat er rowSums gemaakt kunnen worden
    group_by(regio_code, week) %>%
    # Som het aantal inwoners van gemeente & aantal RNA per rwzi/gemeente bij elkaar op
    mutate(aantal_mensen = sum(inwoners_gemeente_per_rwzi),
           aantal_rna = sum(rna_rwzi_gemeente),
           percentage_geimputeerd = round(sum(ifelse(is_echt, 0, (inwoners_gemeente_per_rwzi/aantal_mensen)*100)))) %>%
    ungroup() %>%
    # * 100000 om op rna flow per 100.0000 uit te komen
    mutate(rna = aantal_rna/aantal_mensen * 1e5) %>%
    # Opschonen; 1 rij per gemeente
    mutate(gemeenteweek = paste(regio_code, week)) %>%
    # unieke rijen
    distinct(gemeenteweek, .keep_all = TRUE)
  
  
  
  rna_per_nl <- rna_per_gemeente_vr %>%
    filter(regio_code %like% "VR") %>%
    #Percentage geimputeerd terugrekenen naar aantal mensen met geimputeerde data
    mutate(mensen_geimputeerd = (percentage_geimputeerd/100) * aantal_mensen) %>%
    group_by(week) %>%
    summarise(aantal_mensen = sum(aantal_mensen),
              aantal_rna    = sum(aantal_rna),
              percentage_geimputeerd = round(sum(mensen_geimputeerd / aantal_mensen * 100))
    )%>%
    ungroup()%>%
    mutate(rna = aantal_rna/aantal_mensen * 1e5) %>%
    select(week, rna, percentage_geimputeerd)
  
  
  
  ## Welke weken bestaan er in de data en welke ontbreken? 
  # rna_per_gemeente <- subset(rna_per_gemeente_en_vr, regio_type == "GM")
  weken_gemeente_vr_data <- unique(rna_per_gemeente_vr$week)
  # welke gemeenten en VR
  gemeente_vr_data <- unique(rna_per_gemeente_vr$regio_code)
  # Alle mogelijke combinaties van gemeenten & weken
  gemeenten_vr_weken <- expand.grid(weken_gemeente_vr_data, gemeente_vr_data) 
  colnames(gemeenten_vr_weken) <- c("week", "regio_code")
  
  
  # merge rioolwater met regio code
  rna_per_gemeente_vr <- rna_per_gemeente_vr %>%
    # alleen relevante variabelen behouden
    select(regio_code, week, rna, is_echt, A_metingen) %>%
    #mutate(isecht = TRUE) %>%
    # zoals hierboven genoemd ontbreken er soms gegevens per week; hieronder worden
    # combinaties van week en gemeente die nog niet bestaan aangevuld en wordt er een 
    # 4-wekelijks terugkijkend moving average genomen (obv eerdere data van gemeente);
    full_join(gemeenten_vr_weken, by = c("week","regio_code")) %>%
    #mutate(isecht = ifelse(is.na(isecht), FALSE, isecht)) %>%
    # sorteren op gemeente en datum
    arrange(regio_code, week) %>%
    # groeperen op gemeente
    group_by(regio_code) %>%
    ungroup()
  
  
  
  # op basis van de simulatie lijkt de interpolatie methode het beste te werken.
  #rna_per_gemeente_vr$rna <- na_interpolation(rna_per_gemeente_vr$rna, maxgap = 2)
  
  #rna_per_gemeente_vr <- subset(rna_per_gemeente_vr, year(rna_per_gemeente_vr$week) %in% 2022)
  
  # rna_per_gemeente_vr <- rna_per_gemeente_vr %>%
  #   group_by(regio_code) %>%
  #   mutate(rna = na_interpolation(rna, maxgap = 2, yright = NA))
  
  
  data <- rna_per_gemeente_vr
  # Alle weken in data
  alleweken <- unique(rna_per_gemeente_vr$week[!is.na(rna_per_gemeente_vr$week)]) %>% sort()
  # Laatste vier weken 
  laatste_vier_weken <- alleweken[(length(alleweken)-3):length(alleweken)]
  # Laatste vier weken overhouden
  data_recent <- rna_per_gemeente_vr[rna_per_gemeente_vr$week %in% laatste_vier_weken, ] %>%
    #x100 miljard om leesbare legenda te krijgen
    mutate(rna = rna / 1e11)
  # dataset 'breed' maken zodat iedere week een variabele is
  #data_recent$isecht <- NULL
  data_map <- data_recent %>% select(regio_code, week, rna) %>% tidyr::spread(key = week, rna)
  
  
  ## maak overzicht op hoeveel metingen de rna week gemiddelden zijn gebaseerd. 
  ## If NA, dan zijn de waarden geschat!
  # hoeveel metingen zijn er
  a_meting_GM_laaste_vier_weken <- data_recent %>% select(regio_code, A_metingen, week) %>% tidyr::spread(key = week, A_metingen)
  a_meting_GM_laaste_vier_weken <- merge(a_meting_GM_laaste_vier_weken, gz_codes_en_namen, all.x = TRUE)
  
  a_meting_GM_laaste_vier_weken <- a_meting_GM_laaste_vier_weken[order(a_meting_GM_laaste_vier_weken$naam), ]
  row.names(a_meting_GM_laaste_vier_weken) <- NULL
  a_meting_GM_laaste_vier_weken <- a_meting_GM_laaste_vier_weken[!is.na(a_meting_GM_laaste_vier_weken$naam), ]
  
  #inwoners_per_rwzi_gemeente_VR <- subset(inwoners_per_rwzi_gemeente_VR, regio_code %in% c(gz_codes_en_namen$regio_code, VR))
  
  output$table <- renderTable(a_meting_GM_laaste_vier_weken, spacing = "xs",
                              striped = TRUE)
  
  # Polygoon en data mergen
  kaart_recent <- merge(gemeente_sf,
                        data_map,
                        by.x = "statcode", 
                        by.y = "regio_code")
  
  output$tmap <- renderPlot({

    # alle waarden groter dan 1000 worden op 1001 gezet ivm tm_fill niet overweg kan
    # met Inf breaks.
  tmp <- kaart_recent[, paste(laatste_vier_weken), drop = TRUE]
  tmp[tmp  > 1000] <- 1001
  kaart_recent[, paste(laatste_vier_weken)] <- tmp
  
  
  tm_shape(kaart_recent) +
    tm_fill(col = paste(laatste_vier_weken),
            breaks = c(0.01, 50, 250, 500, 750, 1000, 1001),
            labels = paste(c(0.01, 50, 250, 500, 750, 1000, "")),
            style = "cont",
            drop.levels = FALSE,
            title = "Gemiddeld aantal virusdeeltjes (SARS-CoV-2) per 100.000 inwoners (x100 miljard)",
            legend.is.portrait = FALSE,
            palette = "Blues") +
    tm_borders() +
    tm_facets(nrow = 2, free.scales = FALSE) +
    tm_layout(legend.outside = TRUE,
              legend.outside.position = "bottom",
              legend.title.fontface = "bold",
              panel.labels = c(laatste_vier_weken),
              legend.format = list(digits = 0)) 
  
  # tm_shape(kaart_recent) +
  #   tm_fill(col = paste(laatste_vier_weken),
  #           breaks = c(0.01, 50, 250, 500, 750, 1000, 2000, 3000),
  #           style = "cont",
  #           title = "Gemiddeld aantal virusdeeltjes (SARS-CoV-2) per 100.000 inwoners (x100 miljard)",
  #           legend.is.portrait = FALSE,
  #           palette = "Blues") +
  #   tm_borders() +
  #   tm_facets(nrow = 2, free.scales = TRUE) +
  #   tm_layout(legend.outside = TRUE,
  #             legend.outside.position = "bottom",
  #             legend.title.fontface = "bold",
  #             panel.labels = c(laatste_vier_weken),
  #             legend.format = list(digits = 0))
  
  #tmap_save(m, "figures/tmap.png", width = 8, height = 8)
  

  })

    
    
  
  # VR ----------------------------------------------------------------------
  

  rna_per_vr <- subset(rna_per_gemeente_vr, regio_code %like% "VR") %>% select(-c(is_echt, A_metingen))
  rna_per_nl$regio_code <- "NL"
  rna_per_nl <- rna_per_nl %>% select(-percentage_geimputeerd)
  rna_per_vr_nl <- rbind(rna_per_vr, rna_per_nl)
  
  
  # Lijngrafiek VRniveau laatste x weken GZ
  w <- 14 #w = aantal weken terug
  alle_weken <- unique(rna_per_vr_nl$week) %>% sort()
  x_weken_terug <- alle_weken[length(alle_weken) - w]

  rna_mwb_linechart <- subset(rna_per_vr_nl, week >= x_weken_terug) #%>%
  #left_join(gz_codes_en_namen, by = "regio_code")


  axis.theme <- theme(
    axis.text.x  = element_text(size = 12),
    axis.title.x = element_text(size = 12, margin = margin(10,0,0,0)),
    axis.text.y  = element_text(size = 11),
    axis.title.y = element_text(size = 12, margin = margin(0,10,0,0)),
    axis.title.y.right = element_text(size = 12, margin = margin(0,0,0,10)),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.background = element_blank()
  )



  p1 <- 
    ggplot(data = rna_mwb_linechart) +
    ylab("Virusdeeltjes (SARS-CoV-2) per 100.000 inwoners (x100 miljard)") +
    xlab("") +
    geom_line(aes(x = week, y = rna/1e11, colour = "Landelijk"),
              data = rna_mwb_linechart %>% group_by(week) %>% summarise(rna = median(rna[regio_code == "NL"])),
              lwd = 1.1) +
    
    geom_line(aes(x = week, y = rna/1e11, colour = "Veiligheidsregio MWB"),
              data = subset(rna_mwb_linechart, regio_code == VR),
              lwd = 1.1) +
    geom_point(aes(x = week, y = rna/1e11, colour = "Veiligheidsregios")) +
    scale_color_manual(labels = c("Landelijk", "Veiligheidsregio MWB", "Veiligheidsregios"),
                       values = c("purple", "orange", "darkgreen"),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid", "solid", "blank"),
                         size = c(2, 2, 2))
                       )
    ) +
    axis.theme

  output$VR <- renderPlot({
    p1  
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
