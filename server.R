options(shiny.maxRequestSize=30*1024^2)
# options(shiny.error = browser)
library(shiny)
library(DT)
library(readxl)
library(gridExtra)
library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(sf)
library(tmap)
library(tmaptools)

smoothFunction <- function(x){
  t <- vector(length = length(x))
  for(s in 1:length(x)){
    if(s == 1) t[s] <- mean(x[s:(s + 1)], na.rm = T)
    if(s > 1 & s < length(x)) t[s] <-  mean(x[(s - 1):(s + 1)], na.rm = T)
    if(s == length(x)) t[s] <-  mean(x[(s - 1):s], na.rm = T)
  }
  t
}

database <- readRDS("HarvestData2020.rds")
wmus.all <- readRDS("ProvincialWMUs.rds")
water <- readRDS("MajorWaters_2Mil.rds")
regions <- readRDS("RegionsPoly.rds")
HarvList <- readRDS("HarvList.rds")
HuntList <- readRDS("HuntList.rds")
SuccessList <- readRDS("SuccessList.rds")

subrlist <- list()
westkoots <- c(407,408,409,414,415,416,417,418,419,427,428,429,430,431,432,433,438,439)
eastkoots <- c(401,402,403,404,405,406,420,421,422,423,424,425,426,434,435,436,437,440)
subrlist[[1]] <- westkoots
subrlist[[2]] <- eastkoots
subr <- c("West Kootenays", "East Kootenays")
specieslist <- c("Black Bear", "Grizzly Bear", "Blue Grouse", "BTPI", "Caribou", "Chukar", "Cougar", "All Deer",
                 "Ducks", "Elk", "Mountain Goat", "Geese", "Mourning Dove", "Moose", "Pheasant", "Quail", "Ruffed Grouse",
                 "Sheep", "Spruce Grouse", "Sharp-tailed Grouse", "Wolf", "GRPA", "Ptarmigan", "Mule Deer",
                 "White-tailed Deer", "PART", "Turkey")

minyear <- 1976
greens <- colorRampPalette(c("white", "dark green"))(100)

#### SERVER ####
server <- function(input, output){

  inputs <- reactive({
    species <- input$speciesInput
    if(is.null(species)){
      return()
    }
    specieslist <- c("Black Bear", "Grizzly Bear", "Blue Grouse", "BTPI", "Caribou", "Chukar", "Cougar", "All Deer",
                     "Ducks", "Elk", "Mountain Goat", "Geese", "Mourning Dove", "Moose", "Pheasant", "Quail", 
                     "Ruffed Grouse", "Sheep", "Spruce Grouse", "Sharp-tailed Grouse", "Wolf", "GRPA", "Ptarmigan",
                     "Mule Deer", "White-tailed Deer", "PART", "Turkey")
    if("All Species" %in% species) species <- specieslist
    if(input$unitsInput == "") units <- NULL

    if(input$subregionInput != ""){
      units <- subrlist[[which(subr == input$subregionInput)]]
    }
    if(input$unitsInput != ""){
      units <- input$unitsInput
    }

    region <- input$regionInput
    if("All" %in% region) region <- c("1", "2", "3", "4", "5", "6", "7A", "7B", "8")

    if(input$unitsInput != "") {
      units <- as.numeric(strsplit(units, split = ",")[[1]])
    }
    if(!is.null(units)) region <- NULL
    arealabel <- input$labelInput
    if(input$subregionInput != "" & input$unitsInput == ""){
      arealabel <- paste("the", input$subregionInput, sep = " ")
    }
    if(arealabel == "") arealabel <- NULL
    smooth <- input$smoothInput
    years <- input$yearsInput[1]:input$yearsInput[2]

    if(is.null(species)){
      return()
    }
    if(is.null(region) & is.null(units)){
      return()
    }

    label <- ifelse(!is.null(arealabel), arealabel,
                    {ifelse(!is.null(region),
                            ifelse("All" %in% input$regionInput, "All Regions",
                                   paste("Region", paste(region, collapse = ", "), sep = " ")),
                            if(!is.null(units)){paste(units, collapse = ", ")})})
    specieslab <- ifelse("All Species" %in% input$speciesInput, input$speciesInput,
                         paste(species, collapse = ", "))

    listselect <- which(specieslist %in% species)

    yearlab <- ifelse(length(years) == 1, years, paste0(min(years), "-", max(years)))
    avglab <- ifelse(length(years) == 1, "", "Average")
    kmlab <- ifelse(input$byareaInput, "/1000km2", "")
    statistic <- input$statInput

    if(statistic == "Harvest"){
      mainlab <- paste(avglab, specieslab, statistic, kmlab, "in",
                       label, "in", yearlab, sep = " ")
    }
    if(statistic == "Number of Hunters"){
      mainlab <- paste(avglab, "Number of", specieslab, "Hunters", kmlab, "in",
                       label, "in", yearlab, sep = " ")
    }
    if(statistic == "Hunter Success"){
      mainlab <-  paste(avglab, specieslab, statistic, "in", label, "in", yearlab, sep = " ")
    }
  
    if(all(any((length(units) == 1), (!is.null(region) & length(region) %in% c(1,9))), length(species) == 1)){
      CI <- T} else(CI <- F)
    
    
    inputs <- list()
    inputs[[1]] <- species
    inputs[[2]] <- units
    inputs[[3]] <- region
    inputs[[4]] <- arealabel
    inputs[[5]] <- years
    inputs[[6]] <- label
    inputs[[7]] <- specieslab
    inputs[[8]] <- statistic
    inputs[[9]] <- listselect
    inputs[[10]] <- mainlab
    inputs[[11]] <- CI

    inputs
  })

  datatable <- reactive({
    if(input$tabInput != "Figures & Table") {
      return()
    }
    if(is.null(inputs())){
      return()
    }

    params <- inputs()
    species <- params[[1]]
    units <- params[[2]]
    region <- params[[3]]
    arealabel <- params[[4]]
    years <- params[[5]]
    CI <- params[[11]]

    x <- database[database$Species %in% species,]
    x99 <- x[x$WMU %in% c("199", "299", "399", "499", "599", "699", "799.1", "799.2", "899",
                          "999"),]
    # x99 <- x99[!(x99$WMU =="999" & x99$`PROV FLAG` == 0),]
    x <- x[!(x$WMU %in% c("199", "299", "399", "499", "599", "699", "799", "799.1", "799.2",
                          "899", "999")),]
    x <- x[!is.na(x$WMU),]
    x <- x[x$Year %in% years,]
    if(!is.null(units)) x <- x[x$WMU %in% units,]
    if(!is.null(region)) x <- x[x$Region %in% region,]
    if(is.null(region)) xeffort <- x
    if(!is.null(region)) {
      xeffort <- x99[as.character(x99$Region) %in% region & x99$Species %in% species & x99$Year %in% years,]
    }
    if("All" %in% input$regionInput & !is.null(region)){
      xeffort <- x99[x99$WMU == "999" & x99$Species %in% species & x99$Year %in% years,]
      x <- database[database$Species %in% species & database$WMU == "999",]
    }
    
    ###### Calculations with new  formatted data ######
    tHarvest <- vector(length = length(years))
    mHarvest <- vector(length = length(years))
    fHarvest <- vector(length = length(years))
    uHarvest <- vector(length = length(years))
    jHarvest <- vector(length = length(years))
    rtHarvest <- vector(length = length(years))
    rDaysPer <- vector(length = length(years))
    rSuccess <- vector(length = length(years))
    rEffort <- vector(length = length(years))
    for(i in 1:length(years)){
      mHarvest[i] <- round(sum(x$Kills[x$Year == years[i]] * x$`Male Ratio`[x$Year == years[i]], na.rm = T))
      fHarvest[i] <- round(sum(x$Kills[x$Year == years[i]] * x$`Female Ratio`[x$Year == years[i]], na.rm = T))
      uHarvest[i] <- round(sum(x$Kills[x$Year == years[i]] * x$`Unknown Ratio`[x$Year == years[i]], na.rm = T))
      jHarvest[i] <- round(sum(x$Kills[x$Year == years[i]] * x$`Juvenile Ratio`[x$Year == years[i]],na.rm = T))
      
      tHarvest[i] <- round(sum(x$Kills[x$Year == years[i]], na.rm = T))
      rDaysPer[i] <- (sum(x$Days[x$Year == years[i]], na.rm = T)) / (sum(x$Kills[x$Year == years[i]], na.rm = T))
      rSuccess[i] <- tHarvest[i] / (sum(xeffort$Hunters[xeffort$Year == years[i]], na.rm = T))
      rEffort[i] <- round(sum(xeffort$Hunters[xeffort$Year == years[i]], na.rm = T))
    }
    oHarvest <- uHarvest + jHarvest
    
    rDaysPer[rDaysPer == Inf] <- NA
    rSuccess[rSuccess == Inf] <- NA
    
    
    days.95.lower <- vector(length = length(years))
    days.95.upper <- vector(length = length(years))
    kills.95.lower <- vector(length = length(years))
    kills.95.upper <- vector(length = length(years))
    hunters.95.lower <- vector(length = length(years))
    hunters.95.upper <- vector(length = length(years))
    
    if(CI){
      for(i in 1:length(days.95.lower)){
        days.95.lower[i] <- sum(x$`Days 95% Lower`[x$Year == years[i]], na.rm = T)
        days.95.upper[i] <- sum(x$`Days 95% Upper`[x$Year == years[i]], na.rm = T)
        kills.95.lower[i] <- sum(xeffort$`Kills 95% Lower`[xeffort$Year == years[i]], na.rm = T)
        kills.95.upper[i] <- sum(xeffort$`Kills 95% Upper`[xeffort$Year == years[i]], na.rm = T)
        hunters.95.lower[i] <- sum(xeffort$`Hunters 95% Lower`[xeffort$Year == years[i]], na.rm = T)
        hunters.95.upper[i] <- sum(xeffort$`Hunters 95% Upper`[xeffort$Year == years[i]], na.rm = T)
      }
      days.NA <- days.95.lower == 0 & days.95.upper == 0
      days.95.lower[days.NA] <- NA
      days.95.upper[days.NA] <- NA
      kills.NA <- kills.95.lower == 0 & kills.95.upper == 0
      kills.95.lower[kills.NA] <- NA
      kills.95.upper[kills.NA] <- NA
      hunters.NA <- hunters.95.lower == 0 & hunters.95.upper == 0
      hunters.95.lower[hunters.NA] <- NA
      hunters.95.upper[hunters.NA] <- NA
      
      
      dt <- data.frame(cbind(years, tHarvest, kills.95.lower, kills.95.upper, mHarvest,
                             fHarvest, oHarvest, rDaysPer, rEffort, hunters.95.lower, hunters.95.upper, rSuccess))
    } else(dt <- data.frame(cbind(years, tHarvest, mHarvest,
                           fHarvest, oHarvest, rDaysPer, rEffort, rSuccess)))

    
    ###
    
    dt
  })


   output$coolplot <- renderPlot({
     if(input$tabInput != "Figures & Table"){
       return()
     }
     if(is.null(datatable())){
       return()
     }
     data <- datatable()

     if(is.null(inputs())){
       return()
     }
     params <- inputs()
     species <- params[[1]]
     units <- params[[2]]
     region <- params[[3]]
     arealabel <- params[[4]]
     years <- params[[5]]
     CI <- params[[11]]

     smooth <- input$smoothInput
     if(smooth){
       for(c in 2:ncol(data)){
        data[,c] <- smoothFunction(data[,c])
       }
     }

     label <- params[[6]]
     specieslab <- params[[7]]

     #### Harvest Plot ####
       main <- paste(specieslab, "Total Harvest in", label, sep = " ")
       ylab <- ifelse(smooth, "Total Harvest (Smoothed)", "Total Harvest")
       ymax <- ifelse(CI, max(data$tHarvest, data$kills.95.upper, na.rm = T),
                      (max(data$tHarvest, na.rm = T))*1.2)
       ymin <- 0

       ##Trying ggplot
       datalong <- gather(data, "Type", "Value", tHarvest:rSuccess)
       xscale <- scale_x_continuous(limit = c(min(years), max(years)),
                                    breaks = seq(min(years), max(years), 5))
       textsize <- ifelse(input$plotInput == "Main Plot (4 Panels)", 16, 22)
       HPlot.data <- datalong %>%
         filter(Type == "tHarvest" | Type == "kills.95.lower" | Type == "kills.95.upper")
       
       if(CI){
         HPlot <- ggplot(data, aes(x = years, y = tHarvest)) +
           geom_ribbon(aes(ymin = kills.95.lower, ymax = kills.95.upper), linetype = 2, alpha = 0.2) +
           geom_line(aes(x = years, y = tHarvest), size = 2, colour = "#00BA38")
       }
       if(!CI){
            HPlot <- ggplot(HPlot.data, aes(x = years, y = Value))+
               geom_line(aes(x = years, y = Value), size = 2, colour = "#00BA38")
       }
       HPlot <- HPlot +
         labs(x = "Year", y = ylab, title = main) +
         scale_y_continuous(limit = c(ymin, ymax)) +
         scale_colour_hue(name = "", labels = c("Total")) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = "none", legend.title = element_blank(),
               legend.direction = "horizontal", legend.background = element_blank(),
               text = element_text(size = textsize), plot.title = element_text(size = rel(0.92)))

       HPlot <- HPlot + xscale
       

    ##### Sex specific plot ####
         main <- paste(specieslab, "Harvest Composition", label, sep = " ")
         ylab <- ifelse(smooth, "Total Harvest (Smoothed)", "Total Harvest")
         ymax <- max(c(data$mHarvest, data$fHarvest, data$oHarvest), na.rm = T)*1.2
         ymin <- 0

         CompPlot <- datalong %>%
           filter(Type == "mHarvest" | Type == "fHarvest" | Type == "oHarvest") %>%
           ggplot(aes(x = years, y = Value, group = Type, color = factor(Type)))+
           geom_line(size = 2)+
           labs(x = "Year", y = ylab, title = main) +
           scale_y_continuous(limit = c(ymin, ymax)) +
           scale_colour_hue(labels = c("Females", "Males", "Other")) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1),
                 legend.position = c(0.5, 0.95), legend.title = element_blank(),
                 legend.direction = "horizontal", legend.background = element_blank(),
                 text = element_text(size = textsize), plot.title = element_text(size = rel(0.92)))

         CompPlot <- CompPlot + xscale



       fr <- data$fHarvest/data$tHarvest*100
       or <- data$oHarvest/data$tHarvest*100
       d.temp <- data.frame(cbind(years, fr, or))
       datalong.temp <- gather(d.temp, "Type", "Value", fr:or)
       main <- paste("Female Ratio of", specieslab, "Harvest in", label, sep = " ")
       ylab <- ifelse(smooth, "% of Females in Harvest (Smoothed)", "% of Females in Harvest")
       ymax <- 100
       ymin <- 0

       RatioPlot <- ggplot(datalong.temp, aes(x = years, y = Value, group = Type,
                                              color = factor(Type)))+
         geom_line(size = 2)+
         labs(x = "Year", y = ylab, title = main) +
         scale_y_continuous(limit = c(ymin, ymax)) +
         scale_colour_hue(labels = c("Female Ratio", "Other Ratio")) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = "none", legend.title = element_blank(),
               legend.direction = "horizontal", legend.background = element_blank(),
               text = element_text(size = textsize), plot.title = element_text(size = rel(0.92)))

       RatioPlot <- RatioPlot + xscale


    #### Hunter Success ####
       if(input$effortInput == "Days Per Harvest") {
         r <- data$rDaysPer
       }
       if(input$effortInput == "Hunter Success Rate"){
         r <- data$rSuccess*100
       }
       main <- ifelse(input$effortInput == "Days Per Harvest",
                      paste("Days per", specieslab, "Harvest in", label, sep = " "),
                      paste(specieslab, "Hunter Success Rate (%) in", label, sep = " "))
       ylab <- ifelse(input$effortInput == "Days Per Harvest",
                      ifelse(smooth, "Days per Kill (Smoothed)", "Days per Kill"),
                      ifelse(smooth, "Hunter Success Rate (Smoothed)", "Hunter Success Rate"))

       ymax <- ifelse(input$effortInput == "Days Per Harvest", (max(c(r), na.rm = T)*1.2),
                      110)
       ymin <- 0
       d.temp <- data.frame(cbind(years, r))
       datalong.temp <- gather(d.temp, "Type", "Value", r)

       SuccessPlot <- ggplot(datalong.temp, aes(x = years, y = Value, group = Type,
                                                color = "#00BA38"))+
         geom_line(size = 2, color = "#00BA38")+
         labs(x = "Year", y = ylab, title = main) +
         scale_y_continuous(limit = c(ymin, ymax)) +
         scale_colour_hue(labels = c("Residents")) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = "none", legend.title = element_blank(),
               legend.direction = "horizontal", legend.background = element_blank(),
               text = element_text(size = textsize), plot.title = element_text(size = rel(0.92)))
       SuccessPlot <- SuccessPlot + xscale

     #### Total Effort ####
       main <- paste(specieslab, "Hunters in", label, sep = " ")
       ylab <- ifelse(smooth, "Number of Hunters (Smoothed)", "Number of Hunters")
       ymax <- ifelse(CI, max(data$rEffort, data$hunters.95.upper, na.rm = T),
                      (max(data$rEffort, na.rm = T))*1.2)
       ymin <- 0

       if(CI){
         EffortPlot <- ggplot(data, aes(x = years, y = rEffort)) +
           geom_ribbon(aes(ymin = hunters.95.lower, ymax = hunters.95.upper), linetype = 2, alpha = 0.2) +
           geom_line(aes(x = years, y = rEffort), size = 2, colour = "#00BA38")
       } else({
          EffortPlot <- datalong %>%
            filter(Type == "rEffort") %>%
            ggplot(aes(x = years, y = Value, group = Type, color = "#00BA38"))+
            geom_line(size = 2, color = "#00BA38")
            })
       
       EffortPlot <- EffortPlot +
         labs(x = "Year", y = ylab, title = main) +
         scale_y_continuous(limit = c(ymin, ymax)) +
         scale_colour_hue(labels = c("Residents")) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = "none", legend.title = element_blank(),
               legend.direction = "horizontal", legend.background = element_blank(),
               text = element_text(size = textsize), plot.title = element_text(size = rel(0.93)))
       EffortPlot <- EffortPlot + xscale

       if(input$plotInput == "Main Plot (4 Panels)"){
         if(input$sexDisplay == "Female Ratio"){
         grid.arrange(HPlot, RatioPlot, SuccessPlot, EffortPlot)}
         if(input$sexDisplay == "Harvest Composition"){
           grid.arrange(HPlot, CompPlot, SuccessPlot, EffortPlot)}
       }

       if(input$plotInput == "Harvest") grid.arrange(HPlot)
       if(input$plotInput == "Harvest Composition") grid.arrange(CompPlot)
       if(input$plotInput == "Female Ratio") grid.arrange(RatioPlot)
       if(input$plotInput == "Hunter Success/Days Per Harvest") grid.arrange(SuccessPlot)
       if(input$plotInput == "Number of Hunters") grid.arrange(EffortPlot)

  })


   tableOut <- reactive({
     if(is.null(datatable())){
       return()
     }
     dt <- datatable()
     dt$rDaysPer <- round(dt$rDaysPer, digits = 1)
     dt$rSuccess <- round(dt$rSuccess, digits = 2)
     if(ncol(dt) == 8){
       colnames(dt) <- c("Year", "Total Harvest","Male Harvest", "Female Harvest", "Other Harvest",
                       "Days Per Harvest", "Hunters", "Success")} else({
                         cols.rm <- which(colnames(dt) %in% c("hunters.95.lower", "hunters.95.upper"))
                         dt <- dt[,-cols.rm]
                         colnames(dt) <- c("Year", "Total Harvest", "Harvest Lower CI",  
                                           "Harvest Upper CI", "Male Harvest", "Female Harvest", "Other Harvest",
                                           "Days Per Harvest", "Hunters", "Success")
                       })
     dt <- dt[order(dt$Year, decreasing = T),]
     dt
   })


   output$table <- DT:: renderDataTable(tableOut())

   output$downloadData <- downloadHandler(
     filename = function(){
       specieslab <- gsub(pattern = ",", x = inputs()[[7]], replacement = "")
       specieslab <- paste(unlist(strsplit(specieslab, split = " ")), collapse = "")

       arealab <- gsub(pattern = ",", x = inputs()[[6]], replacement = "")
       arealab <- paste(unlist(strsplit(arealab, split = " ")), collapse = "")

       paste0(paste(specieslab, "StatsIn", arealab, sep = "_"), ".csv")
     },
        content = function(file){
                    write.csv(tableOut(), file, row.names = F)
                    })

   ##### Map Panel #####
   wmusOut <- reactive({
     if(input$tabInput != "Mapping"){
       return()
     }
     if(is.null(inputs())){
       return()
     }
     params <- inputs()
     species <- params[[1]]
     units <- params[[2]]
     region <- params[[3]]
     arealabel <- params[[4]]
     years <- params[[5]]
     label <- params[[6]]
     specieslab <- params[[7]]
     statistic <- params[[8]]
     listselect <- params[[9]]

     if(statistic == "Harvest") data.full <- HarvList
     if(statistic == "Number of Hunters") data.full <- HuntList
     if(statistic == "Hunter Success") data.full <- SuccessList
     calc <- ifelse(statistic == "Hunter Success", "mean", "sum")

     data <- data.full[[1]][1,]
     data <- data[-1,]
     for(s in 1:length(listselect)){
       data <- rbind(data, data.full[[listselect[s]]])
     }
     if(!is.null(region) & is.null(units)){
       units <- unique(database$WMU[database$Region %in% region])
     }

     cuts <- 1
     if(input$facetInput) cuts <- input$timeseriesInput
     facet.cuts <- round(seq(from = min(years), to = max(years), length.out = cuts+1))
     facets <- list()
     for(t in 1:cuts)  facets[[t]] <- facet.cuts[t:(t+1)]

     for(f in 1:cuts){
       fyears <- c(facets[[f]][1] : facets[[f]][2])
       columns <- (min(fyears) - minyear + 3) : (max(fyears) - minyear + 3)
       data <- data[data$WMU %in% units,]
       wmus <- wmus.all[wmus.all$MU %in% units,]

       wmusMU <- as.numeric(as.character(wmus$MU))
       yearharv <- vector(length = length(fyears))
       stat <- vector(length = nrow(wmus))
       if(calc == "sum"){
         for(i in 1:nrow(wmus)){
           for(y in 1:length(fyears)){
             yearharv[y] <- sum(data[,columns[y]][data$WMU == wmusMU[i]], na.rm = T)
           }
           stat[i] <- mean(yearharv, na.rm = T)
         }
       }

       if(calc == "mean"){
         for(i in 1:nrow(wmus)){
           for(y in 1:length(fyears)){
             yearharv[y] <- mean(data[,columns[y]][data$WMU == wmusMU[i]], na.rm = T)
           }
           stat[i] <- mean(yearharv, na.rm = T)
         }
       }

       statraw <- stat
       if(input$byareaInput) stat <- (stat / (wmus$FEATURE_AR/1e+6))*1000
       wmus$stat <- stat
       
       if(input$outlierInput){
         maxMU <- wmus$MU[wmus$stat == max(wmus$stat)]
         wmus$stat[wmus$MU %in% maxMU] <- NA
         
       }
       
       wmus$yearlabel <- paste0(min(fyears), "-", max(fyears))
       if(f == 1) wmus.plot <- wmus
       if(f > 1) wmus.plot <- rbind(wmus.plot, wmus)
     }
     wmus.plot
   })

   output$coolmap <- renderPlot({
     if(is.null(wmusOut())){
       return()
     }
     wmus <- wmusOut()

     params <- inputs()
     units <- params[[2]]
     region <- params[[3]]
     mainlab <- params[[10]]

     if(!is.null(region) & is.null(units)){
       units <- unique(database$WMU[database$Region %in% region])
     }

     if(is.null(units)){
       return()
     }

     cuts <- 1
     if(input$facetInput) cuts <- input$timeseriesInput
     cols <- max(round((cuts+1)/2), 1)
     if(cuts == 6 | cuts == 8) cols <- 3
     rows <- max(round((cuts)/cols), 1)

     if(input$facetInput){
       mu_map <- tm_shape(wmus) +
         tm_fill(col = "stat", style = "cont", palette = greens, legend.show = T, title = "") +
         tm_facets(by = "yearlabel", nrow = rows, ncol = cols) +
         tm_borders(lwd = 0.5)}

     if(!input$facetInput){
       mu_map <- tm_shape(wmus) +
         tm_fill(col = "stat", style = "cont", palette = greens, legend.show = T, title = "") +
         tm_borders(lwd = 0.5)}

     mu_map <- mu_map +
       tm_shape(water) +
       tm_fill(col = "lightskyblue") +
       tm_shape(regions) +
       tm_borders(lwd = 2)

     if(length(units) < nrow(wmus.all)){
       mu_map <- mu_map + tm_shape(wmus.all[!wmus.all$MU %in% units,]) +
         tm_fill(col = "white") +
         tm_borders(col = "white", lwd = 1.5)}
     mu_map <- mu_map +
       tm_shape(wmus) +
       tm_borders(lwd = 1) +
       tm_layout(main.title = mainlab, frame = F) +
       tm_legend(scale = 1.8)
     mu_map
   })

}
