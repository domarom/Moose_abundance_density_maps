library(shiny)
fluidPage(
  titlePanel("BC Big Game Harvest"),
  sidebarLayout(
    sidebarPanel(
      ## Consistent inputs between tabs ##
      selectInput(inputId = "speciesInput", label = "Species",
                  choices = c("All Species", "Black Bear", "Grizzly Bear", "Caribou", "Cougar", "Elk", "Mountain Goat", 
                              "Moose", "Sheep", "Wolf", "Mule Deer", "White-tailed Deer", "Turkey",
                              "Blue Grouse", "Ruffed Grouse", "Spruce Grouse", "Sharp-tailed Grouse", 
                              "Ducks", "Geese", "Mourning Dove", "Pheasant", "Quail", "Chukar", 
                              "Ptarmigan"), multiple = T, selected = "Elk"),
      selectInput(inputId = "regionInput", label = "Region", multiple = T,
                  choices = c("All", "1", "2", "3", "4", "5","6", "7A", "7B", "8"),
                  selected = "All"),
      selectInput(inputId = "subregionInput", label = "Subregion", multiple = F,
                  choices = c("", "West Kootenays", "East Kootenays"), selected = ""),
      textInput(inputId = "unitsInput", label = "MUs", value = ""),
      helpText("Enter as whole numbers (e.g. 408) separated by commas"),
      textInput(inputId = "labelInput", label = "Area Name (Optional)",
                value = NULL),
      sliderInput(inputId = "yearsInput", label = "Years", min = 1976, max = 2020,
                                         value = c(1972, 2020), sep = ""),
      conditionalPanel(condition = "input.tabInput == 'Figures & Table'",
        checkboxInput(inputId = "smoothInput", label = "Smoothed (3 Year Moving Average)"),
        selectInput(inputId = "sexDisplay", label = "Harvest Composition Figure",
                    multiple = F, choices = c("Harvest Composition", "Female Ratio"),
                    selected = "Harvest Composition"),
        selectInput(inputId = "effortInput", label = "Hunter Success Figure",
                    choices = c("Days Per Harvest", "Hunter Success Rate"),
                    multiple = F, selected = "Days Per Harvest"),
        selectInput(inputId = "plotInput", label = "Plot View",
                    choices = c("Main Plot (4 Panels)", "Harvest", "Harvest Composition",
                                "Female Ratio", "Hunter Success/Days Per Harvest",
                                "Number of Hunters"), multiple = F,
                    selected = c("Main Plot (4 Panels")),
        downloadButton("downloadData", "Download Table")
      )
    ,
      conditionalPanel(condition = "input.tabInput == 'Mapping'",
                       selectInput(inputId = "statInput", label = "Statistic",
                                   choices = c("Harvest", "Number of Hunters", "Hunter Success"),
                                   selected = "Harvest"),
                       checkboxInput(inputId = "byareaInput", label = "Calculate Statistic per Square Km"),
                       checkboxInput(inputId = "facetInput", label = "Break Figure into Timeseries?"),
                       numericInput(inputId = "timeseriesInput", label = "Number of Timeseries Intervals",
                                    value = 1),
                       helpText("Only relevant if you want to plot by timeseries. It takes a minute."),
                       checkboxInput(inputId = "outlierInput", label = "Remove Max Value")
      )
    ,
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Figures & Table",
          helpText("Disclaimer: This data includes preliminary 2020 data. Finalized 2020 data may differ slightly
                    from what it presented here. This tool currently shows only data from B.C. resident hunters 
                    from the B.C. Hunter Survey and has not been updated or verified with data from 
                   Compulsory Inspections or Non-Resident data. This tool will be updated when the full dataset 
                   is available. A version of this tool with the full dataset up to 2018 is available here: 
                   https://kootenaywildlife.shinyapps.io/BCHarvestData_2018/"),
          plotOutput("coolplot", height = "700px"),
          helpText("Be careful with interpreting the number of hunters. This is only accurate as a statistic
                  for a single species at the scale of a single MU, region, or the whole province.
                  It may be useful for analyzing trends in other circumstances, but interpret with caution.
                  95% confidence intervals are only available when looking at an individual species
                  in an individual WMU or Region, or the entire province."),
          DT:: dataTableOutput("table")
          ),
        tabPanel("Mapping", 
                 helpText("Disclaimer: This data includes preliminary 2020 data. Finalized 2020 data may differ slightly
                    from what it presented here. This tool currently shows only data from B.C. resident hunters 
                    from the B.C. Hunter Survey and has not been updated or verified with data from 
                   Compulsory Inspections or Non-Resident data. This tool will be updated when the full dataset 
                   is available. A version of this tool with the full dataset up to 2018 is available here: 
                   https://kootenaywildlife.shinyapps.io/BCHarvestData_2018/"),
                 plotOutput("coolmap", height = "700px")),
        id = "tabInput"
      )
    )
  )
)
