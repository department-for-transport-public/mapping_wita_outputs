# =========================================================================
# MAPPING WITA OUTPUTS - BETA VERSION
# =========================================================================

# This code produces chloropleth maps of the data produced by WITA. An
# instructions manual accompanies this script and should be consulted when
# working with this code.

# Use this mapping tool at your own risk. The Department for Transport 
# assumes no liability for the use of this tool or the output it produces.

# _________________________________________________________________________

# Load packages -----------------------------------------------------------

  require("tidyverse")
  require("rgeos")
  require("rgdal")
  require("maptools")
  require("readxl")
  require("stringr")

# ## USER INPUT ## Define variables  --------------------------------------

  #choose "sum_of_appraisal_period" or a number corresponding to a year in the appraisal period, eg. 2040
  forecast_year <- "sum_of_appraisal_period"
  
  #choose "agglomeration_manufacturing", "agglomeration_construction", "agglomeration_consumer_services", 
  #       "agglomeration_producer_services", "labour_supply_impact", "M2MLPJs", "total_agglomeration" or
  #       "total_wider_economic_impact"
  forecast_data <- "total_wider_economic_impact"
  
  #choose "yes" or "no"
  per_employee <- "no"
  
  #choose "2011", "2016", "2021", ..., "2081"
  employment_forecast <- "2021"
  
  #choose "Great Britain", "East Midlands", "East of England", "London", "North East", "North West", 
  #       "Scotland", "South East", "South West", "Wales", "West Midlands", "Yorkshire and The Humber"
  data_region <- "Great Britain"

# ## USER INPUT ## Define aesthetics for map plot  ------------------------

  high_colour <- print("darkgreen") #choose colour representing a high value
  
  mid_colour <- print("grey95") #choose colour representing a zero value
  
  low_colour <- print("red3") #choose colour representing a low value
  
  title <- print("black") #choose "black" to display the title, or "white" to not
  
  legend <- print("right") #choose "right" to display the legend, or "none" to not

# Generate map data -------------------------------------------------------

  map <- readOGR("Data/LAD SHP 2015 SG/Local_Authority_Districts_(December_2015)_Boundaries.shp")
  map <- fortify(map, region = "lad15cd")

# ##USER INPUT## Load WITA output data ------------------------------------

  #Change file name where appropriate
  wita_output <- read.csv("Data/Templates/Data Template Example.csv")

# ##USER INPUT## Load WITA zone to LADs matrix ----------------------------

  #Change file name where appropriate
  wita_zones <- read.csv("Data/Templates/Zone Template Example.csv")
  wita_zones <- as.data.frame(wita_zones)

# Merge WITA output data with LADs ----------------------------------------

  wita_clean <- merge(wita_zones, wita_output, by.x = "WITA", by.y = "Zone",  all.x = TRUE)

# Making column names user friendly ---------------------------------------

  names(wita_clean)[names(wita_clean) == "WITA"] <- "wita_zone"
  names(wita_clean)[names(wita_clean) == "Agglomeration...Manufacturing"] <- "agglomeration_manufacturing"
  names(wita_clean)[names(wita_clean) == "Agglomeration...Construction"] <- "agglomeration_construction"
  names(wita_clean)[names(wita_clean) == "Agglomeration...Consumer.Services"] <- "agglomeration_consumer_services"
  names(wita_clean)[names(wita_clean) == "Agglomeration...Producer.Services"] <- "agglomeration_producer_services"
  names(wita_clean)[names(wita_clean) == "Labour.supply.impact"] <- "labour_supply_impact"
  names(wita_clean)[names(wita_clean) == "Move.to.more...less.productive.jobs"] <- "M2MLPJs"

# Generate total impact option --------------------------------------------

  wita_clean <- mutate(wita_clean, total_wider_economic_impact = agglomeration_manufacturing + agglomeration_construction + agglomeration_consumer_services + agglomeration_producer_services + labour_supply_impact + M2MLPJs)
  wita_clean <- mutate(wita_clean, total_agglomeration = agglomeration_manufacturing + agglomeration_construction + agglomeration_consumer_services + agglomeration_producer_services)

# Generate aggregate option -----------------------------------------------

  wita_aggregate <- aggregate(. ~ LAD, data = wita_clean, sum)
  wita_aggregate["Year"][wita_aggregate["Year"] == wita_aggregate[1,"Year"]] <- "sum_of_appraisal_period"

# Attach total option to WITA output data ---------------------------------

  wita_clean <- rbind(wita_clean, wita_aggregate)

# Filter WITA data to desired forecast year and type ----------------------
  
  wita_data <- filter(wita_clean, Year == forecast_year)
  wita_data <- wita_data[c("LAD", forecast_data)]

# Load employment data ----------------------------------------------------

  employment_data <- read.csv("Data/Employment Forecasts.csv")
  names(employment_data)[names(employment_data) == "ï..LAD"] <- "LAD"
  names(employment_data)[names(employment_data) == paste("X", employment_forecast, sep = "")] <- "employment"
  employment_data <- employment_data[,c("LAD", "employment")]

  #The standardised data is sometimes skewed by small populations. These LADs are fixed at a higher value.
  employment_data[which(employment_data == "E06000053"),2] = employment_data[which(employment_data == "E06000052"),2]
  employment_data[which(employment_data == "S12000023"),2] = employment_data[which(employment_data == "S12000017"),2]
  employment_data[which(employment_data == "S12000013"),2] = employment_data[which(employment_data == "S12000017"),2]
  employment_data[which(employment_data == "S12000027"),2] = employment_data[which(employment_data == "S12000017"),2]

# Generate data per employee ----------------------------------------------

  wita_standardised <- merge(wita_data, employment_data, by = "LAD")
  wita_standardised <- mutate(wita_standardised, variable_standardised = eval(parse(text = forecast_data)) / employment)
  wita_standardised <- wita_standardised[,c("LAD", "variable_standardised")]
  names(wita_standardised)[names(wita_standardised) == "variable_standardised"] <- forecast_data 

# Choose appropriate data -------------------------------------------------

  wita_data <- if(per_employee == "yes"){wita_standardised}else{wita_data}

# Add on regions ----------------------------------------------------------

  wita_regions <- read.csv("Data/LAD to Region.csv")
  names(wita_regions)[names(wita_regions) == "ï..LAD15CD"] <- "LAD"
  names(wita_regions)[names(wita_regions) == "RGN15NM"] <- "region"
  wita_data <- merge(wita_data, wita_regions, by = "LAD")

# Merge desired WITA data with map data -----------------------------------

  wita_map <- merge(map, wita_data, by.x = "id", by.y = "LAD", all.x = TRUE)
  wita_map <- if(!data_region == "Great Britain"){filter(wita_map, region == data_region)}else{wita_map}
  wita_map <- arrange(wita_map, order)

# Parameters for scale ----------------------------------------------------

  scale_breaks <- c(
    -signif(2*signif(max(c(max(wita_map[forecast_data]),abs(min(wita_map[forecast_data])))),2)/2,3),
    -signif(1*signif(max(c(max(wita_map[forecast_data]),abs(min(wita_map[forecast_data])))),2)/2,3),
    0,
    signif(1*signif(max(c(max(wita_map[forecast_data]),abs(min(wita_map[forecast_data])))),2)/2,3),
    signif(2*signif(max(c(max(wita_map[forecast_data]),abs(min(wita_map[forecast_data])))),2)/2,3)
  )
  
  scale_labels <- c(
    format(-signif(2*signif(max(c(max(wita_map[forecast_data]),abs(min(wita_map[forecast_data])))),2)/2,3), big.mark = ",", scientific = FALSE),
    format(-signif(1*signif(max(c(max(wita_map[forecast_data]),abs(min(wita_map[forecast_data])))),2)/2,3), big.mark = ",", scientific = FALSE),
    0,
    format(signif(1*signif(max(c(max(wita_map[forecast_data]),abs(min(wita_map[forecast_data])))),2)/2,3), big.mark = ",", scientific = FALSE),
    format(signif(2*signif(max(c(max(wita_map[forecast_data]),abs(min(wita_map[forecast_data])))),2)/2,3), big.mark = ",", scientific = FALSE)
  )
  
  scale_limits <- c(
    -signif(signif(max(c(max(wita_map[forecast_data]),abs(min(wita_map[forecast_data])))),2)*1.05,3),
    signif(signif(max(c(max(wita_map[forecast_data]),abs(min(wita_map[forecast_data])))),2)*1.05,3)
  )

# Titles ------------------------------------------------------------------

  title_text <- if(forecast_year == "sum_of_appraisal_period"){
    paste(str_to_title(str_replace_all(forecast_data, "_", " ")),": ", str_to_title(str_replace_all(forecast_year, "_", " ")), sep = "")
  }else{paste(str_to_title(str_replace_all(forecast_data, "_", " ")),": Appraisal Year = ", str_to_title(str_replace_all(forecast_year, "_", " ")), sep = "")
  }
  
  title_text <- if(per_employee == "yes"){
    if(forecast_year == "sum_of_appraisal_period"){
      paste(str_to_title(str_replace_all(forecast_data, "_", " "))," per Employee: ", str_to_title(str_replace_all(forecast_year, "_", " ")), sep = "")
    }else{paste(str_to_title(str_replace_all(forecast_data, "_", " "))," per Employee: Appraisal Year = ", str_to_title(str_replace_all(forecast_year, "_", " ")), sep = "")
    }
  }else{title_text
  }
  
  legend_title <- if(per_employee == "yes"){
    print(paste("£ 000s, 2010 prices, ", employment_forecast, " employment", sep = ""))
  }else{print("£ 000s, 2010 prices")
  }

# ##USER INPUT## Plot map -------------------------------------------------

  plot <- ggplot(data = wita_map, aes(x = long, y = lat, group = group, fill = eval(parse(text = forecast_data)))) + 
    #define geom_polygon(size = 0.1, colour = "black") to include LAD borders, or geom_polygon() to remove them
    geom_polygon() +
    coord_equal() + 
    theme_void() + 
    theme(legend.text = element_text(colour = "black"),
          legend.title = element_text(size = 10.5, colour = "black"),
          legend.position = legend,
          plot.title = element_text(colour = title, size = 13, hjust = 0.5)
    ) +
    ggtitle(title_text) +
    labs(fill = legend_title) +
    scale_fill_gradient2(low = low_colour, 
                         mid = mid_colour, 
                         high = high_colour,  
                         midpoint = 0, 
                         breaks = scale_breaks,
                         labels = scale_labels,
                         limits = scale_limits
    )
  
  plot
