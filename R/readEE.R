#' Tidy import tool for EarthExplorer .csv export files
#' 
#' Imports and tidies CSV files exported from EarthExplorer into data.frames and annotates missing fields.
#' 
#' @param x Character, Character or list. One or more paths to EarthExplorer export files.
#' @return data.frame
#' @details 
#' The \href{https://earthexplorer.usgs.gov/}{EarthExplorer} CSV file can be produced from the search results page. Above the results click on 'export results' and select 'comma (,) delimited'.
#' 
#' Note that only a subset of columns is imported which was deemed interesting. Please contact the maintainer if you think an omited column should be included. 
#' 
#' @export 
#' @examples 
#' library(ggplot2)
#' ee <- readEE(system.file("external/EarthExplorer_LS8.txt", package = "RStoolbox"))
#' 
#' ## Scenes with cloud cover < 20%
#' ee[ee$Cloud.Cover < 20,]
#' 
#' ## Available time-series
#' ggplot(ee) + 
#'      geom_segment(aes(x = Date, xend = Date, y = 0, yend = 100 - Cloud.Cover, 
#'      col = as.factor(Year))) +
#'         scale_y_continuous(name = "Scene quality (% clear sky)")
#' 
readEE <- function(x) {
    
    llee <- lapply(x, function(ix){
                if(!file.exists(ix)) stop(paste0("Can't find file ", ix), call.=FALSE )
                df <- read.csv(ix, stringsAsFactors = FALSE, quote = "", fileEncoding = "latin1")
                namesLut <- c("Scene.Cloud.Cover"="Cloud.Cover", "Acquisition.Date"="Date.Acquired",  "Day.Night.Indicator"="Day.Night")
                for(i in names(namesLut)) {
                    names(df)[names(df) == i] <- namesLut[i]
                }
                allLScats <- c("Landsat.Scene.Identifier", "WRS.Path", "WRS.Row", "Data.Category", "Cloud.Cover",
                        "Station.Identifier", "Day.Night", "Data.Type.Level.1", "Date.Acquired", 
                        "Sun.Elevation", "Sun.Azimuth", "Geometric.RMSE.Model.X", 
                        "Geometric.RMSE.Model.Y", "Display.ID", "Ordering.ID", "Download.Link", "Browse.Link")        
                
                inter <- allLScats %in% colnames(df)
                df <- df[,allLScats[inter]]
                df[,allLScats[!inter]] <- NA
                df <- df[, allLScats]
                df$Date <- as.POSIXct(df$Date.Acquired, "%Y/%m/%d")
                df$Doy  <- as.numeric(format(df$Date, format = "%j"))
                df$Year <- as.numeric(format(df$Date, format = "%Y"))
                df$Satellite <- paste0("LS", substr(df$Landsat.Scene.Identifier, 3, 3))
                df$Num <- as.numeric(substr(df$Landsat.Scene.Identifier,3,3))
                df
            })
    out <- do.call("rbind", llee)
    return(out)
}

