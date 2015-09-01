#' Tidy import tool for EarthExplorer .csv export files
#' 
#' Imports CSV files exported from EarthExplorer into data.frames and annotates missing fields 
#' 
#' @param x Character, Character or list. One or more paths to EarthExplorer export files.
#' @return data.frame
#' @details 
#' The \href{http://earthexplorer.usgs.gov/}{EarthExplorer} CSV file can be produced from the search results page. Above the results click on 'export results' and select 'comma (,) delimited'.
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
#' 		geom_segment(aes(x = Date, xend = Date, y = 0, yend = 100 - Cloud.Cover, 
#'      col = as.factor(Year))) +
#' 		scale_y_continuous(name = "Scene quality (% clear sky)")
#' 
readEE <- function(x) {
	llee <- lapply(x, function(ix){
				df <- read.csv(ix, stringsAsFactors = FALSE, quote = "", fileEncoding = "latin1")
				names(df)[names(df) == "Scene.Cloud.Cover"] <- "Cloud.Cover"
				allLScats <- c("Landsat.Scene.Identifier", "WRS.Path", "WRS.Row", "Data.Category", "Cloud.Cover",
						"Station.Identifier", "Day.Night", "Data.Type.Level.1", "Date.Acquired", 
						"Sun.Elevation", "Sun.Azimuth", "Geometric.RMSE.Model.X", 
						"Geometric.RMSE.Model.Y", "Display.ID", "Ordering.ID", "Download.Link")
				df <- df[,allLScats]
				df$Date <- strptime(df$Date.Acquired, "%Y/%m/%d")
				df$Doy  <- as.numeric(strftime(df$Date, format = "%j"))
				df$Year <- as.numeric(strftime(df$Date, format = "%Y"))
				df$Satellite <- paste0("LS", substr(df$Landsat.Scene.Identifier, 3, 3))
				df$Num <- as.numeric(substr(df$Landsat.Scene.Identifier,3,3))
				df
			})
	out <- do.call("rbind", llee)
	return(out)
}

