#' Tidy import tool for EarthExplorer .csv Expors
#' 
#' @param x Character, Character or list. One or more paths to EarthExplorer export files.
#' @export 
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
	do.call("rbind", llee)
}

