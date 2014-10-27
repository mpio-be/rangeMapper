

######## SERVER
add.alpha <- function (col,alpha) { sprintf("%s%02X",col,floor(alpha*256))	
	}

validString <-function(x) { 
	!is.null(x) && nchar(x) > 0
	}

getPath <- function(input) { 
	if (input$open == 'OPEN') {
		path = input$openProjPath
		if(!file.exists(path)) stop("File does not exist!")
	}
	
	if (input$open == 'NEW') {
		path = 	paste( input$newProjPath, paste(input$newProjNam, 'sqlite', sep = '.'), sep = .Platform$file.sep)
		if(!file.exists(path)) stop("There is no new project!")
	}
		return(path)

	
	
}
	
BioFieldNames <- function(input) { 
	prefix = "BIO_"
	path = getPath(input)
	
		dbcon = rangeMap.open(path)
		
		tabs = dbGetQuery(dbcon, paste('SELECT name FROM  sqlite_master where type = "table" and name like', shQuote(paste(prefix,"%", sep = '') )))$name
		cols = lapply(tabs, function(x) dbGetQuery(dbcon, paste("PRAGMA table_info(", x, ")"))[, 'name', FALSE] )
		if(length(cols) > 0) for(i in 1:length(cols)) cols[[i]]$table = tabs[[i]] else
		cols = list(data.frame(name = "N/A", table = "N/A"))
		cols = do.call(rbind, cols)
		cols$name_table = paste(cols$name, cols$table, sep = ":")
		dbDisconnect(dbcon)
		return(cols)

}

MapNames <- function(input) { 
		path = getPath(input)
		dbcon = rangeMap.open(path)
		res = dbGetQuery(dbcon, 'SELECT name FROM  sqlite_master where type = "table" and name like "MAP_%"')$name
		res = gsub("MAP_", "", res)
		dbDisconnect(dbcon)
		res
}

projInfo <- function(input) {

		path = getPath(input)

		dbcon = rangeMap.open(path)
		s = summary(new("rangeMap", CON = dbcon))
		
		out = data.frame(variable = c("Project location", "Proj4 string", "Grid Size"), description = c(s$Project_location, s$Proj4, s$CellSize))
		
		dbDisconnect(dbcon)
		out
		
	}

newProj <- function(input, ranges) {
	
	dbcon = rangeMap.start(file = paste(input$newProjNam, 'sqlite', sep = '.'), dir = input$newProjPath, overwrite = TRUE)
	global.bbox.save(con = dbcon, bbox = ranges)
	if( is.null(input$gridSize)) 
		gridSize.save(dbcon) else 
			gridSize.save(dbcon, gridSize = input$gridSize)
	canvas.save(dbcon)
	processRanges(spdf = ranges, con = dbcon, ID = input$rangeID)
	dbDisconnect(dbcon)
	
}

importBIO <-function(input) {
		path = getPath(input)
		dbcon = rangeMap.open(path)
		bio.save(con = dbcon, loc = input$bioPath,  ID = input$bioID)
		dbDisconnect(dbcon)

	}
	
makeTraitMap <- function(input) {
	path = getPath(input)
	dbcon = rangeMap.open(path)

	v = strsplit(input$mapVar, ":")[[1]]
	if(v[1] == "species richness") rangeMap.save(dbcon) else
	rangeMap.save(dbcon, biotab = gsub("BIO_", "", v[2]) , FUN = input$FUN, biotrait = v[1], tableName = input$mapNam)
	
	
	dbDisconnect(dbcon)
	}
	
makeSRMap <- function(input) {
	path = getPath(input)
	dbcon = rangeMap.open(path)
	rangeMap.save(dbcon)
	dbDisconnect(dbcon)
	}
	
plotMap <- function(input) {
	path = getPath(input)
	dbcon = rangeMap.open(path)

	m = rangeMap.fetch(dbcon, input$mapPlotNam)
	cpal = brewer.pal.get(input$palette)
	if(input$inversePal) cpal = rev(cpal)
	plot(m, scales = TRUE, style = input$classIntervals, colorpalette = cpal )
	dbDisconnect(dbcon)
	}







































  