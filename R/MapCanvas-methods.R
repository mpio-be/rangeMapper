
.extract.p4s <- function(ShpFiles) {
#ShpFiles = selectShpFiles(paste(system.file(package="rangeMapper"), "extdata", "wrens", "vector", sep = .Platform$file.sep))
	fl = split(ShpFiles, ShpFiles$layer)
	unlist(lapply(fl, FUN = function(x) OGRSpatialRef(x[,1], x[,2])  ))
}

.rect2spp <- function(xmin, xmax, ymin, ymax) {
		bb = cbind(c(xmin, xmax, xmax, xmin, xmin), c(ymin, ymin, ymax, ymax, ymin) )
		SpatialPolygons(Srl = list(Polygons(list(Polygon(bb)), "bb")) )
}


#### BBOX ###

setGeneric("rangeMapBbox", function(object,...)   	             	standardGeneric("rangeMapBbox") )
setGeneric("rangeMapBboxSave", function(object,bbox, p4s,...)  standardGeneric("rangeMapBboxSave") )
setGeneric("rangeMapBboxFetch", function(object,...)   		standardGeneric("rangeMapBboxFetch") )

setMethod("rangeMapBbox",  
	signature  = c(object = "rangeFiles"),
		definition = function(object, checkProj = TRUE) {
		shpFiles = rangeFiles(object)
		
		message(paste("Computing global bounding box for",length(shpFiles ), "ranges...") )
		
		nfo = lapply(shpFiles, getinfo.shape)
			
		bb = do.call(rbind, lapply(nfo, function(x) c(x$minbounds[1:2], x$maxbounds[1:2]) ) )
		bb = c( xmin = min(bb[,1]), xmax = max(bb[,3]), ymin = min(bb[,2]), ymax = max(bb[,4]) )
		
		ogrShpFiles = data.frame(dsn = dirname(shpFiles), layer = gsub(".shp", "", basename(shpFiles)), stringsAsFactors = FALSE)
		
		if(checkProj) {
		message("Checking for proj4 string differences...")
			p4s = .extract.p4s(ogrShpFiles) 
			p4s = p4s[!duplicated(p4s)]
			if(length(p4s) > 1) warning(paste("More than one projection found:\n", paste("  *",p4s, collapse = "\n")))
			}	else 
				p4s = .extract.p4s(ogrShpFiles[1, ])
		
		attributes(bb)$p4s = as.character(p4s)
		message("Done!")
		
		bb
		
		}
	)

	setMethod("rangeMapBboxSave",  
		signature  = c(object = "rangeMap", bbox = "missing", p4s = "missing"),
		definition = function(object,bbox, p4s) {
	if(! .is.empty(object@CON, object@BBOX) ) stop("Bounding box was allready saved for this project.")
	
	bb = structure(c(-180, 180, -90, 90), 
		.Names = c("xmin", "xmax", "ymin", "ymax"), 
		p4s = NA) # "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
	
	warning(paste("Using unprojected global bounding box [", paste(bb, collapse = ","), "]..." ) )
	
	res1 = dbWriteTable(object@CON, object@BBOX, data.frame(t(bb)), append = TRUE, row.names = FALSE)
	res2 = dbWriteTable(object@CON, object@PROJ4STRING, data.frame(p4s = attributes(bb)$p4s), append = TRUE, row.names = FALSE)
	
	res = all(res1, res2)
	
	if(res) 
		message(c("Bounding box uploaded.", "PROJ4STRING set to ", attributes(bb)$p4s) ) else 
		warning("Bounding box upload failed.")
 
	 }
)

setMethod("rangeMapBboxSave",  
	signature  = c(object = "rangeMap", bbox = "missing", p4s = "CRS"),
		definition = function(object, bbox, p4s) {
		if(! .is.empty(object@CON, object@BBOX) ) stop("Bounding box was allready saved for this project.")
		
	bb = structure(c(-180, 180, -90,90), 
		.Names = c("xmin", "xmax", "ymin", "ymax"), 
		p4s = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
	
	warning(paste("Using unprojected global bounding box [", paste(bb, collapse = ","), "]..." ) )
	
		message(paste("Converting to", p4s@projargs) )
			bbnew = .rect2spp(bb[1], bb[2], bb[3], bb[4])
			bbnew =  spsample(bbnew, n = 1000, type = "regular", offset = c(0,0))
			proj4string(bbnew) = attributes(bb)$p4s
			bbnew = spTransform(bbnew , p4s )
			bb = c(bbox(bbnew )[1, ], bbox(bbnew )[2, ] )
			attributes(bb)$p4s = p4s@projargs
		
		res1 = dbWriteTable(object@CON, object@BBOX, data.frame(t(bb)), append = TRUE, row.names = FALSE)
		res2 = dbWriteTable(object@CON, object@PROJ4STRING, data.frame(p4s = attributes(bb)$p4s), append = TRUE, row.names = FALSE)
		
		res = all(res1, res2)
		
		if(res) 
			message(c("Bounding box uploaded.", "PROJ4STRING set to ", attributes(bb)$p4s) ) else 
			warning("Bounding box upload failed.")
	 
	 }
)


	setMethod("rangeMapBboxSave",  
		signature  = c(object = "rangeMap", bbox = "character", p4s = "missing"),
		definition = function(object,bbox, p4s) {
		if(! .is.empty(object@CON, object@BBOX) ) stop("Bounding box was allready saved for this project.")
		
		# bbox  the path to the range file(s) directory, pass to new("rangeFiles" ....
		
		bb = rangeMapBbox( new("rangeFiles", dir = bbox, ogr = FALSE), checkProj = TRUE )
		
		res1 = dbWriteTable(object@CON, object@BBOX, data.frame(t(bb)), append = TRUE, row.names = FALSE)
		res2 = dbWriteTable(object@CON, object@PROJ4STRING, data.frame(p4s = attributes(bb)$p4s), append = TRUE, row.names = FALSE)
		
		res = all(res1, res2)
		
		if(res) 
			message( paste("Bounding box uploaded.", "PROJ4STRING set to ", attributes(bb)$p4s) ) else 
			warning("Bounding box upload failed.")
	 
	 }
)

setMethod("rangeMapBboxSave",  
		signature  = c(object = "rangeMap", bbox = "character", p4s = "CRS"),
		definition = function(object, bbox, p4s) {
		if(! .is.empty(object@CON, object@BBOX) ) stop("Bounding box was allready saved for this project.")
		
		bb = rangeMapBbox( new("rangeFiles", dir = bbox, ogr = FALSE),checkProj = TRUE )

		message(paste("Converting to", p4s@projargs) )
			bbnew = .rect2spp(bb[1], bb[2], bb[3], bb[4])
			bbnew =  spsample(bbnew, n = 1000, type = "regular", offset = c(0,0) )
			proj4string(bbnew) = attributes(bb)$p4s
			bbnew = spTransform(bbnew , p4s )
			bb = c(bbox(bbnew )[1, ], bbox(bbnew )[2, ] )
			attributes(bb)$p4s = p4s@projargs
		
		res1 = dbWriteTable(object@CON, object@BBOX, data.frame(t(bb)), append = TRUE, row.names = FALSE)
		res2 = dbWriteTable(object@CON, object@PROJ4STRING, data.frame(p4s = attributes(bb)$p4s), append = TRUE, row.names = FALSE)
		
		res = all(res1, res2)
		
		if(res) 
			message(c("Bounding box uploaded.", "PROJ4STRING set to ", attributes(bb)$p4s) ) else 
			warning("Bounding box upload failed.")
	 
	 }
)

setMethod("rangeMapBboxSave",  
		signature  = c(object = "rangeMap", bbox = "Spatial", p4s = "missing"),
		definition = function(object, bbox, p4s) {
		if(! .is.empty(object@CON, object@BBOX) ) stop("Bounding box was allready saved for this project.")
		
		bb = c( bbox(bbox)[1, ], bbox(bbox)[2, ])
		p4s = proj4string(bbox)
		
		res1 = dbWriteTable(object@CON, object@BBOX, data.frame(t(bb)), append = TRUE, row.names = FALSE)
		res2 = dbWriteTable(object@CON, object@PROJ4STRING, data.frame(p4s), append = TRUE, row.names = FALSE)
		
		res = all(res1, res2)
		
		if(res) 
			message(c("Bounding box uploaded.", "PROJ4STRING set to ", p4s) ) else 
			warning("Bounding box upload failed.")
		

	 
	 }
)

setMethod("rangeMapBboxFetch",  
	signature  = "rangeMap",
		definition = function(object) {
		if(.is.empty(object@CON, object@BBOX ) ) stop("Bounding box not yet constructed for this project!")
		md = dbReadTable(object@CON, object@BBOX)
		p4s = dbReadTable(object@CON, object@PROJ4STRING)
		
		bb = .rect2spp(md$xmin, md$xmax, md$ymin, md$ymax)
		proj4string(bb) = p4s$p4s
		return(bb)
		 
	 }
	)

#user level
global.bbox.save  <- function(con, ...) { 
	x = new("rangeMap", CON = con)
	rangeMapBboxSave(x, ... )

}

#user level
global.bbox.fetch <- function(con) {
 
 x = new("rangeMap", CON = con)
 rangeMapBboxFetch(x)


}



#### GRID SIZE ####
setGeneric("gridSizeSave", function(object, ...)   					standardGeneric("gridSizeSave") )

setMethod("gridSizeSave",  
	signature  = "gridSize",
		definition = function(object) {
				if( length(object@gridSize)!=1  ) {
				bb  = global.bbox.fetch(object@CON)
				minSpan = min(diff(bbox(bb)[1, ]), diff(bbox(bb)[2, ]))
				object@gridSize = minSpan/100
				warning(paste("Default grid size used!"))
			}
			
			grd = data.frame(object@gridSize)
			names(grd) = object@GRIDSIZE
			res = dbWriteTable(object@CON, object@GRIDSIZE, grd, append = TRUE, row.names = FALSE)
			
			if(res) message( paste("Grid size set to", object@gridSize, "map units.") )
	}
)
	
	
setGeneric("gridSizeFetch", function(object, ...)  					standardGeneric("gridSizeFetch") )

setMethod("gridSizeFetch",  
	signature  = "rangeMap",
		definition = function(object) {
		
		if(.is.empty(object@CON, object@GRIDSIZE)) stop("The grid size is not yet defined for this project!")
		
		res = dbReadTable(object@CON, object@GRIDSIZE)[1,1]
		return(res)
	 }
	)	
	
#user level
gridSize.save <- function(con,...) {
	
	x = new("gridSize", CON = con,...)
		
	gridSizeSave(x)	
		
}

gridSize.fetch <- function(con) { 
	x = new("rangeMap", CON = con)
	gridSizeFetch(x)	

}



#### CANVAS ####
setGeneric("canvasFetch", function(object, ...)   					standardGeneric("canvasFetch") )
setGeneric("canvasSave", function(object, ...)   					standardGeneric("canvasSave") )


setMethod("canvasSave",  
	signature  = "rangeMap", 
		definition = function(object) {
		
		if(!.is.empty(object@CON, object@CANVAS) ) stop("The canvas was allready constructed!")
		if(.is.empty(object@CON, object@GRIDSIZE) )  stop("The grid size is missing!")

		bbox     = global.bbox.fetch(object@CON)
		cellsize = gridSize.fetch(object@CON)
		
		cnv = spsample(bbox, cellsize = cellsize, type = "regular", offset = c(0.5,0.5))
		
		cnv = data.frame(coordinates(cnv), id = 1:nrow(coordinates(cnv)))

		names(cnv) = c("x", "y", "id")

		res = dbWriteTable(object@CON, object@CANVAS, cnv, append = TRUE, row.names = FALSE) 

		if(res) message("Canvas uploaded.")
		
		}
	)	

setMethod("canvasFetch",  
	signature  = "rangeMap", 
		definition = function(object) {
		
		cnv = dbGetQuery(object@CON, 'SELECT * FROM canvas' )

		if(nrow(cnv) == 0) stop("The canvas is empty, did you run canvas.save()?")

		coordinates(cnv) = ~ x + y

		p4s = dbReadTable(object@CON, object@PROJ4STRING)[1,1]
		
		proj4string(cnv) = CRS(p4s)
		
		gridded(cnv) = TRUE
		
		cnv
		
		}
	)	

# user level	
canvas.fetch <- function(con) { 
	x = new("rangeMap", CON = con)
	canvasFetch(x)	

} 

canvas.save  <- function(con) {

	x = new("rangeMap", CON = con)
	canvasSave(x)	


}

