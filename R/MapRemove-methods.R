
setGeneric("rangeMapRemove", function(object, ...)   		     	standardGeneric("rangeMapRemove") )


setMethod("rangeMapRemove",  
		signature = "rangeMapRemove", 
		definition = function(object){
		
		if(length(object@tableName) == 0 ) 
			object@tableName = RMQuery(object@CON, 
				'select name from sqlite_master where type = "table" and 
				(tbl_name like "MAP_%" OR tbl_name like "BIO_%")')$name

		if( length(object@tablePrefix) > 0 )
		object@tableName = object@tableName[grep(object@tablePrefix, object@tableName)]		
				
			sql = paste("DROP TABLE ", object@tableName )
			
		for (i in 1:length(sql)) RMQuery(object@CON , sql[i]) 
		
	message(paste( paste(object@tableName, collapse = "; "), "removed" , collapse = " ") )

		
		}
)


# user level
rm.rangeMapper <- function(con, ...) {

	 x =  new("rangeMapRemove", CON = con, ...)
	 rangeMapRemove(x)

 }

 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 


	



