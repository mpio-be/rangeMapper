
NEW FEATURES
------------
  * Write methods for hexagonal canvas
  * add spatialPolygonsRangeMapper class containing a range ID slot. Update coresponding methods
  * polygon overlap on spatialPolygonsRangeMapper()
  * add match.call() to rangeMap.save()  and save to project
  * rangeMap.save() if FUN returns two values than two maps should be saved.
  * new method for processRanges() based on the doMC package.
  * write a few 'as' methods (e.g. for raster).
  * define operations in maps (e.g. map1 + map2) both wrapper for raster and direct sql methods
  * improve summary() rangeMapper* objects. 
  * Write ignette  to document project db structure. 


BUGS, FIXES
-----------
  * Fix Dependencies: suggest shiny (> 0.6?) , R > 3.0.x
  * Update rangeMap.save, raster method: use over() instead of deprecated overlay() or raster only implementation 
  * bio.merge() has a call to a orphaned dbcon
  * rm.rangeMapper() should not give an error when nothig to remove is found
  * rangeMap.save() should return TRUE if a valid MAP_ is saved ot the project
  * rangeMap.save() should not save empty maps
  * .extract.indexed, if index is missing return meaningful message.


