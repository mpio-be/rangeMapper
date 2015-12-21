

TODO
----
* move to roxigen2
* write help to reflect changes in rangeMap.fetch


NEW FEATURES
------------
  * Write methods for hexagonal canvas
  * add spatialPolygonsRangeMapper class containing a range ID slot. Update corresponding methods
  * polygon overlap on spatialPolygonsRangeMapper()
  * add match.call() to rangeMap.save()  and save to project
  * rangeMap.save() if FUN returns two values than two maps should be saved.
  * write a few 'as' methods (e.g. for raster).
  * define operations in maps (e.g. map1 + map2) both wrapper for raster and direct sql methods
  * improve summary() rangeMapper* objects.
  * Write vignette  to document project db structure.


BUGS, FIXES
-----------
  * rangeMap.save() should return TRUE if a (non-empty) MAP_ is saved to the project or FALSE
  if an empty map was created.





