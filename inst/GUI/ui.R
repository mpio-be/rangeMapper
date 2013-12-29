 	source("functions.R")

shinyUI(pageWithSidebar(

  headerPanel = headerPanel(
	HTML( paste('<h4>rangeMapper', packageVersion('rangeMapper') ) ) 
  
  ), 
  		
  mainPanel = mainPanel(
    # includes	
	includeHTML('tooltip.js'),
	includeHTML('bootstrap-switch.html'),
	
	# messages
	 HTML('<span class="label label-important">PROJECT INFO: </span>' ),
	
	
	htmlOutput("messages"),
	htmlOutput("Print"),
	
	# plot
	div(class="span12", plotOutput("PLOT"))
  ), 
 
sidebarPanel = sidebarPanel(

###
HTML('



<script type="text/javascript">

</script>

'),

#### PROJECT ####
	# PROJ type
	HTML('<form class="well form-inline" >
		<span class="add-on" data-placement="right" data-toggle="tooltip" class="label label-success" title=  "Open or create a rangeMapper project" >  
		 <i class="icon-folder-open"></i>
		 <strong> PROJECT </strong> </span>
		 <br>
		<select id = "open" class = "input-small" >
			<option>NEW</option>
			<option>OPEN</option>
		</select>
		'),
			
	# OPEN PROJECT
	conditionalPanel(condition = "input.open == 'OPEN'", 
		HTML('<label> File path: 
				<input type="text" id="openProjPath" class="input-large"
				data-placement="bottom" data-toggle="tooltip" title =  "Paste the path to the rangeMapper project file here."  /> </label>
			') ),
			
	
	# NEW PROJECT
	conditionalPanel(condition = "input.open == 'NEW'", 


		#  proj name
		HTML('
			Project: 
				<input type="text" id="newProjPath" class="input-large" value = ',
				shQuote( tempdir() ),
				'data-placement="bottom" data-toggle="tooltip" title = "Enter directory path here"/> 
			
				<input type="text" id="newProjNam" class="input-small" value = "wrens"
				data-toggle="tooltip" title = "Enter file name here, an sqllite file will be created on disk using this name."
				/>
			'), 	
			
		# ranges
				HTML('<br>
					Ranges: 
					<input type="text" id="shpPath" class="input-large"  value = ',
					shQuote(system.file(package = "rangeMapper", "extdata", "wrens", "vector_combined", "wrens.shp")),
					'data-toggle="tooltip" title = "Enter the path to the range (*.shp) files on disk"/> 
				
					<input type="text" id="rangeID" class="input-small" value = "sci_name" 
					data-toggle="tooltip" title = "Enter the polygon (i.e. range) name identifier e.g. sci_name for the case of the wrens file." /> 
				'),
				
		# grid size
				HTML('<br>
					Grid size: 
						<input type="number" id="gridSize" class="input-mini" value = 1.5
						data-toggle="tooltip" title = "Enter the grid size (map units), leave empty for default." /> 
				'),		
		
		# GO!
		HTML('<br><div id ="goNewBut" class="switch" data-on-label= "WAIT!" data-off-label="START" data-on="danger" data-off="primary"><input id = "goNew" type="checkbox" /></div>')			
	), 
	
	# BIO
				HTML(' <br><hr>
						<em data-toggle="tooltip" title = "Optionally import a table (e.g. life history data) to the rangeMapper project. ; 
						More than one table can be imported." > 
						BIO table: </em>
						<input type="text" id="bioPath" class="input-large"  value = ',
						shQuote(system.file(package = "rangeMapper", "data", "wrens.csv")),
						'data-toggle="tooltip" title = "Enter the path to the BIO (csv ; delimited) table on disk"/> 
					
						<input type="text" id="bioID" class="input-small" value = "sci_name" 
						data-toggle="tooltip" title = "ID; the column corresponding to the names of the range files"
						 /> 
					
					
					'),

		# GO!
		HTML('<br><div class="switch" data-on-label="WAIT!" data-off-label="START" data-on="danger" data-off="primary"><input id = "goBio" type="checkbox" /></div>'),	

	
# MAPPING
HTML('<hr>
	<span class="add-on" data-placement="right" data-toggle="tooltip" class="label label-success" title=  "Save a MAP table to the active project." >  
	<i class="icon-globe"></i>
	<strong> MAP </strong> </span>
	<br>
		<select id = "mapType" class = "input-small" >
			<option>Trait</option>
			<option>Species richness</option>
		</select>'),
		
conditionalPanel(condition = "input.mapType == 'Trait'",		
	HTML('
	Variable: 
		<select id = "mapVar" class = "input-medium" data-placement="top" data-toggle="tooltip" class="label label-success" title=  "Variable to map, variable:table"> 
		<option> </option> </select>
	
	Name:
		<input id = "mapNam" type="text" class = "input-small" value = "map_1" data-placement="top" data-toggle="tooltip"  title=  "Map name"> 
		
		<br>
  
	Function: 
		<select id = "FUN" class = "input-medium" data-placement="top" data-toggle="tooltip"  title=  "Choose a function"> 
		 <option> avg </option>
		 <option> count </option>
		 <option> max </option>
		 <option> min </option>
		 <option> total </option>
		 <option> sum </option>
		 <option> stdev </option>
		 <option> variance </option>
		 <option> mode </option>
		 <option selected> median </option>
		 <option> lower_quartile </option>
		 <option> upper_quartile </option>

		</select>
	
	
	<br><div class="switch" data-on-label="WAIT!" data-off-label="START" data-on="danger" data-off="primary"><input id = "goMap" type="checkbox" /></div>
	
	')),	

	
conditionalPanel(condition = "input.mapType == 'Species richness'",		
	HTML('
	<br><div class="switch" data-on-label="WAIT!" data-off-label="START" data-on="danger" data-off="primary"><input id = "goMapSR" type="checkbox" /></div>
	')),	
	
	

# PLOT
HTML('<hr>
	<i class="icon-eye-open"></i> <strong> DISPLAY </strong> </span> 
	<br>

	Map: 
		<select id = "mapPlotNam" multiple="multiple" class = "input-medium" data-placement="top" data-toggle="tooltip" class="label label-success" title=  "Choose a map to display. Keep the Shift or Ctrl pressed to select more than one map."> 
		<option> </option></select>
	
	 
	<a href="http://cran.r-project.org/web/packages/classInt/" target="_blank"> Style: </a> 
		<select id = "classIntervals" class = "input-small" data-placement="top" data-toggle="tooltip" class="label label-success" title=  "Choose a class interval style."> 
			<option> equal </option> <option> sd </option> <option> pretty </option> <option> quantile </option> <option> kmeans </option> <option> hclust </option> <option> bclust </option> <option> fisher </option> <option> jenks </option> 		
	</select>
	
	<br>
	
	 Palette:
		<select id = "palette"  class = "input-small" data-placement="top" data-toggle="tooltip" class="label label-success" title=  "Choose a color palette."> 
			<option>BrBG</option> <option>PiYG</option> <option>PRGn</option> <option>PuOr</option> <option>RdBu</option> <option>RdGy</option> <option>RdYlBu</option> <option>RdYlGn</option> <option>Spectral</option> <option>Blues</option> <option>BuGn</option> <option>BuPu</option> <option>GnBu</option> <option>Greens</option> <option>Greys</option> <option>Oranges</option> <option>OrRd</option> <option>PuBu</option> <option>PuBuGn</option> <option>PuRd</option> <option>Purples</option> <option>RdPu</option> <option>Reds</option> <option>YlGn</option> <option>YlGnBu</option> <option>YlOrBr</option> <option>YlOrRd</option>
		</select>
	
	Reverse palette:
	<input type="checkbox" id="inversePal"/>
	
	<u data-display-if="input.goPlot == 0">	
	Show palettes:
	<input id="showPalettes" class="shiny-bound-input" type="checkbox">
	</u>
	

	
	<br><div class="switch" data-on-label="ON" data-off-label="OFF" data-on="warning" data-off="success"><input id = "goPlot" type="checkbox" /></div>

</form>
	'),
	

	
	
#### footnote
	HTML(paste('<p><small>Running with', strsplit(R.version$version.string, "\\(")[[1]][1], '& shiny', packageVersion('shiny'), '</small></p>' ) ),
	HTML( '<p> <small> Questions and suggestions &rarr; Mihai Valcu (valcu@orn.mpg.de) </small></p>')

)
))


























