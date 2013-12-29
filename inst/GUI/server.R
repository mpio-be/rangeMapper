

    

shinyServer( function(input, output, session) {
	source("functions.R")

	# project info
	output$messages <- renderTable({

		if( input$goNew ) {
			r = readOGR( dirname(input$shpPath) , gsub('.shp$', '', basename(input$shpPath) ), verbose = FALSE)
			newProj(input, r)
			updateCheckboxInput(session, "goNew", value = FALSE)
			updateCheckboxInput(session, "goPlot", value = FALSE)
	}

		if( input$goBio ) {
			importBIO(input)
			updateCheckboxInput(session, "goBio", value = FALSE)
	}

		if( input$goMap ) {
			makeTraitMap(input)
			updateCheckboxInput(session, "goMap", value = FALSE)
		}
		
		if( input$goMapSR ) {
			makeSRMap(input)
			updateCheckboxInput(session, "goMapSR", value = FALSE)
		}
		



	updateSelectInput(session, "mapVar", choices = BioFieldNames(input)$name_table )
	updateSelectInput(session, "mapPlotNam", choices = MapNames(input) )
	projInfo(input)



	}, include.rownames = FALSE, include.colnames = FALSE)

	output$PLOT <- renderPlot({

		if( input$goPlot & !is.null(input$mapPlotNam) ) {
			plotMap(input)
		}		
		
		if( !input$goPlot &  input$showPalettes) {
			display.brewer.all()
		}
		
		
		
		
		

	} 

	,width  = 297*2.5 , height = 210*2.5
	)

	output$Print <- renderPrint({
	 cat('<span class="label label-important">MAPS:</span>')
	})
  
  
  
  
  
 })
 
 
 
 
 
