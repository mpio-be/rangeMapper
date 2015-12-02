
#' rangeMapper GUI
#' @export
#' @examples
#' rangeMapper()
#'
rangeMapper <- function() {
	require(shiny)
	shiny::runApp(system.file('GUI', package = 'rangeMapper'))
	}


