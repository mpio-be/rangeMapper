
.onAttach <- function(libname, pkgname) {
	dcf <- read.dcf(file=system.file("DESCRIPTION", package=pkgname) )
    
	packageStartupMessage("---------------------------------------------------------------------------------------")
	
	packageStartupMessage(paste('This is', pkgname, dcf[, "Version"] ))
	packageStartupMessage("   o ", "Type ", sQuote("rangeMapper()"), " to start the graphical user interface.")
	packageStartupMessage("   o ","Type ", sQuote("?rangeMapper"), "to access the help files.")
	packageStartupMessage("   o ", "The latest version of", dQuote(" AppendixS2_S5.R"), " in Valcu, M., et al (2012) GEB 21, 945-951")
	packageStartupMessage("   ", " can be found at ",  strsplit(dcf[, "URL"], ",")[[1]][[1]] )
	
	packageStartupMessage("---------------------------------------------------------------------------------------")
}

