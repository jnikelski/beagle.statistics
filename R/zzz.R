.First.lib <- function(libpath, pkgname) {
#
#  library and package names are the same (in this case)
	# libName = pkgname
	# cat(paste("Loading library: ", libName, "  (Package: ", pkgname, ", Library path: ", libpath,")\n", sep=""))
	# library.dynam(libName, package=pkgname, lib.loc=libpath)

}

.Last.lib <- function(libpath) {
	#
	#  library and package names are the same (in this case)
	# libName <- "mniMincIO"
	# pkgName <- "mniMincIO"
	# cat(paste("Unloading shared library: ", libName, ". Library unload path is ", libpath,"\n", sep=""))
	# library.dynam.unload(libName, file.ext=".so", verbose=TRUE, libpath=libpath)
}
