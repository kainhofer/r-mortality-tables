data.Namespace.name = "data:MortalityTables"


# When unloading, also remove all loaded mortality tables from the "data:MortalityTables" namespace
.onDetach <- function(libpath) {
    if (data.Namespace.name %in% search()) {
        detach(data.Namespace.name, character.only = TRUE)
    }
}

#' Load a named set of mortality tables provided by the \link{MortalityTables} package
#'
#' @param dataset The set(s) of life tables to be loaded. A list of all available
#'                data sets is provided by the function \code{\link{mortalityTables.list}}.
#'                Wildcards (*) are allowed to match and load multiple datasets.
#' @param package The package that contains the dataset in its \code{extdata/}
#'                directory. Defaults to all packages starting with names that
#'                start with "MortalityTables" or "PensionTables".
#'                Multiple packages can be given as a vector, even using regular expressions.
#'                This package is not automatically loaded. If a provided
#'                dataset needs its proving package loaded, it can do so explicitly.
#' @param prefix The prefix for the data sets (default is "MortalityTables").
#'
#' @examples
#' mortalityTables.list()
#' mortalityTables.load("Austria_Annuities_*")
#' mortalityTables.load("Austria_Annuities_AVOe2005R")
#' mortalityTables.load("*Annuities")
#' \dontrun{mortalityTables.load("MyCustomTable", package = c("MyCustomPackage"))}
#'
#' @export
mortalityTables.load = function(dataset, package = c("^MortalityTables", "^PensionTables"), prefix = "MortalityTables") {
    # TODO: Generalize lib.loc to a function parameter
    # We want all files that are of the following form:
    #   [LIBDIR]/MortalityTables*/extdata/[PREFIX]_[NAME].R
    # where [NAME] matches the dataset argument and load them

    if (missing(dataset)) {
        warning("No datasets given to load mortality tables. Please list at least one dataset (or a corresponding pattern)")
    }

    for (set in dataset) {
        lib.loc <- .libPaths()
        # Get a list of all directories under lib.loc for MortalityTables / PensionTable extensions packages
        packs = unlist(lapply(package, FUN = function(p) { list.files(lib.loc, p, full.names = TRUE)}))
        # From those directories, list all extdata/[prefix]_[set].R files
        files = Sys.glob(file.path(packs, "extdata", paste(prefix, "_", set, ".R", sep = "")))

        if (length(files) == 0) {
            warning(sprintf("Unable to locate dataset '%s' provided by the package(s) %s!", dataset, paste(c(package), collapse = " or ")));
        }

        loaded = FALSE
        for(filename in files) {
            # TODO: Extract the dataset name from the filename
            sname = gsub(paste('^', prefix, '_(.*).R$', sep = ""), '\\1', basename(filename))
            sname = gsub("[^-A-Za-z0-9_.]", "", sname);

            # Make sure the data namespace is available in the search path!
            if (!data.Namespace.name %in% search()) {
                attach(list(), name = data.Namespace.name)
                oop <- options(topLevelEnvironment = globalenv())
                on.exit(options(oop))
            }
            dataenv = as.environment(data.Namespace.name)

            if (sname %in% dataenv$.tables.loaded) {
                message("Dataset '", sname, "' was already loaded...")
                loaded = TRUE
                next
            } else {
                message("Loading table dataset '", sname, "'");
            }
            # Taken from the definition of sys.source and adjusted to include the
            # encoding (required for Windows, otherwise UTF8-strings will be broken!)
            lines = readLines(filename, encoding = "UTF-8", warn = FALSE)
            srcfile = srcfilecopy(filename, lines, file.mtime(filename), isFile = TRUE)
            exprs = parse(text = lines, srcfile = srcfile, keep.source = TRUE)
            for (i in seq_along(exprs))
                # eval(exprs[i], envir = globalenv())
                eval(exprs[i], envir = dataenv)

            # sys.source(filename, envir = globalenv())
            loaded = TRUE
            # .tables.loaded <<- c(.tables.loaded, sname)
            assign(x = ".tables.loaded", value = c(dataenv$.tables.loaded, sname), envir = dataenv)
            print(dataenv$.tables.loaded)
        }
        if (!loaded) {
            warning(sprintf("Unable to locate dataset '%s' provided by the %s package!", set, package));
        }
    }
}


#' Load a named set of pension tables provided by the \link{MortalityTables} package
#'
#' @param dataset The set of lifpensione tables to be loaded. A list of all available
#'                data sets is provided by the function \code{\link{pensionTables.list}}.
#'                Wildcards (*) are allowed to match and load multiple datasets.
#' @param package The package that contains the dataset in its \code{extdata/}
#'                directory. Defaults to all packages starting with names that
#'                start with "MortalityTables" or "PensionTables".
#'                Multiple packages can be given as a vector, even using regular expressions.
#'
#' pensionTables.list()
#' pensionTables.load("*")
#' pensionTables.load("USA_PensionPlan_RP2014")
#'
#' @export
pensionTables.load = function(dataset, package = c("^MortalityTables", "^PensionTables")) {
    mortalityTables.load(dataset = dataset, package = package, prefix = "PensionTables")
}

