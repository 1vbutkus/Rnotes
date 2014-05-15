
install <- FALSE
wait <- FALSE

setwd("/media/F/Vygantas/Konspektavimas/R/Rnotes/")
doc = readLines("RPackages.in")

packages <- c("stringr", "RCurl", "XML", "XML2R")
for(package in packages){
   if (!require(package, character.only = TRUE)){
      install.packages(package)
   }
}


pause <- function() {
  cat("Press ENTER/RETURN/NEWLINE to continue.")
  if (wait) readLines(n=1)
  invisible()
}


# remove empty lines
doc <- doc[nchar(doc) > 0]

# remove comments
doc <- gsub("#.+$", "", doc)

# remove dublicate space
doc <- gsub("\\s+", " ", doc)

# Trim the rest
doc <- str_trim(doc)

# remove empty lines
doc <- doc[nchar(doc) > 0]

# getting package lines
pid <- grep("package:", doc, ignore.case = TRUE)
rid <- diff(c(pid, length(doc) + 1))

PackageList <- list()


for (i in seq_along(pid)) {
    StrVec <- doc[1:rid[i] + pid[i] - 1]
    pid2 <- grep("^.+:", StrVec)
    rid2 <- diff(c(pid2, length(StrVec) + 1))
    
    elem <- list()
    for (j in seq_along(pid2)) {
        str <- paste(StrVec[1:rid2[j] + pid2[j] - 1], collapse = "\n")
        name <- tolower(str_trim(sub(":.*$", "", str)))
        value <- str_trim(sub("^.+:", "", str))
        elem[[name]] <- value
    }
    if (length(elem$rating) > 0) 
        elem$rating <- as.numeric(elem$rating) else elem$rating <- 5
    if (length(elem$tags) > 0) {
        value <- str_replace_all(elem$tags, "\n", ",")
        value <- strsplit(value, ",|;")[[1]]
        value <- str_trim(value)
        elem$tags <- value[nchar(value) > 0]
    }
    class(elem) <- "PackageInfo"
    if (elem$package %in% names(PackageList)) 
        warning("Package `", elem$package, "` has dublicate. It was updated.")
    PackageList[[elem$package]] <- elem
}
class(PackageList) <- "PackageList"


print.PackageInfo <- function(elem) {
    
    # reordering
    NameList <- c(none = 1, package = 10, rating = 9, tags = 8, comment = -6, heading = -7, description = -8)
    id <- order(NameList[match(names(elem), names(NameList), nomatch = 1)], decreasing = TRUE)
    elem <- elem[id]
    
    str <- sapply(elem, function(e) paste(e, collapse = ", "))
    str <- paste(names(str), ": ", str, sep = "")
    cat(paste(str, collapse = " \n"))
    cat("\n")
}

print.PackageList <- function(PackageList) {
    for (i in seq_along(PackageList)) {
        print(PackageList[[i]])
        cat("\n")
    }
}

summary.PackageList <- function(PackageList) {
    class(PackageList) <- c("summary.PackageList", "PackageList")
    PackageList
}

print.summary.PackageList <- function(PackageList, ...) {
    df = data.frame(package = names(PackageList), heading = sapply(PackageList, function(e) e$heading))
    str <- paste(format(df$package, justify = "left"), " # ", df$heading, sep = "")
    write.table(str, quote = FALSE, col.names = FALSE, row.names = FALSE, ...)
}


######################################################################## geting extra info



UrlBase = "http://cran.r-project.org/web/packages/<package>/index.html"
for (i in seq_along(PackageList)) {
    package <- PackageList[[i]]$package
    Url <- sub("<package>", package, UrlBase)
    if (url.exists(Url)) {
        con <- url(Url)
        html <- htmlParse(file = readLines(con))
        close(con)
        PackageList[[i]]$heading <- sub("^.+?: ", "", xmlValue(getNodeSet(html, "//h2")[[1]]))
        PackageList[[i]]$description <- xmlValue(getNodeSet(html, "//p")[[1]])
        if (install) {

            Install <- TRUE
            if (length(PackageList[[package]]$install)>0)
               if (PackageList[[package]]$install=="FALSE")
                 Install <- FALSE
            if (Install & !require(package, character.only = TRUE)) {
                cat(paste("Will install: ", package, "\n", sep=""))
                pause()
                install.packages(package)
                cat(package)
                cat("\n")
                pause()
            }
        }
    } else {
        PackageList[[i]]$heading <- "NA"
        PackageList[[i]]$description <- "NA"
        warning(paste("There is no link to package `", PackageList[[i]]$package, "`. ", sep = ""))
    }
}


summary(PackageList)
# PackageList

############################################ TagList ###
TagsDf <- data.frame()

for (i in seq_along(PackageList)) {
    elem <- data.frame(package = PackageList[[i]]$package, tag = PackageList[[i]]$tags, tagrank = seq_along(PackageList[[i]]$tags), 
        stringsAsFactors = FALSE)
    TagsDf <- rbind(TagsDf, elem)
}




TakePackages <- function(tags, and = TRUE, print = TRUE, TagsData = TagsDf, PackagesData = PackageList) {
    
    PackagesNames <- function(tags, and = and, TagsData = TagsData) {
        tags <- tolower(tags)
        
        if (!and) {
            return(unique(subset(TagsData, subset = TagsData$tag %in% tags)$package))
        }
        
        if (length(tags) == 0) 
            return(character(0))
        
        packages = subset(TagsData, subset = TagsData$tag %in% tags[1])$package
        if (length(tags) == 1) 
            return(packages)
        
        for (i in 2:length(tags)) {
            packages <- intersect(packages, subset(TagsData, subset = TagsDf$tag %in% tags[i])$package)
        }
        return(packages)
    }
    
    Names <- PackagesNames(tags, and = and, TagsData = TagsData)
    
    PackagesData <- PackageList[Names]
    
    df = data.frame(package = names(PackagesData), rating = sapply(PackagesData, function(e) e$rating), heading = sapply(PackagesData, 
        function(e) e$heading))
    df$heading <- gsub("\\n", " ", df$heading)
    df <- df[order(df$rating, decreasing = TRUE), ]
    
    
    str <- paste(format(df$package, justify = "left"), " # ", format(df$rating), " -", df$heading, sep = "")
    if (print) 
        write.table(str, quote = FALSE, col.names = FALSE, row.names = FALSE)
    invisible(df)
}

# TakePackages(c("data", "input-output"))

save(PackageList, TagsDf, TakePackages, file = "RPackages.RData")



 
