library(stringr)
library(RCurl)
library(XML)
#========================================
# cran_archive()   
#========================================
cran_archive = function(x){
  return (paste("http://cran.r-project.org/src/contrib/Archive/", x, collapse = "", sep = "")) 
}

#========================================
# is_package()  
#========================================
is_package = function(x){
  return (url.exists(cran_archive(x)))
}

#========================================
# download_archive
#========================================
download_archive = function(x){
  return (if(is_package(x)) readHTMLTable(cran_archive(x))[[1]])
}

#========================================
# clean_archive_table() 
#========================================
clean_archive_table = function(x){
  frame = download_archive(x)
  return (frame[c(-1,-2, -nrow(frame)), c("Name", "Last modified", "Size")])
}

#========================================
# version_names()
#========================================
version_names = function(x){
  return (gsub(pattern = "_.+gz$", replacement = "", x = clean_archive_table(x)$Name))
}

#========================================
# version_nums()
#========================================
version_nums = function(x){
  return (gsub(pattern = ".+_|\\.tar.+gz$", replacement = "", x = clean_archive_table(x)$Name))
}

#========================================
# version_dates()
#========================================
version_dates = function(x){
  return (as.Date(clean_archive_table(x)$`Last modified`, "%d-%b-%Y"))
}

#========================================
# version_sizes()
#========================================
version_sizes = function(x){
  x = gsub(pattern = "M", replacement = "", x = clean_archive_table(x)$Size)
  x[c(grep(pattern = "[[:digit:]]$",x = x))] = as.numeric(x[c(grep(pattern = "[[:digit:]]$",x = x))]) * 1000.0
  return (as.numeric(gsub(pattern = "K", replacement = "", x = x)))
}

#========================================
# build_table()
#========================================
build_table = function(x){
  output = data.frame(version_names(x), version_nums(x), version_dates(x), version_sizes(x))
  colnames(output) = c("name", "number", "date", "size")
  return (output)
}

#========================================
# get_archive_table()
#========================================
get_archive_table = function(x){
  return (build_table(x))
}

#========================================
# ggstep()
#========================================
library(ggplot2)
ggstep = function(x){
  return (ggplot(data = x, aes(x = date, y = size)) + geom_step() + 
          labs(title = sprintf("%s size versions timeline", unique(x$name)), y = "Size(Kilobyte)"))
}












