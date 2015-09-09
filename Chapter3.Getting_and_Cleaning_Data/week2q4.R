fileUrl <- "http://biostat.jhsph.edu/~jleek/contact.html"

con <- url(fileUrl)
contact <- readLines(con)
nchar(contact[10])
nchar(contact[20])
nchar(contact[30])
nchar(contact[100])
