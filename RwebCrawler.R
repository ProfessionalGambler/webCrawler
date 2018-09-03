#libraries
library("rvest")


# read in html
page <- read_html("https://www.imdb.com/chart/top")

#get movie titles
title <- page %>%html_nodes(".titleColumn a") %>%html_text() 

#get movie rankings and make numeric
rating <- page%>%html_nodes(".imdbRating")%>%html_text() %>% as.numeric()

year <-page%>%html_nodes(".secondaryInfo")%>%html_text()

#get rid of special charcaters 
year <-gsub("[^[:alnum:][:blank:]+?&/\\-]", "", year)

#turn year from char to number
year <-year %>% as.numeric()

#get image links
img <- page%>%html_nodes("#main img")
link <- html_attr(img, "src")

#make data set with information
imdb_table <- data.frame(title, year, rating, link)

#make histogram of ratings
hist(imdb_table$rating,
     main="Top IMDB Movie Ratings",
     xlab = "Ratings",
     ylab = "Frequency of Ratings",
     border = "green",
     col = "blue",
     xlim = c(8,10),
     ylim = c(0,100)
     #,prob = TRUE
     )
#make bar graph of year and rating
counts<- table(imdb_table$year, imdb_table$year)
barplot(counts, main="Movie Distribution by Year and Ratings",
        xlab="Years",
        ylab = "Ratings",
        col = c("blue","red")
        #legend = rownames(counts)
        )
#make density plot
den <- density(imdb_table$rating)
plot(den, 
     main="IMDB Movie Rating Density",
     xlab="Ratings"
      )
polygon(den, col="gold", border="darkblue") #fill in color


     