# ============================================================
# BioR — Class 5 setup (Programming with R)
# Installs missing packages, then loads them.
# ============================================================


pkgs <- c(
  "tm",
  "wordcloud2",
  "scholar",
  "stringr",
  "wordcloud"
)


# Install missing packages
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

# Load packages (grouped, so students see what is for what)
library(tm)
library(wordcloud2)
library(scholar)
library(stringr)
library(wordcloud)
library(tm)
library(ggplot2)








sample(c("Heads", "Tails"), size = 1)
#write a loop function

flip <- sample(c("Heads", "Tails"), size = 1)

ntails <- 0
nheads <- 0

if (flip == "Heads") {
  nheads <- nheads + 1
} else {
  ntails <- ntails + 1
}



results = NULL
for (i in 1:100) {
  flip <- sample(c("Heads", "Tails"), size = 1)
  
  if (flip == "Heads") {
    nheads <- nheads + 1
  } else {
    ntails <- ntails + 1
  }
  
  results [i] <- flip
}

####positional arguments
results = NULL
flip1 <- sample(c("Heads", "Tails"), size = 1)
results [1] <- flip1

results = NULL
flip1 <- sample(c("Heads", "Tails"), size = 1)
results [2] <- flip1


scientist <- "EyMe2b8AAAAJ&hl" #LUCA RINDI

profile <- get_profile(scientist)
profile$name

papers <- get_publications(scientist)
str(papers)
View(papers)

papers$title
papers[1,1]
papers[2,1]

words1 <- str_split(papers$title, " ")
title <- str_split(papers[1,1], pattern = " ") [[1]]



all_words <- NULL

filler_words <- c("the", "and", "of", "in", "to", 
                  "a", "is", "for", "on", "with", "by",
                  "as", "that", "are", "from", "this",
                  "be", "at", "or", "an", "which", "it", "was", "but", "&")


for (i in 1:nrow(papers)) {
  title <- str_split(papers[i,1], pattern = " ") [[1]]
  all_words <- c(all_words, title)
}

all_words <- all_words[!all_words %in% filler_words]



data <- table(all_words)
View(data)

all_words <- tolower(all_words)

words <- tolower(all_words)

docs <- Corpus(VectorSource(words))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(word_freqs), freq = word_freqs) 

wordcloud2(d, size = 0.5, color = "random-light", backgroundColor = "black")



wordcloud(docs, colors = "aquamarine")


###TEXT MINING

# function to add three numbers
add_three <- function(a, b, c) {
  return(a + b + c)
}

#calling the function
result <- add_three(2, 3, 5)
print(result)


p1 <- c(5,3)
p2 <- c(2,2)

euc_dist <- function(p1, p2) {
  sqrt((p2[1] - p1[1])^2 + (p2[2] - p1[2])^2)
}

print(euc_dist(p1, p2))


x11()
plot(c(1:5),c(1:5))
locator(2)

#geerate a data and plot it, then click on two points to measure the distance between them
gen_data <- matrix(runif(50), nrow = 25, ncol = 2)
x11()
plot(gen_data, xlab = "X", ylab = "Y", cex = 2, col = "blue")


points_clicked <- locator(n = 2)
p1 <- c(points_clicked$x[1], points_clicked$y[1])
p2 <- c(points_clicked$x[2], points_clicked$y[2])


print(euc_dist(p1, p2))

