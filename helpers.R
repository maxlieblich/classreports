library(reshape)
library(ggplot2)

dehex <- function(x) {
  switch(x, 
         "0" = 0,
         "1" = 1,
         "2" = 2,
         "3" = 3,
         "4" = 4,
         "5" = 5,
         "6" = 6,
         "7" = 7,
         "8" = 8,
         "9" = 9,
         a = 10,
         b = 11,
         c = 12,
         d = 13,
         e = 14,
         f = 15)
}

grades <- function (x, questions) {
  if (x == "" | is.na(x)) {
    rep(NA, times = questions)
  } else {
    L <- sapply(strsplit(tolower(x), split = "")[[1]], dehex)
    names(L) <- paste("Q", 1:questions, sep=".")
    L
  }
}

# need to deal with NA right above and below
# assigning questions here is kludge; better to figure it out
test <- function (x, questions) {
  data.frame(t(sapply(x, function(y) grades(y, questions))))
}

makeTest <- function (file.name="", test.name="", 
                      total.points=NA, 
                      id="Username") {
  # data ingestion
  # Assuming that a cleaned csv file is presented
  cats <- read.csv(file.name, colClasses="character")
  test.name <- gsub(" ", ".", test.name) # do what read.csv does
  # the number of questions is the median of the number of questions in each record
  # assuming that all but a tiny number of students will have the right number of qs!
  questions <- median(sapply(cats[[test.name]], nchar))
  curr.test <- test(cats[[test.name]], questions)
  curr.test$total <- rowSums(curr.test)
  curr.test[[id]] <- cats[[id]]
  curr.test <- curr.test[complete.cases(curr.test),]
  # exclude the 0s; this probably means something was very wrong, like students absent
  curr.test <- curr.test[curr.test$total != 0,]
  
  list(curr.test = curr.test, 
       total.points = total.points, 
       questions = questions,
       name = test.name,
       id = id)
}

makeTests <- function (file.name = "", test.names="", questions=NA, total.points=NA) {
  cats <- read.csv(file.name, colClasses="character")
  test.names <- lapply(test.names, function (x) {gsub(" ", ".", x)}) # do what read.csv does
  
}

questionBoxPlot <- function (test) {
  qs <- test$questions
  g <- ggplot(melt(test$curr.test[,1:qs])) + geom_boxplot(aes(x=variable, y=value)) + xlab("Problem") + ylab("Points")
  g
}

pairPlot <- function (test) {
  library(GGally)
  g <- ggpairs(test$curr.test, 
          1:test$questions,
          axisLabels = "none",
          params = c(binwidth=1),
          lower = list(continuous="points", combo="dot"),
          alpha=0.4,
          diag = list(continuous="bar"))
  g
}

testHistogram <- function (test) {
  g <- ggplot(test$curr.test) + geom_histogram(aes(x=total), binwidth=2)
  g
}

testDensityPlot <- function (test) {
  df <- test$curr.test[test$curr.test$total != 0, ]
  g <- ggplot(df, aes(x=total)) + geom_density() + 
    stat_function(fun=dnorm, args=list(mean=mean(df$total), sd=sd(df$total)), color='red')
  g
}

compareTwoTests <- function (test1, test2) {
  m1 <- test1$curr.test
  m2 <- test2$curr.test
  ms <- merge(m1, m2, by=test1$id)
  g <- ggplot(ms, aes(x=total.x, y=total.y)) + geom_point(alpha=0.4) + xlab(test1$name) + ylab(test2$name)
  g <- g + geom_smooth()
  g
}