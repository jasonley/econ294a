# Question 0

JasonLeyAssignment2 <- list(
  firstName = "Jason",
  lastName  = "Ley",
  email     = "jley@ucsc.edu",
  studentID = 1248754
)

# Question 1
diamonds <- get(  
  load(
      file = url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData")
      )
  )

JasonLeyAssignment2$s1a <- nrow(diamonds)
JasonLeyAssignment2$s1b <- ncol(diamonds)
JasonLeyAssignment2$s1c <- names(diamonds)
JasonLeyAssignment2$s1d <- summary(diamonds$price)

# Question 2
NHIS <- read.delim(file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")

JasonLeyAssignment2$s2a <- nrow(NHIS)
JasonLeyAssignment2$s2b <- ncol(NHIS)
JasonLeyAssignment2$s2c <- names(NHIS)
JasonLeyAssignment2$s2d <- mean(NHIS$weight)
JasonLeyAssignment2$s2e <- median(NHIS$weight)

hist(NHIS$weight)
table(NHIS$weight)

NHIS$weight2<-ifelse(NHIS$weight>=996, NA, NHIS$weight)

JasonLeyAssignment2$s2f <- mean(NHIS$weight2, na.rm=TRUE)
JasonLeyAssignment2$s2g <- median(NHIS$weight2, na.rm=TRUE)

NHISmale<-subset(NHIS,(SEX==1))
NHISfemale<-subset(NHIS,(SEX==2))

JasonLeyAssignment2$s2h <- summary(NHISmale$weight)
JasonLeyAssignment2$s2i <- summary(NHISfemale$weight)

# Question 3
vec <- c(letters,LETTERS)

JasonLeyAssignment2$s3a <- vec[1:26*2]
JasonLeyAssignment2$s3b <- vec[c(36,1,19)]

arr <- array(c(letters,LETTERS), dim=c(3,3,3))

JasonLeyAssignment2$s3c <- arr[,1,2]
JasonLeyAssignment2$s3d <- arr[2,2,]
JasonLeyAssignment2$s3e <- paste(arr[1,1,2],arr[1,1,1],arr[1,1,3])

# Question 4

library(foreign)
org_example <- read.dta(
    file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

sort(unique(org_example$year))
sort(unique(org_example$month))
sort(unique(org_example$educ))

stndz <- function(x){
      (x - mean(x, na.rm = T))
    }
  temp <- split(
      org_example$rw, c(org_example$year)
    )
  temp.2 <- lapply(
      temp, 
      stndz
    )
  temp.3 <- unsplit(
      temp.2,
      c(org_example$year)
    )
  org_example$averwyear <- temp.3
  mean(subset(org_example, year == 2013)$rw, na.rm = T) # [1] 21.84623
  mean(subset(org_example, year == 2008)$rw, na.rm = T) # [1] 21.84623
  mean(subset(org_example, year == 2003)$rw, na.rm = T) # [1] 21.84623
  mean(subset(org_example, year == 1998)$rw, na.rm = T) # [1] 21.84623
  mean(subset(org_example, year == 1993)$rw, na.rm = T) # [1] 21.84623
  mean(subset(org_example, year == 1988)$rw, na.rm = T) # [1] 21.84623
  mean(subset(org_example, year == 1993)$rw, na.rm = T) # [1] 21.84623
  
  
problem4 <- data.frame(
    col1 = org_example$year,
    col2 = org_example$month,
    col3 = org_example$educ,
    col4 = org_example$averwyear
)
