
# every company should exactly five rows, for each year 2011:2015.  They don't. I want to
# "blow out" the data.frame and fill in empty rows of data as appropriate.

gc(reset=TRUE)
x <- read.csv("fakedata.csv", as.is=TRUE) # ~ 10 seconds, not bad
gc()            # Yes -- memory overhead, but not a deal-breaker here
object.size(x)  # 152 MB
x[1:10, 905:ncol(x)]
dim(x)
table(table(x$company))
x$HasRawData <- TRUE
dim(x)
foo <- tapply(x$year, x$company, length)
table(foo)
length(foo)      # company
# For checking below
foo <- foo[foo<5]  # The ones that need year row/rows filled out
yyy <-list()
for(i in 1:length(foo)) yyy[[i]] <- 0
i <- 1
# four ways to speed up:
# 1.try library(data.table) rbindlist
# 2.try docall(rbind,), but not as efficient as 1.
# 3. try determine the structure first and fill in
# 4. parallel programming
library(data.table)
system.time({
for (company in names(foo)) {
  yy <- x[x$company==company,]
  yyyy <- list()
  hasyears <- yy$year
  yy <- yy[1,]
  yy$HasRawData <- FALSE
  yyyy <- lapply(1:(5-foo[company]),function(i) yyyy[[i]]<-yy)
  y <- as.data.frame(rbindlist(yyyy))
  y$year <- setdiff(2011:2015, hasyears)
  y$Status <- "Unlisted"
  y[,5:(ncol(y)-4)] <- NA       # Careful with this, only remove the data.
  yyy[[i]] <- y
  i <- i+1
  }
  z <- as.data.frame(rbindlist(yyy))
})
# user  system elapsed 
# 147.369  43.932 192.682

# Finish up:
xx <- as.data.frame(rbindlist(list(x, z)))
xx <- xx[order(xx$company, xx$year),]
table(tapply(xx$year, xx$company, length))   # Check it!

##### parallel programming ####
i <- 1
library(doMC)
registerDoMC(4)
system.time({
  yyy <- foreach (company = names(foo)) %dopar% {
    yy <- x[x$company==company,]
    yyyy <- list()
    hasyears <- yy$year
    yy <- yy[1,]
    yy$HasRawData <- FALSE
    yyyy <- lapply(1:(5-foo[company]),function(i) yyyy[[i]]<-yy)
    y <- as.data.frame(rbindlist(yyyy))
    y$year <- setdiff(2011:2015, hasyears)
    y$Status <- "Unlisted"
    y[,5:(ncol(y)-4)] <- NA       # Careful with this, only remove the data.
    i <- i+1
    return(y)
  }
  z <- as.data.frame(rbindlist(yyy))
})
# user  system elapsed 
# 182.504  27.580  75.740 
# Finish up:
xx <- as.data.frame(rbindlist(list(x, z)))
xx <- xx[order(xx$company, xx$year),]
table(tapply(xx$year, xx$company, length))   # Check it!

# revise
library(doMC)
registerDoMC(4)
system.time({
  yyy <- foreach(company = names(foo), .combine = rbind) %dopar% {
    yy <- x[x$company==company,]
    hasyears <- yy$year
    yy <- yy[1,]
    yy$HasRawData <- FALSE
    yyyy <- list()
    for(i in 1:(5-foo[company])){
      yyyy[[i]] <- yy
    }
    yyyy <- as.data.frame(rbindlist(yyyy))
    yyyy$year <- setdiff(2011:2015, hasyears)
    yyyy$Status <- "Unlisted"
    yyyy[,5:(ncol(yyyy)-4)] <- NA       # Careful with this, only remove the data.
    return(yyyy)
  }
})
# user  system elapsed 
# 157.345  27.491 118.269 
# Finish up:
xx <- as.data.frame(rbindlist(list(x, z)))
xx <- xx[order(xx$company, xx$year),]
table(tapply(xx$year, xx$company, length))   # Check it!

