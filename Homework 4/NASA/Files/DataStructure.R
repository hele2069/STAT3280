#### Build graph corresponding to 24 x 24 grid

ele <- read.table("../Files/elevation.dat",header=TRUE)

colnames(ele)
rownames(ele)

elemat <- as.matrix(ele)

n <- nrow(elemat)

image(t(elemat[n:1,]))

hist(as.numeric(elemat))

image(log(t(elemat[n:1,])+1))


Lon <- colnames(ele)
Lon <- unlist(lapply(Lon,function(x)gsub("X.","",x)))
names(ele) <- Lon

ele.Lon <- as.numeric(Lon)
ele.Lat <- as.numeric(rownames(ele))

metric.Lon <- scan("../Files/Lon.txt",what="",sep="\t")
metric.Lon <- unlist(lapply(metric.Lon,function(x) gsub("W","",x)))
metric.Lon <- as.numeric(metric.Lon)

metric.Lat <- scan("../Files/Lat.txt",what="",sep="\n")
metric.Lat <- unlist(lapply(metric.Lat,function(x){
    x <- gsub("N","",x)
    x <- gsub("S","",x)
})                            )
metric.Lat <- as.numeric(metric.Lat)
metric.Lat[16:24] <- -metric.Lat[16:24]


ele.Lon
metric.Lon

ele.Lat
metric.Lat


####### use the elevation data set to find approximate elevation for the 24 x 24 grid 
## (approximation is needed because the positions of the measurements are not the same as the elevation data )
approx.elevation <- matrix(0,24,24)
### use closet location for approximation
for(i in 1:24){
    lat <- metric.Lat[i]
    dist.seq.lat <- abs(ele.Lat-lat)
    lat.index <- which.min(dist.seq.lat)

    for(j in 1:24){
        lon <- metric.Lon[j]
        dist.seq.lon <- abs(ele.Lon-lon)
        lon.index <- which.min(dist.seq.lon)
        approx.elevation[i,j] <- ele[lat.index,lon.index]
    }
}

image(log(approx.elevation+1))

image(t(log(approx.elevation+1)[24:1,]))


image(t(log(approx.elevation+1)[24:1,]),col = terrain.colors(100))

image(t(log(approx.elevation+1)[24:1,]),col = terrain.colors(100),xaxt="n",yaxt="n")

points(x=c(0.5,0.6),y=c(0.6,0.5),pch=16,col="red")
lines(x=c(0.5,0.6),y=c(0.6,0.5),lwd=2,col="blue")
x.seq <- runif(10)
y.seq <- runif(10)
points(x=x.seq,y=y.seq,pch=1,col="red")


x.seq <- 1:100
y.seq <- sin(x.seq)
plot(x.seq,y.seq,type="b")

x.scaled <- (x.seq-50)/101*(1/23)
y.scaled <- (y.seq)/2*(1/23)



plot(x.scaled,y.scaled,"b")


x.new.scaled <- x.scaled + 3/23
y.new.scaled <- y.scaled + 23/23
image(t(log(approx.elevation+1)[24:1,]),col = terrain.colors(100))
points(x.new.scaled,y.new.scaled,pch=20,lwd=0.6,cex=0.3)














elevation <- approx.elevation


GridTimeSeries <- list()

#### remark: bad notation. Do not use t, because it is overwriting the matrix transpose

for(tt in 1:72){
pressure <- read.table(paste("../Files/pressure",tt,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
pressure <- as.numeric(as.matrix(pressure[,-(1:3)]))

ozone <- read.table(paste("../Files/ozone",tt,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
ozone <- as.numeric(as.matrix(ozone[,-(1:3)]))

surftemp <- read.table(paste("../Files/surftemp",tt,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
surftemp <- as.numeric(as.matrix(surftemp[,-(1:3)]))

cloudhigh <- read.table(paste("../Files/cloudhigh",tt,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
cloudhigh <- as.numeric(as.matrix(cloudhigh[,-(1:3)]))


cloudmid <-  read.table(paste("../Files/cloudmid",tt,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
cloudmid <- as.numeric(as.matrix(cloudmid[,-(1:3)]))

cloudlow <- read.table(paste("../Files/cloudlow",tt,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
cloudlow <- as.numeric(as.matrix(cloudlow[,-(1:3)]))

temperature <- read.table(paste("../Files/temperature",tt,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
temperature <- as.numeric(as.matrix(temperature[,-(1:3)]))



#load("ApproxElevation.Rda")
elevation <- as.numeric(elevation)

X <- data.frame(pressure=pressure,ozone=ozone,surftemp=surftemp,cloudhigh=cloudhigh,cloudmid=cloudmid,cloudlow=cloudlow,temperature=temperature,elevation=elevation)

with.missing <- sum(is.na(X)) > 0

GridTimeSeries[[tt]] <- list(X=X,with.missing=with.missing)
}

GridTimeSeries[[2]]


dt <- GridTimeSeries[[15]]$X


dim(dt)





save(GridTimeSeries,file="NASAGridTimeSeries.Rda")



