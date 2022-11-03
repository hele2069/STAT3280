
### Question 1a
library(heplots)
data(AddHealth)
dt1 <- heplots::AddHealth

library(RColorBrewer)
colors <- brewer.pal(5,"Greys")
par(mfrow=c(1,3))

# margin=2 columns; margin=1 rows;
tb1 <- prop.table(table(dt1$depression,dt1$grade),margin=2)
tb2 <- prop.table(table(dt1$anxiety,dt1$grade),margin=2)
matrix1 <- rbind(table(dt1$depression),table(dt1$anxiety))
tb3 <- prop.table(as.table(matrix1),margin=2)

barplot(tb1,col=colors,xlab="Grade Level",
     main="Depression Density by Grade",asp=14,ylim=c(0:1))

legend("top",title="Severity",legend=0:4,
       pch=15,col=colors,cex=0.9,ncol=5)

barplot(tb2,col=colors,xlab="Grade Level",
        main="Anxiety Density by Grade",asp=14,ylim=(0:1))

legend("top",title="Severity",legend=0:4,
       pch=15,col=colors,cex=0.9,ncol=5)

barplot(tb3,col=colors,xlab="Severity Level",
        main="Depression~Anxiety by Severity",asp=12,ylim=(0:1))

legend("top",title="Type",legend=c("Depression","Anxiety"),
       pch=15,col=c("grey85","grey60"),cex=0.9,ncol=2)

### Question 1b
library(gscaLCA)
data(AddHealth)
dt2 <- gscaLCA::AddHealth

par(mfrow=c(1,2))
tbl4=table(dt2$Marijuana,dt2$Edu)
tbl5=table(dt2$Cocaine,dt2$Edu)

# marijuana
plot(dt2$Marijuana,dt2$Gender,xlab="Marijuana Use",ylab="Gender",
     cex.main=1,main="Marijuana~Gender")

mosaicplot(tbl4,xlab="Marijuana Use",ylab="Education level",
           main="Marijuana~Education level",
           col=c("red","green","yellow","blue","black",
                 "white","pink","purple","cyan","gray","orange","brown","lightblue"))

# cocaine 
plot(dt2$Cocaine,dt2$Gender,xlab="Cocaine Use",ylab="Gender",
     cex.main=1,main="Cocaine~Gender")
mosaicplot(tbl5,xlab="Cocaine Use",ylab="Education level",
           main="Cocaine~Education level",
           col=c("red","green","yellow","blue","black",
                 "white","pink","purple","cyan","gray","orange","brown","lightblue"))

# Question 2
rivalry=read.csv("/Users/yhe/Desktop/Homework 1/Rivalry Data.csv")
rivalry1=data.frame(Year=rivalry$Year,City=as.factor(rivalry$City),
                    State=as.factor(rivalry$State),NC=rivalry$North.Carolina,
                    VA=rivalry$Virginia,VA_Win=as.factor(rivalry$VA_Win),
                    NC_Win=as.factor(rivalry$NC_Win),HomeVA=as.factor(rivalry$Home.VA),
                    HomeNC=as.factor(rivalry$Home.NC),ScoreDiff=rivalry$ScoreDiff)
colors <- brewer.pal(5,"Greys")

par(mfrow=c(1,2))

plot(x=rivalry1$VA_Win,y=rivalry1$HomeVA,xlab="Win (Y/N/Tie)",ylab="Played Home (Y/N)",
     cex.main=1,main="UVA Game Result(1)",col=c("Dark Blue","Dark Orange1"))

plot(rivalry1$Year,rivalry1$ScoreDiff,xlab="Year",ylab="ScoreDiff",
     main="Score Difference with Year(2)",
     col=c("cornflowerblue","Black","Dark Orange1")[as.numeric(rivalry1$VA_Win)])
abline(h=0,col="grey 50",lty=3,lwd=3)
abline(v=1925,col="Green",lty=3,lwd=3)
abline(v=1983,col="Green",lty=3,lwd=3)
legend("topright",c("VA Won","NC Won","Tie"),
       pch=1,col=c("Dark Orange1","cornflowerblue","Black"),cex=0.7)

hist(rivalry$Virginia,col=c("Dark Blue","Dark Orange1"),
     xlab="Score", main="UVA Scores Distribution(3)",ylim=c(0,50))
hist(rivalry$North.Carolina,col=c("cornflowerblue","white"),
     xlab="Score",main="UNC Scores Distribution(4)",ylim=c(0,50))
plot(rivalry1$HomeVA,rivalry1$VA,xlab="Home",ylab="UVA Scores",
     main="UVA Game~Score(5)",col=c("Dark Blue","Dark Orange1"))
plot(rivalry1$HomeNC,rivalry1$NC,xlab="Home",ylab="UNC Scores",
     main="UNC Game~Score(6)",col=c("cornflowerblue","white"))

