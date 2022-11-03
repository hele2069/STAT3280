### Question 1

face <- read.table("/Users/yhe/Desktop/3280-Homework 2/face-data.txt")
face1=face[,1]
face2=face[,2]
face3=face[,3]
face4=face[,4]
mat1=matrix(face1,nrow=64,ncol=64)
mat2=matrix(face2,nrow=64,ncol=64)
mat3=matrix(face3,nrow=64,ncol=64)
mat4=matrix(face4,nrow=64,ncol=64)
mat11=apply(t(mat1),2,rev)
mat111=apply(t(mat11),2,rev)
mat1111=apply(t(mat111),2,rev)
mat22=apply(t(mat2),2,rev)
mat222=apply(t(mat22),2,rev)
mat2222=apply(t(mat222),2,rev)
mat22222=apply(t(mat2222),1,rev)
mat33=apply(t(mat3),2,rev)
mat333=apply(t(mat33),2,rev)
mat3333=apply(t(mat333),2,rev)
mat44=apply(t(mat4),2,rev)
mat444=apply(t(mat44),2,rev)
mat4444=apply(t(mat444),2,rev)
mat44444=apply(t(mat4444),1,rev)
par(mfrow=c(4,1))
image(mat1111)
image(mat22222)
image(mat3333)
image(mat44444)


### Question 2

#Question2
library(YaleToolkit)
x <- read.csv("/Users/yhe/Desktop/3280-Homework 2/TeacherHires.csv")
class(x$Hired)
x <- read.csv("/Users/yhe/Desktop/3280-Homework 2/TeacherHires.csv",
              stringsAsFactors = TRUE)
class(x$Hired)
#### Depending on your R version, the default value for the argument stringsAsFactors may be TRUE or FALSE.
#### I will demonstrate the case when this is TRUE. It would be slightly easier if you set it as FALSE.
#It is also helpful to improve variable names; try to use short-yet-descriptive words, without awkward capitalization. I use the type.truncate option to narrow the whatis display.
names(x) <- c("interviewed", "hired", "appdate",
              "age", "sex", "residence", "GPA.u",
              "GPA.g", "MA", "substitute",
              "teaching", "experience", "workkids",
              "volunteer")

whatis(x)
whatis(x, type.truncate = 4)
summary(x)
# Clean up sex: Why are there 5 distinct values for this variable?
x$sex
x$sex <- as.character(x$sex)
unique(x$sex)
levels(x$sex)
x$sex[x$sex=="  "] <- NA
x$sex[x$sex=="M"] <- "Male"
x$sex[x$sex=="F"] <- "Female"
x$sex <- factor(x$sex)
levels(x$sex)
table(x$sex)
whatis(x)
# Clean up age: age was listed as a factor character
class(x$age)
table(x$age)

### how about as.numeric?
as.numeric(x$age)
levels(x$age)
summary(as.numeric(x$age))
## this is because age is a factor now
## if you change a factor to numeric, the value will follow the level order, not the level name
levels(x$age)
x$age[1]
as.numeric(x$age[1])
as.numeric("N/A")
as.numeric("")
as.numeric("22")
summary(as.numeric(as.character(x$age)))
## this is what we want
x$age <- as.numeric(as.character(x$age))
whatis(x)
######interviewed cleaning
x$interviewed
x$interviewed <- as.character(x$interviewed)
unique(x$interviewed)
levels(x$interviewed)
x$interviewed[x$interviewed=="yes "] <- "yes"
x$interviewed <- factor(x$interviewed)
levels(x$interviewed)
table(x$interviewed)
######hired cleaning
x$hired
x$hired <- as.character(x$hired)
unique(x$hired)
levels(x$hired)
x$hired[x$hired=="yes "] <- "yes"
x$hired[x$hired=="yes*"] <- "yes"
x$hired <- factor(x$hired)
levels(x$hired)
table(x$hired)
######GPA.u cleaning
x$GPA.u=as.character(x$GPA.u)
levels(x$GPA.u)
unique(x$GPA.u)
x$GPA.u[x$GPA.u=="N/A"] <- NA
x$GPA.u[x$GPA.u==""] <- NA
summary(as.numeric(x$GPA.u))
x$GPA.u <- as.numeric(x$GPA.u)
whatis(x)
######GPA.g cleaning
x$GPA.g=as.character(x$GPA.g)
levels(x$GPA.g)
unique(x$GPA.g)
x$GPA.g[x$GPA.g=="N/A"] <- NA
x$GPA.g[x$GPA.g==""] <- NA
x$GPA.g[x$GPA.g==" N/A"] <- NA
summary(as.numeric(x$GPA.g))
x$GPA.g <- as.numeric(x$GPA.g)
table(x$GPA.g)
whatis(x)
######MA cleaning
x$MA
x$MA <- as.character(x$MA)
unique(x$MA)
levels(x$MA)
x$MA[x$MA=="yes "] <- "yes"
x$MA[x$MA=="N/A"] <- NA
x$MA <- factor(x$MA)
levels(x$MA)
table(x$MA)
######substitute cleaning
x$substitute
x$substitute <- as.character(x$substitute)
unique(x$substitute)
x$substitute[x$substitute=="no "] <- "no"
x$substitute[x$substitute==""] <- NA
x$substitute <- factor(x$substitute)
table(x$substitute)
######teaching cleaning
x$teaching <- as.character(x$teaching)
unique(x$teaching)
x$teaching[x$teaching=="yes "] <- "yes"
x$teaching[x$teaching=="N/A"] <- NA
x$teaching[x$teaching==""] <- NA
x$teaching <- factor(x$teaching)
table(x$teaching)
######experience cleaning
x$experience <- as.character(x$experience)
unique(x$experience)
x$experience[x$experience=="N/A"] <- NA
x$experience[x$experience==""] <- NA
x$experience[x$experience=="2 months"] <- 2/12
x$experience[x$experience=="3 months"] <- 3/12
x$experience[x$experience=="5 months"] <- 5/12
x$experience[x$experience=="4 months"] <- 4/12
x$experience[x$experience=="9 months"] <- 9/12
x$experience[x$experience=="11 months"] <- 11/12
x$experience[x$experience=="16 years"] <- 16
x$experience <- as.numeric(x$experience)
table(x$experience)
summary(x$experience)
######workkids cleaning
x$workkids <- as.character(x$workkids)
unique(x$workkids)
x$workkids[x$workkids=="no "] <- "no"
x$workkids[x$workkids==""] <- NA
x$workkids<- factor(x$workkids)
table(x$workkids)
######volunteer cleaning
x$volunteer <- as.character(x$volunteer)
unique(x$volunteer)
x$volunteer[x$volunteer==""] <- NA
x$volunteer<- factor(x$volunteer)
table(x$volunteer)
###############
above40=x[which(x$age>40),]
below40=x[which(x$age<=40),]
h=x[which(x$hired=="yes"),]
nh=x[which(x$hired=="no"),]
h$age40=ifelse(h$age>40,"yes","no")
nh$age40=ifelse(nh$age>40,"yes","no")
x$hired40=ifelse(x$hired=="yes"&x$age>=40,"40+ & Hired",ifelse(
  x$hired=="yes"&x$age<=40,"40- & Hired",ifelse(
    x$hired=="no"&x$age>=40,"40+ & Rejected","40- & Rejected"
  )
)
)
x$int40=ifelse(x$interviewed=="yes"&x$age>=40,"40+ & Int",ifelse(
  x$interviewed=="yes"&x$age<=40,"40- & Int",ifelse(
    x$interviewed=="no"&x$age>=40,"40+ & NoInt","40- & NoInt"
  )
)
)
##Analysis
###workkids,teahching,volunteer are positive related to acceptance rate.

x
par(mfrow=c(1,2))
mosaicplot(table(x$hired40,x$workkids),xlab="Hire status by age",
           ylab="Work with kids or not",main = "Workkids & Hire & Age",col=c(2,3))
mosaicplot(table(x$int40,x$workkids),xlab="Interview status by age",
           ylab="Work with kids or not",main = "Workkids & Interview & Age",col=c(2,3))
mosaicplot(table(x$hired40,x$MA),xlab="Hire status by age",
           ylab="Masters status",main = "Masters & Hire & Age",col=c(2,3,4))
mosaicplot(table(x$int40,x$MA),xlab="Interview status by age",
           ylab="Masters status",main = "MA & Interview & Age",col=c(2,3,4))


###
par(mfrow=c(1,2))
barplot(x=x$hired,y=x$age,xlab="Hired",ylab="Age",
     col=c("navy","darkorange"),main="Hire Distribution By Age")
plot(x=x$interviewed,y=x$age,xlab="Interviewed",ylab="Age",
     col=c("navy","darkorange"),main="Interview Distribution By Age")

### whether GPA is an influence 
par(mfrow=c(1,3))
plot(x=x$hired,y=x$experience,xlab="Hired or not",ylab="Work experience")
plot(x=x$hired,y=x$GPA.u,xlab="Hired or not",ylab="Undergrad GPA",ylim=c(2,5))
plot(x=x$hired,y=x$GPA.g,xlab="Hired or not",ylab="Grad GPA",ylim=c(2,5))


### whether qualitative sub-attributes are influence
par(mfrow=c(1,5))
plot(x=x$hired,y=x$workkids,xlab="Hired or not",ylab="Former job with children or not")
plot(x=x$hired,y=x$volunteer,xlab="Hired or not",ylab="Volunteered in school or not")
plot(x=x$hired,y=x$teaching,xlab="Hired or not",ylab="Student teaching or not")
plot(x=x$hired,y=x$substitute,xlab="Hired or not",ylab="Been a substitute or not")
plot(x=x$hired,y=x$MA,xlab="Hired or not",ylab="Grade teaching or not")




