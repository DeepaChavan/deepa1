library(caTools)
data <- data.frame(
  + Years_Exp = c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7),
  + Salary = c(39343.00, 46205.00, 37731.00, 43525.00,
               + 39891.00, 56642.00, 60150.00, 54445.00, 64445.00, 57189.00)
  + )
data
plot(data$Years_Exp, data$Salary,
       + xlab = "Years Experienced",
       + ylab = "Salary",
       + main = "Scatter Plot of Years Experienced vs Salary")
linregress=lm(formula = Salary~Years_Exp,data = data)
linregress
summary(linregress)
abline(linregress,col="pink")
new_data <- data.frame(Years_Exp = c(4.0, 4.5, 5.0))
predict(linregress,newdata=new_data)

#linear regression :
a=head(mtcars)
reg=lm(wt~mpg,data=mtcars)
reg
plot(mtcars$mpg,mtcars$wt,xlab = "weight of cars",ylab = "Miles per gallon",main =
       "MtCars Scatter Plot")
abline(reg,col="blue")
summary(reg)

#multiple regression :
df <- data.frame(
  + Shipment = 1:20,
  + Distance = c(67, 59, 50, 58, 45, 63, 58, 60, 63, 62,
                 + 51, 48, 63, 64, 56, 49, 55, 57, 59, 59),
  + Nof_items = c(32, 12, 32, 31, 20, 19, 28, 27, 30, 25,
                  + 20, 27, 27, 10, 27, 23, 31, 24, 26, 12),
  + Cost= c(28, 30, 16, 24, 14, 37, 32, 36, 29, 30,
            + 17, 18, 45, 48, 41, 20, 26, 27, 34, 50)
  + )
df
reg=lm(Cost~Distance+Nof_items,data=df)
reg
summary(reg)
plot(reg)

new_data <- data.frame(Distance =50,Nof_items=32)
predict(reg,newdata=new_data)

new_data <- data.frame(Distance =c(50,58,64),Nof_items=c(32,31,10))
predict(reg,newdata=new_data)
par(mfrow=c(2,2))
plot(reg)

#on empty cars

data("mtcars")
a=head(mtcars)
a
reg=lm(mpg~wt+hp,data=mtcars)
reg
summary(reg)
par(mfrow=c(2,2))
plot(reg)
new_data <- data.frame(wt =2.620,hp=110)
predict(reg,newdata=new_data)
new_data <- data.frame(wt =c(2.875,3.215,3.460),hp=c(110,110,105))
predict(reg,newdata=new_data)
