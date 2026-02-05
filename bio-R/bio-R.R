cat("here is the directory:", getwd())
vectors
matrix
dataframe
array
list
dictionary
install.packages("graphics")
num_vector <- c(1, 2, 3, 5, 7, 4:10)
length(num_vector)
char_vector

myobject <- c(5:10,100)
length(myobject)

char_vector <- c("a", "b", "c", "home", "red")
length(char_vector)
char_vector[2]
char_vector[4]
char_vector[1:3]
char_vector[c(1,3,5)]

library(tidyr)
?tidy  #logical | or/and & operators
mydate<- data.frame(
  Name = c("John", "Alice", "Bob"),
  Age = c(28, 34, 23),
  Height = c(5.9, 5.5, 6.0)
)
mydate

#logical operator search, print Alice age
my <- mydate$Age[mydate$Name== "Alice"]

my

mydate$Name
mydate$Age
mydate[2,3]
mydate[,2]
View(mydate)

plot(mydate$Age, mydate$Height, main="Age vs Height", xlab="Age", ylab="Height", pch=19)

?rnorm #for simulation

matrix1 <- matrix(1:12, nrow=3, ncol=4)
matrix1

rnorm(99, mean=0, sd=1) #generate 10 random numbers from a normal distribution with mean 0 and standard deviation 1
seed(123) #set seed for reproducibility
rnorm(99, mean=0, sd=1)
set.seed(123) #set seed for reproducibility
pippo <- rnorm(99, mean=0, sd=1)

matrix1 <- matrix(data = pippo, nrow=33, ncol=3)
matrix1
View(matrix1)
install.packages("beepr")
library(beepr)
beep(sound = 8)#play a sound when the code finishes running




myarr<- array(pippo, dim=c(11,3,3)) #create a 3D array with dimensions 11x3x3 using the data in pipp
myarr[1,,3] #access the first element of the first row and second column of the array
list1 <- list(name="John", age=30, height=5.9) #create a list with three elements: name, age, and height
list1$name #access the name element of the list


obj1 <- c(1:5)
obj2 <- c("black", "red", "orange", "yellow", "green")

my_list <- list(obj1=obj1, obj2=obj2) #create a list with two elements: obj1 and obj2
my_list$obj1 #access the obj1 element of the list
my_list$obj2 #access the obj2 element of the list


?lm
my_dict <- list(key1="value1", key2="value2", key3="value3") #create a dictionary with three key-value pairs
my_dict$key1 #access the value associated with key1 in the dictionary
my_dict$key2 #access the value associated with key2 in the dictionary
my_dict$key3 #access the value associated with key3 in the dictionary
