library(ggplot2)
data(iris)
x = iris$Petal.Length
petal_length_mean = mean(x)
petal_length_mean
histogram = ggplot(data=iris, aes(x))
histogram + geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Petal Length") +  ylab("Frequency") + ggtitle("Histogram of Petal Length")
