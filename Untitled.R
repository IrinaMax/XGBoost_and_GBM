## This is Exercises for ggplot section
library(ggplot2)
library(ggthemes)
head(mpg)
##Histogram of hwy mpg values:
ex.pl <- ggplot(mpg, aes(x=hwy))
ex.pl1 <- ex.pl + geom_histogram(binwidth = 1.5, color = "red", fill = "red", alpha = 0.2)
print(ex.pl1)

str(mpg)
df <- mpg
#Barplot of car counts per manufacturer with color fill defined by cyl count
ex.pl2 <- ggplot(df, aes(x=manufacturer))
print(ex.pl2 + geom_bar(aes(x=manufacturer, fill=factor(cyl))))
##Create a scatterplot of volume versus sales. Afterwards play around with alpha and color arguments to clarify information.
head(txhousing)

ex.pl3 <- ggplot(txhousing, aes(x=sales, y=volume))
plot(ex.pl3 + geom_point(aes(alpha =0.5, fill =sales, size = sales)))
plot(ex.pl3 + geom_point(aes(color = sales, alpha = 0.5)))
plot(ex.pl3 + geom_point(aes(color= date)))

#Add a smooth fit line to the scatterplot from above. Hint: You may need to look up geom_smooth()
plot(ex.pl3 +geom_point(aes(color = sales, alpha = 0.5))+geom_smooth(color="red"))

