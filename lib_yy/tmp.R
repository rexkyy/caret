#load required library
library(ggplot2)
#load data from sugar_content.csv (remember to save the Excel file as a csv!)
drink_samples <- read.csv(file= "C:/Users/wonlab/Downloads/sugar_content.csv")
#create plot
p <- ggplot(data=drink_samples, aes(x=drink_samples$sugar_content, y=c(0)))
p <- p + geom_point() + geom_text(label=drink_samples$sugar_content,
                                  size=2.5, vjust=2, hjust=0.5)
#p created in previous code block!
#dataframe to hold separators
d_bounds <- data.frame(sep=c(9.1,9.7))
#add layer containing decision boundaries to previous plot
p <- p + geom_point(data=d_bounds, aes(x=d_bounds$sep, y=c(0)), colour= "red", size=3)
#add labels for candidate decision boundaries
p <- p + geom_text(data=d_bounds, aes(x=d_bounds$sep, y=c(0)),
                   label=d_bounds$sep, size=2.5,
                   vjust=2, hjust=0.5, colour="red")

ifelse(drink_samples$sugar_content < 9.1, "Choke-R","Choke")
#dataframe to hold max margin separator
mm_sep <- data.frame(sep=c((8.8+10)/2))
#add layer containing max margin separator to previous plot
p <- p + geom_point(data=mm_sep,aes(x=mm_sep$sep, y=c(0)), colour= "blue", size=4)
#number of datapoints
n <- 200
#Generate dataframe with 2 uniformly distributed predictors x1 and x2 in (0,1)
df <- data.frame(x1=runif(n),x2=runif(n))

#load ggplot
library(ggplot2)
#build scatter plot
p <- ggplot(data=df, aes(x=x1,y=x2)) + geom_point()
# if x1>x2 then -1, else +1
df$y <- factor(ifelse(df$x1-df$x2>0,-1,1),levels=c(-1,1))

