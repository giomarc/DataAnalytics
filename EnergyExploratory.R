# read csv
energydata <- read.csv(file = "EnergyEfficiencyData.csv", header = TRUE)
head(energydata)
facCol <- c("Overall.Height", "Orientation", "Glazing.Area.Distribution")
energydata[, facCol] <- lapply(energydata[,facCol], as.factor)
str(energydata)
summary(energydata)

# histogram all values are they really numerics? 
# find number of unique values in each column
lapply(lapply(energydata, unique), length)

# visual illustration
require(ggplot2)
histFun <- function(x)
{
  cnames <- names(x)
  for(i in seq_along(cnames)){
    print(ggplot(x, aes_string(x = cnames[i])) + geom_histogram())
  }
}

#remove factors
energydataSub <- energydata[, -which(names(energydata) %in% facCol)]
# create histograms 
histFun(energydataSub)

# histogram of glazing area is this really a numeric?

# correlation scatterplot on numerics? potentially group on factors with color and shape...
require(car)
scatterplotMatrix(x = energydata, var.labels = colnames(energydata), diagonal = c("histogram"))

require(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
R <- cor(x = energydataSub, method = c("pearson"))
print(R)

# correlation plot code based on article: 
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

p.mat <- cor.mtest(energydataSub)
head(p.mat)

corrplot(R, method="color", col=col(200),  
         type="lower", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

ggplot(energydata, aes(Relative.Compactness, Heating.Load)) + geom_point(aes(color = factor(Overall.Height))) + 
  xlab('Relative Compactness') + ylab('Heating Load') + 
  ggtitle('Rel Compt vs. Energy Eff (Heating Load)')