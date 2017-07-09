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

ggplot(energydata, aes(Cooling.Load, Heating.Load)) + geom_point(aes(color = factor(Overall.Height)), alpha = 0.3) + 
  xlab('Cooling Load') + ylab('Heating Load') + 
  ggtitle('Cooling vs. Heating Load with Height')

ggplot(energydata, aes(Cooling.Load, Heating.Load)) + geom_point(aes(color = factor(Wall.Area)), alpha = 0.3) + 
  xlab('Cooling Load') + ylab('Heating Load') + 
  ggtitle('Cooling vs. Heating Load with Wall Area')

ggplot(energydata, aes(Cooling.Load, Heating.Load)) + geom_point(aes(color = factor(Roof.Area)), alpha = 0.3) + 
  xlab('Cooling Load') + ylab('Heating Load') + 
  ggtitle('Cooling vs. Heating Load with Roof Area')

ggplot(energydata, aes(Cooling.Load, Heating.Load)) + geom_point(aes(color = factor(Glazing.Area)), alpha = 0.3) + 
  xlab('Cooling Load') + ylab('Heating Load') + 
  facet_grid(Overall.Height~Roof.Area) +
  ggtitle('Cooling vs. Heating Load with Glazing Area by Roof Area and Overall Height')

ggplot(energydata, aes(Cooling.Load, Heating.Load)) + geom_point(aes(color = factor(Glazing.Area)), alpha = 0.3) + 
  xlab('Cooling Load') + ylab('Heating Load') + 
  facet_grid(Overall.Height~Wall.Area) +
  ggtitle('Cooling vs. Heating Load with Glazing Area by Wall Area and Overall Height')

ggplot(energydata, aes(Relative.Compactness, Heating.Load)) + geom_point(aes(color = factor(Roof.Area), size = Surface.Area,
                                                                             shape = factor(Overall.Height)), alpha = 0.3) + 
  xlab('Relative Compactness') + ylab('Heating Load') + 
  ggtitle('Relative Compactness vs. Heating Load by Roof area and Height')

ggplot(energydata, aes(Surface.Area, Heating.Load)) + geom_point(aes(color = factor(Roof.Area),
                                                                             shape = factor(Overall.Height)), alpha = 0.3) + 
  xlab('Surface Area') + ylab('Heating Load') + 
  ggtitle('Surface Area vs. Heating Load by Roof Area and Height')

ggplot(energydata, aes(Relative.Compactness, Cooling.Load)) + geom_point(aes(color = factor(Overall.Height))) + 
  xlab('Relative Compactness') + ylab('Heating Load') + 
  ggtitle('Rel Compt vs. Energy Eff (Cooling Load)')

ggplot(energydata, aes(Relative.Compactness, Cooling.Load)) + geom_point(aes(color = factor(Overall.Height))) + 
  xlab('Relative Compactness') + ylab('Heating Load') + 
  ggtitle('Rel Compt vs. Energy Eff (Cooling Load)')

ggplot(energydata, aes(Relative.Compactness, Surface.Area)) + geom_point(aes(color = factor(Roof.Area), #size = Heating.Load,
                                                                             shape = factor(Overall.Height)), alpha = 0.3) + 
  xlab('Relative Compactness') + ylab('Surface Area') + 
  ggtitle('How Relative Compactness relates \n to Surface Area with \n Height and Roof Area')

ggplot(energydata, aes(Relative.Compactness, Wall.Area)) + geom_point(aes(color = factor(Roof.Area), #size = Heating.Load,
                                                                             shape = factor(Overall.Height)), alpha = 0.3) + 
  xlab('Relative Compactness') + ylab('Wall Area') + 
  ggtitle('How Relative Compactness relates \n to Wall Area with \n Height and Roof Area')

ggplot(energydata, aes(Relative.Compactness, Heating.Load)) + geom_point(aes(color = factor(Roof.Area), size = Wall.Area,
                                                                             shape = factor(Overall.Height)), alpha = 0.3) + 
  xlab('Relative Compactness') + ylab('Heating Load') + 
  ggtitle('Rel Compt vs. Energy Eff (Heating Load)')

ggplot(energydata, aes(Relative.Compactness, Heating.Load)) + geom_point(aes(color = factor(Roof.Area), size = Surface.Area,
                                                                             shape = factor(Overall.Height)), alpha = 0.3) + 
  facet_grid(.~Wall.Area) +
  xlab('Relative Compactness') + ylab('Heating Load') + 
  ggtitle('Relative Compactness vs. Heating Load by Wall Area')