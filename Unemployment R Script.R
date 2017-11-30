#Data on the Rocks
#Posted January 31, 2017
#Unemployment

#REQUIRED PACKAGES
library(xlsx) #package to import xls files directly
library(pastecs) #summary stats function stat.desc
library(ggplot2)
library(grid) #for multiplot

#CUSTOM FUNCTIONS
Sp.Desc <- function(data)
{
  print(t(stat.desc(data))[,-c(2,3,6,7,11,14)])
}

DoR.Theme <- function(axis.text.size=16, axis.title.size=16, title.size=20, legend.position="none")
{
  theme(panel.grid.major = element_line(colour="grey90"), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x=element_line(colour="black"), axis.line.y=element_line(colour="black"),
        axis.title.x=element_text(colour = "black", size=axis.title.size), axis.title.y=element_text(colour = "black", size=axis.title.size),
        axis.text.x=element_text(colour = "black", size=axis.text.size), axis.text.y=element_text(colour = "black", size=axis.text.size),
        plot.title=element_text(colour = "black",size=title.size, face="bold", hjust=.5),
        axis.ticks=element_line(colour="black"), legend.position = legend.position, legend.key=element_blank())
}

#multiplot to grid plot all subject outputs
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#DATA IMPORT----
Unemp <- read.xlsx("C:/Users/sbuja/Documents/Data on the Rocks/Unemployment/Unemployment data.xlsx", sheetName="Sheet1")
Sp.Desc(Unemp)
Trump.Rates <- data.frame(Year=rep(2017,5), TR=c(20, 24, 30, 32, 42))
Sp.Desc(Trump.Rates)

write.csv(Trump.Rates, file="Trump.Rates.csv", row.names=F)

#GRAPHING DATA----
colorscale <- scales::seq_gradient_pal("slateblue1", "navyblue", "Lab")(seq(0,1,length.out=2))
Unemp.plot <- ggplot(data=Unemp, aes(x=Year, y=U3)) + 
  annotate("rect", xmin=2007.917, xmax=2009.5, ymin=0, ymax=50, alpha=.1) +
  annotate("text", x=2003.5, y=43, label="2008 Recession", size=6, fontface="bold", hjust=0, colour="grey50")+
  geom_line(size=2, colour=colorscale[1]) + 
  annotate("text", x=2012.5, y=3.5, label="Unemployment Rate", size=6, fontface="bold", hjust=.5, colour=colorscale[1])+
  geom_line(aes(x=Year, y=U6), size=2, colour=colorscale[2]) + 
  annotate("text", x=2001.3, y=22, label="Unemployment + Underemployment Rate", size=6, fontface="bold", hjust=0, colour=colorscale[2])+
  geom_boxplot(data=Trump.Rates, aes(x=Year, y=TR), fill="#F8876E", colour="#B01D03", size=1.2) +
  annotate("text", x=2016.2, y=35, label="Trump's\nClaimed\nRange", size=6, fontface="bold", hjust=1, vjust=0.5, colour="#B01D03")+
  scale_x_continuous(limits=c(2001,2018), breaks=seq(2001,2017,2), expand = c(0,0)) +
  scale_y_continuous("Unemployment Rate", limits=c(0,50), breaks=seq(5,50,5), expand = c(0,0)) +
  ggtitle("Unemployment in America") + DoR.Theme()
Unemp.plot

ggsave(Unemp.plot, filename="Unemp.plot.png", width = 8, height=7, dpi=500)


#Follow-up
#March 12 2017
#Find the Phony

Unemp.Phony <- read.xlsx("C:/Users/sbuja/Documents/Data on the Rocks/Unemployment/Unemployment data wPhony.xlsx", sheetName="Sheet1")
Sp.Desc(Unemp.Phony)

Unemp.Phony.A.plot <- ggplot(data=Unemp.Phony, aes(x=Year, y=U3)) + 
  geom_line(size=2, colour="navyblue") + 
  scale_x_continuous(limits=c(2001,2018), breaks=seq(2001,2017,4), expand = c(0,0)) +
  scale_y_continuous("Unemployment Rate", limits=c(0,35), breaks=seq(5,50,5), expand = c(0,0)) +
  ggtitle("Option A") + DoR.Theme()
Unemp.Phony.A.plot

Unemp.Phony.B.plot <- ggplot(data=Unemp.Phony, aes(x=Year, y=Tr.U3)) + 
  geom_line(size=2, colour="#B01D03") + 
  scale_x_continuous(limits=c(2001,2018), breaks=seq(2001,2017,4), expand = c(0,0)) +
  scale_y_continuous("Unemployment Rate", limits=c(0,35), breaks=seq(5,50,5), expand = c(0,0)) +
  ggtitle("Option B") + DoR.Theme()
Unemp.Phony.B.plot

multiplot(Unemp.Phony.A.plot, Unemp.Phony.B.plot, cols=2)

outfile="Unemp.Phony.Plot.png"
png(outfile, width=12, height=6.5, units="in", res=500)
multiplot(Unemp.Phony.A.plot, Unemp.Phony.B.plot, cols=2)
dev.off()

