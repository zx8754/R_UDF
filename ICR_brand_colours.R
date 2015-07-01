library(ggplot2)

ICRcolour <- read.table(text="
Pallete	Colour	Hex	Pantone	CMYK	RGB
Primary	White	#FFFFFF	NA	NA	NA
Primary	Black	#000000	NA	NA	NA
Primary	Green	#C9DD03	NA	NA	NA
Primary	Yellow	#FFD602	NA	NA	NA
Primary	Orange	#F9A100	NA	NA	NA
Primary	Pink	#EE7EA6	NA	NA	NA
Primary	Red	#A71930	NA	NA	NA
Primary	Grey	#616365	NA	NA	NA
Secondary	Olive	#726E20	NA	NA	NA
Secondary	Damson	#6E273D	NA	NA	NA
Secondary	BrightRed	#F00034	NA	NA	NA
Secondary	LightGrey	#ADAFAF	NA	NA	NA
Secondary	Blue	#003D4C	NA	NA	NA
",comment.char="",header=TRUE,stringsAsFactors=FALSE)

ggplot(ICRcolour,
       aes(ymin=1:nrow(ICRcolour), ymax=0.9+(1:nrow(ICRcolour)),
           xmin=0,xmax=1,fill=Hex,col="grey")) +
  geom_rect() +
  scale_color_identity() +
  scale_fill_identity() +
  scale_y_reverse(breaks=0.4+(1:nrow(ICRcolour)),
                  labels=paste(ICRcolour$Colour,ICRcolour$Hex)) +
  ggtitle("ICR Colour Palette") +
  theme_classic() +
  theme(
    axis.title=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    axis.line=element_blank(),
    axis.text=element_text(family="mono"))

