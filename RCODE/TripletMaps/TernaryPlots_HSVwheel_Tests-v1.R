
library(colorspace)

# hue <- seq(0,1,length.out = 100000)
# chroma <- seq(0,1,length.out = 100000)
# luminance <- seq(0,1,length.out = 100000)

h <- runif(100000)
s <- runif(100000)
v <- runif(100000)

# h <- runif(10000,0,360)
# s <- runif(10000,0,100)
# v <- runif(10000,0,100)

#cols <- sequential_hcl(100, h = c(100,360), c = c(0,100), l = c(0,100), power = 1)
cols <- hsv(h = h, s = s, v = 0.8)

#cols <- hcl(h,c=s,l=0.8)

#hclplot(cols)

TernaryPlot()
TernaryPoints(data.frame(h,s,v), col=cols, pch=19, cex=0.1)

df <- data.frame(h=h,s=s,v=0.8)*100

ggplot(data=df,aes(h,s,v)) +
  #coord_tern() +
  geom_point(color=cols) + 
  labs(x="X",y="Y",z="Z",title="Title")


# Create hsv grid
d = expand.grid(h=seq(0,1,0.025), s=seq(0,1,0.025), v=1)

p1 = ggplot() +
  coord_polar(theta="x") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  scale_fill_identity() +
  geom_rect(data=d, mapping=aes(xmin=h, xmax=h+resolution(h), 
                                ymin=s, ymax=s+resolution(s), 
                                fill=hsv(h,s,v)))
dev.new()
plot(p1)
