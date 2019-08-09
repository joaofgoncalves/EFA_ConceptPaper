

library(ggplot2)

# Create HSV color wheel
#
d = expand.grid(h=seq(0,0.95,length.out = 12), s=seq(0,1,length.out = 5), v=0.9)

p1 = ggplot() +
  coord_polar(theta="x") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  scale_fill_identity() +
  #geom_point(data = d, mapping = aes(x=h, y=s)) + 
  geom_rect(data=d, mapping=aes(xmin=h, xmax=h+resolution(h), 
                                ymin=s, ymax=s+resolution(s), 
                                fill=hsv(h,s,v))) + 
  theme_void()

plot(p1)

ggsave(plot=p1, filename = "./OUT/HSV_ColorWheel-v1.png")


# Create HCL color wheel
#
d = expand.grid(h=seq(0,360,length.out = 25), c=seq(0,100,length.out = 5), l=50)

p2 = ggplot() +
  coord_polar(theta="x") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  scale_fill_identity() +
  #geom_point(data = d, mapping = aes(x=h, y=s)) + 
  geom_rect(data=d, mapping=aes(xmin=h, xmax=h+resolution(h), 
                                ymin=c, ymax=c+resolution(c), 
                                fill=hcl(h,c,l))) + 
  theme_void()

plot(p2)
