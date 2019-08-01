
library(colorspace)



s9 <- sequential_hcl(12, "Purples 3")
demoplot(s9, "heatmap")
hclplot(s9)
specplot(s9, type = "o")


library(sf)
library(tricolore)
library(ggplot2)
library(ggtern)

# color-code the data set and generate a color-key
tric_educ <- Tricolore(euro_example,
                       p1 = 'ed_0to2', p2 = 'ed_3to4', p3 = 'ed_5to8')

# add the vector of colors to the `euro_example` data
euro_example$educ_rgb <- tric_educ$rgb

plot_educ <-
  # using data sf data `euro_example`...
  ggplot(euro_example) +
  # ...draw a choropleth map
  geom_sf(aes(fill = educ_rgb, geometry = geometry), size = 0.1) +
  # ...and color each region according to the color-code
  # in the variable `educ_rgb`
  scale_fill_identity()

plot_educ

library(ggtern)
plot_educ +
  annotation_custom(
    ggplotGrob(tric_educ$key),
    xmin = 55e5, xmax = 75e5, ymin = 8e5, ymax = 80e5
  )


library(ggplot2)
library(ggtern)

df = data.frame(x = runif(50),
                y = runif(50),
                z = runif(50),
                Value = runif(50,1,10),
                Group = as.factor(round(runif(50,1,2))))


ggplot(data=df,aes(x,y,z,color=Group)) +
  coord_tern() +
  theme_rgbw() + 
  geom_point() + geom_path() + 
  labs(x="X",y="Y",z="Z",title="Title")



