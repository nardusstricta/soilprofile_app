library(mapedit)
library(leaflet)
library(mapview)
border <- df_global3
new_borders <- mapview(border) %>%
  editMap("border")

p <- ggplot(mtcars, aes(x = hp, y = wt)) + geom_point() + geom_smooth()
p2 <- ggedit(p)
names(p2) # will show you which objects are available.
plot(p2) # shows the updated plot (it is available in the first element of p2)
