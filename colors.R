# my corporate colors
my_colors <- c(
  `liberal`        = "darkred",
  `conservative`      = "darkblue",
  `ndp`       = "orange",
  `green`     = "darkgreen",
  `laurier1`     = "darkorchid",
  `laurier2` = "gold")

#' Function to extract my colors as hex codes
#'
#' @param ... Character names of my_colors 
#'
my_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (my_colors)
  
  my_colors[cols]
}

my_cols()
#>        red      green       blue     orange     yellow light grey 
#>  "#d11141"  "#00b159"  "#00aedb"  "#f37735"  "#ffc425"  "#cccccc" 
#>  dark grey 
#>  "#8c8c8c"

my_cols("red")
#>       red 
#> "#d11141"

my_cols("red", "blue")
#>       red      blue 
#> "#d11141" "#00aedb"

my_cols("blue", "red")
#>      blue       red 
#> "#00aedb" "#d11141"


ggplot(mtcars, aes(hp, mpg)) +
  geom_point(color = my_cols("laurier1"),
             size = 4, alpha = .8)
my_palettes <- list(
  `federal`  = my_cols("liberal", "conservative", "ndp", "green"),
  
  `ontario`  = my_cols("conservative", "liberal", "ndp", "green"),
  
  `wlu`   = my_cols("laurier1", "laurier2")
)


my_pal("wlu")(6)

# Color by discrete variable using default palette
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_color_mine(palette="ontario")
# Fill by discrete variable with different palette + remove legend (guide)
ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_mine(palette = "wlu", guide = "none")
