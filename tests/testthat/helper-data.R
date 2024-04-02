i_diamonds = interfacer::iface(
  carat = numeric ~ "the carat column",
  cut = enum(`Fair`,`Good`,`Very Good`,`Premium`,`Ideal`, .ordered=TRUE) ~ "the cut column",
  color = enum(`D`,`E`,`F`,`G`,`H`,`I`,`J`, .ordered=TRUE) ~ "the color column",
  clarity = enum(`I1`,`SI2`,`SI1`,`VS2`,`VS1`,`VVS2`,`VVS1`,`IF`, .ordered=TRUE) ~ "the clarity column",
  depth = numeric ~ "the depth column",
  table = numeric ~ "the table column",
  price = integer ~ "the price column",
  x = numeric ~ "the x column",
  y = numeric ~ "the y column",
  z = numeric ~ "the z column",
  .groups = FALSE
)

i_diamonds_data = interfacer::iface(
  carat = numeric ~ "the carat column",
  depth = numeric ~ "the depth column",
  table = numeric ~ "the table column",
  price = integer ~ "the price column",
  x = numeric ~ "the x column",
  y = numeric ~ "the y column",
  z = numeric ~ "the z column",
  .groups = FALSE
)

i_diamonds_cat = interfacer::iface(
  cut = enum(`Fair`,`Good`,`Very Good`,`Premium`,`Ideal`, .ordered=TRUE) ~ "the cut column",
  color = enum(`D`,`E`,`F`,`G`,`H`,`I`,`J`, .ordered=TRUE) ~ "the color column",
  clarity = enum(`I1`,`SI2`,`SI1`,`VS2`,`VS1`,`VVS2`,`VVS1`,`IF`, .ordered=TRUE) ~ "the clarity column",
  data = list(i_diamonds_data) ~ "A nested data column must be specified as a list",
  .groups = FALSE
)

nested_diamonds = ggplot2::diamonds %>%
  tidyr::nest(data = c(-cut,-color,-clarity))

i_iris = interfacer::iface(
  Sepal.Length = numeric ~ "the Sepal.Length column",
  Sepal.Width = numeric ~ "the Sepal.Width column",
  Petal.Length = numeric ~ "the Petal.Length column",
  Petal.Width = numeric ~ "the Petal.Width column",
  Species = enum(`setosa`,`versicolor`,`virginica`) ~ "the Species column",
  .groups = NULL,
  .default = iris
)

