#' @include graphicsR_utils.R

#' Apply func
#'
#' @name gr_graphics
#' @rdname gr_graphics
#' @keywords internal
#'
#' @examples
#'
#' gr_densfn_animate(
#' fn = "norm",
#' args = list(mean = 2:2, sd = 0:1),
#' limits = c(.1, .9)
#' )
#'
#' gr_densfn_animate(
#'   fn = "cauchy",
#'   args = list(location = 0:4, scale = 1:4),
#'   limits = c(.15, .85)
#' )
#'
#' library(sf)
#' london_gpkg
#'
#' parties <- names(london_gpkg)[4:8]
#' # set up a colour scale for these if so inclined
#' colours <- c("deepskyblue", "red", "gold", "purple", "green")
#' names(colours) <- parties
#'
#' # calculate the dot positions for each column
#' london_dots <- calc_dots(
#'   sf_data = london_gpkg,
#'   col_names = parties,
#'   n_per_dot = 1000
#' )
#'
#' # plot the results
#' library(ggplot2)
#' london_plot <- ggplot() +
#'   # first add the shape as a background
#'   geom_sf(data = london_gpkg, fill = "transparent", colour = "white") +
#'   # add the dots
#'   geom_point(data = london_dots, aes(lon, lat, colour = variable), size = 0.5) +
#'   # colour based on the scale already defined
#'   scale_colour_manual(name = "Party", values = colours) +
#'   # title
#'   ggtitle("Dot Density Map of London in the 2017 General Election",
#'           subtitle = paste("one dot equals", 1000, "people")
#'   ) +
#'   theme_dotdensity(palette = "flat_dark") +
#'   # make the legend shapes bigger so it's possible to see them clearly
#'   guides(colour = guide_legend(override.aes = list(size = 10)))
#'
#' # plot
#' london_plot
#'
#'
#' # Basic scatter plots
#' data(cars)
#' p <- cars %>% ggplot() + geom_point(aes(speed, dist))
#' p
#'
#' # Set log scale
#' p %>% grf_yscale("log2", .format = TRUE)
#'
#' # Add legend title to a specific aesthetic
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() +
#'   gr_legend_add_title(col = "Number of Cylinders")
#'
#' # Add legend title to all aesthetics
#' ggplot(mtcars, aes(wt, mpg, colour = cyl)) +
#'   geom_point() +
#'   gr_legend_add_title("Number of Cylinders")
#'
#' # Remove all legends
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() +
#'   gr_legend_acction(remove_legend = TRUE)
#'
#' # remove just size legend
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() +
#'   gr_legend_acction("size",remove_legend = TRUE)
#'
#' # can also use:
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() +
#'   gr_legend_acction(size,remove_legend = TRUE)
#'
#' # Remove more than one
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() +
#'   gr_legend_acction(size, color,remove_legend = TRUE)
#'
#'
#' # remove legend title from all aesthetics
#' ggplot(mtcars, aes(wt, mpg, colour = cyl)) +
#'   geom_point() +
#'   gr_legend_acction(remove_tittle = TRUE)
#'
#'
#'
#' # Move legends to bottom
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() +
#'   gr_legend_acction(position = "bottom")
#'
#' # Make legends horizontal
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() +
#'   gr_legend_acction(direction = "horizontal")
#'
#' # Justify legends to the bottom and justify to the right
#' ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
#'   geom_point() +
#'   gr_legend_acction(position = "bottom",justification = "right")
#'
#'
#'
#' library(sf)
#'
#' gr_map_hex(
#'   data_sf = mgalda::map_hex$data_sf,
#'   polygon = mgalda::map_hex$polygon,
#'   cellsize = 5000,
#'   fill = enero,
#'   labs_objs = ggplot2::labs(
#'     title = "Temperatura Superficial Mes de Enero",
#'     subtitle = "MOD11A1 PROMEDIO 2010-2020.",
#'     fill = "Regiones Valparaiso y Metropolitana, Chile"
#'   )
#' )
#'
#' library(sf)
#'
#' data <- mgalda::map_pois$pois
#'
#' gr_map_pois(
#'   pois = data,
#'   col = tipo,
#'   labs_objs = ggplot2::labs(
#'     title = toupper("Rutas del desierto de Atacama"),
#'     subtitle = toupper("Region de Antofagasta, Chile")
#'   )
#' )
#'
#' library(sf)
#'
#' mgalda::map_grid$points
#'
#' gr_map_grid(
#'   polygons = mgalda::map_grid$polygon,
#'   points = st_as_sf(
#'     x = mgalda::map_grid$points,
#'     coords = c("X", "Y"),
#'     crs = st_crs(mgalda::map_grid$polygon)
#'   ),
#'   labs_objs = ggplot2::labs(
#'     fill = expression(Paradas ~ por ~ km^{
#'       2
#'     }),
#'     title = toupper("Paradas de transporte publico"),
#'     subtitle = toupper("Santiago de Chile")
#'   ),
#'   box_size = 1000
#' )
#'
#' library(sf)
#'
#' gr_map_fromto(
#'   data = mgalda::map_fromto$data,
#'   polygon = mgalda::map_fromto$polygon,
#'   to = ComunaBajada,
#'   from = ComunaSubida,
#'   by = Comuna,
#'   size = ViajeLaboralPromedio,
#'   labs_objs = ggplot2::labs(
#'     x = "",
#'     y = "",
#'     title = "Origen y Destino 7:00 AM",
#'     subtitle = "Santiago de Chile"
#'   )
#' )
#'
#' data("mtcars")
#'
#' p1 <- ggplot2::qplot(mpg, wt, data = mtcars, colour = cyl)
#' p2 <- ggplot2::qplot(mpg, data = mtcars) + ggplot2::ggtitle("title")
#' p3 <- ggplot2::qplot(mpg, data = mtcars, geom = "dotplot")
#'
#'
#' gr_grid(p1, p2, p3)
#'
#' rm_ls(sssss)
#'
#' ## scale break
#'
#' d <- data.frame(
#'   x = 1:20,
#'   y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22)
#' )
#'
#' p1 <- ggplot(d, aes(y, x)) +
#'   geom_col(orientation = "y")
#' d2 <-
#'   data.frame(
#'     x = c(2, 18),
#'     y = c(7, 26),
#'     label = c("hello", "world")
#'   )
#' p2 <- p1 + scale_x_break(c(7, 17)) +
#'   geom_text(
#'     aes(y, x, label = label),
#'     data = d2,
#'     hjust = 1,
#'     colour = "firebrick"
#'   ) +
#'   xlab(NULL) + ylab(NULL) + theme_minimal()
#'
#' p1
#' p2
#'
#' # #fig.keep='last'#
#' g <- ggplot(d, aes(x, y)) +
#'   geom_col()
#' g2 <- g + scale_y_break(c(7, 17), scales = 1.5) +
#'   scale_y_break(c(18, 21), scale = 2) + scale_y_reverse()
#' g + g2
#'
#'
#' g + coord_flip() + scale_y_break(c(7, 18))
#'
#' rm_ls(sssss)
#' set.seed(2019 - 01 - 19)
#' d <- data.frame(
#'   x = 1:20,
#'   y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22),
#'   group = c(rep("A", 10), rep("B", 10)),
#'   face = c(rep("C", 5), rep("D", 5), rep("E", 5), rep("F", 5))
#' )
#'
#' p <- ggplot(d, aes(x = x, y = y)) +
#'   geom_col(orientation = "x") +
#'   scale_y_reverse() +
#'   facet_wrap(group ~ .,
#'              scales = "free_y",
#'              strip.position = "right",
#'              nrow = 2
#'   ) +
#'   coord_flip()
#' pg <- p +
#'   scale_y_break(c(7, 17), scales = "free") +
#'   scale_y_break(c(19, 21), scales = "free")
#' print(pg)
#'
#'
#' pg <- pg + aes(fill = group) + theme(legend.position = "bottom")
#' print(pg)
#'
#'
#' pg + labs(
#'   title = "test title",
#'   subtitle = "test subtitle",
#'   tag = "A tag",
#'   caption = "A caption"
#' ) +
#'   theme_bw() +
#'   theme(
#'     legend.position = "bottom",
#'     strip.placement = "outside",
#'     axis.title.x = element_text(size = 10),
#'     plot.title = element_text(size = 22),
#'     plot.subtitle = element_text(size = 16),
#'     plot.tag = element_text(size = 10),
#'     plot.title.position = "plot",
#'     plot.tag.position = "topright",
#'     plot.caption = element_text(face = "bold.italic"),
#'   )
#'
#' # #message=FALSE, fig.width=10, fig.height=6#
#' set.seed(2019 - 01 - 19)
#' d <- data.frame(
#'   x = 1:20,
#'   y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22),
#'   group = c(rep("A", 10), rep("B", 10))
#' )
#'
#' p <- ggplot(d, aes(x = x, y = y)) +
#'   scale_y_reverse() +
#'   scale_x_reverse() +
#'   geom_col(aes(fill = group)) +
#'   scale_fill_manual(values = c("#00AED7", "#009E73")) +
#'   facet_wrap(group ~ .,
#'              scales = "free_y",
#'              strip.position = "right",
#'              nrow = 2
#'   ) +
#'   coord_flip()
#'
#' p +
#'   scale_y_break(c(7, 10),
#'                 scales = 0.5,
#'                 ticklabels = c(10, 11.5, 13)
#'   ) +
#'   scale_y_break(c(13, 17), scales = 0.5, ticklabels = c(17, 18, 19)) +
#'   scale_y_break(c(19, 21), scales = 1, ticklabels = c(21, 22, 23))
#'
#' # #fig.width=7, fig.height=5.5#
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   scale_y_continuous("mpg (US)",
#'                      sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#'   ) +
#'   theme(
#'     axis.title.y.left = element_text(color = "deepskyblue"),
#'     axis.title.y.right = element_text(color = "orange")
#'   )
#' p1 <- p + scale_y_break(breaks = c(20, 30))
#' p1
#'
#' set.seed(2019 - 01 - 19)
#' d <- data.frame(
#'   x = 1:20,
#'   y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22)
#' )
#' p <- ggplot(d, aes(x, y)) +
#'   geom_col()
#' x <- p + scale_y_break(c(7, 17))
#'
#' x + p
#'
#' # #
#' set.seed(2019 - 01 - 19)
#' d <- data.frame(
#'   x = 1:20,
#'   y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22)
#' )
#' p <- ggplot(d, aes(x, y)) +
#'   geom_col()
#' p + scale_y_cut(
#'   breaks = c(7, 18),
#'   which = c(1, 3),
#'   scales = c(3, 0.5)
#' )
#'
#' # #
#' p + scale_y_cut(
#'   breaks = c(7, 18),
#'   which = c(1, 3),
#'   scales = c(3, 0.5),
#'   space = .5
#' )
#'
#'
#' rm_ls(sssss)
#' ## geom_tree
#'
#' set.seed(2016 - 10 - 31)
#' tr <- ape::rtree(50)
#' tr$tip.label <- paste(tr$tip.label, tr$tip.label, sep = "_")
#'
#' p <- ggplot(tr) + geom_tree()
#' p
#' d <- data.frame(id = tr$tip.label, v = rnorm(50),w = rcauchy(50))
#' d
#'
#' p + tree_layout(layout = "circular")
#'
#'
#' rm_ls(sssss)
#' ## gr_grid
#'
#'
#' no_legend <- theme(legend.position = "none")
#'
#' d <-
#'   dplyr::group_by(mtcars, cyl) %>%
#'   dplyr::summarize(mean = mean(disp), sd = sd(disp))
#'
#'
#' d2 <- dplyr::filter(mtcars, cyl != 8) %>% dplyr::rename(var = cyl)
#'
#' p1 <- ggplot(d, aes(x = cyl, y = mean)) +
#'   geom_col(aes(fill = factor(cyl)), width = 1) +
#'   no_legend
#'
#' p1
#' p2 <- ggplot(d2, aes(var, disp)) +
#'   geom_jitter(aes(color = factor(var)), width = .5) +
#'   no_legend
#'
#' p2
#' p3 <- ggplot(dplyr::filter(d, cyl != 4), aes(mean, cyl)) +
#'   geom_point(aes(fill = factor(cyl)), width = .6) +
#'   coord_flip() +
#'   no_legend
#'
#' pp <- list(p1, p2, p3)
#'
#' gr_grid(gglist = pp, ncol = 1)
#'
#' pp2 <- lapply(pp, function(p) {
#'   p + xlim2(limits = c(3, 11))
#' })
#' pp3 <- lapply(pp, function(p) {
#'   p + xlim2(p1)
#' })
#'
#' gr_grid(gglist = pp2, ncol = 1)
#' gr_grid(gglist = pp3, ncol = 1)
#'
#' rm_ls(sssss)
#'
#' ## gr_scatterpie
#'
#' set.seed(123)
#' long <- rnorm(50, sd = 100)
#' lat <- rnorm(50, sd = 50)
#' d <- data.frame(long = long, lat = lat)
#' d <- with(d, d[abs(long) < 150 & abs(lat) < 70, ])
#' n <- nrow(d)
#' d$region <- factor(1:n)
#' d$A <- abs(rnorm(n, sd = 1))
#' d$B <- abs(rnorm(n, sd = 2))
#' d$C <- abs(rnorm(n, sd = 3))
#' d$D <- abs(rnorm(n, sd = 4))
#' d[1, 4:7] <- d[1, 4:7] * 3
#' head(d)
#'
#' # ---- fig.width=10
#' ggplot() +
#'   gr_scatterpie(aes(x = long, y = lat, group = region),
#'                 data = d,
#'                 cols = LETTERS[1:4]
#'   ) +
#'   coord_equal()
#'
#' # ---- fig.width=10
#' d$radius <- 6 * abs(rnorm(n))
#' p <-
#'   ggplot() +
#'   gr_scatterpie(
#'     aes(
#'       x = long,
#'       y = lat,
#'       group = region,
#'       r = radius
#'     ),
#'     data = d,
#'     cols = LETTERS[1:4],
#'     color = NA
#'   ) +
#'   coord_equal()
#'
#' p + gr_scatterpie_legend(d$radius, x = -140, y = -70)
#'
#' # ---- fig.width=10
#' world <- map_data("world")
#' p <- ggplot(world, aes(long, lat)) +
#'   geom_map(
#'     map = world,
#'     aes(map_id = region),
#'     fill = NA,
#'     color = "black"
#'   ) +
#'   coord_quickmap()
#' p + gr_scatterpie(
#'   aes(
#'     x = long,
#'     y = lat,
#'     group = region,
#'     r = radius
#'   ),
#'   data = d,
#'   cols = LETTERS[1:4],
#'   color = NA,
#'   alpha = .8
#' ) +
#'   gr_scatterpie_legend(d$radius, x = -160, y = -55)
#'
#' p + gr_scatterpie(
#'   aes(
#'     x = long,
#'     y = lat,
#'     group = region,
#'     r = radius
#'   ),
#'   data = d,
#'   cols = LETTERS[1:4],
#'   color = NA,
#'   alpha = .8
#' ) +
#'   gr_scatterpie_legend(
#'     d$radius,
#'     x = -160,
#'     y = -55,
#'     n = 3,
#'     labeller = function(x) {
#'       1000 * x^2
#'     }
#'   )
#'
#' d <- data.frame(x = rnorm(5), y = rnorm(5))
#' d$A <- abs(rnorm(5, sd = 1))
#' d$B <- abs(rnorm(5, sd = 2))
#' d$C <- abs(rnorm(5, sd = 3))
#' ggplot() +
#'   gr_scatterpie(aes(x = x, y = y),
#'                 data = d, cols = c("A", "B", "C")) +
#'   coord_fixed()
#'
#' d <- tidyr::gather(d, key = "letters", value = "value", -x:-y)
#' ggplot() +
#'   gr_scatterpie(aes(x = x, y = y),
#'                 data = d,
#'                 cols = "letters",
#'                 long_format = TRUE
#'   ) +
#'   coord_fixed()
#'
#'
#' tr <- ape::rtree(10)
#' ##  add root point
#' ggtree(tr) + geom_rootpoint()
#'
#' ggtree(tr) + geom_rootpoint(
#'   size = 2,
#'   color = "red",
#'   shape = 2
#' )
#'
#' ## add point by aes(subset)
#' tr <- ape::rtree(10)
#' # group tip and node
#' ggtree(tr) + geom_point2(aes(shape = isTip, color = isTip), size = 3)
#' # specify a node to display
#' ggtree(tr) + geom_point2(aes(subset = (node == 15)),
#'                          shape = 21,
#'                          size = 5,
#'                          fill = "green"
#' )
#' # specify a tip to display
#' ggtree(tr) + geom_point2(aes(subset = (label %in% c("t1", "t3"))),
#'                          shape = 21,
#'                          size = 5,
#'                          fill = "green"
#' )
#'
#'
#' ## color point with continuous variables
#'
#'
#' beast_file <-
#'   system.file("examples/MCC_FluA_H3.tree", package = "ggtree")
#'
#' beast_tree <- treeio::read.beast(beast_file)
#' p <- ggtree(beast_tree) +
#'   geom_tiplab(hjust = -.1) +
#'   geom_nodepoint(aes(fill = rate), shape = 21, size = 4) +
#'   scale_fill_continuous(low = "blue", high = "red") +
#'   theme_tree2() + theme(legend.position = "right")
#' p
#'
#' tr <- ape::rtree(10)
#' ##  add root point
#' ggtree(tr) + geom_rootpoint()
#' ggtree(tr) + geom_rootpoint(
#'   size = 2,
#'   color = "red",
#'   shape = 2
#' )
#'
#' tr <- ape::rtree(10)
#' ggtree(tr) + geom_tiplab()
#'
#' set.seed(2015 - 12 - 21)
#' tree <- ape::rtree(5)
#' tree$tip.label[2] <- "long string for test"
#' label_pad(tree$tip.label)
#'
#'
#'
#' data(mtcars)
#' mtcars_z <- dplyr::transmute(
#'   .data = mtcars,
#'   model = row.names(mtcars),
#'   hpz = scale(hp)
#' )
#'
#'
#' gr_bardiverging(mtcars_z, model, hpz, text_size = 5)
#'
#' ## Change the colors
#' gr_bardiverging(
#'   mtcars_z,
#'   model,
#'   hpz,colors = c("darkgreen", "darkred"),
#'   text_size = 5
#' )
#'
#' ## Decrease the axis label font size
#' gr_bardiverging(mtcars_z, model, hpz, text_size = 5)
#'
#' ## Display the axis label text in the same color as the bars
#' gr_bardiverging(mtcars_z, model, hpz,text_color = c("#1F77B4", "#FF7F0E"))
#'
#'
#' popeurope <- datalearn$popeurope
#'
#' gr_dumbbell(popeurope, country, pop1952, pop2007,point_colors =  "Viridis")
#'
#' # Display only the top 10 countries in terms of population in 2007
#' gr_dumbbell(popeurope, country, pop1952, pop2007, top_n = 10)
#'
#' # Change line and point color
#' gr_dumbbell(popeurope, country, pop1952, pop2007,
#'             top_n = 10,
#'             line_color = "lightgray", point_color = c("lightgray", "black")
#' )
#' # Add custom legend labels
#' gr_dumbbell(popeurope, country, pop1952, pop2007,
#'             top_n = 10,
#'             legend_labels = c("1952", "2007")
#' )
#'
#' # Increase line width and point size
#' gr_dumbbell(popeurope, country, pop1952, pop2007,
#'             top_n = 10,
#'             line_size = 2, point_size = 5
#' )
#'
#' d1<- truncate_distr(pdqr_distr(f = "dnorm",mean = 35,sd = 35),upper = 80,lower = 40)
#' d2 <- pdqr_distr(f = "dunif",min = 0,max = 40)
#'
#' popch <-
#'   dplyr::tibble(
#'     age = c(random_dist(d1, 500), random_dist(d2, 1000)),
#'     sex = sample(c("H", "M"), 1500, T)
#'   ) %>%
#'   dplyr::mutate(
#'     sex = as.factor(sex),
#'     age = cut(
#'       x = age,
#'       breaks = seq(0, 80, 10),
#'       right = T,
#'       include.lowest = T
#'     )
#'   ) %>%
#'   dplyr::group_by(age, sex) %>%
#'   dplyr::summarise(pop = dplyr::n())
#'
#' gr_pyramid(popch, age, pop, sex)
#'
#' ## Change bar colors
#' gr_pyramid(popch, age, pop, sex, bar_colors = c("darkgreen", "darkorange"))
#'
#' ## Change x axis label and add title
#' gr_pyramid(popch, age, pop, sex, xlab = "Population", title = "Switzerland 2020")
#'
#' ggplot(iris, aes(x = Petal.Length, group = Species, alpha = Species)) +
#'   geom_histogram() +
#'   scale_alpha_focus(Species == "versicolor",alpha_focus = 1,alpha_other = .5)
#'
#' ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, size = Species)) +
#'   geom_point() +
#'   scale_size_focus(Species == "versicolor")
#'
#' ggplot(iris, aes(x = Species, fill = Species)) +
#'   geom_bar() +
#'   scale_fill_focus(Species=="versicolor",Petal.Length > 3.8,color_focus = "red")
#'
#' ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, color = Species)) +
#'   geom_point()+
#'   scale_color_focus(Species=="versicolor",Petal.Length > 3.8)
#'
#'
#' mtcars2 <-mgalda::group_summarise(mtcars,.groups = c(cyl),median_wt = median(wt),response = "wt")
#' data(penguins, package = "palmerpenguins")
#' penguins2 <-
#'   palmerpenguins::penguins %>%
#'   dplyr::filter(!is.na(sex), !is.na(bill_depth_mm), !is.na(bill_length_mm))
#'
#'
#' grf_point(bill_length_mm ~ bill_depth_mm, data = penguins2) %>%
#'   grf_abline(
#'     intercept = 10,
#'     slope = 2,
#'     color = "red"
#'   )
#'
#' grf_plot(data = penguins2) %>%
#'   grf_point(bill_length_mm ~ bill_depth_mm) %>%
#'   grf_abline(
#'     intercept = 10:12,
#'     slope = 2,
#'     color = c("red", "green", "blue")
#'   )
#' grf_point(wt ~ hp,
#'           size = ~wt,
#'           color = ~cyl,
#'           data = mtcars
#' ) %>% grf_abline(
#'   slope = ~0,
#'   intercept = ~median_wt,
#'   color = ~cyl,
#'   data = mtcars2
#' )
#' grf_point(wt ~ hp,
#'           size = ~wt,
#'           color = ~cyl,
#'           data = mtcars
#' ) %>% grf_abline(
#'   slope = ~0,
#'   intercept = ~3,
#'   color = "green"
#' )
#' grf_point(wt ~ hp,
#'           size = ~wt,
#'           color = ~cyl,
#'           data = mtcars
#' ) %>% grf_hline(
#'   yintercept = ~median_wt,
#'   color = ~cyl,
#'   data = mtcars2
#' )
#' grf_point(mpg ~ hp,
#'           color = ~cyl,
#'           size = ~wt,
#'           data = mtcars
#' ) %>% grf_abline(
#'   color = "red",
#'   slope = ~ -0.1,
#'   intercept = ~35
#' )
#' grf_point(mpg ~ hp,
#'           color = ~cyl,
#'           size = ~wt,
#'           data = mtcars
#' ) %>% grf_abline(
#'   color = "red",
#'   slope = ~slope,
#'   intercept = ~intercept,
#'   data = data.frame(slope = -0.1, intercept = 33:35)
#' )
#' Temps <-
#'   mosaicData::Weather %>% dplyr::filter(city == "Chicago", year == 2016, month <= 4)
#' grf_ribbon(
#'   low_temp + high_temp ~ date,
#'   data = Temps,
#'   color = "navy",
#'   alpha = 0.3
#' )
#' grf_area(high_temp ~ date,
#'          data = Temps,
#'          color = "navy",
#'          alpha = 0.3
#' )
#' grf_ribbon(low_temp + high_temp ~ date,
#'            data = mosaicData::Weather,
#'            alpha = 0.3
#' ) + facet_grid(city ~ .)
#' grf_bar(~species, data = penguins2)
#' D <- data.frame(group = LETTERS[1:3], count = c(20, 25, 18))
#' grf_col(count ~ group, data = D)
#' SomeData <- data.frame(group = LETTERS[1:3], count = c(20, 25, 18))
#' grf_bar(substance ~ ., data = mosaicData::HELPrct)
#' grf_col(group ~ count, data = SomeData)
#' grf_bin2d(eruptions ~ waiting, data = faithful, bins = 15)
#' grf_point(eruptions ~ waiting, data = faithful) %>% grf_contour(density ~ waiting + eruptions, data = faithfuld)
#' grf_point(eruptions ~ waiting, data = faithful) %>% grf_density2d(eruptions ~ waiting, data = faithful)
#' grf_point(eruptions ~ waiting, data = faithful) %>% grf_density_2d(eruptions ~ waiting, data = faithful)
#' grf_boxplot(bill_length_mm ~ species,
#'             color = ~species,
#'             data = penguins2
#' )
#' grf_boxplot(
#'   age ~ substance,
#'   data = mosaicData::HELPrct,
#'   color = ~sex,
#'   position = position_dodge(width = 0.9)
#' )
#' grf_boxplot(
#'   age ~ substance |
#'     sex,
#'   data = mosaicData::HELPrct,
#'   coef = 5,
#'   width = 0.4
#' ) %>% grf_jitter(
#'   width = 0.2,
#'   alpha = 0.3,
#'   seed = 123
#' )
#' mdl <- lm(bill_length_mm ~ bill_depth_mm, data = penguins2)
#' grf_point(bill_length_mm ~ bill_depth_mm, data = penguins2) %>% grf_coefline(coef = coef(mdl))
#' grf_point(bill_length_mm ~ bill_depth_mm, data = penguins2) %>% grf_coefline(model = mdl)
#' grf_density_2d(eruptions ~ waiting,
#'                data = faithful,
#'                alpha = 0.5,
#'                color = "navy"
#' ) %>% grf_contour(
#'   density ~ waiting + eruptions,
#'   data = faithfuld,
#'   bins = 10,
#'   color = "red"
#' )
#' grf_jitter(
#'   avg_drinks ~ age,
#'   data = mosaicData::HELPrct,
#'   seed = 123,
#'   color = ~sex,
#'   alpha = 0.2,
#'   width = 0.4,
#'   height = 0.4
#' ) %>% grf_density_2d()
#' grf_jitter(
#'   avg_drinks ~ age,
#'   data = mosaicData::HELPrct,
#'   seed = 123,
#'   color = ~sex,
#'   alpha = 0.2,
#'   width = 0.4,
#'   height = 0.4
#' ) %>% grf_density2d()
#' grf_counts(
#'   substance ~ .,
#'   data = mosaicData::HELPrct,
#'   fill = ~sex,
#'   position = "dodge"
#' )
#' grf_percents(
#'   substance ~ .,
#'   data = mosaicData::HELPrct,
#'   fill = ~sex,
#'   position = "dodge"
#' )
#' grf_props(
#'   substance ~ .,
#'   data = mosaicData::HELPrct,
#'   fill = ~sex,
#'   position = "dodge"
#' )
#' grf_counts(
#'   ~substance,
#'   data = mosaicData::HELPrct,
#'   fill = ~sex,
#'   position = position_dodge()
#' )
#' grf_props(
#'   ~substance,
#'   data = mosaicData::HELPrct,
#'   fill = ~sex,
#'   position = position_dodge()
#' )
#' grf_percents(
#'   ~substance,
#'   data = mosaicData::HELPrct,
#'   fill = ~sex,
#'   position = position_dodge()
#' )
#' HELP2 <-
#'   mosaicData::HELPrct %>%
#'   dplyr::group_by(substance, sex) %>%
#'   dplyr::summarise(
#'     mean.age = mean(age),
#'     median.age = median(age),
#'     max.age = max(age),
#'     min.age = min(age),
#'     sd.age = sd(age),
#'     lo = mean.age - sd.age,
#'     hi = mean.age + sd.age
#'   )
#' grf_jitter(
#'   age ~ substance,
#'   data = mosaicData::HELPrct,
#'   seed = 123,
#'   alpha = 0.5,
#'   width = 0.2,
#'   height = 0,
#'   color = "skyblue"
#' ) %>%
#'   grf_pointrange(mean.age + lo + hi ~ substance, data = HELP2) %>%
#'   grf_facet_grid(~
#'                    sex)
#' grf_jitter(
#'   age ~ substance,
#'   data = mosaicData::HELPrct,
#'   seed = 123,
#'   alpha = 0.5,
#'   width = 0.2,
#'   height = 0,
#'   color = "skyblue"
#' ) %>%
#'   grf_errorbar(lo + hi ~ substance, data = HELP2, inherit = FALSE) %>%
#'   grf_facet_grid(~
#'                    sex)
#' grf_jitter(
#'   age ~ substance,
#'   data = mosaicData::HELPrct,
#'   seed = 123,
#'   alpha = 0.5,
#'   width = 0.2,
#'   height = 0,
#'   color = "skyblue"
#' ) %>%
#'   grf_boxplot(age ~ substance, data = mosaicData::HELPrct, color = "red") %>%
#'   grf_crossbar(mean.age + lo + hi ~ substance, data = HELP2) %>%
#'   grf_facet_grid(~
#'                    sex)
#' HELP2 <-
#'   mosaicData::HELPrct %>%
#'   dplyr::group_by(substance, sex) %>%
#'   dplyr::summarise(
#'     mean.age = mean(age),
#'     median.age = median(age),
#'     max.age = max(age),
#'     min.age = min(age),
#'     sd.age = sd(age),
#'     lo = mean.age - sd.age,
#'     hi = mean.age + sd.age
#'   )
#' grf_jitter(
#'   substance ~ age,
#'   data = mosaicData::HELPrct,
#'   seed = 123,
#'   alpha = 0.5,
#'   height = 0.2,
#'   width = 0,
#'   color = "skyblue"
#' ) %>%
#'   grf_pointrange(substance ~ mean.age + lo + hi, data = HELP2) %>%
#'   grf_facet_grid(sex ~ .)
#' grf_jitter(
#'   substance ~ age,
#'   data = mosaicData::HELPrct,
#'   seed = 123,
#'   alpha = 0.5,
#'   height = 0.2,
#'   width = 0,
#'   color = "skyblue"
#' ) %>%
#'   grf_pointrange(substance ~ mean.age + lo + hi, data = HELP2) %>%
#'   grf_facet_grid(sex ~ .)
#' grf_jitter(
#'   substance ~ age,
#'   data = mosaicData::HELPrct,
#'   seed = 123,
#'   alpha = 0.5,
#'   height = 0.2,
#'   width = 0,
#'   color = "skyblue"
#' ) %>%
#'   grf_errorbar(substance ~ lo + hi, data = HELP2, inherit = FALSE) %>%
#'   grf_facet_grid(sex ~ .)
#' grf_jitter(
#'   substance ~ age,
#'   data = mosaicData::HELPrct,
#'   seed = 123,
#'   alpha = 0.5,
#'   height = 0.2,
#'   width = 0,
#'   color = "skyblue"
#' ) %>%
#'   grf_errorbar(substance ~ lo + hi, data = HELP2, inherit = FALSE) %>%
#'   grf_facet_grid(sex ~ .)
#' grf_jitter(
#'   substance ~ age,
#'   data = mosaicData::HELPrct,
#'   seed = 123,
#'   alpha = 0.5,
#'   height = 0.2,
#'   width = 0,
#'   color = "skyblue"
#' ) %>%
#'   grf_crossbar(substance ~ mean.age + lo + hi, data = HELP2) %>%
#'   grf_facet_grid(sex ~ .)
#' SomeData <- data.frame(
#'   x1 = 2.62,
#'   x2 = 3.57,
#'   y1 = 21,
#'   y2 = 15
#' )
#' grf_point(mpg ~ wt, data = mtcars) %>%
#'   grf_curve(y1 + y2 ~ x1 + x2, data = SomeData, color = "navy") %>%
#'   grf_segment(y1 + y2 ~ x1 + x2, data = SomeData, color = "red")
#' grf_dens(~bill_length_mm, data = penguins2)
#' grf_dens(~bill_length_mm, data = penguins2, color = ~species)
#' grf_density(
#'   ~bill_length_mm,
#'   fill = ~species,
#'   data = penguins2,
#'   alpha = 0.5
#' )
#' grf_density(
#'   ~ bill_length_mm |
#'     species ~ .,
#'   fill = ~species,
#'   data = penguins2,
#'   alpha = 0.5
#' )
#'
#'
#' grf_dist("norm", mean = 10, sd = 2)
#' grf_dist("norm", fill = ~ (abs(x) <= 2), geom = "area")
#' grf_dist("norm", fill = "red", kind = "histogram")
#' grf_dist("norm",
#'          color = "red",
#'          kind = "qqstep",
#'          resolution = 25
#' ) %>% grf_dist(
#'   "norm",
#'   color = "black",
#'   kind = "qq",
#'   resolution = 25,
#'   size = 2,
#'   alpha = 0.5
#' )
#' grf_dist("binom",
#'          size = 20,
#'          prob = 0.25,
#'          plot_size = 2
#' )
#' grf_dist("binom",
#'          params = list(size = 20, prob = 0.25),
#'          size = 2
#' )
#' grf_dotplot(
#'   ~bill_length_mm,
#'   fill = ~species,
#'   data = penguins2,
#'   binwidth = 0.2
#' ) %>% grf_labs(title = "dotdensity")
#' grf_dotplot(
#'   ~bill_length_mm,
#'   fill = ~species,
#'   data = penguins2,
#'   binwidth = 0.2,
#'   method = "histodot"
#' ) %>% grf_labs(title = "histodot")
#' grf_empty() %>% grf_labs(title = "empty")
#' grf_empty() %>%
#'   grf_point(bill_length_mm ~ bill_depth_mm,
#'             data = penguins2,
#'             color = ~species
#'   ) %>%
#'   grf_labs(title = "empty + point")
#' grf_frame(c(0, 10) ~ c(0, 5)) %>% grf_labs(title = "frame")
#' grf_point((c(0, 1)) ~ (c(0, 5))) %>% grf_blank((c(0, 3)) ~ (c(-2, 7)))
#' set.seed(12345)
#' Dat <-
#'   data.frame(g = rgamma(500, 3, 10), f = rf(500, df1 = 3, df2 = 47))
#' grf_freqpoly(~bill_length_mm, color = ~species, data = penguins2)
#' grf_freqpoly(
#'   ~bill_length_mm,
#'   color = ~species,
#'   data = penguins2,
#'   binwidth = 0.5
#' )
#' grf_fun(x~sin(x), color = ~"sin", xlim = pi * c(-2, 2)) %>%
#'   grf_fun(x~cos(x), color = ~
#'             "cosine", xlim = pi * c(-2, 2))
#'
#' grf_point(length ~ width, data = mosaicData::KidsFeet) %>%
#'   grf_fun(x~4 * cos(5 * x) + 24,
#'           color = ~"cosine",
#'           xlim = pi * c(-2, 2)
#'   ) %>%
#'   grf_labs(color = "")
#'
#' grf_fun(x~sin(x), color = ~"sin", xlim = pi * c(-2, 2)) %>% grf_fun(x ~ cos(x), color = ~
#'                                                                       "cosine", xlim = pi * c(-2, 2))
#'
#' grf_hex(avg_drinks ~ age, data = mosaicData::HELPrct, bins = 15) %>% grf_density2d(
#'   avg_drinks ~ age,
#'   data = mosaicData::HELPrct,
#'   color = "yellow",
#'   alpha = 0.5
#' )
#' grf_histogram(~bill_length_mm, data = penguins2)
#' grf_histogram(
#'   ~ bill_length_mm |
#'     species ~ .,
#'   fill = ~species,
#'   data = penguins2,
#'   alpha = 0.5,
#'   binwidth = 0.25
#' )
#' grf_line(births ~ date, data = mosaicData::Births78)
#' grf_line(births ~ date, color = ~wday, data = mosaicData::Births78)
#' grf_label(width ~ length,
#'           data = mosaicData::KidsFeet,
#'           label = ~name
#' )
#' grf_label(
#'   width ~ length,
#'   data = mosaicData::KidsFeet,
#'   label = ~name,
#'   nudge_x = 0.1,
#'   nudge_y = -0.05,
#'   color = ~sex
#' ) %>% grf_point()
#' grf_text(width ~ length,
#'          data = mosaicData::KidsFeet,
#'          label = ~name
#' )
#' grf_text(
#'   width ~ length,
#'   data = mosaicData::KidsFeet,
#'   label = ~name,
#'   color = ~sex,
#'   nudge_x = 0.1,
#'   nudge_y = -0.05
#' ) %>% grf_point()
#' grf_linerange(low_temp + high_temp ~ date,
#'               data = mosaicData::Weather,
#'               color = ~avg_temp
#' ) %>%
#'   grf_facet_grid(city ~ year, scale = "free") %>%
#'   grf_refine(scale_color_viridis_c(
#'     option = "C",
#'     begin = 0.1,
#'     end = 0.8
#'   ))
#' grf_pointrange(
#'   avg_temp + low_temp + high_temp ~ date,
#'   data = mosaicData::Weather %>% head(200),
#'   color = ~avg_temp
#' ) %>%
#'   grf_facet_grid(city ~ year, scale = "free") %>%
#'   grf_refine(scale_color_viridis_c(
#'     option = "C",
#'     begin = 0.1,
#'     end = 0.8
#'   ))
#' grf_point(bill_length_mm ~ bill_depth_mm, data = penguins2)
#' grf_point(bill_length_mm ~ bill_depth_mm |
#'             species,
#'           color = ~species,
#'           data = penguins2
#' )
#' grf_qq(~ age |substance, data = mosaicData::HELPrct) %>% grf_qqline()
#'
#' grf_qqstep(~ age |
#'              substance, data = mosaicData::HELPrct) %>% grf_qqline()
#' grf_point((1 / hwy) ~ displ, data = mpg) %>%
#'   grf_quantile((1 / hwy) ~ displ, quantiles = 0.5, color = "red") %>%
#'   grf_quantile((1 /
#'                   hwy) ~ displ, quantiles = c(0.2, 0.8))
#' grf_raster(density ~ eruptions + waiting, data = faithfuld)
#' grf_tile(density ~ eruptions + waiting, data = faithfuld) %>%
#'   grf_contour(density ~ eruptions + waiting, color = "yellow") %>%
#'   grf_refine(scale_fill_viridis_c(begin = 0.2))
#' grf_density2d(eruptions ~ waiting, data = faithful)
#' grf_rect(1.5 + 3 ~ 40 + 68, fill = "red", alpha = 0.2) %>%
#'   grf_rect(3 + 5.5 ~ 68 + 100, fill = "green", alpha = 0.2) %>%
#'   grf_point(eruptions ~ waiting, data = faithful)
#'
#' set.seed(1234)
#' SomeData <- expand.grid(x = 1:10, y = 1:10)
#' SomeData$angle <- runif(100, 0, 2 * pi)
#' SomeData$speed <- runif(100, 0, sqrt(0.1 * SomeData$x))
#' grf_point(y ~ x, data = SomeData) %>% grf_spoke(y ~ x, angle = ~angle, radius = 0.5)
#' grf_point(y ~ x, data = SomeData) %>% grf_spoke(y ~ x, angle = ~angle, radius = ~
#'                                                   speed)
#' set.seed(1234)
#' grf_point(bill_length_mm ~ bill_depth_mm, data = penguins2) %>%
#'   grf_rugx(~
#'              bill_depth_mm, data = penguins2, color = "red") %>%
#'   grf_rugy(bill_length_mm ~ ., data = penguins2, color = "green")
#' grf_jitter(bill_length_mm ~ bill_depth_mm,
#'            data = penguins2,
#'            seed = 123
#' ) %>%
#'   grf_rugx(
#'     ~bill_depth_mm,
#'     data = penguins2,
#'     color = "red",
#'     position = "jitter",
#'     seed = 123
#'   ) %>%
#'   grf_rugy(
#'     bill_length_mm ~ .,
#'     data = penguins2,
#'     color = "green",
#'     position = "jitter",
#'     seed = 123
#'   )
#' grf_dhistogram(~bill_length_mm, data = penguins2) %>% grf_rugx(
#'   position = "jitter",
#'   alpha = 0.4,
#'   color = "red",
#'   seed = 123
#' )
#' grf_point(bill_length_mm ~ bill_depth_mm, data = penguins2) %>%
#'   grf_rug(. ~ bill_depth_mm,
#'           data = penguins2,
#'           color = "red",
#'           inherit = FALSE
#'   ) %>%
#'   grf_rug(bill_length_mm ~ .,
#'           data = penguins2,
#'           color = "green",
#'           inherit = FALSE
#'   )
#' grf_point(bill_length_mm ~ bill_depth_mm, data = penguins2) %>%
#'   grf_rug(. ~ bill_depth_mm,
#'           data = penguins2,
#'           color = "red",
#'           sides = "b"
#'   ) %>%
#'   grf_rug(bill_length_mm ~ .,
#'           data = penguins2,
#'           color = "green",
#'           sides = "l"
#'   )
#' grf_jitter(bill_length_mm ~ bill_depth_mm, data = penguins2) %>% grf_rug(
#'   color = "green",
#'   sides = "b",
#'   position = "jitter",
#'   seed = 123
#' )
#' grf_histogram(~eruptions, data = faithful) %>%
#'   grf_rug(~eruptions, data = faithful, color = "red") %>%
#'   grf_rug(~
#'             eruptions,
#'           data = faithful,
#'           color = "navy",
#'           sides = "t"
#'   )
#' grf_histogram(~eruptions, data = faithful) %>%
#'   grf_rug(color = "red") %>%
#'   grf_rug(color = "navy", sides = "t")
#'
#' grf_dhistogram(~eruptions, data = faithful) %>% grf_rug(~eruptions,
#'                                                         data = faithful,
#'                                                         color = "red",
#'                                                         inherit = FALSE
#' )
#' grf_dhistogram(~bill_depth_mm, data = penguins2) %>% grf_rug(
#'   0 ~ bill_depth_mm,
#'   data = penguins2,
#'   color = "green",
#'   sides = "b",
#'   position = "jitter",
#'   seed = 123
#' )
#' grf_dhistogram(~bill_depth_mm, data = penguins2) %>% grf_rug(
#'   0.5 ~ bill_depth_mm,
#'   data = penguins2,
#'   color = "green",
#'   sides = "b",
#'   position = "jitter",
#'   seed = 123
#' )
#' grf_histogram(~bill_length_mm, data = penguins2) %>%
#'   grf_refine(scale_x_continuous(breaks = discrete_breaks()))
#'
#' grf_histogram(~bill_length_mm, data = penguins2) %>%
#'   grf_refine(scale_x_continuous(breaks = discrete_breaks(2)))
#'
#'
#' #rm_ls()
#'
NULL

#' @export
ggplot2::autoplot

#' @export
gr_scatterpie <-
  function(mapping = NULL,
           data,
           cols,
           pie_scale = 1,
           sorted_by_radius = FALSE,
           legend_name = "type",
           long_format = FALSE,
           ...) {
    if (is.null(mapping)) {
      mapping <- aes_(x = ~x, y = ~y)
    }
    mapping <- modifyList(
      mapping,
      aes_(
        r0 = 0,
        fill = as.formula(paste0("~", legend_name)),
        amount = ~value
      )
    )

    if (!"r" %in% names(mapping)) {
      xvar <- get_aes_var(mapping, "x")
      size <- diff(range(data[, xvar])) / 50 * pie_scale
      data$r <- size
      mapping <- modifyList(mapping, aes_(r = size))
    }

    names(mapping)[match(c("x", "y"), names(mapping))] <-
      c("x0", "y0")
    if (long_format == TRUE) {
      df <- data
      names(df)[which(names(df) == cols)] <- legend_name
      cols2 <- enquo(cols)
    } else {
      data <- data[rowSums(data[, cols]) > 0, ]
      ## df <- gather_(data, "type", "value", cols)
      cols2 <- enquo(cols)
      df <- tidyr::gather(data, "type", "value", !!cols2)
      df$type <-
        factor(df$type, levels = cols) # set legend order based on order of "cols"
      names(df)[which(names(df) == "type")] <- legend_name
    }
    ## df <- gather_(data, "type", "value", cols)
    # cols2 <- enquo(cols)
    # df <- gather(data, "type", "value", !!cols2)
    # names(df)[which(names(df) == "type")] = legend_name

    ## df$type <- factor(df$type, levels=cols)
    if (!sorted_by_radius) {
      return(ggforce::geom_arc_bar(
        mapping,
        data = df,
        stat = "pie",
        inherit.aes = FALSE,
        ...
      ))
    }

    lapply(split(df, df$r)[as.character(sort(unique(df$r), decreasing = TRUE))], function(d) {
      ggforce::geom_arc_bar(
        mapping,
        data = d,
        stat = "pie",
        inherit.aes = FALSE,
        ...
      )
    })
  }

#' @export
gr_scatterpie_legend <- function(radius, x, y, n = 5, labeller) {
  if (length(radius) > n) {
    radius <-
      unique(sapply(seq(
        min(radius), max(radius),
        length.out = n
      ), round_digit))
  }

  label <- FALSE
  if (!missing(labeller)) {
    if (!inherits(labeller, "function")) {
      cat_stop("labeller should be a function for converting radius")
    }
    label <- TRUE
  }

  dd <-
    data.frame(
      r = radius,
      start = 0,
      end = 2 * pi,
      x = x,
      y = y + radius - max(radius),
      maxr = max(radius)
    )

  if (label) {
    dd$label <- labeller(dd$r)
  } else {
    dd$label <- dd$r
  }

  list(
    ggforce::geom_arc_bar(
      aes_(
        x0 = ~x,
        y0 = ~y,
        r0 = ~r,
        r = ~r,
        start = ~start,
        end = ~end
      ),
      data = dd,
      inherit.aes = FALSE
    ),
    geom_segment(
      aes_(
        x = ~x,
        xend = ~ x + maxr * 1.5,
        y = ~ y + r,
        yend = ~ y + r
      ),
      data = dd,
      inherit.aes = FALSE
    ),
    geom_text(
      aes_(
        x = ~ x + maxr * 1.6,
        y = ~ y + r,
        label = ~label
      ),
      data = dd,
      hjust = "left",
      inherit.aes = FALSE
    )
  )
}

#' @export
gr_dumbbell <-
  function(data,
           x,
           y1,
           y2,
           line_size = 1.5,
           line_color = "lightgray",
           point_size = 4,
           point_colors = c("#1F77B4", "#FF7F0E"),
           sort = TRUE,
           horizontal = TRUE,
           top_n = NULL,
           legend = TRUE,
           legend_labels = waiver(),
           limit = NULL,
           .theme = NULL) {
    x <- rlang::enquo(x)
    y1 <- rlang::enquo(y1)
    y2 <- rlang::enquo(y2)

    if (is_gr_palette_name(point_colors)) {
      scale_fn <-
        scale_pal_gr(palette = point_colors,
                     scale = "discrete",
                     aesthetics = "color")
      scale_fn <- extract_pal_gr(scale_fn)
      point_colors <- scale_fn(2)
    }
    data <-
      pre_dumbbell(data,
                   !!x,
                   !!y2,
                   sort = sort,
                   top_n = top_n,
                   limit = limit)

    plot <- ggplot(data, aes(x = !!x)) +
      geom_segment(
        mapping = aes(
          xend = !!x,
          y = !!y1,
          yend = !!y2
        ),
        color = line_color,
        size = line_size
      ) +
      geom_point(aes(y = !!y1, color = rlang::as_name(y1)), size = point_size) +
      geom_point(aes(y = !!y2, color = rlang::as_name(y2)), size = point_size) +
      scale_color_manual(values = point_colors, labels = legend_labels) +
      labs(x = NULL) +
      if (is_empty(.theme)) {
        gr_current_theme(grid = if (horizontal) {
          "Y"
        } else {
          "X"
        })
      } else {
        .theme
      }


    if (legend) {
      plot <- plot +
        theme(legend.position = "top") +
        guides(color = guide_legend(title = NULL))
    } else {
      plot <- plot + theme(legend.position = "none")
    }

    if (horizontal) {
      plot <- plot + coord_flip()
    }

    plot
  }

#' @export
gr_bardiverging <- function(data,
                            x,
                            y,
                            colors = c("#1F77B4", "#FF7F0E"),
                            line_size = 0.75,
                            point_size = 3,
                            text_color = "auto",
                            text_size = "auto",
                            .theme = NULL) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  if (length(text_color) == 1 && text_color == "auto") {
    text_color <- gr_current_theme()$text$colour
  }
  if (length(text_size) == 1 && text_size == "auto") {
    text_size <- gr_current_theme()$text$size
  }



  data <- dplyr::mutate(
    .data = data,
    !!x := reorder(!!x, !!y),
    .color = ifelse(!!y >= 0, colors[1], colors[2])
  )

  limit <- max(dplyr::pull(data, !!y)) * 1.05
  if (length(text_color) == 1) {
    text_color <- rep(text_color, 2)
  }

  ggplot(data, aes(!!x, !!y, fill = .data$.color)) +
    geom_col() +
    scale_fill_identity() +
    coord_flip() +
    geom_text(
      data = dplyr::filter(data, !!y >= 0),
      color = text_color[1],
      size = text_size,
      aes(
        label = !!x,
        y = 0,
        hjust = "right"
      ),
      nudge_y = -limit * .01
    ) +
    geom_text(
      data = dplyr::filter(data, !!y < 0),
      color = text_color[2],
      size = text_size,
      aes(
        label = !!x,
        y = 0,
        hjust = "left"
      ),
      nudge_y = limit * .013
    ) +
    geom_hline(
      yintercept = 0,
      color = gr_current_theme()$text$colour,
      size = .4
    ) +
    labs(x = NULL) +
    guides(y = guide_none())+
    ylim(-limit, limit) +
    if (is_empty(.theme)) {
      gr_current_theme(grid = "Y")
    } else {
      .theme
    }
}

#' @export
gr_pyramid <-
  function(data,
           x,
           y,
           group,
           bar_colors = c("#1F77B4", "#FF7F0E"),
           sort = "no",
           xlab = NULL,
           title = NULL) {
    sort <- match.arg(sort, c("no", "descending", "ascending"))
    x <- rlang::enquo(x)
    y <- rlang::enquo(y)
    group <- rlang::enquo(group)

    groups <- data %>%
      dplyr::pull(!!group) %>%
      unique()
    if (length(groups) != 2) {
      err_msg <- paste0(
        "There must be 2 unique values in `group`, not ",
        length(groups),
        "."
      )
      cat_stop(err_msg)
    }
    names(bar_colors) <- groups

    if (sort != "no") {
      order <- data %>%
        split(data[[rlang::as_name(group)]]) %>%
        lapply(function(x) {
          x[[rlang::as_name(y)]]
        }) %>%
        (function(list) {
          list[[1]] + list[[2]]
        })

      if (sort == "ascending") {
        order <- -order
      }
    } else {
      order <- seq_len(nrow(data))
    }


    limit <- data %>%
      dplyr::pull(!!y) %>%
      abs() %>%
      max()
    sides <- c("left", "right")
    plots <- vector("list", 2L)
    for (i in 1:2) {
      if (i == 1L) {
        y_scale <- scale_y_reverse(
          limits = c(limit, 0),
          expand = expansion(mult = c(.05, 0))
        )
      } else {
        y_scale <- scale_y_continuous(
          limits = c(0, limit),
          expand = expansion(mult = c(0, .05))
        )
      }

      plots[[i]] <- data %>%
        dplyr::filter(!!group == groups[i]) %>%
        dplyr::mutate(!!x := reorder_other(!!x, order)) %>%
        ggplot(aes(!!x, !!y)) +
        geom_col(fill = bar_colors[i], width = .7) +
        scale_x_discrete(expand = expansion(add = .5)) +
        y_scale +
        coord_flip() +
        pyramid_theme(sides[i]) +
        ggtitle(groups[i])
    }

    x_label <- if (is.null(xlab)) {
      rlang::as_name(y)
    } else {
      xlab
    }
    plots[[1]] + plots[[2]] +
      patchwork::plot_annotation(
        caption = x_label,
        title = title,
        theme = theme(plot.caption = element_text(hjust = .5, size = 13))
      )
  }

# maps


#' @export
gr_map_fromto <-
  function(data,
           polygon,
           from,
           to,
           by,
           size = NULL,
           labs_objs = NULL,
           theme_map = gr_current_theme()) {
    if (!is_empty(labs_objs)) {
      if (class(labs_objs) != class(labs())) {
        labs_objs <- NULL
      }
    }
    from <- rlang::enexpr(from)
    to <- rlang::enexpr(to)
    by <- rlang::enexpr(by)
    size <- rlang::enexpr(size)

    centroids <-
      eval_expr(
        expr = st_centrscoords(polygon, !!by),
        envir = env_curr()
      )

    cntr_from <-
      eval_parse(paste("rename(centroids,", from, "=", by, ", x = X, y = Y)"),
                 envir = env_curr()
      )

    cntr_to <-
      eval_parse(paste("rename(centroids,", to, "=", by, ", xend = X, yend = Y)"),
                 envir = env_curr()
      )

    data <- data %>%
      inner_join(cntr_from, by = rlang::expr_deparse(from)) %>%
      inner_join(cntr_to, by = rlang::expr_deparse(to)) %>%
      mutate(
        xend = dplyr::if_else(!!from == !!to, xend + 10, xend),
        yend = dplyr::if_else(!!from == !!to, yend + 10, yend)
      )

    unique_by <-
      unique(c(data[[rlang::as_label(from)]], data[[rlang::as_label(to)]]))
    polygon <-
      polygon[polygon[[rlang::as_label(by)]] %in% unique_by, ]

    centroids <-
      centroids[centroids[[rlang::as_label(by)]] %in% unique_by, ]

    limx <- range(centroids$X) + c(-10000, +10000)
    limy <- range(centroids$Y) + c(-10000, +10000)

    p <-
      ggplot() +
      ggplot2::geom_curve(
        data = data,
        aes(
          x = x,
          y = y,
          size = !!size,
          xend = xend,
          yend = yend,
          colour = !!to
        ),
        arrow = ggplot2::arrow(length = unit(0.5, "cm")),
        show.legend = F
      ) +
      ggplot2::geom_sf(
        data = polygon,
        fill = "transparent",
        colour = "grey80"
      ) +
      ggplot2::geom_sf_text(
        data = st_as_sf(
          centroids,
          coords = c("X", "Y"),
          crs = st_crs(polygon)
        ),
        aes(label = !!by),
        colour = theme_map$text$colour,
        size = 2
      ) +
      ggplot2::scale_size(range = c(0.01, 2.5))

    if (!is_empty(labs_objs)) {
      p <- p + labs_objs
    }

    p <- p + ggplot2::coord_sf(xlim = limx, ylim = limy)

    p +
      theme(
        legend.position = "none",
        plot.background = element_rect(
          fill = theme_map$plot.background$fill,
          colour = theme_map$plot.background$colour
        ),
        panel.background = element_rect(fill = theme_map$panel.background$fill),
        panel.grid = element_line(
          colour = theme_map$panel.grid$colour,
          linetype = "dashed",
          size = 0.3
        ),
        axis.text = element_text(colour = theme_map$axis.text$colour),
        axis.ticks = element_line(colour = theme_map$axis.ticks$colour),
        text = element_text(colour = theme_map$text$colour)
      )
  }


#' @export
gr_map_hex <-
  function(data_sf,
           polygon,
           fill,
           cellsize = 1000,
           labs_objs,
           legend_angle = 90,
           pallete = "Temps",
           theme_map = gr_current_theme()) {
    if (!is_empty(labs_objs)) {
      if (class(labs_objs) != class(labs())) {
        labs_objs <- NULL
      }
    }
    fill <- rlang::enexpr(fill)

    data_sf$index_target <- 1:nrow(data_sf)

    grid <- sf::st_make_grid(
      data_sf,
      cellsize,
      crs = st_crs(data_sf),
      what = "polygons",
      square = FALSE
    )

    grid <- sf::st_sf(index = 1:length(lengths(grid)), grid)
    cent_grid <- suppressall(sf::st_centroid(grid))
    cent_merge <-
      sf::st_join(cent_grid, data_sf["index_target"], left = F)
    grid_new <-
      inner_join(grid, sf::st_drop_geometry(cent_merge), by = "index")

    hex_geom <-
      aggregate(
        grid_new,
        by = list(grid_new$index_target),
        FUN = min,
        do_union = FALSE
      )

    hex_comb <-
      left_join(hex_geom %>% dplyr::select(index_target),
                sf::st_drop_geometry(data_sf),
                by = "index_target"
      ) %>%
      dplyr::select(-index_target)

    hex_comb_cut <- hex_comb %>%
      sf::st_filter(polygon) %>%
      st_transform(crs = 4326)

    limx <- st_bbox(hex_comb_cut)[c(1, 3)] #+ c(-10000,+10000)
    limy <- st_bbox(hex_comb_cut)[c(2, 4)] #+ c(-10000,+10000)

    pal_colours <- metacolours$plot_colour(x = pallete, rev = F)

    p <-
      ggplot() +
      ggplot2::geom_sf(
        data = hex_comb_cut,
        aes(fill = !!fill),
        colour = "transparent"
      ) +
      ggplot2::continuous_scale(
        aesthetics = "fill",
        scale_name = "manual",
        palette = scales::gradient_n_pal(
          colours = pal_colours$colours,
          space = "Lab"
        )
      ) +
      ggplot2::geom_sf(
        data = polygon,
        fill = "transparent",
        colour = "grey85",
        size = 0.3
      )

    if (!is_empty(labs_objs)) {
      p <- p + labs_objs
    }

    p <-
      p +
      ggplot2::coord_sf(
        crs = 4326,
        xlim = limx,
        ylim = limy
      )



    p +
      theme(
        text = element_text(
          family = theme_map$text$family,
          colour = theme_map$text$colour
        ),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        plot.title = element_text(
          size = theme_map$plot.title$size,
          face = theme_map$plot.title$face
        ),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(
          fill = theme_map$plot.background$fill,
          colour = theme_map$plot.background$colour
        ),
        axis.text = element_text(colour = theme_map$axis.text$colour),
        axis.ticks = element_line(colour = theme_map$axis.ticks$colour),
        panel.grid = element_line(colour = theme_map$panel.grid$colour, linetype = "dotted"),
        legend.background = element_rect(fill = theme_map$legend.background$fill),
        legend.key = element_rect(fill = theme_map$legend.key$fill),
        legend.text = element_text(colour = theme_map$legend.text$colour),
        panel.ontop = TRUE
      ) +
      guides(fill = ggplot2::guide_colourbar(
        title.position = "left",
        title.theme = element_text(
          angle = legend_angle,
          family = theme_map$text$family,
          colour = theme_map$text$colour,
          hjust = 0.5
        ),
      ))
  }


#' @export
gr_map_grid <- function(polygons,
                        points,
                        labs_objs,
                        box_size = 1000,
                        theme_map = gr_current_theme()) {
  if (!is_empty(labs_objs)) {
    if (class(labs_objs) != class(labs())) {
      labs_objs <- NULL
    }
  }

  points$count <- 1

  template <-
    stars::st_as_stars(
      .x = st_bbox(points),
      dx = box_size,
      dy = box_size,
      values = -1
    )

  s <- stars::st_rasterize(
    points[, "count"],
    template = template,
    options = c("MERGE_ALG=ADD", "ALL_TOUCHED=TRUE")
  )

  s[s == -1] <- NA
  grid <- st_as_sf(s)

  limx <- st_bbox(points)[c(1, 3)]
  limy <- st_bbox(points)[c(2, 4)]

  p <- ggplot() +
    ggplot2::geom_sf(
      data = grid,
      aes(fill = count),
      size = ggplot2::rel(1),
      alpha = 0.7,
      colour = "grey50"
    ) +
    scale_pal_gr(aesthetics = "fill",scale = "continuous",palette = "Purp") +
    ggplot2::geom_sf(
      data = polygons,
      fill = "transparent",
      size = 0.4,
      linetype = "dotted",
      colour = "cyan4"
    )

  if (!is_empty(labs_objs)) {
    p <- p + labs_objs
  }

  p + ggplot2::coord_sf(xlim = limx, ylim = limy) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.justification = "left",
      plot.margin = unit(c(5, 0, 5, 0), "mm"),
      panel.spacing = unit(0, "mm"),
      panel.background = element_rect(
        fill = theme_map$panel.background$fill,
        colour = "transparent"
      ),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      text = element_text(size = theme_map$text$size, colour = theme_map$text$colour),
      plot.title = element_text(hjust = 0),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-10, -10, -10, -10)
    ) +
    guides(
      fill = ggplot2::guide_colourbar(
        title.position = "top",
        title.theme = element_text(size = 6, colour = theme_map$text$colour),
        barheight = unit(2, "mm"),
        barwidth = unit(20, "mm")
      )
    )
}

#' @export
gr_map_pois <-
  function(pois,
           col,
           labs_objs,
           theme_map = gr_current_theme()) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      cat_stop("package sf required, please install it first")
    }

    if (!is_empty(labs_objs)) {
      if (class(labs_objs) != class(labs())) {
        labs_objs <- NULL
      }
    }


    col <- rlang::enexpr(col)
    bb <- st_bbox(pois) + c(-0.08, -0.02, +0.08, +0.02)
    names(bb) <- c("left", "bottom", "right", "top")
    basemap <-
      ggmap::get_map(
        location = bb,
        zoom = 12,
        maptype = "roadmap",
        force = T
      )
    colores <- scale_pal_gr(aesthetics = "colour",scale = "discrete",palette = "Safe")
    colores <-
      as_l(environment(colores$palette))$f(n = length(unique(pois[[rlang::as_label(col)]])))

    q <- ggmap::ggmap(basemap, darken = c(0.4, "#c8fadb")) +
      ggplot2::geom_sf(
        data = pois,
        aes(colour = !!col),
        inherit.aes = F,
        size = 2.5,
        alpha = 0.6
      ) +
      ggplot2::scale_colour_manual(values = colores) +
      guides(colour = guide_legend(nrow = 2)) +
      theme(
        panel.background = element_blank(),
        plot.background = element_rect(
          fill = theme_map$plot.background$fill,
          colour = theme_map$plot.background$colour
        ),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        legend.title = element_blank(),
        text = element_text(size = 8, colour = theme_map$text$colour),
        legend.text = element_text(size = 6),
        plot.title.position = "plot",
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(
          size = 7,
          vjust = 2,
          hjust = 0.5
        ),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 6, vjust = 2),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.key = element_blank(),
        legend.key.height = unit(2, "mm")
      )
    if (!is_empty(labs_objs)) {
      q + labs_objs
    }
    q
  }


# animate

#' @export
gr_densfn_animate <-
  function(fn,
           args,
           n_args = 5,
           n_samp = 500,
           pltt = scale_pal_gr(palette = "Temps",scale = "discrete",aesthetics = "fill"),
           limits = c(.025, .975),
           .tol = 1e-3) {
    args_list <- lapply(args, function(x, n) {
      seq(min(x), max(x), length.out = n_args)
    }, n = n_args)

    args_list <- tidyr::expand_grid(!!!args_list)

    args_list <-
      lapply(split(args_list, seq_len(nrow(args_list))), function(x, n) {
        x <- as_l(x)
        x$n <- n
        x
      }, n = n_samp)


    data <-
      purrr::map_dfr(args_list, function(x) {
        x$x <- do.call(paste0("r", fn), x)
        x$x <- x$x[!is_nonnum(x$x)]
        x_dens <- x[names(x) != "n"]
        x$density <- do.call(paste0("d", fn), x_dens)
        as_tbl(x)
      })
    data <- data[data$density > .tol, ]
    syms_nm <- rlang::syms(names(args))

    xrange <- range(data$x)

    data_args <- dplyr::select(data, !!!syms_nm)

    data$transition <-
      vapply(seq_len(nrow(data)), function(i) {
        w <- round_any(as_num(data_args[i, ]), accuracy = .01)
        paste(paste(names(data_args), ":", w), collapse = ",")
      }, character(1))

    data$transition <- factor(data$transition)


    thm <-
      gr_current_theme() %+replace% theme(legend.position = "none")

    levels_fct <-
      data %>%
      arrange(!!!syms_nm) %>%
      pull(transition) %>%
      unique() %>%
      levels()

    xrange <-
      vapply(1:2, function(p) {
        f <- if (p == 1) {
          min
        } else {
          max
        }
        f(vaggr(
          x = data$x,
          .group = data$transition,
          .fn = function(x) {
            quantile(x, limits[p], na.rm = TRUE)
          }
        ))
      }, numeric(1))

    data %>%
      mutate(
        transition = factor(transition),
        transition = forcats::fct_relevel(.f = transition, !!levels_fct),
        fn = factor(fn)
      ) %>%
      ggplot(aes(x = x, fill = fn), alpha = .4) +
      ggplot2::geom_density(aes(y = ..density..)) +
      gganimate::transition_states(states = transition, , transition_length = 2) +
      labs(title = "Argumentos: {closest_state}") +
      gganimate::shadow_mark(
        alpha = .2,
        colour = "gray",
        past = n_args
      ) +
      pltt +
      thm
  }


## grf

#' @export
grf_plot <- function(...) {
  dots <- list(...)
  formulas <- sapply(dots, rlang::is_formula)
  flist <- dots[formulas]
  mlist <- lapply(flist, rlang::f_rhs)
  olist <- dots[!formulas]
  mapping <- do.call(aes, mlist)
  do.call(ggplot, c(list(mapping = mapping), olist))
}


#' @keywords internal
create_formals <-
  function(extras = list(),layer_fun,geom,stat,position,inherit.aes = TRUE) {
    layer_fun <- rlang::eval_tidy(layer_fun)
    res <-
      c(
        list(
          object = NULL,
          gformula = NULL,
          data = NULL
        ),
        alist(... = ),
        extras[setdiff(
          names(extras),
          c("xlab", "ylab", "title", "subtitle", "caption")
        )],
        if (is.null(extras[["xlab"]])) {
          alist(xlab = )
        } else {
          list(xlab = extras[["xlab"]])
        },
        if (is.null(extras[["ylab"]])) {
          alist(ylab = )
        } else {
          list(ylab = extras[["ylab"]])
        },
        if (is.null(extras[["title"]])) {
          alist(title = )
        } else {
          list(title = extras[["title"]])
        },
        if (is.null(extras[["subtitle"]])) {
          alist(subtitle = )
        } else {
          list(subtitle = extras[["subtitle"]])
        },
        if (is.null(extras[["caption"]])) {
          alist(caption = )
        } else {
          list(caption = extras[["caption"]])
        },
        list(
          geom = geom,
          stat = stat,
          position = position,
          show.legend = NA,
          show.help = NULL,
          inherit = inherit.aes,
          environment = quote(parent.frame())
        )
      )

    for (f in c("geom", "stat", "position")) {
      if (!f %in% names(formals(layer_fun))) {
        res[[f]] <- NULL
      }
    }
    res
  }

#' @keywords internal
layer_factory <-
  function(geom = "point",
           position = "identity",
           stat = "identity",
           pre =
             {

             },
           aes_form = y ~ x,
           extras = alist(),
           note = NULL,
           aesthetics = aes(),
           inherit.aes = TRUE,
           check.aes = TRUE,
           data = NULL,
           layer_fun = rlang::quo(ggplot2::layer),
           ...)
  {
    pre <- substitute(pre)

    if (!is.logical(inherit.aes)) {
      inherited.aes <- inherit.aes
      inherit.aes <- FALSE
    } else {
      inherited.aes <- character(0)
    }

    res <-
      function(xlab,
               ylab,
               title,
               subtitle,
               caption,
               show.legend,
               function_name,
               inherit,
               environment = parent.frame(),
               ...) {
        geom <- rlang::eval_tidy(geom)
        stat <- rlang::eval_tidy(stat)
        position <- rlang::eval_tidy(position)
        layer_fun <- rlang::eval_tidy(layer_fun)

        function_name <- as.character(match.call()[1])
        orig_args <- as.list(match.call())[-1]

        eval(pre) # pre will be placed in the function environment so it is available here

        if (!is.list(aes_form)) {
          aes_form <- list(aes_form)
        }

        if (is.null(show.help)) {
          show.help <- length(orig_args) < 1
        }

        if (inherits(object, "formula")) {
          gformula <- object
          object <- NULL
        }

        if (inherits(object, "data.frame")) {
          data <- object
          object <- NULL
        }



        gformula <- response2explanatory(gformula, aes_form)

        aes_form <-
          first_matching_formula(
            gformula,
            aes_form,
            object,
            inherit,
            inherited.aes,
            function_name
          )


        stat_formals <- grab_formals(stat, "stat")
        geom_formals <- grab_formals(geom, "geom")
        extras_and_dots <-
          create_extras_and_dots(
            args = orig_args,
            formals = formals(),
            stat_formals = stat_formals,
            geom_formals = geom_formals,
            extras = extras,
            env = environment
          )
        if (is.character(position)) {
          position_fun <- paste0("position_", position)
          pdots <-
            extras_and_dots[intersect(names(extras_and_dots), names(formals(position_fun)))]
          position <- do.call(position_fun, pdots)
        }

        if (length(extras_and_dots) > 0) {
          extras_and_dots <-
            extras_and_dots[sapply(extras_and_dots, function(x) {
              !is.symbol(x)
            })]
        }

        add <- inherits(object, c("gg", "ggplot"))

        if (add) {
          for (aes.name in inherited.aes) {
            aesthetics[[aes.name]] <- object$mapping[[aes.name]]
          }
        }

        if (length(extras_and_dots) > 0) {
          w <- which(sapply(extras_and_dots, function(x) {
            rlang::is_formula(x) && length(x) == 2L
          }))
          aesthetics <-
            add_aes(aesthetics, extras_and_dots[w], environment)
          extras_and_dots[w] <- NULL
        }
        ingredients <-
          grf_ingredients(
            formula = gformula,
            data = data,
            gg_object = object,
            extras = extras_and_dots,
            aes_form = aes_form,
            aesthetics = aesthetics,
            envir = environment
          )

        if ("params" %in% names(formals(layer_fun))) {
          layer_args <-
            list(
              geom = geom,
              stat = stat,
              data = ingredients[["data"]],
              mapping = ingredients[["mapping"]],
              position = position,
              params = remove_from_list(ingredients[["params"]], "inherit"),
              check.aes = check.aes,
              check.param = FALSE,
              show.legend = show.legend,
              inherit.aes = inherit
            )
        } else {
          layer_args <-
            c(
              list(
                data = ingredients[["data"]],
                mapping = ingredients[["mapping"]],
                show.legend = show.legend,
                geom = geom,
                stat = stat
              ),
              remove_from_list(ingredients[["params"]], "inherit")
            )
        }

        if (!"..." %in% names(formals(layer_fun))) {
          layer_args <- cull_list(layer_args, names(formals(layer_fun)))
        }

        for (f in c("geom", "stat", "position")) {
          if (!f %in% names(formals(layer_fun))) {
            layer_args[[f]] <- NULL
          }
        }

        layer_args <- layer_args[unique(names(layer_args))]

        new_layer <-
          do.call(layer_fun, layer_args, envir = environment)

        if (is.null(ingredients[["facet"]])) {
          if (add) {
            p <- object + new_layer
          } else {
            p <-
              do.call(ggplot,
                      list(
                        data = ingredients$data,
                        mapping = ingredients[["mapping"]]
                      ),
                      envir = environment
              ) +
              new_layer
          }
        } else {
          if (add) {
            p <- object + new_layer + ingredients[["facet"]]
          } else {
            p <-
              do.call(ggplot,
                      list(
                        data = ingredients$data,
                        mapping = ingredients[["mapping"]]
                      ),
                      envir = environment
              ) +
              new_layer +
              ingredients[["facet"]]
          }
        }

        if (!rlang::is_missing(ylab)) {
          p <- p + ggplot2::ylab(ylab)
        }
        if (!rlang::is_missing(xlab)) {
          p <- p + ggplot2::xlab(xlab)
        }
        if (!rlang::is_missing(title)) {
          p <- p + ggplot2::labs(title = title)
        }
        if (!rlang::is_missing(subtitle)) {
          p <- p + ggplot2::labs(subtitle = subtitle)
        }
        if (!rlang::is_missing(caption)) {
          p <- p + ggplot2::labs(caption = caption)
        }
        class(p) <- unique(c("grf_ggplot", class(p)))
        p
      }
    formals(res) <-
      c(
        create_formals(
          extras,
          layer_fun = layer_fun,
          geom = geom,
          stat = stat,
          position = position,
          inherit.aes = inherit.aes
        ),
        list(...)
      )

    assign("inherit.aes", inherit.aes, environment(res))
    assign("check.aes", check.aes, environment(res))
    assign("pre", pre, environment(res))
    assign("extras", extras, environment(res))
    res
  }
#' @export
grf_point <-
  layer_factory(
    geom = "point",
    extras = alist(
      alpha = ,
      color = ,
      size = ,
      shape = ,
      fill = ,
      group = ,
      stroke =
    )
  )

#' @export
grf_jitter <-
  layer_factory(
    geom = "point",
    position = "jitter",
    extras = alist(
      alpha = ,
      color = ,
      size = ,
      shape = ,
      fill = ,
      width = ,
      height = ,
      group = ,
      stroke =
    )
  )

#' @export
grf_line <-
  layer_factory(
    geom = "line",
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size = ,
      lineend = ,
      linejoin = ,
      linemitre = ,
      arrow =
    )
  )

#' @export
grf_path <-
  layer_factory(
    geom = "path",
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      lineend = "butt",
      linejoin = "round",
      linemitre = 1,
      arrow = NULL
    )
  )

#' @export

grf_ellipse <-
  layer_factory(
    geom = "path",
    stat = "ellipse",
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      type = "t",
      level = 0.95,
      segments = 51
    )
  )

#' @export
grf_polygon <-
  layer_factory(
    geom = "polygon",
    extras = alist(
      alpha = ,
      color = ,
      size = ,
      shape = ,
      fill = ,
      group = ,
      stroke =
    )
  )


#' @export
grf_smooth <-
  layer_factory(
    geom = "smooth",
    stat = "smooth",
    extras = alist(
      method = "auto",
      formula = y ~ x,
      se = FALSE,
      method.args = ,
      n = 80,
      span = 0.75,
      fullrange = FALSE,
      level = 0.95
    )
  )

#' @export

grf_lm <-
  layer_factory(
    geom = "lm",
    stat = "lm",
    aes_form = y ~ x,
    extras = alist(
      alpha = 0.3,
      lm.args = list(),
      interval = "none",
      level = 0.95,
      fullrange = TRUE
    )
  )

#' @export
grf_spline <-
  layer_factory(
    geom = "line",
    stat = "spline",
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      weight = ,
      df = ,
      spar = ,
      tol =
    )
  )

#' @export
grf_raster <-
  layer_factory(
    geom = "raster",
    aes_form = list(y ~ x, fill ~ x + y),
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size = ,
      hjust = 0.5,
      vjust = 0.5,
      interpolate = FALSE
    )
  )

#' @export
grf_quantile <-
  layer_factory(
    geom = "quantile",
    stat = "quantile",
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      weight = ,
      lineend = "butt",
      linejoin = "round",
      linemitre = 1,
      quantiles = ,
      formula = ,
      method = ,
      method.args =
    )
  )

#' @export
grf_density_2d <-
  layer_factory(
    geom = "density_2d",
    stat = "density_2d",
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      contour = TRUE,
      n = 100,
      h = NULL,
      lineend = "butt",
      linejoin = "round",
      linemitre = 1
    )
  )

#' @export
grf_density_2d_filled <-
  layer_factory(
    geom = "density_2d_filled",
    stat = "density_2d_filled",
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      contour = TRUE,
      n = 100,
      h = NULL,
      lineend = "butt",
      linejoin = "round",
      linemitre = 1
    )
  )


#' @export
grf_density2d <-
  layer_factory(
    geom = "density2d",
    stat = "density2d",
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      contour = TRUE,
      n = 100,
      h = NULL,
      lineend = "butt",
      linejoin = "round",
      linemitre = 1
    )
  )

#' @export
grf_density2d_filled <-
  layer_factory(
    geom = "density2d_filled",
    stat = "density_2d_filled",
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      contour = TRUE,
      n = 100,
      h = NULL,
      lineend = "butt",
      linejoin = "round",
      linemitre = 1
    )
  )

#' @export
grf_hex <-
  layer_factory(
    geom = "hex",
    stat = "binhex",
    extras = alist(
      bins = ,
      binwidth = ,
      alpha = ,
      color = ,
      fill = ,
      group = ,
      size =
    )
  )

#' @export
grf_boxplot <-
  layer_factory(
    aes_form = list(y ~ x, ~x, y ~ .),
    geom = "boxplot",
    stat = "boxplot",
    position = "dodge",
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size = ,
      coef = ,
      outlier.color = NULL,
      outlier.fill = NULL,
      outlier.shape = 19,
      outlier.size = 1.5,
      outlier.stroke = 0.5,
      outlier.alpha = NULL,
      notch = FALSE,
      notchwidth = 0.5,
      varwidth = FALSE
    )
  )

#' @export
grf_text <-
  layer_factory(
    geom = "text",
    position = "nudge",
    pre = {
      if ((nudge_x != 0) || (nudge_y != 0)) {
        position <- position_nudge(nudge_x, nudge_y)
      }
    },
    extras = alist(
      label = ,
      alpha = ,
      angle = ,
      color = ,
      family = ,
      fontface = ,
      group = ,
      hjust = ,
      lineheight = ,
      size = ,
      vjust = ,
      parse = FALSE,
      nudge_x = 0,
      nudge_y = 0,
      check_overlap = FALSE
    )
  )

#' @export
grf_label <-
  layer_factory(
    stat = "identity",
    geom = "label",
    position = "nudge",
    pre = {
      if ((nudge_x != 0) || (nudge_y != 0)) {
        position <- position_nudge(nudge_x, nudge_y)
      }
    },
    layer_fun = rlang::quo(ggplot2::geom_label),
    extras = alist(
      label = ,
      alpha = ,
      angle = ,
      color = ,
      family = ,
      fontface = ,
      group = ,
      hjust = ,
      label = ,
      alpha = ,
      angle = ,
      color = ,
      family = ,
      fontface = ,
      group = ,
      hjust = ,
      vjust = ,
      lineheight = ,
      size = ,
      parse = ,
      nudge_x = 0,
      nudge_y = 0,
      label.padding = unit(0.25, "lines"),
      label.r = unit(0.15, "lines"),
      label.size = 0.25
    )
  )

#' @export
grf_area <-
  layer_factory(
    geom = "area",
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size =
    )
  )

#' @export
grf_violin <-
  layer_factory(
    geom = "violin",
    stat = "ydensity",
    position = "dodge",
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size = ,
      weight = ,
      draw_quantiles = NULL,
      trim = TRUE,
      scale = "area",
      bw = ,
      adjust = 1,
      kernel = "gaussian"
    )
  )

#' @export
grf_spoke <-
  layer_factory(
    geom = "spoke",
    extras = alist(
      angle = ,
      radius = ,
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size =
    ),
    note = "Note: angle and radius must be set or mapped."
  )


#' @export
grf_step <-
  layer_factory(
    geom = "step",
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      direction = "hv"
    )
  )

#' @export
grf_tile <-
  layer_factory(
    geom = "tile",
    aes_form = list(y ~ x, fill ~ x + y),
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size =
    )
  )

#' @export
grf_bin2d <-
  layer_factory(
    geom = "tile",
    stat = "bin2d",
    aes_form = list(y ~ x),
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size =
    )
  )

#' @export
grf_count <-
  layer_factory(
    geom = "point",
    stat = "sum",
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      shape = ,
      size = ,
      stroke =
    )
  )

#' @export
grf_col <-
  layer_factory(
    geom = "col",
    position = "stack",
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size =
    )
  )

#' @export
grf_blank <-
  layer_factory(geom = "blank", check.aes = FALSE)

#' @export
grf_frame <-
  layer_factory(geom = "blank", check.aes = FALSE)


#' @export
grf_histogram <-
  layer_factory(
    geom = "bar",
    stat = "bin",
    position = "stack",
    aes_form = list(~x, y ~ ., y ~ x),
    extras = alist(
      bins = 25,
      binwidth = ,
      alpha = 0.5,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size =
    ),
    note =
      "y may be stat(density) or stat(count) or stat(ndensity) or stat(ncount), but see grf_dhistogram().",
  )

#' @export
grf_dhistogram <-
  layer_factory(
    geom = "bar",
    stat = "bin",
    position = "stack",
    aes_form = list(~x, y ~ ., y ~ x),
    extras =
      alist(
        bins = 25,
        binwidth = ,
        alpha = 0.5,
        color = ,
        fill = ,
        group = ,
        linetype = ,
        size =
      ),
    note =
      "y may be stat(density) or stat(count) or stat(ndensity) or stat(ncount)",
    aesthetics = aes(y = stat(density))
  )

#' @export
grf_density <-
  layer_factory(
    geom = "area",
    stat = "density",
    aes_form = list(~x, y ~ .),
    extras = alist(
      alpha = 0.5,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size = ,
      kernel = "gaussian",
      n = 512,
      trim = FALSE
    ),
    aesthetics = aes(y = stat(density))
  )

#' @export

grf_dens <-
  layer_factory(
    geom = "line",
    stat = "density",
    aes_form = list(~x, y ~ .),
    extras = alist(
      alpha = 0.5,
      color = ,
      fill = NA,
      group = ,
      linetype = ,
      size = ,
      kernel = "gaussian",
      n = 512,
      trim = FALSE
    ),
    aesthetics = aes(y = stat(density))
  )

#' @export

grf_dens2 <-
  layer_factory(
    geom = "density_line",
    stat = "density",
    aes_form = list(~x, y ~ .),
    extras = alist(
      alpha = 0.5,
      color = ,
      fill = NA,
      group = ,
      linetype = ,
      size = ,
      kernel = "gaussian",
      n = 512,
      trim = FALSE
    ),
    aesthetics = aes(y = stat(density))
  )
#' @export
grf_dotplot <-
  layer_factory(
    geom = "dotplot",
    stat = rlang::quo(ggplot2::StatBin),
    layer_fun = rlang::quo(ggplot2::geom_dotplot),
    aes_form = ~x,
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      binwidth = NULL,
      binaxis = "x",
      method = "dotdensity",
      binpositions = "bygroup",
      stackdir = "up",
      stackratio = 1,
      dotsize = 1,
      stackgroups = FALSE,
      origin = NULL,
      right = TRUE,
      width = 0.9,
      drop = FALSE
    )
  )

#' @export
grf_bar <-
  layer_factory(
    geom = "bar",
    stat = "count",
    position = "stack",
    aes_form = list(~x, y ~ ., y ~ x),
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size = ,
      width = NULL
    )
  )

#' @export

grf_counts <-
  layer_factory(
    geom = "bar",
    stat = "count",
    position = "stack",
    aes_form = list(~x, y ~ .),
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size = ,
      width = NULL
    )
  )

#' @export

percs_by_group <-
  function(x, group) {
    tibble(x, group = rep(!!group, length.out = length(x))) %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(s = sum(x), p = 100 * x / s) %>%
      dplyr::pull(p)
  }

#' @export
props_by_group <-
  function(x, group) {
    tibble(x, group = rep(!!group, length.out = length(x))) %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(s = sum(x), p = x / s) %>%
      dplyr::pull(p)
  }

#' @export
grf_props <-
  layer_factory(
    geom = "bar",
    stat = "count",
    position = "stack",
    aes_form = list(~x, y ~ ., y ~ x),
    extras =
      alist(
        alpha = ,
        color = ,
        fill = ,
        group = ,
        linetype = ,
        size = ,
        ylab = "proportion"
      ),
    aesthetics = aes(y = after_stat(props_by_group(count, DENOM))),
    pre = {
      yaes_expr <- rlang::quo_get_expr(aesthetics[["y"]])

      yaes_expr[[2]][[3]] <- rlang::f_rhs(denom)

      aesthetics[["y"]] <- yaes_expr
    },
    denom = ~PANEL
  )

#' @export
grf_percents <-
  layer_factory(
    geom = "bar",
    stat = "count",
    position = "stack",
    aes_form = list(~x, y ~ .),
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size = ,
      ylab = "percent"
    ),
    aesthetics = aes(y = after_stat(percs_by_group(count, DENOM))),
    pre = {
      yaes_expr <- rlang::quo_get_expr(aesthetics[["y"]])

      yaes_expr[[2]][[3]] <- rlang::f_rhs(denom)

      aesthetics[["y"]] <- yaes_expr
    },
    denom = ~PANEL
  )

#' @export
grf_freqpoly <-
  layer_factory(
    geom = "path",
    stat = "bin",
    aes_form = list(~x, y ~ .),
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      binwidth = ,
      bins = ,
      center = ,
      boundary =
    ),
    note =
      "y may be omitted or stat(density) or stat(count) or stat(ndensity) or stant(ncount)."
  )

#' @export
grf_qq <-
  layer_factory(
    geom = "point",
    stat = "qq",
    aes_form = ~sample,
    extras = alist(
      group = ,
      distribution = stats::qnorm,
      dparams = list()
    )
  )
#' @export
grf_qqline <-
  layer_factory(
    geom = "line",
    stat = "qqline",
    aes_form = ~sample,
    extras = alist(
      group = ,
      distribution = stats::qnorm,
      dparams = list(),
      linetype = "dashed",
      alpha = 0.7
    )
  )

#' @export

grf_qqstep <-
  layer_factory(
    geom = "step",
    stat = "qq",
    position = "identity",
    aes_form = ~sample,
    extras = alist(
      group = ,
      distribution = stats::qnorm,
      dparams = list()
    )
  )

#' @export


grf_ecdf <-
  layer_factory(
    geom = "step",
    stat = "ecdf",
    position = "identity",
    aes_form = list(~x, y ~ .),
    extras = alist(group = , pad = , n = NULL)
  )

#' @export
grf_rug <-
  layer_factory(
    geom = "rug",
    aes_form = list(~x, y ~ x, NULL),
    extras = alist(
      sides = "bl",
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size =
    )
  )

#' @export
grf_rugx <-
  layer_factory(
    geom = "rug",
    aes_form = list(~x, y ~ x, NULL),
    check.aes = FALSE,
    extras = alist(
      sides = "b",
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      height = 0
    ),
    pre = {
      if (inherits(object, "gg")) {
        if (uses_stat(object$mapping$y)) {
          orig_args[["y"]] <- orig_args[["y"]] %||% ~0
        }
      }
    }
  )

#' @export
grf_rugy <-
  layer_factory(
    geom = "rug",
    aes_form = list(~y, y ~ ., NULL),
    extras = alist(
      sides = "l",
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      width = 0
    ),
    pre = {
      if (inherits(object, "gg")) {
        if (uses_stat(object$mapping$x)) {
          orig_args[["x"]] <- orig_args[["x"]] %||% ~0
        }
      }
    }
  )

#' @export
grf_contour <-
  layer_factory(
    geom = "contour",
    stat = "contour",
    aes_form = z ~ x + y
  )

#' @export
grf_contour_filled <-
  layer_factory(
    geom = "contour_filled",
    stat = "contour_filled",
    aes_form = z ~ x + y
  )

#' @export
grf_ribbon <-
  layer_factory(
    geom = "ribbon",
    aes_form = list(ymin + ymax ~ x, y ~ xmin + xmax),
    extras = list(alpha = 0.3)
  )

#' @export
grf_curve <-
  layer_factory(
    geom = "curve",
    aes_form = y + yend ~ x + xend,
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      curvature = 0.5,
      angle = 90,
      ncp = 5,
      arrow = NULL,
      lineend = "butt"
    )
  )

#' @export
grf_segment <-
  layer_factory(
    geom = "segment",
    aes_form = y + yend ~ x + xend,
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      arrow = NULL,
      lineend = "butt"
    )
  )

#' @export
grf_linerange <-
  layer_factory(
    geom = "linerange",
    aes_form = list(ymin + ymax ~ x, y ~ xmin + xmax),
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size =
    )
  )

#' @export
grf_pointrange <-
  layer_factory(
    geom = "pointrange",
    aes_form = list(y + ymin + ymax ~ x, y ~ x + xmin + xmax),
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      fatten = 2
    )
  )

#' @export
grf_summary <-
  layer_factory(
    geom = "pointrange",
    stat = "summary",
    aes_form = y ~ x,
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      fun.y = NULL,
      fun.ymax = NULL,
      fun.ymin = NULL,
      fun.args = list(),
      fatten = 2
    )
  )

#' @export
grf_crossbar <-
  layer_factory(
    geom = "crossbar",
    aes_form = list(y + ymin + ymax ~ x, y ~ x + xmin + xmax),
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size = ,
      fatten = 2.5
    )
  )

#' @export
grf_errorbar <-
  layer_factory(
    geom = "errorbar",
    aes_form = list(ymin + ymax ~ x, y ~ xmin + xmax),
    inherit.aes = TRUE,
    check.aes = FALSE,
    extras = alist(
      alpha = ,
      color = ,
      group = ,
      linetype = ,
      size =
    )
  )

#' @export
grf_rect <-
  layer_factory(
    geom = "rect",
    aes_form = ymin + ymax ~ xmin + xmax,
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size =
    )
  )


#' @export
grf_abline <-
  layer_factory(
    geom = "abline",
    aes_form = NULL,
    extras = alist(
      slope = ,
      intercept = ,
      color = ,
      size = ,
      linetype = ,
      alpha =
    ),
    inherit.aes = FALSE,
    data = NA,
    layer_fun = rlang::quo(ggplot2::geom_abline)
  )

#' @export
grf_hline <-
  layer_factory(
    geom = "hline",
    aes_form = NULL,
    extras = alist(
      yintercept = ,
      color = ,
      size = ,
      linetype = ,
      alpha =
    ),
    inherit.aes = FALSE,
    data = NA,
    layer_fun = rlang::quo(ggplot2::geom_hline)
  )

#' @export
grf_vline <-
  layer_factory(
    geom = "vline",
    aes_form = NULL,
    extras = alist(
      xintercept = ,
      color = ,
      size = ,
      linetype = ,
      alpha =
    ),
    inherit.aes = FALSE,
    data = NA,
    layer_fun = rlang::quo(ggplot2::geom_vline)
  )

#' @export

grf_coefline <-
  function(object = NULL,
           coef = NULL,
           model = NULL,
           ...) {
    if (is.null(coef) + is.null(model) != 1) {
      cat_stop("must specify exactly one of coef or model")
    }
    if (is.null(coef)) {
      coef <- coef(model)
    }
    if (length(coef) > 2) {
      cat_warn("Ignoring all but first two values of coef.")
    }
    if (length(coef) < 2) {
      cat_stop("coef must be of length at least 2.")
    }
    grf_abline(
      object = object,
      intercept = ~ coef[1],
      slope = ~ coef[2],
      ...,
      inherit = TRUE
    )
  }
#' @export
grf_sf <-
  layer_factory(
    layer_fun = rlang::quo(ggplot2::geom_sf),
    geom = "sf",
    stat = "sf",
    position = "identity",
    aes_form = list(NULL),
    extras = alist(
      alpha = ,
      color = ,
      fill = ,
      group = ,
      linetype = ,
      size = ,
      geometry =
    ),
    pre = {
      if (!requireNamespace("sf", quietly = TRUE)) {
        cat_stop("The sf package is required.  Please install and try again.")
      }
    }
  )


#' @export
grf_empty <- function(environment = parent.frame()) {
  ggplot2::ggplot(environment = environment)
}

#' @export
grf_dist <-
  function(object = ggplot(),
           dist,
           ...,
           xlim = NULL,
           kind = c("density", "cdf", "qq", "qqstep", "histogram"),
           resolution = 5000L,
           params = NULL) {
    if (missing(dist)) {
      if (is.character(object)) {
        dist <- object
      } else {
        cat_stop("You must specify a distribution.")
      }
    }

    if (!is.character(dist)) {
      cat_stop("`dist' must be a string naming a distribution; don't forget the quotes.")
    }

    kind <- match.arg(kind)

    ddist <- paste("d", dist, sep = "")
    qdist <- paste("q", dist, sep = "")
    pdist <- paste("p", dist, sep = "")


    dots <- list(...)
    original_call <- match.call()
    dots <- original_call
    dots[[1]] <- NULL
    unnamed_dots <- original_call
    named_dots <- original_call
    unnamed_dots[[1]] <- NULL
    named_dots[[1]] <- NULL
    groupless_dots <- original_call
    groupless_dots[[1]] <- NULL
    for (i in length(unnamed_dots):1) {
      if (names(unnamed_dots)[i] != "") {
        unnamed_dots[i] <- NULL
      } else {
        named_dots[i] <- NULL
      }
    }
    if (is.null(params)) {
      params <- original_call
      params[[1]] <- NULL
      for (item in names(formals())) {
        if (item %in% names(params)) {
          params[[item]] <- NULL
        }
      }
      dparams <-
        c(unnamed(params), named_among(params, names(formals(ddist))))
      pparams <-
        c(unnamed(params), named_among(params, names(formals(pdist))))
      qparams <-
        c(unnamed(params), named_among(params, names(formals(qdist))))
      dots[names(dparams) %>%
             union(names(pparams)) %>%
             union(names(qparams))] <- NULL
    } else {
      dparams <- params
      pparams <- params
      qparams <- params
      dots[["params"]] <- NULL
    }
    names(dots) <- gsub("plot_", "", names(dots))
    if ("object" %in% names(dots)) {
      dots[["object"]] <- NULL
    }
    if ("dist" %in% names(dots)) {
      dots[["dist"]] <- NULL
    }

    env <- parent.frame()
    dparams <- lapply(dparams, function(x) {
      eval(x, env)
    })
    pparams <- lapply(pparams, function(x) {
      eval(x, env)
    })
    qparams <- lapply(qparams, function(x) {
      eval(x, env)
    })

    sample_values <-
      do.call(qdist, c(p = list(ppoints(resolution)), qparams))

    unique_values <- unique(sample_values)
    discrete <-
      length(unique_values) < 0.9 * length(sample_values)

    if (is.null(xlim)) {
      xlim_opts <-
        do.call(qdist, c(list(p = c(
          0, 0.001, 0.999, 1
        )), qparams))
      dxlim_opts <- diff(xlim_opts)
      xlim <- xlim_opts[2:3]
      if (dxlim_opts[1] < dxlim_opts[2]) {
        xlim[1] <- xlim_opts[1]
      }
      if (dxlim_opts[3] < dxlim_opts[2]) {
        xlim[2] <- xlim_opts[4]
      }
    }
    plim <- do.call(pdist, c(list(q = xlim), pparams))

    if (!discrete) {
      unif_values <- seq(do.call(qdist, c(list(p = plim[1]), qparams)),
                         do.call(qdist, c(list(p = plim[2]), qparams)),
                         length.out = resolution
      )
      fewer_values <- unif_values
    } else {
      fewer_values <- unique_values
    }

    if (kind == "cdf") {
      if (discrete) {
        step <- min(diff(fewer_values))
        cdfx <-
          seq(min(fewer_values) - 1.5 * step,
              max(fewer_values) + 1.5 * step,
              length.out = resolution
          )
        cdfx <- sort(unique(c(fewer_values, cdfx)))
        cdfy <-
          approxfun(
            fewer_values,
            do.call(pdist, c(list(q = fewer_values), pparams)),
            method = "constant",
            f = 0,
            yleft = 0,
            yright = 1
          )(cdfx)
        PlotData <- data.frame(y = cdfy, x = cdfx)
      } else {
        cdfx <- unif_values
        cdfy <-
          do.call(pdist, c(list(q = unif_values), pparams))
        PlotData <- data.frame(y = cdfy, x = cdfx)
      }
    }

    ydata <-
      switch(kind,
             density = do.call(ddist, c(list(x = fewer_values), dparams)),
             cdf = cdfy,
             qq = NULL,
             qqstep = NULL,
             histogram = do.call(ddist, c(list(x = sample_values), dparams))
      )


    if (discrete) {
      switch(kind,
             density =
               do.call(
                 grf_point,
                 c(
                   list(
                     do.call(
                       grf_segment,
                       c(
                         list(
                           object,
                           rlang::set_env(density + 0 ~ x + x, parent.frame()),
                           data = data.frame(density = ydata, x = fewer_values)
                         ),
                         dots
                       )
                     ),
                     rlang::set_env(y ~ x, parent.frame()),
                     data = data.frame(y = ydata, x = fewer_values)
                   ),
                   dots
                 )
               ),
             cdf =
               do.call(
                 grf_step,
                 c(
                   list(
                     object,
                     rlang::set_env(cumulative_density ~ x, parent.frame()),
                     data = data.frame(cumulative_density = ydata, x = cdfx)
                   ),
                   dots
                 )
               ),
             qq =
               do.call(
                 grf_qq,
                 c(
                   list(
                     object,
                     rlang::set_env(~x, parent.frame()),
                     data = data.frame(x = sample_values)
                   ),
                   dots
                 )
               ),
             qqstep =
               do.call(
                 grf_qqstep,
                 c(
                   list(
                     object,
                     rlang::set_env(~x, parent.frame()),
                     data = data.frame(x = sample_values)
                   ),
                   dots
                 )
               ),
             histogram =
               do.call(
                 grf_histogram,
                 c(
                   list(
                     object,
                     rlang::set_env(..density.. ~ x, parent.frame()),
                     data = data.frame(x = sample_values)
                   ),
                   dots
                 )
               )
      )
    } else {
      switch(kind,
             density =
               do.call(
                 grf_line,
                 c(
                   list(
                     object,
                     rlang::set_env(density ~ x, parent.frame()),
                     data = data.frame(density = ydata, x = fewer_values)
                   ),
                   dots
                 )
               ),
             cdf =
               do.call(
                 grf_line,
                 c(
                   list(
                     object,
                     rlang::set_env(cumulative_density ~ x, parent.frame()),
                     data = data.frame(cumulative_density = ydata, x = cdfx)
                   ),
                   dots
                 )
               ),
             qq =
               do.call(
                 grf_qq,
                 c(
                   list(
                     object,
                     rlang::set_env(~x, parent.frame()),
                     data = data.frame(x = sample_values)
                   ),
                   dots
                 )
               ),
             qqstep =
               do.call(
                 grf_qqstep,
                 c(
                   list(
                     object,
                     rlang::set_env(~x, parent.frame()),
                     data = data.frame(x = sample_values)
                   ),
                   dots
                 )
               ),
             histogram =
               do.call(
                 grf_dhistogram,
                 c(
                   list(
                     object,
                     rlang::set_env(~x, parent.frame()),
                     data = data.frame(x = sample_values)
                   ),
                   dots
                 )
               )
      )
    }
  }

#' @export
grf_fun <-
  function(object = NULL,
           formula,
           xlim,
           ...,
           inherit = FALSE) {
    if (rlang::is_formula(object) && missing(formula)) {
      formula <- object
      object <- NULL
    }

    if (is.null(object)) {
      object <- ggplot(data = data.frame(x = xlim), aes(x))
      inherit <- TRUE
    }
    qdots <- rlang::quos(...)
    fun <- frm2fn(formula)
    afq <- aes_from_qdots(qdots)

    p <- object +
      do.call(
        ggplot2::layer,
        list(
          geom = "path",
          stat = "function",
          position = "identity",
          mapping = afq$mapping,
          inherit.aes = inherit,
          data = if (missing(xlim))
            NULL
          else
            data.frame(x = xlim),
          params = c(list(fun = fun), lapply(afq$qdots, rlang::f_rhs))
        )
      )
    class(p) <- unique(c('grf_ggplot', class(p)))
    p
  }

## addicionales
#' @export
grf_labs <- function(object, ...) {
  object + ggplot2::labs(...)
}

#' @export
grf_lims <- function(object, ...) {
  object + ggplot2::lims(...)
}

#' @export
grf_theme <- function(object, theme, ...) {
  if (missing(theme)) {
    object + ggplot2::theme(...)
  } else {
    if (is.function(theme)) {
      object + do.call(theme, list(...))
    } else {
      object + theme
    }
  }
}

#' @export
grf_facet_wrap <- function(object, ...) {
  object + ggplot2::facet_wrap(...)
}

#' @export
grf_facet_grid <- function(object, ...) {
  object + ggplot2::facet_grid(...)
}

#' @export
grf_refine <- function(object, ...) {
  Reduce(`+`, list(...), init = object)
}

#' @export
discrete_breaks <- function(resolution = 1) {
  res <-
    function(x, resolution) {
      a <- floor(range(x))[1]
      b <- ceiling(range(x))[2]
      bks <- seq(0, b, by = resolution)
      bks[bks > a]
    }
  formals(res) <- c(alist(x = ), list(resolution = resolution))
  res
}
