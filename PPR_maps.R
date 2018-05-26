# Primary Production Required (PPR) footprint maps


# 01 INITIAL SETUP --------------------------------------------------------

# Setup

# Note this part only works if you are in RStudio. If not using RStudio, set wd manually to wherever this script is stored.
# Set working directory to directory this R script is stored in.
set_wd <- function() {
  if (!require(rstudioapi)) {
    install.packages("rstudioapi", repos = "http://cran.stat.sfu.ca/")
    require(rstudioapi)
  }
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path))
  print(getwd())
}

set_wd()

if (!require(data.table)) { # quickly read large csv tables
  install.packages("data.table", repos = "http://cran.stat.sfu.ca/")
  require(data.table)
}
if (!require(rgdal)) { # for readOGR and various spatial manipulations
  install.packages("rgdal", repos = "http://cran.stat.sfu.ca/")
  require(rgdal)
}
if (!require(broom)) { # for tidy(x) - fortify shp to dataframe
  install.packages("broom", repos = "http://cran.stat.sfu.ca/")
  require(broom)
}
if (!require(ggplot2)) { # for plotting
  install.packages("ggplot2", repos = "http://cran.stat.sfu.ca/")
  require(ggplot2)
}
if (!require(scales)) { # for plotting
  install.packages("scales", repos = "http://cran.stat.sfu.ca/")
  require(scales)
}
if (!require(viridis)) { # for plotting
  install.packages("viridis", repos = "http://cran.stat.sfu.ca/")
  require(viridis)
}
if (!require(maptools)) { # for land map data
  install.packages("maptools", repos = "http://cran.stat.sfu.ca/")
  require(maptools)
}
if (!require(rmapshaper)) { # to simplify map data
  install.packages("rmapshaper", repos = "http://cran.stat.sfu.ca/")
  require(rmapshaper)
}

# Make it pretty
if (!require(showtext)) {
  install.packages("showtext", repos = "http://cran.stat.sfu.ca/")
  require(showtext)
}
font_add_google("Karla", "karla") # Add nice google font
showtext_auto() # Tell R to use showtext to render google font


# 02 LOAD DATA ------------------------------------------------------------

# LORGE cell-by-cell catch by trophic level and year dataset (will take some time to load). 
# MUST do this using fread. Would take literally hours/days to do this with base R. 
# 136.9 million rows. 
catch <- fread('Data/tl_cell_v47.csv')

#SUBSET
y1950 <- catch[year == "1950"]

# Units of pprate are most likely gC/km2/year.
pp <- read.csv('Data/Primary_production/pprate.csv')
names(pp) <- c("x", "y", "cell_id", "pprate")
pp$pprate[pp$pprate==-1] <- 0 # replace -1 PP rows with 0

cells <- read.csv('Data/Cell IDs coordinates and water_area.csv')
cells <- cells[, c("x", "y", "seq")] # we only want x/y/cell_id
names(cells) <- c("x","y","cell_id")

pprdata <- merge(y1950, pp, by = c("cell_id")) # merge catch and pp by cell_id
pprdata <- pprdata[,c("cell_id", "x.x", "y.x", "year", "tl", "sum", "pprate")]
names(pprdata) <- c("cell_id", "x", "y", "year", "tl", "catch", "pprate")

# probably not doing the thing below:
#pprdata <- merge(pprdata, cells, by = c("x", "y"), all=F) # merge resulting pprdata and cells by x, y coordinates to get water area per cell

data(wrld_simpl)
land <- ms_simplify(wrld_simpl, keep = 0.3) # simplify using rmapshaper package
rm(wrld_simpl) # remove big extra dataset

# 03 PPR CALCULATION ------------------------------------------------------

# The formula for PPR is expressed as:
#
# PPR = SUM(i=1 -> n) (Ci/CR) * (1/TE)^(TLi -1) where
#   Ci  = catch of species "i"
#   CR  = conversion rate of wet weight to carbon (9:1)
#   TE  = transfer efficiency between trophic levels (10%)
#   TLi = trophic leevl of species "i"
#   n   = number of species caught in given area (i.e., per cell?)

CR = 9
TE = 0.1
TLi = pprdata$tl
Ci = pprdata$catch

# Might not need to put this into a function yet? haven't decided. 
#PPR <- function(){
#  Ci <- pprdata$catch # this needs to be in the apply thing somehow
#  TLi =  
#  ppr_taxon_year <- (Ci/CR) * (1/TE)^(TLi -1)
#}

pprdata$ppr <- ((Ci/CR) * (1/TE)^(TLi - 1)) * 1000000 # multiply by 1e6 to convert tonnes wet weight carbon to grams carbon! final unit: gC/m2/year.
#I THINK THIS IS WRONG -> ppr1950$ppr_year <- ppr1950$ppr/1000000*365 # divide by 1e6 to convert m2 to km2, multiply by 365 to get per year
ppr1950 <- aggregate(ppr ~ cell_id + x + y + year + pprate, pprdata, sum) # add up all ppr's by cell, + keep x, y, year, pprate columns
ppr1950$ppr_km2 <- ppr1950$ppr/1000000 # divide by 1e6 to convert m2 to km2

#pprdata$ppr_km <- pprdata$ppr/pprdata$water_area # divide ppr per cell by the km2 water area per cell to get ppr per km2 per cell
ppr1950$percentpp <- ppr1950$ppr_km2/ppr1950$pprate * 100
ppr1950 <- ppr1950[ppr1950$percentpp > 0,]

# log everything so we can see it mapped out a bit better.
ppr1950$logppr <- ppr1950$ppr # chuck all ppr into a log column so we can take the log for funsies.
ppr1950$logppr[ppr1950$logppr<1] <- 1 # replace <1 PPR rows with 1 so we can take log
ppr1950$logppr <- log(ppr1950$logppr)


# 04 PLOT -----------------------------------------------------------------

# Basic aesthetics
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Karla", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      # panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      plot.background = element_rect(fill = "gray50"),
      ...
    )
}


# 04-1 PP PLOT ------------------------------------------------------------

# Test plot primary production 
pp_plot <- ggplot() + 
  # plot pprate data as a raster
  geom_raster(data = pp,
              aes(
                x = x,
                y = y,
                fill=pprate)
              ) +
  # plot land on top as a polygon
  geom_polygon(data = land, 
               aes(
                 x = long, 
                 y = lat,
                 group = group
               ),
               fill = "gray80",
               colour = "gray80",
               size = 0.2
               ) +
  # set map aspect ratio, clip to world boundaries
  coord_fixed(
    xlim = c(-180, 180),
    ylim = c(-90, 90)
    ) +
  # Scalebar & fill
  scale_fill_viridis(
    name = "Primary production (gC/km2/year)",
    begin = 0.15, # have color scale skip purple and start at blue
    end = 1
    ) +
  # labels
  labs(
    x = "Latitude",
    y = "Longitude",
    title = "Yearly primary production",
    subtitle = "Averaged over one decade, 1998-2007",
    caption = expression(paste("Data: ", italic("Sea Around Us,"), " 2015"))
    ) +
  # set y axis scale
  scale_y_continuous(
    expand = c(0,0), #removes stupid gap btwn plot & axes
    breaks = seq(-90, 90, 30),
    limits = c(-90, 90)
    ) +
  # set x axis scale
  scale_x_continuous(
    expand = c(0,0), # removes stupid gap btwn plot & axes
    breaks = seq(-180, 180, 30),
    limits = c(-180, 180)
    )
  
plot(pp_plot)


# 04-2 PPR PLOT -----------------------------------------------------------

# Test plot primary production required
ppr_plot <- ggplot() + 
  # plot pprate data as a raster
  geom_raster(data = ppr1950,
              aes(
                x = x,
                y = y,
                fill=ppr_km2)
  ) +
  # plot land on top as a polygon
  geom_polygon(data = land, 
               aes(
                 x = long, 
                 y = lat,
                 group = group
               ),
               fill = "gray80",
               colour = "gray80",
               size = 0.2
  ) +
  # set map aspect ratio, clip to world boundaries
  coord_fixed(
    xlim = c(-180, 180),
    ylim = c(-90, 90)
  ) +
  # Scalebar & fill
  scale_fill_viridis(
    name = "PPR (gC/km2/year)",
    begin = 0.15, # have color scale skip purple and start at blue
    end = 1
  ) +
  # labels
  labs(
    x = "Latitude",
    y = "Longitude",
    title = "Primary production required",
    subtitle = "1950",
    caption = expression(paste(italic("Sea Around Us,"), " 2018"))
  ) +
  # set y axis scale
  scale_y_continuous(
    expand = c(0,0), #removes stupid gap btwn plot & axes
    breaks = seq(-90, 90, 30),
    limits = c(-90, 90)
  ) +
  # set x axis scale
  scale_x_continuous(
    expand = c(0,0), # removes stupid gap btwn plot & axes
    breaks = seq(-180, 180, 30),
    limits = c(-180, 180)
  )

plot(ppr_plot)


# 04-3 LOG(PPR) PLOT ------------------------------------------------------

# Test plot primary production required
ppr_plot <- ggplot() + 
  # plot pprate data as a raster
  geom_raster(data = ppr1950,
              aes(
                x = x,
                y = y,
                fill=logppr)
  ) +
  # plot land on top as a polygon
  geom_polygon(data = land, 
               aes(
                 x = long, 
                 y = lat,
                 group = group
               ),
               fill = "gray80",
               colour = "gray80",
               size = 0.2
  ) +
  # set map aspect ratio, clip to world boundaries
  coord_fixed(
    xlim = c(-180, 180),
    ylim = c(-90, 90)
  ) +
  # Scalebar & fill
  scale_fill_viridis(
    name = "log PPR (gC)",
    begin = 0.15, # have color scale skip purple and start at blue
    end = 1
  ) +
  # labels
  labs(
    x = "Latitude",
    y = "Longitude",
    title = "Primary production required",
    subtitle = "1950",
    caption = expression(paste(italic("Sea Around Us,"), " 2018"))
  ) +
  # set y axis scale
  scale_y_continuous(
    expand = c(0,0), #removes stupid gap btwn plot & axes
    breaks = seq(-90, 90, 30),
    limits = c(-90, 90)
  ) +
  # set x axis scale
  scale_x_continuous(
    expand = c(0,0), # removes stupid gap btwn plot & axes
    breaks = seq(-180, 180, 30),
    limits = c(-180, 180)
  )

plot(ppr_plot)

# 04-3 PPR% PLOT ----------------------------------------------------------

# test plot PPR as percent PP
# hack together a colourbar - taken from https://github.com/blmoore/blogR/blob/master/R/measles_incidence_heatmap.R
cols <- c(colorRampPalette(c("#080616", "#1E0848", "#43006A", "#6B116F", "#981D69",
                             "#E03B50", "#F66B4D"))(5),
          colorRampPalette(c("#FA8657", "#FA8657",
                             "#FBC17D", "#FCDF96","#FCFFB2"),
                           bias=2)(95))

ppr_plot <- ggplot() + 
  # plot pprate data as a raster
  geom_raster(data = ppr1950,
              aes(
                x = x,
                y = y,
                fill=percentpp)
  ) +
  # plot land on top as a polygon
  geom_polygon(data = land, 
               aes(
                 x = long, 
                 y = lat,
                 group = group
               ),
               fill = "gray80",
               colour = "gray80",
               size = 0.2
  ) +
  # set map aspect ratio, clip to world boundaries
  coord_fixed(
    xlim = c(-180, 180),
    ylim = c(-90, 90)
  ) +
  # Scalebar & fill
  scale_fill_gradientn(colours=cols, limits=c(0, 200),
                       breaks=seq(0, 200, by=25), 
                       na.value=rgb(246, 246, 246, max=255),
                       labels=c("0%", "25%", "50%", "75%", "100%", "125%", "150%", "175%", ">200%"),
                       oob = squish, # squish astronomically high %s into scale color
                       guide=guide_colourbar(ticks=T, nbin=50,
                                             label=T,
                                             barwidth=.75)) +
  # labels
  labs(
    x = "Latitude",
    y = "Longitude",
    title = "Primary production required as %age of PP",
    subtitle = "1990-1999",
    caption = expression(paste("TEST DATA: Global average catch, 1990-1999,", italic("Sea Around Us,"), " 2018"))
  ) +
  # set y axis scale
  scale_y_continuous(
    expand = c(0,0), #removes stupid gap btwn plot & axes
    breaks = seq(-90, 90, 30),
    limits = c(-90, 90)
  ) +
  # set x axis scale
  scale_x_continuous(
    expand = c(0,0), # removes stupid gap btwn plot & axes
    breaks = seq(-180, 180, 30),
    limits = c(-180, 180)
  )

plot(ppr_plot)
