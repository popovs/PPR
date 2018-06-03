# Primary Production Required (PPR) footprint maps


# 01 INITIAL SETUP -------------------------------------------------------

# Setup

# Note this part only works if you are in RStudio. If not using RStudio, set wd manually to wherever this script is stored.
# Set working directory to directory this R script is stored in.
set_wd <- function() {
  if (!require(rstudioapi)) {
    install.packages("rstudioapi", repos = "http://cran.utstat.utoronto.ca/")
    require(rstudioapi)
  }
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path))
  print(getwd())
}

set_wd()

if (!require(data.table)) { # quickly read large csv tables
  install.packages("data.table", repos = "http://cran.utstat.utoronto.ca/")
  require(data.table)
}
if (!require(rgdal)) { # for readOGR and various spatial manipulations
  install.packages("rgdal", repos = "http://cran.utstat.utoronto.ca/")
  require(rgdal)
}
if (!require(dplyr)) { # for all sorts of R stuff
  install.packages("dplyr", repos = "http://cran.utstat.utoronto.ca/")
  require(dplyr)
}
if (!require(broom)) { # for tidy(x) - fortify shp to dataframe
  install.packages("broom", repos = "http://cran.utstat.utoronto.ca/")
  require(broom)
}
if (!require(ggplot2)) { # for plotting
  install.packages("ggplot2", repos = "http://cran.utstat.utoronto.ca/")
  require(ggplot2)
}
if (!require(scales)) { # for plotting
  install.packages("scales", repos = "http://cran.utstat.utoronto.ca/")
  require(scales)
}
if (!require(maptools)) { # for land map data
  install.packages("maptools", repos = "http://cran.utstat.utoronto.ca/")
  require(maptools)
}
if (!require(rmapshaper)) { # to simplify map data
  install.packages("rmapshaper", repos = "http://cran.utstat.utoronto.ca/")
  require(rmapshaper)
}

# Make it pretty
if (!require(showtext)) {
  install.packages("showtext", repos = "http://cran.utstat.utoronto.ca/")
  require(showtext)
}
font_add_google("Karla", "karla") # Add nice google font
showtext_auto() # Tell R to use showtext to render google font
if (!require(extrafont)) {
  install.packages("extrafont", repos = "http://cran.utstat.utoronto.ca/")
  require(extrafont)
}
#font_import(pattern = "Karla") # only need to do this once

# 1. custom "not in" function
"%!in%" <- function(x, y) ! ("%in%" (x, y))

# 02 LOAD DATA ------------------------------------------------------------

# LORGE cell-by-cell catch by trophic level and year dataset (will take some time to load). 
# MUST do this using fread. Would take literally hours/days to do this with base R. 
# 136.9 million rows. 
#catch <- fread('Data/tl_cell_v47.csv')
#save(catch, file="catch.Rda")

# LOAD FULL CATCH DATASET IF YOU NEED TO EDIT BASE DATA
#load("catch.Rda") # this will take a minute

# OTHERWISE LOAD PPRMAPS DATASET
load("allmaps.Rda") # this will take a minute

# primary production data
# Units of pprate are most likely gC/m2/day....? I have no clue.
pp <- read.csv('Data/Primary_production/pprate.csv')
pp <- pp[pp$pprate > -1,]
names(pp) <- c("x", "y", "cell_id", "pprate")

# cell data
cells <- read.csv('Data/Cell IDs coordinates and water_area.csv')
cells <- cells[, c("seq", "water_area")] # we only want cell_id/water_area
names(cells) <- c("cell_id", "water_area")

# Land shapefiles for maps
data(wrld_simpl)
land <- ms_simplify(wrld_simpl, keep = 0.3) # simplify using rmapshaper package
rm(wrld_simpl) # remove big extra dataset


# 02-1 Bad cells fix ------------------------------------------------------

# EDIT MAY 31: FIXING MESSED UP 1950-1952 CELLS
# For whateverass reason we have fishing occuring in the middle of the Pacific ocean in 1950, 1951, and 1952. Replacing them with 1953 data until underlying SAU data are fixed. 
# NOTE THAT MEANS THAT THE PPR MAP FUNCTION WILL NOT HAVE THESE CORRECTIONS AS IT PULLS DATA FROM THE ORIGINAL SAU CATCH DATASET.

fix <- read.csv('Data/messed_up_cells_1950-1952.csv', stringsAsFactors = FALSE)
fix <- fix[1]

clean <- allmaps %>% filter(allmaps$cell_id %in% fix$cell_id & allmaps$year == 1953)

fixyears <- 1950:1952

# delete bad cells from bad years
allmaps <- allmaps[!(cell_id %in% fix$cell_id & allmaps$year %in% fixyears)]

# looptidy woop & bind clean
for (i in fixyears){
  clean$year <- i
  allmaps <- rbind(allmaps, clean)
}

rm(i)
rm(fix)
rm(clean)
rm(fixyears)



# 02-2 Blank cells fix ----------------------------------------------------

# Some years are missing cell data, which later causes the ratchet function to fuck up. When a cell disappears and then reappears the next year, the ratchet function for that cell resets. 
# This can be fixed by putting in cells w zero catch across the whole globe for all years. 

blanks <- read.csv('Data/Cell IDs coordinates and water_area.csv')
blanks <- blanks[,c("seq", "x", "y")]
names(blanks) <- c("cell_id", "x", "y")
blanks$year <- 1950
blanks$percentpp <- 0

# Duplicate the blanks dataframe for every year, tack the year onto the end, and append it to the original blanks dataframe.
# for (i in (1951:1955)){
#   df <- blanks
#   df$year <- i
#   blanks <- rbind(blanks, df)
# }

# 02-3 Map aesthetics-------------------------------------------------------


# Basic map aesthetics
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Karla", color = "#22211d"),
      plot.title=element_text(size=25), # Plot title
      plot.subtitle=element_text(size=15), # Plot subtitle
      legend.title=element_text(size=15), # Legend title
      legend.text=element_text(size=12), # Legend items
      axis.title=element_text(size=15), # Axis titles
      axis.text=element_text(size=10), # Axis labels
      axis.title.y=element_text(margin=margin(0,10,0,0)),
      axis.title.x=element_text(margin=margin(10,0,0,0)),
      #axis.line = element_blank(),
      #axis.text.x = element_blank(),
      #axis.text.y = element_blank(),
      #axis.ticks = element_blank(),
      #axis.title.x = element_blank(),
      #axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      # panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(), # infuriatingly doesn't work
      aspect.ratio = 9 / 16, # 16:9 aspect ratio
      ...
    )
}

# hack together a colourbar - taken from https://github.com/blmoore/blogR/blob/master/R/measles_incidence_heatmap.R
cols <- c(colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc",
                             "#4ab04a", "#ffd73e"))(5),
          colorRampPalette(c("#eec73a", "#e29421",
                             "#e29421", "#f05336","#ce472e"),
                           bias=2)(95))

# 03 PPR CALCULATION -----------------------------------------------------

# The formula for PPR is expressed as:
#
# PPR = SUM(i=1 -> n) (Ci/CR) * (1/TE)^(TLi -1) where
#   Ci  = catch of species "i"
#   CR  = conversion rate of wet weight to carbon (9:1)
#   TE  = transfer efficiency between trophic levels (10%)
#   TLi = trophic leevl of species "i"
#   n   = number of species caught in given area (i.e., per cell?)

#allmaps <- list() # create empty allmaps list to fill with map data from below function.
pprmap <- function(yr, savecsv=FALSE, savepng=FALSE) {
  
  # INITIAL SETUP #
  
  flush.console() # allows function to print status updates in console.
  mapdata <- data.table(catch[year == yr]) # pull map data from giant dataset for particular year, make it a datatable for faster calcs.
  print(paste0("Catch data subset for year ", yr," done."))
  
  pprdata <- merge(mapdata, pp, by = c("cell_id"), all=TRUE) # merge w pp
  pprdata <- pprdata[,c("cell_id", "x.y", "y.y", "year", "tl", "sum", "pprate")]
  names(pprdata) <- c("cell_id", "x", "y", "year", "tl", "catch", "pprate")
  
  # Clean rows with NAs
  pprdata$year <- yr
  pprdata$tl[is.na(pprdata$tl)] <- 0
  pprdata$catch[is.na(pprdata$catch)] <- 0
  
  # PPR CALCULATION #
  
  CR = 9
  TE = 0.1
  TLi = pprdata$tl
  Ci = pprdata$catch
  
  pprdata$ppr <- ((Ci/CR) * (1/TE)^(TLi - 1)) * 1000000 # multiply by 1e6 to convert tonnes wet weight carbon to grams carbon! final unit: gC/km2/year. Remember, our catch data are in tonnes per km2 per year.
  
  # using data.table "setDT(x)" to aggregate instead of base R as it's way faster. Will overwrite mapdata w actual mapping stuff (maybe not a good idea? We'll see)
  mapdata <- setDT(pprdata)[, .(ppr = sum(ppr)), by = 'cell_id,x,y,year,pprate'] # add up all ppr's by cell, + keep x, y, year, pprate columns
  
  #tbh I have no idea whats up with these units but in theory.. multiply pprate by 1e6 to get into m2 and by 365 to get into year. Multiply by 100 just so that the percents SOMEHOW match up with the original paper. 
  mapdata$percentpp <- mapdata$ppr/(mapdata$pprate * 100 * 1000000 * 365) * 100
  print("PPR calculations done.")
  
  dfname <- paste0("map",yr)
  allmaps[[dfname]] <<- mapdata # add mapdata to a giant dataframe that contains all map data (for later gif.)
  
  # NOW TO MAP IT #
  
  ppr_plot <- ggplot() + 
    # plot pprate data as a raster
    geom_raster(data = mapdata,
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
    scale_fill_gradientn(
      name = "% PP",
      colours=cols, limits=c(0, 21),
      breaks=c(0, 1, 2, 5, 10, 20), 
      na.value=rgb(246, 246, 246, max=255),
      labels=c("0%", "1%", "2%", "5%", "10%", ">20%"),
      oob = squish, # squish astronomically high %s into scale color
      guide=guide_colourbar(ticks=T, nbin=50,
                            label=T,
                            barheight = 10,
                            barwidth=.75)) +
    # labels
    labs(
      x = "Latitude",
      y = "Longitude",
      title = "Primary production required",
      subtitle = yr,
      caption = expression(paste("Data: ", italic("Sea Around Us,"), " 2018"))
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
    ) +
    theme_map()
  
  plot(ppr_plot) 
  
  # SAVE IT #
  
  if(savecsv){ 
    write.csv(mapdata, file=paste0("Results/Yearly csv data//ppr_",yr,".csv"))
      print("csv saved")
  }
  
  if(savepng){
    ggsave(
      filename = paste0("Results/Yearly maps/ppr_map",yr,".png"),
      plot = ppr_plot,
      width=85 * (14/5), # 85 mm is 1 column width of Frontiers journal
      height = 51 * (14/5), # * 1.6 because of this stupid hacky workaround https://stackoverflow.com/questions/44685354/r-ggplot-ggsave-produces-different-plot-element-sizes-than-simply-exporting-in-r
      device = "png",
      dpi = 300,
      units = "mm"
      )
    print("map saved")
  }
  
  print("NEEEXT!")
}


# 04 LOOP -----------------------------------------------------------------

years <- 1950:2014
lapply(years, pprmap)

# bind allmaps into one large dataframe
#allmaps <- rbindlist(allmaps) # combine allmaps together into giant dataframe.

# Save that shit so you don't have to run this lorgeloop(TM) again
#save(allmaps, file="allmaps.Rda")
#load("allmaps.Rda") # this will take a minute

# When you're done, remove catch to free up some RAM.
rm(catch)


# 05 RATCHET --------------------------------------------------------------

# ratcheting 
ratchetmaps <- allmaps %>%
  group_by(cell_id) %>%
  arrange(year) %>%
  mutate(percentpp = cummax(percentpp)) %>%
  ungroup


# create ratchet map function ratchetmap()

ratchetmap <- function(yr, savecsv=TRUE, savepng=TRUE) {
  
  flush.console() # allows function to print status updates in console.
  mapdata <- ratchetmaps[ratchetmaps$year == yr,]
  print(paste0("Map data for ",yr," gathered."))
  
  ratchet_plot <- ggplot() + 
    # plot pprate data as a raster
    geom_raster(data = mapdata,
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
    scale_fill_gradientn(
      name = "% PP",
      colours=cols, limits=c(0, 21),
      breaks=c(0, 2, 5, 10, 20), 
      na.value=rgb(246, 246, 246, max=255),
      labels=c("0%", "2%", "5%", "10%", ">20%"),
      oob = squish, # squish astronomically high %s into scale color
      guide=guide_colourbar(ticks=T, nbin=50,
                            label=T,
                            barheight = 10,
                            barwidth=.75)) +
    # labels
    labs(
      x = "Latitude",
      y = "Longitude",
      title = "Primary production required",
      subtitle = yr,
      caption = expression(paste("Data: ", italic("Sea Around Us,"), " 2018"))
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
    ) +
    theme_map()
  
  plot(ratchet_plot) 
  
  # SAVE IT #
  
  if(savecsv){ 
    write.csv(mapdata, file=paste0("Results/Ratchet csv data/rat_",yr,".csv"))
    print("csv saved")
  }
  
  if(savepng){
    ggsave(
      filename = paste0("Results/Ratchet maps/rat_map",yr,".png"),
      plot = ratchet_plot,
      width=85 * (14/5), # 85 mm is 1 column width of Frontiers journal
      height = 51 * (14/5), # * 1.6 because of this stupid hacky workaround https://stackoverflow.com/questions/44685354/r-ggplot-ggsave-produces-different-plot-element-sizes-than-simply-exporting-in-r
      device = "png",
      dpi = 300,
      units = "mm"
    )
    print("map saved")
    }

  print("NEEEXT!")
}
  
# loop
lapply(years, ratchetmap)






# 06 GGANIMATE ------------------------------------------------------------


# need the following packages to get started
if (!require(animation)) {
  install.packages("animation", repos = "http://cran.utstat.utoronto.ca/")
  require(animation)
}

# need package devtools installed to use install_github
# install_github("dgrtwo/gganimate")
library(gganimate)


# 06-1 PPR animate --------------------------------------------------------

# first you create the plot, then you animate all the layers within the data.
p <- ggplot() + 
  # plot pprate data as a raster
  geom_raster(data = allmaps,
              aes(
                x = x,
                y = y,
                frame = year, # adding in this line is key for the animation
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
  scale_fill_gradientn(
    name = "% PP",
    colours=cols, limits=c(0, 21),
    breaks=c(0, 1, 2, 5, 10, 20), 
    na.value=rgb(246, 246, 246, max=255),
    labels=c("0%", "1%", "2%", "5%", "10%", ">20%"),
    oob = squish, # squish astronomically high %s into scale color
    guide=guide_colourbar(ticks=T, nbin=50,
                          label=T,
                          barheight = 10,
                          barwidth=.75)) +
  # labels
  labs(
    x = "Latitude",
    y = "Longitude",
    title = "Primary production required",
    subtitle = "",
    caption = expression(paste("Data: ", italic("Sea Around Us,"), " 2018"))
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
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Karla", color = "#22211d"),
    #axis.line = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks = element_blank(),
    #axis.title.x = element_blank(),
    #axis.title.y = element_blank(),
    # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    # panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(), # infuriatingly doesn't work
    plot.margin = unit(c(0,0,0,0), "cm"), # also infuriatingly doesn't work
    aspect.ratio = 9 / 16, # 16:9 aspect ratio
    legend.title=element_text(size=10), # Legend title
    legend.text=element_text(size=8), # Legend items
    axis.title=element_text(size=10), # Axis titles
    axis.text=element_text(size=8) # Axis labels
  )

animation::ani.options(ani.width=1000, ani.height=562, ani.res=300)
gganimate(p, "Results/years-PPR-animation.gif", interval=0.2) # will take a minute or two


# 06-2 Ratchet animate ----------------------------------------------------


# first you create the plot, then you animate all the layers within the data.
p <- ggplot() + 
  # plot pprate data as a raster
  geom_raster(data = ratchetmaps,
              aes(
                x = x,
                y = y,
                frame = year, # adding in this line is key for the animation
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
  scale_fill_gradientn(
    name = "% PP",
    colours=cols, limits=c(0, 21),
    breaks=c(0, 2, 5, 10, 20), 
    na.value=rgb(246, 246, 246, max=255),
    labels=c("0%", "2%", "5%", "10%", ">20%"),
    oob = squish, # squish astronomically high %s into scale color
    guide=guide_colourbar(ticks=T, nbin=50,
                          label=T,
                          barheight = 10,
                          barwidth=.75)
    ) +
  # labels
  labs(
    x = "Latitude",
    y = "Longitude",
    title = "Primary production required in",
    subtitle = "",
    caption = expression(paste("Data: ", italic("Sea Around Us,"), " 2018"))
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
  ) +
  theme_map()


#animation::ani.options(ani.width=2000, ani.height=1124, ani.res=1080)
animation::ani.options(ani.width=1000, ani.height=562, ani.res=300)
gganimate(p, "Results/ratchet-PPR-animation.gif", interval=0.2) # will take a minute or two



# 07 CUMULATIVE CURVES ----------------------------------------------------

ratchetmaps <- merge(ratchetmaps, cells, by=c("cell_id")) # might need to move this line up to the cumulative plots above

# Set bins for percent exploitation level
bins <- c(0, 1, 2, 5, 10, 20, 30, 50, Inf)

# Cut data into bins
ratchetmaps$bins <- cut(
  ratchetmaps$percentpp, 
  breaks = bins, 
  labels = c("<1%", "1-2%", "2-5%", "5-10%", "10-20%", "20-30%", "30-50%", ">50%"),
  right = FALSE
)

# Then aggregate by year and bin, grouping by the length of the cell_id vector (eg., the count of the number of cells in each bin group.)
#cumcount <- aggregate(cell_id ~ bins + year, ratchetmaps, length)
#colnames(cumcount)[3] <- "count"

# Jk, we want water area per exploited cell
cumcount <- aggregate(water_area ~ bins + year, ratchetmaps, sum)

# The above result gives us the count of the number of cells within each bin. However, it doesn't tell us the count of cells with that percent exploitation _or greater_. So, for example, our bins for 1-2% cells don't include the count of any cells with a _higher_ exploitation rate as well.

cumcount <- cumcount[cumcount$bins != "<1%",] # first, let's get rid of cells with less than 1% exploitation. We really don't need that. 
  
gt1_2 <- aggregate(water_area ~ year, cumcount, sum) # for all rows where there is a 1-2% bin, replace the cell area with the sum of all 1-2% or GREATER (which is everything).
gt2_5 <- aggregate(water_area ~ year, cumcount[cumcount$bins != "1-2%",], sum) # for all rows where cell is 2-5% or greater
gt5_10 <- aggregate(water_area ~ year, cumcount[cumcount$bins %!in% c("1-2%", "2-5%"),], sum) # for all rows where cell is 5-10% or greater
gt10_20 <- aggregate(water_area ~ year, cumcount[cumcount$bins %!in% c("1-2%", "2-5%", "5-10%"),], sum) # for all rows where cell is 10-20% or greater
gt20_30 <- aggregate(water_area ~ year, cumcount[cumcount$bins %in% c("20-30%", "30-50%", ">50%"),], sum) # for all rows where cell is 20-30% or greater
gt30_50 <- aggregate(water_area ~ year, cumcount[cumcount$bins %in% c("30-50%", ">50%"),], sum) # for all rows where count is 30-50% or greater

# add the "or greater" column
cumcount[cumcount$bins=="1-2%", "orgreater"] <- gt1_2[2]
cumcount[cumcount$bins=="2-5%", "orgreater"] <- gt2_5[2]
cumcount[cumcount$bins=="5-10%", "orgreater"] <- gt5_10[2]
cumcount[cumcount$bins=="10-20%", "orgreater"] <- gt10_20[2]
cumcount[cumcount$bins=="20-30%", "orgreater"] <- gt20_30[2]
cumcount[cumcount$bins=="30-50%", "orgreater"] <- gt30_50[2]
cumcount[cumcount$bins==">50%", "orgreater"] <- cumcount[cumcount$bins==">50%", "water_area"]

# definitely could've looped all of that but whatever lol

cumeplot <- cumcount[cumcount$bins %in% c("10-20%", "20-30%", "30-50%"),]

# oh boy. 
#cumpal <- c("#0099dc", "#ffd73e", "#f29d1f", "#f16c1f", "#f05336", "#d12c17")

cumpal <- c("#ffd73e", "#f29d1f", "#d12c17")

# Divide by xxx (for axes formatting)
divide1000 <- function(){
  function(x)x/1000
}

# now plot that boi
cumplot <- 
  ggplot(
  data = cumeplot,
    aes(
      x = year, 
      y = orgreater, 
      color = bins,
      group = bins
      #,frame = year # for gganimate
      )
  ) +
  geom_line(
    # aes(cumulative = TRUE # for gganimate
    #    )
  ) +
  scale_y_continuous(
      expand = c(0,0), #removes stupid gap btwn plot & axes
      labels = divide1000() # divide units by 1000 w function specified above
    ) +
  scale_x_continuous(
    expand = c(0,0),
    limits = c(1950, 2014),
    breaks = seq(1950, 2014, 10)
  ) +
  scale_color_manual(
    values = cumpal,
    name = "Percent exploitation",
    labels = c(">= 10%", ">= 20%", ">=30%")
    ) +
  labs(
    x = "Year", 
    y = expression('km'^2 %*% 10^3),
    title = "Cumulative frequency of exploited area"
  ) +
  theme_map() + 
  theme(
    axis.line = element_line(color="black", size=0.2)
  )

ggsave(
  filename = "Results/cumulative_plot.png",
  plot = cumplot,
  width=85 * (14/5), # 85 mm is 1 column width of Frontiers journal
  height = 40 * (14/5), # * 1.6 because of this stupid hacky workaround https://stackoverflow.com/questions/44685354/r-ggplot-ggsave-produces-different-plot-element-sizes-than-simply-exporting-in-r
  device = "png",
  dpi = 300,
  units = "mm"
)

animation::ani.options(ani.width=1004, ani.height=472, ani.res=300)
gganimate(cumplot, "Results/cumulative_plot_animation.gif", interval=0.2) # will take a minute or two




# 08 GLOBAL MEAN PPR --------------------------------------------------------
# creating a table of: year | global_mean_ppr

allmaps <- merge(allmaps, cells, by=c("cell_id")) # might need to move this line up to the cumulative plots above
oceans_km2 = 360000000

allmaps$ppr_km <- allmaps$ppr * allmaps$water_area

global_mean <- aggregate(ppr_km ~ year, allmaps, sum)
global_mean$mean <- global_mean$ppr_km/oceans_km2

# Divide by xxxxxx (for axes formatting)
divide1mil <- function(){
  function(x)x/1000000
}

# PPR gC/km2/year
PPR_legend = expression(
  "Mean PPR" ~ ("gC" %*% "km"^-2 %*% "year"^-1)
)

mean_plot <- ggplot(
  data = global_mean, 
  aes(x=year, y=mean)
  ) + 
  geom_line() + 
  scale_x_continuous(
    expand = c(0,0),
    limits = c(1950, 2014),
    breaks = seq(1950, 2014, 10)
  ) +
  scale_y_continuous(
    expand = c(0,0)
  ) +
  labs(
    x = "Year", 
    y = PPR_legend
  ) +
  theme_map() +
  theme(
    axis.line = element_line(color="black", size=0.2),
    axis.title=element_text(size=11) # Axis titles
    )

ggsave(
  filename = "Results/global_mean_ppr.png",
  plot = mean_plot,
  width=70 * (14/5), # 85 mm is 1 column width of Frontiers journal
  height = 40 * (14/5), # * 1.6 because of this stupid hacky workaround https://stackoverflow.com/questions/44685354/r-ggplot-ggsave-produces-different-plot-element-sizes-than-simply-exporting-in-r
  device = "png",
  dpi = 300,
  units = "mm"
)

# S1 MISC. PLOT -----------------------------------------------------------------


# S1-1 PP PLOT ------------------------------------------------------------

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


# S1-2 PPR PLOT -----------------------------------------------------------

# Test plot primary production required
ppr_plot <- ggplot() + 
  # plot pprate data as a raster
  geom_raster(data = ppr1950,
              aes(
                x = x,
                y = y,
                fill=ppr)
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




# S1-3 PPR% PLOT ----------------------------------------------------------

# test plot PPR as percent PP
# hack together a colourbar - taken from https://github.com/blmoore/blogR/blob/master/R/measles_incidence_heatmap.R
# cols <- c(colorRampPalette(c("#080616", "#1E0848", "#43006A", "#6B116F", "#981D69",
#                              "#E03B50", "#F66B4D"))(2),
#           colorRampPalette(c("#FA8657", "#FA8657",
#                              "#FBC17D", "#FCDF96","#FCFFB2"),
#                            bias=2)(98))

cols <- c(colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc",
                             "#4ab04a", "#ffd73e"))(5),
          colorRampPalette(c("#eec73a", "#e29421",
                             "#e29421", "#f05336","#ce472e"),
                           bias=2)(95))

ppr_plot <- ggplot() + 
  # plot pprate data as a raster
  geom_raster(data = allmaps[allmaps$year==1988,],
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
  scale_fill_gradientn(
                      name = "% PP",
                      colours=cols, limits=c(0, 21),
                      breaks=c(0, 2, 5, 10, 20), 
                      na.value=rgb(246, 246, 246, max=255),
                      labels=c("0%", "2%", "5%", "10%", ">20%"),
                      oob = squish, # squish astronomically high %s into scale color
                      guide=guide_colourbar(ticks=T, nbin=50,
                                             label=T,
                                             barheight = 10,
                                             barwidth=.75)) +
  # labels
  labs(
    x = "Latitude",
    y = "Longitude",
    title = "Primary production required",
    subtitle = "1988",
    caption = expression(paste("Data: ", italic("Sea Around Us,"), " 2018"))
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
  ) +
  theme_map()

plot(ppr_plot)

ggsave(
  filename = "test.png",
  plot = ppr_plot,
  width=85 * (14/5), # 85 mm is 1 column width of Frontiers journal
  height = 51 * (14/5), # * 1.6 because of this stupid hacky workaround https://stackoverflow.com/questions/44685354/r-ggplot-ggsave-produces-different-plot-element-sizes-than-simply-exporting-in-r
  device = "png",
  dpi = 300,
  units = "mm"
)

# S1-4 LOG(PPR) PLOT --------------------------------------------------------

# # log everything so we can see it mapped out a bit better.
ppr1950$logppr <- ppr1950$ppr # chuck all ppr into a log column so we can take the log for funsies.
ppr1950$logppr[ppr1950$logppr<1] <- 1 # replace <1 PPR rows with 1 so we can take log
ppr1950$logppr <- log(ppr1950$logppr)

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
