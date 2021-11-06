#++-import libraries-++----
library(tidyverse) 
library(ggplot2)
library(ggthemes)
library(Hmisc)
library(zoo)
library(reshape2)
library(openair)
library(RColorBrewer)
install.packages("quantreg")
install.packages("Hmisc")
install.packages("zoo")
install.packages("reshape2")
install.packages("openair")
install.packages("RColorBrewer")

# Data prep----
mtcars<-mtcars 

#mtcars
# "cyl" is a factor. "fcyl" and am to "fam"
mtcars$fcyl<-factor(mtcars$cyl) # it's now categorical, not just numbers
mtcars$fam<-mtcars$am # same for auto
mtcars$fam<-recode(mtcars$fam,'0'='Automatic','1'='Manual')
mtcars$cars <- rownames(mtcars) # turn car name rows into a col
mtcars$cars <- factor(mtcars$cars) # then a factor
mtcars$fvs <- factor(mtcars$vs)

# vocab
vocab <- read.csv("Vocab.csv")
vocab$year_group2<-factor(vocab$year_group) # for education data

# iris
iris<-iris

# INTERMEDIATE TOPICS----
  # stats
  # coordinates
  # facets
  # best practices

# STATS WITH GEOMS----

  # 2 major types: 1) called within a geom & 2) called independently 

# stats_ is how to call all stats
  # stat_bin() is default for geom_histogram(), geom_freqpoly() 
  # stat_count() is default for geom_bar()
  # stat_smooth() default for geom_smooth()
    # into smooth we can add method="lm" for linear regression, 
        # specify std err. lines with se=TRUE/FALSE
          # by default "lm" models will be split by color (e.g. iris Species) 
            # but fullrange=TRUE can be used to expand estimates from one category across the full range of variables

  # stat_boxplot for geom_boxplot()
  # or for very large datasets
    # stat_bindot() for geom_dotplot
    # stat_bin2d() for geom_bin2d
    # stat_binhex() for geom_hex
    # stat_contour....
    # stat_quantile....
    # stat_csum for geom_count

# * stat_smooth basics----

  # LOESS (default)
mtcars %>% 
  ggplot(aes(wt,mpg))+
  geom_point()+
  geom_smooth() # dy default: method="LOESS" and se=TRUE

  # Linear model
mtcars %>% 
  ggplot(aes(wt,mpg))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)

  # stat_*() instead of geom_*()
mtcars %>% 
  ggplot(aes(wt,mpg))+
  geom_point()+
  stat_smooth(method="lm", se=FALSE) # STAT_SMOOTH gives the same output

  # overall grouped line of best fit 
mtcars %>% 
  ggplot(aes(wt,mpg,color=fcyl))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE)+
  stat_smooth(aes(group=1),method="lm",se=FALSE) 
    # adding this last line adds an overall LOBF, 1 is a dummy variable

# multiple stat_smooth (using LOESS)
mtcars %>% 
  ggplot(aes(wt,mpg))+
  geom_point()+
  stat_smooth(se=FALSE,span=0.9,color="red")+ # larger span = smoother
  stat_smooth(se=FALSE,span=0.6,color="green")+
  stat_smooth(se=FALSE,span=0.3,color="blue") # smaller span = wigglier

?stat_smooth # more info about the arguments
args(stat_smooth)

  # LOESS+lm on same chart
mtcars %>% 
  ggplot(aes(wt,mpg))+
  geom_point()+
  stat_smooth(se=FALSE)+
  stat_smooth(method="lm",se=FALSE)

      # with color categories = LOESS is not good with short datat lines         
mtcars %>% 
  ggplot(aes(wt,mpg,color=fcyl))+
  geom_point()+
  stat_smooth(se=FALSE)+ 
  stat_smooth(method="lm",se=FALSE)

mtcars %>% 
  ggplot(aes(wt,mpg,color=fcyl))+
  geom_point()+
  stat_smooth(aes(color="All"),se=FALSE)+ # but can show "All" together with lm's
  stat_smooth(method="lm",se=FALSE)
# !!! "All" is just a dummy, it could be named anything and still have the same effect

# * modifying stat_smooth()----

# Using Vocab, plot vocabulary vs. education, colored by year group

ggplot(vocab,aes(education,vocabulary,color=year_group2)) +
  geom_jitter(alpha=0.25) +
  stat_smooth(aes(color=year_group2),method="lm",size=2)
# plot many variables at once. 
  # THIS is an extreme example and should never actually be done with such data


# SUM & QUANTILE----

# * stat_quantile()----

  # geom_count & geom_quantile
vocab %>% 
  ggplot(aes(education,vocabulary))+
  geom_jitter(alpha=0.25)+
  stat_quantile(quantiles = c(0.05,0.5,0.95))

?stat_quantile # this requires "quantreg" package

vocab %>% # adding color for year group
  ggplot(aes(education,vocabulary,color=year_group2))+
  geom_jitter(alpha=0.25)+
  stat_quantile(quantiles = c(0.05,0.5,0.95))
# see the quantiles per group! better!

# * stat_sum()----

  # the VOCAB dataset is a great example of overplotting. One solution
  # is using jitter + alpha. SUM is another solution that also has an
  # additional property ..prop.. which displays proportion of values

# with jitter + alpha
vocab %>% 
  ggplot(aes(education,vocabulary))+
  geom_jitter(alpha=0.25)

# with STAT_SUM
vocab %>% 
  ggplot(aes(education,vocabulary))+
  stat_sum()

# adjust range of the scale as per Y-AXIS
vocab %>% 
  ggplot(aes(education,vocabulary))+
  stat_sum()+
  scale_size(range=c(1,10)) 

# adjust range to be ..proportional.. to the dataset
vocab %>% 
  ggplot(aes(education,vocabulary))+
  stat_sum()+
  scale_size(aes(size=..prop..)) # same as the default...

# proportional to the group DO THIS INSIDE stat_sum
vocab %>% 
  ggplot(aes(education,vocabulary,group=education))+
  stat_sum(aes(size=..prop..))


# STATS OUTSIDE GEOMS----

# Hmisc package can summarize mean + - 1 st dev as a vector

smean.sdl(iris.summary$Sepal.Length, mult = 1) # mult = 1 means 1 std dev. can be more

# in ggplot2 "mean_sdl" converts this into a dataframe to be used
  # with the stat_summary(fun.data = ... argument)

mean_sdl(iris.summary$Sepal.Length, mult = 1) 
# in ggplot2:
ggplot(iris,aes(Species,Sepal.Length))+
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult=1))
# it uses geom_pointrange by default

ggplot(iris,aes(Species,Sepal.Length))+
  stat_summary(fun.y = mean,
               geom="point")+
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult=1),
               geom="errorbar", width=0.1)


# Data or chart elements can be prepared outside of the chart as vectors
  # and added laters

# PART 1 create the position objects. These re later called by using
  # position= within geom_*()
posn_j<-position_jitter(width=0.2) # jitter position for later
posn_d<-position_dodge(width=0.1) # dodge
posn_jd<-position_jitterdodge(jitter.width = 0.2,dodge.width = 0.1) # both

# create the base plot
p_wt_vs_fcyl_by_fam <- ggplot(mtcars,aes(fcyl,wt,color=fam))
  # with jitter geom
p_wt_vs_fcyl_by_fam_jit <- p_wt_vs_fcyl_by_fam + geom_jitter()

# PART 2 - apply the positions to the base chart
p_wt_vs_fcyl_by_fam_jit + geom_point(position=posn_j) # jitter only
p_wt_vs_fcyl_by_fam_jit + geom_point(position=posn_d) # dodge only
p_wt_vs_fcyl_by_fam_jit + geom_point(position=posn_jd) # jitterdodge


p_wt_vs_fcyl_by_fam_jit +
  stat_summary(fun.data = mean_sdl, # adding summary stats
               fun.args = list(mult=1),
               geom="errorbar",position = posn_d)

p_wt_vs_fcyl_by_fam_jit +
  stat_summary(fun.data = mean_cl_normal,
               position = posn_d)
  # this chart marks the mean for each category

# COORDINATES LAYER----

# controls dimmension of the plot
  # coord_* functions eg: coord_cartesian() most common

# * zooming in----

iris.smooth <- ggplot(iris,aes(Sepal.Length,Sepal.Width,
                               color=Species))+
  geom_point(alpha=0.7)+
  geom_smooth()
iris.smooth

# I can use this function which will return a WARNING because the limits
# set are a smaller range than the dataset in total, and some data has been
# left out
iris.smooth + scale_x_continuous(limits=c(4.5,5.5)) 
# part of original data has been filtered out, compare to iris.smooth!

iris.smooth + xlim(c(4.4,5.5))
# another alternative, same warning

iris.smooth + coord_cartesian(xlim=c(4.4,5.5))
# COORD CARTESIAN KEEPS DATA!
  # the curves look the same as the original plot!!!!!
iris.smooth


# * aspect ratio----
  # height to width ratio

# this is important for long time series. 
# e.g. 100 years on a plot should probably not be 1:1 aspect ratio
  # meaning x & y should not necessarily be the same size

    # COORD_FIXED

# * practice----

# coord_cartesian / zooming

ggplot(mtcars, aes(x = wt, y = hp, color = fam)) +
  geom_point() +
  geom_smooth()+
  scale_x_continuous(limits=c(3,6)) # this does NOT work too much data loss

ggplot(mtcars, aes(x = wt, y = hp, color = fam)) +
  geom_point() +
  geom_smooth()+
  coord_cartesian(xlim=c(3,6)) # PROPER ZOOM no data loss

# aspect ratios

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  coord_fixed() # adds a fixed 1:1 aspect ratio
                # 1:1 is good when axis have the same scale

# setting ratios
  # prepare some data first SUNPOT dataset + reshape 2 + zoo packages

sunspots.m <- data.frame(year = index(sunspot.month), 
                         value = melt(sunspot.month)$value)
sun_plot <- ggplot(sunspots.m, aes(x = year, y = value)) + 
  geom_line()

?coord_fixed
sun_plot + coord_fixed() # this does not work
sun_plot + coord_fixed(ratio = 1/20) # better ratio. Developments over time clear

# * expand / clip----
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(size = 2) +
  coord_cartesian(expand=0) #sets a buffer between data points and axis lines
  theme_classic()         # 0 brings the axes to the limits of the data
 
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(size = 2) +
  coord_cartesian(expand=0,clip = "off")+ # clip OFF allows data to overlap the axis
  theme_classic()+
    theme(axis.line = element_blank()) # remove axis lines for effect


# COORDINATES vs SCALES----

  # positively skewed data (clustered left)
ggplot(msleep,aes(bodywt,y=1))+
  geom_jitter()+
  scale_x_continuous(limits = c(0,7000),
                     breaks = seq(0,7000,1000))

  # can be transformed to focus on the skew
    # log10 makes skewed data appear more normal
ggplot(msleep,aes(log10(bodywt),y=1))+
  geom_jitter()+
  scale_x_continuous(limits = c(-3,4),
                     breaks = -3:4)+
  # but precision is lost
  annotation_logticks(sides="b") # this helps but lacks context (original values)

# another solution is to have data on a log scale BUT label with original values
ggplot(msleep,aes(bodywt,y=1))+
  geom_jitter()+
  scale_x_log10(limits=c(1e-03,1e+04))

# coord_trans is more flexible, any transformation is possible
?coord_trans
ggplot(msleep,aes(bodywt,y=1))+
  geom_jitter()+
  coord_trans(x="log10") 

# * log transforming scales ----

# Using scale_y_log10() and scale_x_log10() is equivalent to transforming 
  #our actual dataset before getting to ggplot2.
# Using coord_trans(), setting x = "log10" and/or y = "log10" arguments, 
  #transforms the data after statistics have been calculated. 
  # The plot will look the same as with using scale_*_log10(), 
  # but the scales will be different, meaning that we'll see the 
  # original values on our log10 transformed axes. 
  # This can be useful since log scales can be somewhat unintuitive.

# positively skewed data                 
ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  ggtitle("Scale_ functions")

# now adding scale_* functions better plots the data
ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  scale_x_log10()+
  scale_y_log10()+
  ggtitle("Scale_ functions")

# Alternatively.. using coord_trans to specify log10 on both X & Y 
ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  coord_trans(x="log10",y="log10")
  ggtitle("Scale_ functions")
  
# * on a trendline----
  # this works
ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  scale_x_log10()+
  scale_y_log10()+
  ggtitle("Scale_Functions")

  # this does not
ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  coord_trans(x="log10",y="log10")+
  ggtitle("Scale_Functions")

# because the smooth trend line is calculated after scale_* transformations
  # but not after coord_trans transformations!!!!!!!


# DOUBLE & FLIPPED AXIS----
  # double x / y axis is discouraged strongly

# double axis----
airquality <- airquality
# combine dat and month fields
airquality$date <- as.Date(with(airquality, paste(Month, Day,sep="-")), "%m-%d")
airquality$date

ggplot(airquality,aes(date,Temp))+ # temp is in Farenheit
  geom_line()+
  labs(x = "Date (1973)", y = "Fahrenheit")

# Define breaks (Fahrenheit)
y_breaks <- c(59, 68, 77, 86, 95, 104)
y_labels <- (y_breaks)*5/9 # convert F to C, -32 * 5 / 9

# Create a secondary x-axis
secondary_y_axis <- sec_axis(breaks = y_breaks, 
                             labels = y_labels, 
                             trans = identity,
                             name = 'Celsius')

# now with secondary axis
ggplot(airquality,aes(date,Temp))+ # temp is in Farenheit
  geom_line()+
  scale_y_continuous(sec.axis = secondary_y_axis)+
  labs(x = "Date (1973)", y = "Fahrenheit") 
?scale_y_continuous

# flipped axis----

ggplot(mtcars, aes(fcyl,fill=fam))+
  geom_bar(position = position_dodge(width=0.5))+
  coord_flip()

# this charts looks weird and needs to be flipped
ggplot(mtcars, aes(car, wt)) + # car is the brand name
  geom_point() +
  labs(x = "car", y = "weight")+
  coord_flip() # without coord flip x-axis is unreadable

# Polar coordinates----
  # Cartesian (2d) = normal x and y axis
  # Polar coordinates are make modifications to the standard axis

# pie charts----
ggplot(mtcars, aes(x = 1, fill = fcyl)) +
  geom_bar(width = 0.1)+
  # now add
  coord_polar(theta = "y")+
  scale_x_continuous(limits = (0.5:1.5))
# reducing bar width and addind scale_x_cont.. create sa ring plot

# wind rose plots----

ggplot(wind, aes(wd, fill = ws)) +
  geom_bar(width = 1) +
  coord_polar(start = -pi/16) #positions north at the top

# THE FACETS LAYER----
  # concept of small multiples (Edward Tufte)

# split up a plot into smaller plots with the same coordinate system
  # different data sets on each plot for comparisons

# if data are not not encoded as factor variables in the data set, 
  # ggplot2 will coerce variables to factors when used in facets.

# simple, facet grid with 2 variables----
ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() +
  # Facet rows by am and cols by cyl
  facet_grid(rows = vars(am), cols = vars(cyl))

# more complex----
  # create an interaction column where 2 factor columns are combined
#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/interaction
mtcars$fcyl_fam <- interaction(mtcars$cyl,mtcars$fam,sep = ":")
mtcars$fcyl_fam 

# Color the points by fcyl_fam
ggplot(mtcars, aes(wt, mpg, color = fcyl_fam, size = disp)) +
  geom_point() +
  # Use a paired color palette
  scale_color_brewer(palette = "Paired")+
  # Grid facet on gear and vs
  facet_grid(rows = vars(gear), cols = vars(vs))

# formula notation----

ggplot(mtcars, aes(wt, mpg, color = fcyl_fam, size = disp)) +
  geom_point() +
  facet_grid(gear~vs) 
# if only faceting one variable use "." to leave x or y blank
#e.g.
facet_grid(.~vs) 
facet_grid(gear~.) 

# Facet labels and order----
  # FCT_RECODE----
# extremely useful code for renaming factor levels

mtcars$fam <- fct_recode(mtcars$fam,
                         manual = "Manual", # in "..." is original level name
                         auto = "Automatic")
  # FCT_RELEVEL----
# changes the order of levels to a user defined order
mtcars$fam <- fct_relevel(mtcars$fam,
                          c("auto", "manual"))

# Labelling factors
# if factor levels are not clear they can be renamed before plotting
  # to make plots clearer

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  facet_grid(cols=vars(cyl),labeller = label_both) # adds "cyl" to the top row labels
# or
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  facet_grid(cols=vars(vs,cyl),labeller = label_context) 

# where "fam" is the relabelled factor of "am"
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  facet_grid(cols = vars(fam)) # shows manual and auto instead of 0 and 1


# Facet plotting spaces----

# If the data ranges vary wildly between facets, it can be clearer if each facet 
# has its own scale. This is achieved with the scales argument to facet_grid().

# simple factors----           
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() + 
  facet_grid(cols=vars(cyl), scales = "free_x")
# or
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() + 
  facet_grid(rows=vars(cyl), scales = "free_y")

# complex factors----
  # multiple levels and often white space or not values, it is often 
  # desirable to drop the unused factors

ggplot(mtcars, aes(x = mpg, y = cars, color = fam)) +
  geom_point() +
  facet_grid(rows=vars(gear)) # this is BAD!

# remove the blanks
ggplot(mtcars, aes(x = mpg, y = cars, color = fam)) +
  geom_point() +
  facet_grid(rows=vars(gear),
             scales = "free_y",
             space = "free_y") # removes the blank duplicates!

# Facet wrap & margins----

# useful if the plots / variables have different scales but we still want to 
  # show everything together - plots can have their own axis
# and when there are many levels in a group. E.g if a factor has 20 levels
  # with facet_grid it would be 13 rows or cols which is impractical

# wraping many levels----
ggplot(vocab, aes(x = education, y = vocabulary)) +
  stat_smooth(method = "lm", se = FALSE) +
  # Create facets, wrapping by year, using vars()
  facet_wrap(~year, ncol = 8) # ncol = 8 divides up the charts evenly

# margin plots----
ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() +
  # Facet rows by fvs and cols by fam
  facet_grid(rows=vars(fvs,fam),cols=vars(gear),
             margins = TRUE) # adds all possible margins
# this gives an "overall" plot column and row

ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() +
  # Facet rows by fvs and cols by fam
  facet_grid(rows=vars(fvs,fam),cols=vars(gear),
             margins = "fam") # specific levels of fam only

ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() +
  # Facet rows by fvs and cols by fam
  facet_grid(rows=vars(fvs,fam),cols=vars(gear),
             margins = c("gear","fvs")) 

# BAR PLOTS----
  # 2 types of bar plot 1) absolute values 2) distribution (bad)

# bar plots can give wrong impressions because they start at zero.
# point with jitter might often be better to show distribution

# dynamite plots----
  # =  a bar plot with error bars

# Plot wt vs. fcyl
ggplot(mtcars, aes(x = fcyl, y = wt)) +
  # Add a bar summary stat of means, colored skyblue
  stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
  # Add an errorbar summary stat std deviation limits
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", width = 0.1)

# bar plot: position dodge----
# Update the aesthetics to color and fill by fam
ggplot(mtcars, aes(x = fcyl, y = wt, color = fam, fill = fam)) +
  stat_summary(fun = mean, geom = "bar") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1)
# BAD: hard to identify errors and bar data

ggplot(mtcars, aes(x = fcyl, y = wt, color = fam, fill = fam)) +
  stat_summary(fun = mean, geom = "bar",
               alpha = 0.5, position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", position = "dodge", width = 0.1)
# BETTER: adds transparency and dodge (side by side)

# and even better, center the error bar
# Define a dodge position object with width 0.9
posn_d2<-position_dodge(width=0.9) # dodge

# For each summary stat, update the position to posn_d
ggplot(mtcars, aes(x = fcyl, y = wt, color = fam, fill = fam)) +
  stat_summary(fun = mean, geom = "bar", position = posn_d2, alpha = 0.5) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), width = 0.1, 
               position = posn_d2, geom = "errorbar")

# bar plots: aggregated data----

# I can also create an aggregate plot, first needs an aggregate data frame
# using a dataframe with mean, sd and n of wt per cyl
ggplot(mtcars_by_cyl, aes(x = cyl, y = mean_wt)) +
  geom_col(aes(width = prop), fill = "skyblue") +
  geom_errorbar(
    # ... at mean weight plus or minus 1 std dev, width 0.1
    aes(ymin = mean_wt - sd_wt, ymax = mean_wt + sd_wt),width = 0.1)

# HEATMAPS----
  # example
head(barley)
barley<-barley

ggplot(barley,aes(year,variety,fill=yield))+
         geom_tile()+
         facet_wrap(vars(site),ncol = 1)
# this is not effective because there are too many measures. A heatmap would  
  # be better for an overall picture

# DOT PLOT----
ggplot(barley,aes(yield,variety,color=year))+
  geom_point()+
  facet_wrap(vars(site),ncol = 1)
# this is better because differences between years are clearer

# TIME SERIES
ggplot(barley,aes(year,yield,group=variety,color=variety))+
  geom_line()+
  facet_wrap(vars(site),nrow = 1)
# this is easier to see general trends but not as good as dotplot for detail

# practice----

ggplot(barley, aes(year, variety, fill = yield)) +
  geom_tile() + 
  # Facet, wrapping by site, with 1 column
  facet_wrap(vars(site), ncol = 1) +
  # Add a fill scale using an 2-color gradient
  scale_fill_gradient(low = "white", high = "red")

# A palette of 9 reds
red_brewer_palette <- brewer.pal(9, "Reds")
# update the plot
ggplot(barley, aes(year, variety, fill = yield)) +
  geom_tile() + 
  facet_wrap(vars(site), ncol = 1) +
  # Add a fill scale using an 2-color gradient
  scale_fill_gradientn(colors=red_brewer_palette) # note the gradientn

# this is still not easy to read ^^

ggplot(barley,aes(year,yield,
                  group=site, color=variety,fill=site))+
  geom_line()+
facet_wrap(.~site,nrow=1)
# plots side by side

ggplot(barley,aes(year,yield,group=site, color=site,fill=site))+
  stat_summary(fun=mean,geom="line")+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "ribbon", alpha = 0.1, color = NA)
# all in one

# IMPORTANT POINTS----

#colour blindness----
  # avoid red and green on the same plot
  # they can be used together if they have different intensities
  # but generally try to avoid this

# numeric values as factors
  # if numeric data are stored as factors, then the spacing when plotted will be off
  # that can be resolved like this:
df$col <- as.numeric(as.character(df$col))






# end----