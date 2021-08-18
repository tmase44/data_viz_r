#++-import libraries-++----
library(tidyverse) 
library(ggplot2)
library(ggthemes)
library(Hmisc)
library(zoo)
library(reshape2)
library(openair)
install.packages("quantreg")
install.packages("Hmisc")
install.packages("zoo")
install.packages("reshape2")
install.packages("openair")

# Data prep----
mtcars<-mtcars 

# "cyl" is a factor. "fcyl" and am to "fam"
mtcars$fcyl<-factor(mtcars$cyl) # it's now categorical, not just numbers
mtcars$fam<-mtcars$am # same for auto
mtcars$fam<-recode(mtcars$fam,'0'='Automatic','1'='Manual')
vocab <- read.csv("Vocab.csv")
vocab$year_group2<-factor(vocab$year_group) # for education data
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





             