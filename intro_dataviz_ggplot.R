# import libraries------------------------------------------------------------
library(tidyverse) 
library(ggplot2)

# INTRO DATA VIZ - DATACAMP---------------------------------------------------
  # important to first plot data and check if outliers and extreme values 
    # influence the way data should be correctly plotted

str(mtcars)
structure(mtcars)
colnames(mtcars)
mtcars<-mtcars
view(mtcars)

ggplot(mtcars, aes(cyl, mpg)) +
  geom_point()

# make cyl a factor but this makes no difference
ggplot(mtcars, aes(x=factor(cyl), mpg)) +
  geom_point()

# structure of ggplot2
  # 1. data = the data we are using
  # 2. aesthetics = the scales of the data being mapped; X, Y
  # 3. geometrics = visual elements to represent the data
  # 4. themes = non data appearance
  # 5. stats = adjustments to optimize visualizaton
  # 6. coordinates = where data will be plotted
  # 7. facets = multiple plotting

  
# Edit to add a color aesthetic mapped to disp
ggplot(mtcars, aes(wt, mpg, color = disp)) +
  geom_point()
# or use size to show disp
ggplot(mtcars, aes(wt, mpg, size = disp)) +
  geom_point()
# abov chart returns ERROR because disp is numeric/continuous. 
  # shape= is only useful with categories
ggplot(mtcars, aes(wt, mpg, shape = disp)) +
  geom_point()

# LAYERS / GEOMETRY-----------------------------------------------------------
str(diamonds)
diamonds<-diamonds
colnames(diamonds)

ggplot(diamonds, aes(carat, price))+
  geom_point()+ # scatter
  geom_smooth() # line curve
# add color to separate variables
ggplot(diamonds, aes(carat, price,color=clarity))+
  geom_point(alpha=0.4)+ # alpha for transparency
  geom_smooth() # line curve


# SAVING PLOT AS VARIABLES------------------------------------------------
 # Plots can be saved as variables, which can be added two later on 
  #  using the + operator. This is really useful if you want to make multiple 
   # related plots from a common base.

plt_price_vs_carat <- ggplot(diamonds,aes(carat,price)) # plot to variable
  plt_price_vs_carat+geom_point(alpha=0.2) # add a point later to it
  
plt_price_vs_carat_clarity <- ggplot(diamonds,aes(carat,price)) # plot to variable
plt_price_vs_carat_clarity<-plt_price_vs_carat+geom_point(aes(color=clarity)) # give color to the points
  
  
# AESTHETICS & ATTRIBUTES----------------------------------------------------------------

  # Aesthetics------------------------------------------------------------  
# key aesthetics features
  # x, y = axis
  # fill = fill inside color - usually bars / columns / pies
  # color = color for points / outlines
  # size = radius of points or line thickness
  # alpha = transparency
  # linetype = dash or dotted line
  # labels = axis or titles
  # shape = shape, square, round, triangle
    # 1circle, 2triangle, 3+, 4x,5diamond,6triangle2,7boxx,8*,9diamondx,
    # 10circle+, 40(, 60<, 100d and so on forever

# back to MTCARS dataset
    # first I will make a new column where "cyl" is a factor. "fcyl"
 mtcars$fcyl<-factor(mtcars$cyl) # it's now categorical, not just numbers
  mtcars$fam<-mtcars$am # same for auto
  mtcars$fam<-recode(mtcars$fam,'0'='Automatic','1'='Manual')
  
    
  # COLOR = outside
ggplot(mtcars, aes(wt, mpg,color=fcyl))+ 
  geom_point(shape=1,size=4)

# FILL = inside
ggplot(mtcars, aes(wt, mpg,fill=fcyl))+ 
  geom_point(shape=21,size=4,alpha=0.6)

# BOTH - notice there is a legend to explain - added automatically
ggplot(mtcars, aes(wt, mpg,fill=fcyl,color=fam))+ # FILL = inside
  geom_point(shape=21,size=4,alpha=0.6)

plt_mpg_vs_wt<-ggplot(mtcars,aes(wt,mpg))
  plt_mpg_vs_wt+geom_point(aes(size=fcyl)) # try mapping factor to size
# Warning message: Using size for a discrete variable is not advised.  
  plt_mpg_vs_wt+geom_point(aes(shape=fcyl)) # this works
    # size, alpha - numeric values cannot be defined by a factor/character
  
plt_mpg_vs_wt + geom_label(aes(label = fcyl))
plt_mpg_vs_wt + geom_text(aes(label = fcyl))
# this plots the factor level names! cool. just 2 diff styles

  # Attributes------------------------------------------------------
    # aesthetics and attributes are easly mixed up
     # attributes are always called in geom() 

my_blue <- "#4ABEFF" # even colors can be mapped to variables

ggplot(mtcars, aes(wt, mpg, fill = fcyl)) +
  geom_point(color=my_blue,size=10,shape=1)

# Add text layer with label rownames(mtcars) and color red
ggplot(mtcars, aes(wt, mpg, color = fcyl)) +
  geom_text(label=rownames(mtcars),color="red")

# adding more layers
ggplot(mtcars,aes(mpg,qsec,
                  color=fcyl,
                  shape=fam,
                  size=hp/wt)) + # size is a calculation
  geom_point()

# Modifying aesthetics --------------------------------------------
  # identity, dodge, stack, fill, jitter, jitterdodge, nude

  # identity is the default, it means the value in the data frame is 
    # exactly where the value will be positioned in the plot

  # jitter allows random noise to be added - useful for overplotting
    # or high density data sets

# scale functions
  #scale_x_* or y
  #scale_color_*
  #scale_fill_*
  #scale_shape
  #scale_linetype
  #scale_size

# _* is the 3rd argument which is "CONTINUOUS" or "DISCRETE"
  # CONTINUOUS = number or time series, quantitative
  # DISCRETE = factor, categorical, qualitative

ggplot(iris,aes(Sepal.Length,Sepal.Width,color=Species))+
  geom_point(position="jitter")+
  labs(x="Sepal Length", # labels
       y="Sepal Width",
       color="Species",
       title="Iris plot")
  scale_x_continuous("Sepal.Length",
                     limits=c(2,8), # scale range min max
                     breaks=seq(2,8,3), # tick marks, first, last, interval
                     expand=c(0,0))+ # makes sure data does not land on the axes
  scale_color_discrete("Species")

# with mt cars
ggplot(mtcars, aes(fcyl, fill = fam)) +
    geom_bar(position="dodge") +
    labs(x = "Number of Cylinders", y = "Count") +
  scale_fill_manual("Transmission", values = palette())

ggplot(mtcars, aes(mpg, y=0)) + # I can leave y unplotted and get a default value axis
  geom_jitter()+
  ylim(-2,2) # OR define Y as I like

# BEST PRACTICE --------------------------------------------------

# Who is the audience? 
  # Scientific = exploratory data, clear and comprehensive
  # Reader = inform, simple

# Best choice is efficient (faster than numeric summaries)
  # And accurate - minimize data loss

# Choice depends on variable

ToothGrowth<-ToothGrowth
colnames(ToothGrowth)


#GEOMETRIES----------------------------------------------------

 # 48 different geom() options 

# scatterplot: points, jitter, abline, smooth, count

iris.summary<-iris %>% # summary table to be overlaid later
  group_by(Species) %>% 
  summarise_all(mean) 

# STACKING GEOMS!
ggplot(iris,aes(Sepal.Length,Sepal.Width,col=Species))+
  geom_jitter(shape=1,alpha=0.7) # can do this instrad of geom_point(position=jitter)
  geom_point(data=iris.summary,
             shape=21,
             size=5,
             fill="black",
             stroke=2)
  # here I can add on the means, the x,y are inherited from aes!

# OVERPLOTTING---------------------------------------------------
  
  # Issue with large datasets
  # aligned values on a single axis
  # low precision data
  # integer data
  
  # 1. overplotting large datasets-----------------------------------  
plt_price_vs_carat_clarity + geom_point(shape=".",alpha=0.5)
  # "." makes point size = 1 pixel. nice for high density data
  
  # 2. overplotting aligned values----------------------------------
plt_mpg_vs_fcyl_by_fam <- ggplot(mtcars, aes(fcyl, mpg, color = fam))
  plt_mpg_vs_fcyl_by_fam+
    geom_point(position=position_jitter(width=0.3)) # jitter

  # this makes points even further aligned along the x axis
plt_mpg_vs_fcyl_by_fam+
    geom_point(position=position_jitterdodge # jitter dodge
               (jitter.width = 0.3,dodge.width = 0.3)) 
  
  # 3. overplotting low precision data-----------------------------
  # e.g. iris data which is measured in 1mm differences!
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_jitter(alpha = 0.5,width=0.1) # adjust width
  
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(alpha = 0.5,
             position=position_jitter(width=0.1)) # adjust width

  # 4. overplotting integer data----------------------------------
    # using DataCamp Vocab dataset
    
ggplot(vocab, aes(education, vocabulary)) +
  geom_point()  # just looks like a dotted grid  

ggplot(vocab, aes(education, vocabulary)) +
  geom_jitter(alpha=0.3,shape=1) # jitter is better and shows trend 


# HISTOGRAMS-------------------------------------------------------

# type of bar plot that shoes bin(omial) distribution
  # only 1 variable (x) is needed

ggplot(iris,aes(Sepal.Width))+
  geom_histogram(binwidth=0.1)

# x axis labels are between bars, not on them
# no spaces

# Different positions---------------------------------------------
  # STACK (DEFAULT) 
ggplot(iris,aes(Sepal.Width, fill=Species))+ # variable separation  
  geom_histogram(binwidth=0.1,
                 center=0.05) #cleans up a bit
                
  # DODGE
ggplot(iris,aes(Sepal.Width, fill=Species))+ # variable separation  
  geom_histogram(binwidth=0.1,
                 center=0.05,#cleans up a bit
                 position = "dodge") # gives the better representation

# FILL
ggplot(iris,aes(Sepal.Width, fill=Species))+ # variable separation  
  geom_histogram(binwidth=0.1,
                 center=0.05, #cleans up a bit
                 position = "fill") # gives the better representation

# histograms cut up a continuous variable into discrete bins and, 
  # by default, maps the internally calculated count variable 
  # (the number of observations in each bin) onto the y aesthetic. 
  # An internal variable called density can be accessed by using 
  # the .. notation, i.e. ..density... Plotting this variable will 
  # show the relative frequency, which is the height times the width 
  #of each bin.

ggplot(mtcars,aes(mpg,..density..))+ # shows density on the axis
  geom_histogram(binwidth=1, fill="lightblue")

ggplot(mtcars,aes(mpg,fill=fam))+ # splits the bars colored by "fam"
  geom_histogram(binwidth=1)

ggplot(mtcars,aes(mpg,fill=fam))+ 
  geom_histogram(binwidth=1,position="dodge") # separate colored bars

ggplot(mtcars,aes(mpg,fill=fam))+ 
  geom_histogram(binwidth=1,position="fill") # fills

ggplot(mtcars,aes(mpg,fill=fam))+ 
  geom_histogram(binwidth=1,position="identity",
                 alpha=0.4) # overlays 
