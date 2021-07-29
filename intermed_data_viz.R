#++-import libraries-++----
library(tidyverse) 
library(ggplot2)
library(ggthemes)
library(Hmisc)
install.packages("quantreg")
install.packages("Hmisc")

# Data prep----
mtcars<-mtcars 

# "cyl" is a factor. "fcyl" and am to "fam"
mtcars$fcyl<-factor(mtcars$cyl) # it's now categorical, not just numbers
mtcars$fam<-mtcars$am # same for auto
mtcars$fam<-recode(mtcars$fam,'0'='Automatic','1'='Manual')

vocab$year_group2<-factor(vocab$year_group) # for education data

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




                 

