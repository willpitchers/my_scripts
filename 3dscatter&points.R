
# I forget which function you were using... I think it was persp, but
# god knows how to make persp do the things you want... 
# It is do-able with 'cloud' though - NB: 'wireframe' is just a wrapper
# for 'cloud' and it's in 'lattice' so you've probably got it already...

library(lattice)

# The code you sent me was pretty useless without the data, so 
# ...let's make up some simplified data


x <- seq(0, 10, .5)
y <- seq(0, 10, .5)
myGrid <- data.frame(expand.grid(x,y))
colnames(myGrid) <- c("x","y")

# this represents your fitted surface...
myGrid$z <- myGrid$x + myGrid$y^2

# now I'm going to add some noise so that you can see the points more easily...
noise <- rnorm(length(myGrid$z), 0, 2)
myGrid$z2 <- myGrid$x + myGrid$y^2 + noise


# this will give you a surface:
wireframe(myGrid$z ~ myGrid$x * myGrid$y, xlab="X", ylab="Y", zlab="Z")

# ..or this will give you a scatterplot:
cloud(myGrid$z2 ~ myGrid$x * myGrid$y, xlab="X", ylab="Y", zlab="Z")


# to combine them you need to dick with the guts of 'cloud' a little bit...
# 'panel.cloud' & 'panel.wireframe' are function (similar to 'par') that are
# there to allow you to do this sort of tinkering, but you have to set up
# the call to them as a function - this basically tells the plotting function
# to insert YOUR x, y, z values into both the wireframe and cloud sub-functions,
# but to leave the rest alone: the '...'s need to be there for this!

mypanel <- function(x, y, z, z2,...) {
  panel.wireframe( x, y, z,...)
  panel.cloud( x, y, z2, ...)
}

# then call it from with your plot command, thus:

wireframe(z ~ x * y, data=myGrid, xlab="X", ylab="Y", zlab="Z",
          panel=mypanel, z2=myGrid$z2)
          
# NB: for this bit ^ 'cloud(...' and 'wireframe(...' are interchangeable,
# because your 'mypanel' function is going to hijack their defaults anyway.
# At this stage would be where you can change your settings to amke pretty etc.

cloud(z ~ x * y, data=myGrid, xlab="X", ylab="Y", zlab="Z",
          panel=mypanel, z2=myGrid$z2, scales = list(distance = rep(1, 3), arrows = FALSE),
          col=c("red","black"), drape=T, colorkey=F)


