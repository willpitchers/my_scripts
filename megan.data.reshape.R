# OK - here's how this is going to work: first we make an empty list...

new.megan  <- list( )

# then this for loop is going to add a mini-dataframe to the list for each row of the original dataframe.

for ( i in seq(nrow( megan )) ) {
	working.row <- as.vector(megan[i,])
	running.no.ind <- as.numeric( working.row[ length(working.row) ] )
	new.megan[[i]] <- data.frame( matrix(NA, nrow=running.no.ind, ncol=ncol(megan) ) )
	new.megan[[i]][,1] <- rep( working.row[1], running.no.ind )
	new.megan[[i]][,2] <- rep( working.row[2], running.no.ind )
	new.megan[[i]][,3] <- rep( working.row[3], running.no.ind )

# to make this work for your real data, you'll need to change references to "megan" to the name of your real data and add a line at the end for each column in your real data
}

# after you run the loop then 'do.call' turns the list into a new dataframe

new.megan.data  <- data.frame( do.call("rbind", new.megan) )

# then these last bits are all just formatting

colnames( new.megan.data ) <- colnames( megan )

new.megan.data$choice <- sub( "sugar", 1, new.megan.data$choice )
new.megan.data$choice <- sub( "salt", 0, new.megan.data$choice )
new.megan.data$choice <- sub( "both", 0.5, new.megan.data$choice )

# ...and this strips off the last column, which is now unecessary
new.megan.data <- new.megan.data[, -ncol(new.megan.data) ]
