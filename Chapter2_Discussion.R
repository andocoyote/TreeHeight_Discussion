require(mosaic) # Required for favstats(...)

# Plot the histogram for 'data' with the graph parameters provided
create_plot <- function(data, tree_height, xmin, xmax, ymin, ymax)
{
    # Breaks: total number of breaks between all of the bars
    # las: text direction for y-axis label
    # xaxp: from marks param1 to param2, give me param3 ticks on the x-axis
    # yaxp: from marks param1 to param2, give me param3 ticks on the y-axis
    hist(data,
         main=paste("Histogram of", tree_height, "ft trees"), 
         xlab="Tree height",
         ylab="Count of trees",
         border="blue", 
         col="green",
         labels=TRUE,
         xlim=c(xmin, xmax),
         xaxp=c(xmin, xmax, 16),
         ylim=c(ymin, ymax),
         yaxp=c(ymin, ymax, ymax/2),
         las=1, 
         breaks=16)
}

# Generate min, max, median, percentiles, mean, and standard deviation
create_stats <- function(data, ymin, ymax)
{
    # Draw the grey dotted line every 5 counts up the y-axis
    abline(h=seq(ymin, ymax, 1), col="grey", lty="dotted")
    
    # Calculate min, max, mean, Q1, median, Q3, SD, n
    print(favstats(data))
    
    # Check if SD is 75% of IQR
    cpi_sd <- sd(data)
    cpi_iqr <- IQR(data)
    cat("SD / IQR: ", cpi_sd / cpi_iqr, "\n")
    
    # Check if mean and median are similar
    cat("Mean and median: ", mean(data), ",", median(data), "\n")
}

groupA <- rnorm(n=100, m=5.0, sd=0.50)
groupB <- rnorm(n=100, m=25, sd=0.5)

# Create plot and stats for groupA: 5ft trees
create_plot(groupA, 5, 3, 7, 0, 24)
create_stats(groupA, 0, 24)

# Create plot and stats for groupB: 25ft trees
create_plot(groupB, 25, 23, 27, 0, 24)
create_stats(groupB, 0, 24)


