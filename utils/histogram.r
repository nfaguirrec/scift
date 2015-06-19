# Usage:
# $ Rscript histogram.r file.dat

args <- commandArgs( trailingOnly=TRUE )
ifileName <- args[1]
rm(args)

X11(width=8.0, height=3.5)

my.csv.data <<-
read.table(
	file=ifileName,
	header=FALSE,
	sep='\n',
	quote='\"',
	dec='.',
	fill=FALSE,
	comment.char="#",
	na.strings="NA",
	nrows=-1,
	skip=0,
	check.names=TRUE,
	strip.white=FALSE,
	blank.lines.skip=TRUE
)

hist(
	my.csv.data[["V1"]],
	breaks=40,
	freq=FALSE,
	labels=FALSE,
	lty="solid",
	density=-1,
	border="black",
	col="red",
	xlab="X",
	ylab="Frequency",
	main="",
	axes=FALSE
)

axis(2)
axis(1, at=seq(-10,10,by=1), tick=TRUE)
#xaxp=c(2, 9, 7)
#axis(2, xaxp=c(2, 9, 7))

box()

Sys.sleep(100)
