##########
# This script was written (Saturday, 17 October 2015) for Savvas to parse sequence and read quality information
# downloaded from the RTSF "Finch" server.
# ***Make sure that you open this script from the directory where you saved your sequence files.***
# Directions:
#   1. Log in to the Finch site and select your sequences
#   2. Click on the (tiny) "Download data from folder" in the top right
#   3. In the "Download Sequences" panel, choose:
#       - Multi-sequence fasta file
#       - non-trimmed
#     ...and click "Export Sequences"
#   4. In the "Download Quality Scores" panel, choose:
#       - Multi-sequence fasta file
#       - non-trimmed
#     ...and click "Export Qual Strings"
#   5. paste the filenames of your saved files (e.g. "savvas_seq.fa" or "MYQUALSOCRES.fasta" )  into the commands
#       below, and write in an output filename where indicated:

        seq <- readLines( "your_sequence_file_here.fa" )
        qual <- readLines( "your_quality_score_file_here.fa" )
        outn <- "your_output_file_here.fa"

#   6. run the script (ctrl-A, then ctrl-enter)
##########

fastq_length <- length( seq )*2
num_seq <- length( seq )/2
fastq <- list( )
fasta <- list( )

label_lines <- seq( 1, fastq_length, 4 )
seq_lines <- seq( 2, fastq_length, 4 )
blank_lines <- seq( 3, fastq_length, 4 )
qual_lines <- seq( 4, fastq_length, 4 )
row_nums <- seq( 1, length( seq ), 2 )

ascii <- c( "!", '"', "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?", "@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[", "\\", "]", "^", "_", "`", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "{", "|", "}", "~" )

make_ascii <- function( x ) {
  inds <- str_split( unlist( x ), " " )
  inds <- as.numeric( unlist( inds ))
  inds <- toString( paste(ascii[inds], coll=""))
  inds <- gsub( " , " ,"", inds  )
  return( noquote( inds ) )
}

for (i in 1:num_seq) {
  fastq[[ label_lines[i] ]] <- seq[ row_nums[i] ]
  fastq[[ seq_lines[i] ]] <- seq[ row_nums[i]+1 ]
  fastq[[ blank_lines[i] ]] <- "+"
  fastq[[ qual_lines[i] ]] <- make_ascii( qual[ row_nums[i]+1 ] )
  }

write.table( unlist( fastq ), "taco", row.names= FALSE, quote= FALSE, col.names= FALSE )
