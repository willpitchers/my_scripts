#!

# script to look up nearest gene in Drosophila melanogaster genome from SNP no.

# bits
# fix SNP ID -
# use SNP ID as lookup value in reference file
# return relevant lines from reference file

geneID=$1

# make sure that reference file is present
if [ ! -f gene_map_table_fb_2014_06.tsv ]
	then
	wget ftp://ftp.flybase.net/releases/FB2014_06/precomputed_files/genes/gene_map_table_fb_2014_06.tsv.gz
	gunzip gene_map_table_fb_2014_06.tsv.gz
fi

# DeMarquez-ize the geneID
if test ${geneID:0:2} = "10"
	then
	chromo=X
elif test ${geneID:0:2} = "21"
	then
	chromo=2L
elif test ${geneID:0:2} = "22"
	then
	chromo=2R
elif test ${geneID:0:2} = "31"
	then
	chromo=3L
elif test ${geneID:0:2} = "32"
	then
	chromo=3R
else
	then
	echo "error – check your geneID"
fi

SNP=${geneID:2:10}

#
awk -F "\"*\t\"*" '{print $5}' gene_map_table_fb_2014_06.tsv

awk '{chromo=chromo} {SNP=SNP} {string=$5} {str1=${string%%;*} \
	 { if (str1=chromo) {Yoffset=0} else {Yoffset=$3+240} } \
		{Coords="632x480+0+"Yoffset} {system("mogrify -crop " Coords " " Image)}' landmarks.asc

