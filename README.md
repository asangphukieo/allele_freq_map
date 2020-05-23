# allele_freq_map
R script for visualising allele frequency on world map 

![Output_img](https://github.com/asangphukieo/allele_freq_map/blob/master/example_output/out_file.svg)


###### Usage  ######
### create and activate conda mapplot_r and run the script as follows
1. conda env create -f mapplot_r.yml
2. conda activate mapplot_r
3. Rscript script.R ./input_file/test_data.txt ./countries.csv 4 out_file

#countries.csv contains country names, and their latitute and longitute<br>
#4 is number of allele type<br>
#out_file is output file label<br>
#other parameters can be adjusted in script.R<br>

#reference source:https://github.com/mfmakahiya/Map-plot
