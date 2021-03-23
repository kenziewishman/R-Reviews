---

title: "R-Assignment-BCB546-Spring2021-Topping"

author: "Nick Topping"

date: "3/17/2021"

output: html_document

---



```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE)

```



## R Assignment 



To start, create a repository on GitHub.

My repository is named R-Assignment-BCB546-Spring2021-Topping.

To load the working repository into R Studio:



1. Clone the repository to your clipboard

2. Open R Studio and click File, New Project, Version Control, Git

3. Paste the repository URL, fill in the directory name and click Create Project



Then, install and load any R packages needed for this assignment. You will need `tidyverse`, `ggplot2`, and `stringr`. Use `install.packages("package")` and load it into your library if not installed



``` {r}

library(tidyverse)

library(ggplot2)

library(tidyr)

library(dplyr)

library(stringr)

```



### Data Extraction and Modification



Next, open both data files, `fang_et_al_genotypes` and `snp_position.txt`.

Both of these files can be found in the BCB-546-Spring2021 github repository, with the links below in the code. Transform the files into a data frame with the code below:



``` {r}

fang_et_al_genotypes <- read.table("https://raw.githubusercontent.com/EEOB-BioData/BCB546-Spring2021/master/assignments/UNIX_Assignment/fang_et_al_genotypes.txt", header = TRUE,)

snp_position <- read.table("https://raw.githubusercontent.com/EEOB-BioData/BCB546-Spring2021/master/assignments/UNIX_Assignment/snp_position.txt", header = TRUE, sep = "\t")

```



To quickly look at the data files, double click on them in the Environment. This will show the data in a .txt file that you can see.



Note: You can also use the function `view(fang_et_al_genotypes)` to show the data, but it isn't working in my markdown file for some reason...



### Below are a few codes you can use to inspect the data



`object.size()` shows the file size of the data frame



`nrow()` shows the number of rows



`ncol()` shows the number of columns



`dim()` shows the dimensions of the data frame by row and column



`head()` show first 6 rows



`tail()` shows last 6 rows



`str()` shows the structure of the data frame: the name, type and preview of data in each column



`names()` shows the names attributes for the data frame, which are also the column names



The function `colnames()` also will show the column names



`sapply(dataframe, class)` shows the class of each column of data



### fang_et_al_genotypes



``` {r eval=FALSE}

object.size(fang_et_al_genotypes)

```



``` {r}

nrow(fang_et_al_genotypes)

```



``` {r eval = FALSE}

ncol(fang_et_al_genotypes)

```



``` {r}

dim(fang_et_al_genotypes)

```



Note: The output for functions `head()` `tail()` `str()` `names()` and `sapply(dataframe, class)` are not included in this markdown file for this data table as these outputs are very long and clutter the console



### snp_position



``` {r}

object.size(snp_position)

```



``` {r}

nrow(snp_position)

```



``` {r}

ncol(snp_position)

```



``` {r}

dim(snp_position)

```



``` {r}

str(snp_position)

```



``` {r}

names(snp_position)

```



``` {r}

sapply(snp_position, class)

```



For this data table, only `head()` and `tails()` were left out of the snp_position analyses



## Data Processing



Create two new data frames sorted by the ID's of both species from fang_et_al_genotypes with the following code:



``` {r}

Maize_IDs <- fang_et_al_genotypes[which(fang_et_al_genotypes$Group == "ZMMIL" | fang_et_al_genotypes$Group == "ZMMLR" | fang_et_al_genotypes$Group == "ZMMMR"),]

Teosinte_IDs <- fang_et_al_genotypes[which(fang_et_al_genotypes$Group == "ZMPBA" | fang_et_al_genotypes$Group == "ZMPIL" | fang_et_al_genotypes$Group == "ZMPJA"),]

```



Then pull columns 1, 3 and 4 from the file snp_position and combine them into one



``` {r}

SNP_Chrom_Pos <- snp_position[,c(1,3,4)]

```



The next step is to combine the data files so that the first column is "SNP_ID", second is "Chromosome", third is "Position", and the rest are genotype data. In order to do this, `Maize_IDs` and `Teosinte_IDs` must be transposed, THEN combined



``` {r}

Maize_IDs_Transposed <- as.data.frame(t(Maize_IDs[,-1]))

colnames(Maize_IDs_Transposed) <- Maize_IDs$Sample_ID

Teosinte_IDs_Transposed <- as.data.frame(t(Teosinte_IDs[,-1]))

colnames(Teosinte_IDs_Transposed) <- Teosinte_IDs$Sample_ID

```



``` {r}

Maize_IDs_Transposed <- cbind(SNP_ID = row.names(Maize_IDs_Transposed), Maize_IDs_Transposed)

Teosinte_IDs_Transposed <- cbind(SNP_ID = row.names(Teosinte_IDs_Transposed), Teosinte_IDs_Transposed)

```



To combine the transposed data frames with the first file `SNP_Chrom_Pos` merge the files by SNP_ID



Combine the transposed data frames with the previous file `SNP_Chrom_Pos` followed by either maize or teosinte #note that the rows should stay in the same order



``` {r}

Maize_Chromosomes <- merge(SNP_Chrom_Pos,Maize_IDs_Transposed, by = "SNP_ID")

Teosinte_Chromosomes <- merge(SNP_Chrom_Pos,Teosinte_IDs_Transposed, by = "SNP_ID")

```



``` {r}

Maize_Chromosomes[Maize_Chromosomes=="NA"] <- "?/?"

Teosinte_Chromosomes[Teosinte_Chromosomes=="NA"] <- "?/?"

```



Sort both new files by position with the `order()` function



``` {r}

Maize_Chromosomes_Ascending <- Maize_Chromosomes[order(suppressWarnings(as.numeric(Maize_Chromosomes$Position))), ]

Teosinte_Chromosomes_Ascending <- Teosinte_Chromosomes[order(suppressWarnings(as.numeric(Teosinte_Chromosomes$Position))), ]

Maize_Chromosomes_Descending <- Maize_Chromosomes[order(suppressWarnings(as.numeric(Maize_Chromosomes$Position, decreasing = T))), ]

Teosinte_Chromosomes_Descending <- Teosinte_Chromosomes[order(suppressWarnings(as.numeric(Teosinte_Chromosomes$Position, decreasing = T))), ]

```



### Create a function to make 10 files in one command chunk (10 for Maize and Teosinte, ascending and descending order)



``` {r}

Filter_By_Chromosome <- function(x, df){

  df %>% filter(Chromosome == x)

}

Maize_Ascending_By_Chromosome <- 

lapply(sort(unique(Maize_Chromosomes_Ascending$Chromosome)), Filter_By_Chromosome, Maize_Chromosomes_Ascending)

invisible(lapply(Maize_Ascending_By_Chromosome, function(x) write.csv(x, file = paste0("Maize_Ascending_Chromosome_", x$Chromosome[1]))))

```



``` {r}

Maize_Descending_By_Chromosome <- 

lapply(sort(unique(Maize_Chromosomes_Descending$Chromosome)), Filter_By_Chromosome, Maize_Chromosomes_Descending)

invisible(lapply(Maize_Descending_By_Chromosome, function(x) write.csv(x, file = paste0("Maize_Descending_Chromosome_", x$Chromosome[1]))))

```



``` {r}

Teosinte_Ascending_By_Chromosome <- 

lapply(sort(unique(Teosinte_Chromosomes_Ascending$Chromosome)), Filter_By_Chromosome, Teosinte_Chromosomes_Ascending)

invisible(lapply(Teosinte_Ascending_By_Chromosome,function(x) write.csv(x, file = paste0("Teosinte_Ascending_Chromosome_", x$Chromosome[1]))))

```



``` {r}

Teosinte_Descending_By_Chromosome <- 

lapply(sort(unique(Teosinte_Chromosomes_Descending$Chromosome)), Filter_By_Chromosome, Teosinte_Chromosomes_Descending)

invisible(lapply(Teosinte_Descending_By_Chromosome,function(x) write.csv(x, file = paste0("Teosinte_Descending_Chromosome_", x$Chromosome[1]))))

```



# Plots



Transform the data so that both position and chromosome are numeric



``` {r warning=FALSE}

Maize_Chromosomes_Numeric <- transform(Maize_Chromosomes, Chromosome =as.numeric(Chromosome), Position=as.numeric(Position))

Teosinte_Chromosomes_Numeric <- transform(Teosinte_Chromosomes, Chromosome =as.numeric(Chromosome), Position=as.numeric(Position))

```



### Plot the number of SNP's on each chromosome from the `fang_et_al_genotypes` dataset



Tidy up the data using the `pivot_longer()` function. Then merge the pivoted data with the SNP position data and make the data table numeric by chromosome.



``` {r}

fang_pivot <- fang_et_al_genotypes %>% pivot_longer(!c(Sample_ID, JG_OTU, Group), names_to="SNP_ID", values_to= "NT")

merge_fang_pivot <- merge(fang_pivot, snp_position, by="SNP_ID")

merge_fang_pivot_numeric <- merge_fang_pivot[!is.na(as.numeric(merge_fang_pivot$Chromosome)),]

```



This code below gives the total number of SNPs per chromosome for both Maize and Teosinte



```{r}

merge_fang_pivot_numeric %>% 

  select(SNP_ID, Chromosome, Position) %>%

  drop_na() %>% 

  ggplot()+

  geom_bar(mapping = aes(as.numeric(Chromosome)), color="red", fill="red")+ ggtitle("Number of SNPs per Chromosome (Maize and Teosinte)") + labs(x = "Chromosome", y = "Number of SNPs")

```



### Plot the distribution of SNPs along each chromosome 



``` {r warning=FALSE}

ggplot(data = merge_fang_pivot_numeric) + geom_bar(mapping = aes(as.numeric(Chromosome), fill=Group)) + scale_x_discrete(limit=c(1:10)) + labs(x = "Chromosome", y="SNumber of SNPs by group") + ggtitle("Distribution of SNPs along Chromosomes distributed by group") 

```



Add a new column to the `merge_fang_pivot_numeric` data frame called Zygosity



``` {r}

merge_fang_pivot_numeric$Zygosity <- "Heterozygous"

```



Replace all `"?/?"` with `Missing Data` in the Zygosity column



``` {r}

merge_fang_pivot_numeric$Zygosity[merge_fang_pivot_numeric$NT == "?/?"] <- "Missing Data"

```



For every "A/A", "C/C", "G/G", "T/T" site replace the value in the Zygosity column with `Homozygous`



``` {r}

merge_fang_pivot_numeric$Zygosity[merge_fang_pivot_numeric$NT %in% c("A/A", "C/C", "G/G", "T/T")] <- "Homozygous"

```



Plot the data of the proportion of heterozygous and homozygous sites, and missing data sites by Sample ID



``` {r warning=FALSE}

ggplot(data = merge_fang_pivot_numeric) + geom_bar(mapping=aes(x = Sample_ID, fill = Zygosity), position = "fill") + labs(x = "Sample_ID", y="Proportion") + ggtitle("Proportion of Heterozygous and Homozygous sites (with missing data)")

```

Plot the data of the proportion of heterozygous and homozygous sites, and missing data sites by group



``` {r warning=FALSE}

ggplot(data = merge_fang_pivot_numeric) + geom_bar(mapping = aes(x = Group, fill = Zygosity), position = "fill") + labs(x = "Group", y="Proportion") + ggtitle("Proportion of Heterozygous and Homozygous sites (with missing data) by group")

```                                                  



## My own data visualization

   

I decided to plot the total number of SNP's per Chromosome

   

```{r}

   ggplot(data = merge_fang_pivot_numeric) + geom_bar(mapping=aes(x = Chromosome, fill=Zygosity), position="dodge") + labs(x = "Chromosome", y="Number of SNPs") + ggtitle("Amount of Heterozygous and Homozygous individuals including missing data")

```

   
##Density plots
maize_density <- ggplot(maize_combined) + geom_density(aes(x=Position, fill = Chromosome))
ggsave("maize_SNP_density.png", plot = maize_density)
teosinte_density <- ggplot(teosinte_combined) + geom_density(aes(x=Position, fill = Chromosome))
ggsave("teosinte_SNP_density.png", plot = teosinte_density)
maize_density <- ggplot(maize_combined) + geom_density(aes(x=Position, fill = Chromosome)) + facet_wrap(~Chromosome)
ggsave("maize_SNP_density_f.png", plot = maize_density)
teosinte_all_density <- ggplot(teosinte_combined) + geom_density(aes(x=Position, fill = Chromosome)) + facet_wrap(~Chromosome)
ggsave("teosinte_SNP_density_f.png", plot = teosinte_density)

##Dot plot of position on Chromosomes
maize_dot <- ggplot(maize_combined, aes(x=Position, y=Chromosome, fill= Chromosome)) + geom_point()
ggsave("SNP_Position_dot_plot_maize.png", plot = maize_dot)
teosinte_dot <- ggplot(teosinte_combined, aes(x=Position, y=Chromosome, fill= Chromosome)) + geom_point()
ggsave("SNP_Position_dot_plot_teosinte.png", plot = teosinte_dot)
