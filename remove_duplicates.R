
# remove duplicates ending with a specific suffix, remove oldest one and rename file without suffix

setwd("C:/Users/Alessandro Bitetto/Downloads/test")
duplicate_suffix = " -TEST_SUFFIX"


library(tools)
library(data.table)
library(dplyr)
library(tidyverse)

file_list = list.files(full.names = T, recursive = F, include.dirs = F)

final_df = file.info(file_list) %>%
  rownames_to_column("path") %>%
  filter(isdir == F) %>%
  mutate(filename = basename(path),
         dirname = dirname(path),
         fileext = file_ext(filename),
         fileroot = file_path_sans_ext(path) %>% gsub("./", "", .),
         fileroot_nosuffix = gsub(duplicate_suffix, "", fileroot),
         duplic = grepl(duplicate_suffix, fileroot),
         newpath = paste0(dirname, "/", fileroot_nosuffix, ".", fileext)) %>%
  group_by(fileroot_nosuffix) %>%
  mutate(isdupl_check = n()) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate(isdupl = ifelse(isdupl_check == 2, T, F))

if (max(final_df$isdupl_check) > 2){cat("\n ###### error in isdupl_check")}

# single files
single_files = final_df %>%
  filter(isdupl_check == 1)
if (nrow(single_files) > 0){
  cat("\nSingle files:\n")
  print(single_files %>% select(path, mtime, filename, dirname))
}

# duplicated files
final_df = final_df %>%
  filter(isdupl_check == 2) %>%
  filter(!fileroot_nosuffix %in% exclude)

# exclude duplicates with different file extension
final_df = final_df %>%
  group_by(fileroot_nosuffix) %>%
  mutate(number_ext = uniqueN(fileext)) %>%
  filter(number_ext == 1)

# check if latest modified date of duplicated is after "original" file
error_file = c()
for (file in unique(final_df$fileroot_nosuffix)){
  
  original_ctime = final_df %>%
    filter(fileroot_nosuffix == file & duplic == F) %>%
    pull(ctime)
  dupl_ctime = final_df %>%
    filter(fileroot_nosuffix == file & duplic == T) %>%
    pull(ctime)
  
  if (original_ctime > dupl_ctime){error_file = c(error_file, file)}
  
}
if (length(error_file) > 0){cat("\n ###### error in duplicated file last modified time")}
cat("\n\n ------", uniqueN(final_df$fileroot_nosuffix), "files will be removed")


# remove older file and rename duplicated one
removed_list = c()
for (file in unique(final_df$fileroot_nosuffix)){
  
  original_file = final_df %>%
    filter(fileroot_nosuffix == file & duplic == F) %>%
    pull(path)
  dupl_file = final_df %>%
    filter(fileroot_nosuffix == file & duplic == T)

  oo = file.remove(original_file)
  oo = file.rename(dupl_file$path, dupl_file$newpath)
 
  removed_list = c(removed_list, file)
}
