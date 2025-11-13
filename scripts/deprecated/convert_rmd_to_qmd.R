# converting book following https://www.njtierney.com/post/2022/04/11/rmd-to-qmd/

library(fs)
library(stringr)
rmd_names <- dir_ls(path = "./scripts/", glob = "*.Rmd")
qmd_names <- str_replace(string = rmd_names,
                         pattern = "Rmd",
                         replacement = "qmd")
file_move(path = rmd_names,
          new_path = qmd_names)