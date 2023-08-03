# Set working directory to be the one above scripts.
# input takes working directory.
# any word styles documents must be in same folder as input file directory.
# output_file is RELATIVE to input file's directory, however: https://github.com/rstudio/rmarkdown/issues/1902

rmarkdown::render(input = "scripts/Research_Workshops_Marketing_Analysis.Rmd", 
       output_file = paste0("../reports_GO_HERE_FOR_READY_MADE_SLIDES_AND_DATA/",
                            "Research_Workshops_Marketing_Analysis_",
                            Sys.Date(),
                            ".docx"),
       output_format = "all")

       