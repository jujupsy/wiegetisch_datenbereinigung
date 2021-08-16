# wiegetisch_datenbereinigung


## Run app:
### firste create folder structure in your working directory
dir.create("data_temp")
dir.create("exported_data")
dir.create("raw_data") 
### Then run app
shiny::runGitHub("jujupsy/wiegetisch_datenbereinigung", ref="main")
