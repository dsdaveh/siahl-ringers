cd code
quarto render siahl-etl.qmd
quarto render siahl-eda.qmd
R CMD BATCH --no-save siahl_ringers/make_datafiles.R siahl_ringers/make_datafiles.Rout
R CMD BATCH --no-save siahl_ringers/shinyapps_deploy.R siahl_ringers/shinyapps_deploy.Rout