# AKTA-graphing
R code to plot raw data from Cytiva's AKTA systems.

Instructions:
1- Download the R file
2- Export the raw data from the AKTA in (.csv) - Unforunatelly I haven't worked out a workaround for this step
3- Open the file with excel and save it as .xlsx in the same folder (or a subforder) as the R file
   Data can be saved in any number of subfolders, all will get preocessed, see example:
    - My_Cool_project
      - Akta graphs 1.0.R
      - Raw_akta_data
        - Protein1
          - His_col.csv (original raw data, can be deleted after conversion)
          - His_col.xlsx
        - Protein2
          - His_col.xlsx
          - Desalting_col.xlsx
4- Open the R file with R studio
5- Change any settings on the first lines of code to match your desired output style
   Some settings include:
    - Show legend (or change position)
    - Change colours
    - Start plotting at elution / Include Equilibration + Washes
    - Calibrate UV data (useful if you forgot)
    - Show fraction lines
    - Axis titles and font size
    - Plot size
    - Save folder / Save next to data file
6- Save and Run the entire file (Ctr + Shift + Enter)




