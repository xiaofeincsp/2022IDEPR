
rm(list = ls()) 
library(tidyverse)
library(readxl)
library(yarrr)
library(dplyr)
library(knitr)
library(rmarkdown)
library(statgenSTA)
library(statgenGxE)
library(openxlsx)
library(QBMS)




# Cassava BreedBase server
set_qbms_config("https://cassavabase.org/brapi/v1/calls/",
                path = "", time_out = 300, no_auth = TRUE,
                page_size = 10000,
                engine = "breedbase")



# list supported crops in the current bms server
# list_crops()
# select a crop by name
set_crop("Cassava")

# list all breeding programs in the selected crop
# list_programs()
# select a breeding program by name
set_program("CIAT")

# list all studies/trials in the selected program
# list_trials()
# select a specific study/trial by name
set_trial("CIAT_2022")

# # get observation variable ontology in the selected study/trial
# ontology <- get_trial_obs_ontology()
# list all environments/locations information in the selected study/trial
STUDIES <- list_studies()
STUDIES
# select a specific environment/location by name
trials = c("202266IDEPR_ciat",
           "202267IDEPR_repe",
           "202268IDEPR_cere")

raw_data = list()
for(i in 1:length(trials)) {
  set_study(trials[i])
  
  raw_data[[ trials[i] ]] = get_study_data()
  
}


all_raw = data.table::rbindlist(raw_data, fill = TRUE) %>% 
  as.data.frame() %>%
  filter(observationLevel == "plot")  


dim(all_raw)
#str(all_raw)



# names(all_raw)
##  check the missing data -- download all data?

miss_col = c("number of planted stakes per plot counting|CO_334:0000159" ,
             "fresh storage root weight per plot|CO_334:0000012" ,
             "dry matter content by specific gravity method|CO_334:0000160",
             "root evaluation scaled 1-5|CO_334:0000228" 
)
all_raw %>%
  select(studyName, all_of(miss_col)) %>%
  group_by(studyName) %>%
  summarise_all( funs(1-mean(is.na(.))) ) %>%
  ungroup()



# write.csv(all_raw, "D:\\OneDrive - CGIAR\\data.csv", row.names=FALSE)




#
#
#
#
#
#
#
#
#
#
#


#### 2.1 select the target trial data 


# ----------------------------- select one 
local_file = "no"
# local_file = "yes"  # if yes, should define the folder, file et al.


if(local_file == "yes" ) {
  
  folder = "D:\\OneDrive - CGIAR\\01_2022_2022\\01_trial_trial\\2021LAEPR\\"
  file = "phenotype.csv"
  skip_col = 3     # double check the number of col skipped
  trial_interest = "LAEPR"
  year_interest = 2021
  
  header = read.csv(paste(folder, file, sep=""),
                    skip = skip_col,
                    header=F,
                    nrows=1,
                    as.is=T,
                    row.names=NULL,
                    sep= ",") # if from Lukas, sep = "\t")
  all_raw = read.csv(paste(folder, file, sep=""),
                     skip= c(skip_col + 1),
                     header = F,
                     na.strings =c("", "NA"),
                     row.names=NULL,
                     sep="," )# if from Lukas, sep = "\t")
  colnames(all_raw)=header
}



# **************** function  
read_CassavaBase = function(folder = folder,
                            base_data_raw=all_raw,
                            trial_interest = trial_interest,
                            year_interest = year_interest,
                            minimum_share = 20,
                            remove_trial = NA) {
  
  base_data_raw = all_raw
  # there are duplications in 2010, 2011 and 2012 trials in CassavaBase --- 2021 June 5
  # remove the duplicated trials from CassavaBase data
  all_trial = unique(base_data_raw$studyName)
  trial_10_11_12 = c( all_trial[str_detect(all_trial, "2010")], all_trial[str_detect(all_trial, "2011")],
                      all_trial[str_detect(all_trial, "2012")] )
  
  trial_10_11_12_del = trial_10_11_12[!str_detect(trial_10_11_12, "_")]
  base_data = base_data_raw %>%
    filter(!studyName%in%trial_10_11_12_del)
  all_trial = unique(base_data$studyName)
  
  ############################################################################
  #### select trials and historical trials with the clones
  
  # select the trials interested
  sel_trial = base_data %>%
    filter(grepl(trial_interest, studyName)) %>%
    filter(studyYear %in% c(year_interest))
  # unique(sel_trial$studyName)
  
  clones_sel = unique(sel_trial$germplasmName) # clones interested
  # length(clones_sel)
  
  #### historical trials with the clones interested
  clone_trial = base_data %>%
    select(germplasmName, studyName) %>%
    filter(germplasmName %in% clones_sel) %>%
    unique()
  
  trial_kp = data.frame(table(clone_trial$studyName)) %>%
    filter(Freq >= all_of(minimum_share) )   # because of EPR, so set the high number
  dim(trial_kp)
  
  # remove trials by location
  if(!is.na(remove_trial)){
    trial_kp = trial_kp[!trial_kp$Var1 %in% remove_trial,]
  }
  print("Here are the trials and the number of their shared clones with")
  print(paste(year_interest, trial_interest, sep=""))
  print(trial_kp)
  
  # DATA of historical trials with the clones interested
  sel_data = base_data %>%
    filter (studyName %in% trial_kp$Var1)
  dim(sel_data)
  
  save_raw_file = paste("01_", year_interest, trial_interest,  "_raw_data_", Sys.Date(), ".csv", sep = "")
  write.csv(sel_data, paste(folder, save_raw_file, sep=""),
            row.names=FALSE)
  
  return(sel_data)
}

#
#
#
folder = "D:\\OneDrive - CGIAR\\03_Git_Git\\2022trials\\2022IDEPR\\"
trial_interest = "IDEPR"
year_interest = 2022

#
#
#
sel_data = read_CassavaBase(folder = folder  ,
                            base_data_raw = all_raw,
                            trial_interest = trial_interest ,
                            year_interest = year_interest   ,
                            minimum_share = 20    , # number of minumum shared the clones
                            remove_trial = NA    # NA or trial name
)

# names(all_raw)
dim(sel_data)














# names(sel_data)



####  2.2. change column names 

# I download the list of column names of the CIAT program from CassavaBase
# 2021 June 5, there are 104 traits (note, note include the meta data)
# save it in an excel file,
# D:\OneDrive - CGIAR\01_2021_2021\01_CassavaBase_data
# 01_standard_col_names_2021March


# ---------------- function
change_colname = function (sel_data = sel_data ,
                           new_names = NA)   # the standard name of the new names
{
  
  # I have the col name of database and the standardized col names
  new_col_names = read_excel("D:\\OneDrive - CGIAR\\01_2021_2021\\01_CassavaBase_data\\01_standard_col_names_2021March.xlsx",
                             sheet = "2021June05")
  
  # if there is new column, it will tell you what is the new names, then you need give the "new_names"
  # and the re-run the function
  if (sum(!names(sel_data) %in% new_col_names$database_col_name) != 0){
    unique_col = setdiff(names(sel_data), new_col_names$database_col_name)
    print("In the sel_data, there is/are unique column(s):")
    print(unique_col)
    print("Please add the unique column(s) to the standard col names")
    print("File location,
       D:\\OneDrive - CGIAR\\01_2021_2021\\01_CassavaBase_data\\01_standard_col_names_2021March.xlsx,
       sheet = 2021June05" )
    if(is.na(new_names)) {print("ERROR: The unique column(s) not changed yet")}
    if(!is.na(new_names)){
      unique_names = data.frame(database_col_name = unique_col,
                                analysis_col_name = new_names)
      new_col_names = rbind(new_col_names, unique_names)
      
      base_sel_colname = data.frame(names(sel_data))
      names(base_sel_colname) = "database_col_name"
      sel_new_colname = base_sel_colname %>%
        left_join(new_col_names, by = c( "database_col_name"))
      
      names(sel_data) = sel_new_colname$analysis_col_name
      print("Good, new columns were added. The column names are standardized names now!")
      
      ####  remove columns without data
      sel_data_kp = sel_data %>%
        select(   where(  ~!all(is.na(.x)) )    )
      
      return(sel_data_kp)
    }
    
    
  }
  # if there is no new col names, it is easy
  if (sum(!names(sel_data) %in% new_col_names$database_col_name) == 0){
    base_sel_colname = data.frame(names(sel_data))
    names(base_sel_colname) = "database_col_name"
    sel_new_colname = base_sel_colname %>%
      left_join(new_col_names, by = c( "database_col_name"))
    
    names(sel_data) = sel_new_colname$analysis_col_name
    print("Good, the column names are standardized names now!")
    
    ####  remove columns without data
    sel_data_kp = sel_data %>%
      select(   where(  ~!all(is.na(.x)) )   )
    
    return(sel_data_kp)
    
  }
}

#
#
#

#### change the colnames into standard

sel_data_kp = change_colname (sel_data = sel_data,
                              new_names = NA
)

names(sel_data_kp) = gsub("-", "_", names(sel_data_kp))

tail(names(sel_data))
tail(names(sel_data_kp))


sel_data_kp = sel_data_kp %>%
  select(-availableGermplasmSeedlotUniquenames)



#table(sel_data_kp$use_row_number)




#### 2.2.1 change the class of columns
names(sel_data_kp)
obs_col = c( names(sel_data_kp) [str_detect(names(sel_data_kp), "obs_")],
             "use_rep_number", "blockNumber"   ,                      
             "use_plot_number" , "use_row_number",                      
             "use_col_number" , "use_plot_width"  ,                    
             "use_plot_length"   ) 

sel_data_kp = sel_data_kp %>%
  mutate(across(all_of(obs_col), as.character ) )%>%  # factor > character > numeric
  mutate(across(all_of(obs_col), as.numeric ) )










#### 2.3 are there duplicated rows and columns?
#### if so, change or remove



# ---- function
row_col_dup = function (sel_data_kp= sel_data_kp) {
  
  row_col_ct = sel_data_kp %>%
    count(use_trial_name, use_col_number, use_row_number, sort=TRUE) %>%
    filter(n>1) %>%
    arrange(use_row_number, use_col_number)
  
  if (nrow(row_col_ct) >0) {
    print("ERROR: The duplicated row and column combination:")
    print(row_col_ct)
    
    row_col_ct_bind = row_col_ct %>%
      mutate(trial_row_col = paste(use_trial_name, use_col_number, use_row_number, sep = "_"))
    
    duplicated_plot = sel_data_kp %>%
      mutate(trial_row_col = paste(use_trial_name, use_col_number, use_row_number, sep = "_")) %>%
      filter(trial_row_col %in% row_col_ct_bind$trial_row_col) %>%
      select(use_plot_name, use_col_number, use_row_number, use_trial_name, use_plot_number) %>%
      arrange(use_trial_name, use_plot_number, use_row_number, use_col_number )
    
    print("Here are the plot names:")
    print(duplicated_plot)
    print("Please fix the ERROR!")
    return(duplicated_plot)
    
  }
  if (nrow(row_col_ct) == 0) {
    print("Good, there is no duplicated combination of row and column.")
  }
  
}

#
#
#
# show the duplicated row and col
duplicated_plot = row_col_dup(sel_data_kp = sel_data_kp)


if(FALSE) {   # ------------------- If there is dup, run the code below.
  # show the trial layout
  # if good, not duplications
  dup_trial = unique(sel_data_kp$use_trial_name)  # all
  # if duplications found
  dup_trial = unique(duplicated_plot$use_trial_name)  # only trials with dup row col
  for(i in 1:length(dup_trial)) {
    trial_i = sel_data_kp%>%
      filter(use_trial_name %in% all_of(dup_trial[i]))
    myplot <- ggplot(trial_i, aes(x=use_col_number, y= use_row_number, fill=use_rep_number)) +
      geom_tile(color="black", size=0.5) +           # Black border on tiles
      labs(x="col_number", y="row_number", fill = "rep",title = dup_trial[i]) +
      coord_fixed() +                                # Square tiles
      theme_minimal() +                              # Minimal theme, no grey background
      theme(panel.grid=element_blank(),              # No underlying grid lines
            axis.text.x=element_text(                # Vertical text on x axis
              angle=0,vjust=0.5,hjust=0))
    print(myplot)
  }
  
  # View(duplicated_plot)
}














### 2.4 visualize the trial layout

#### function visualize the layout -

trial_layout = function(trial = sel_data_kp){
  trial_list = unique(trial$use_trial_name)
  for (i in 1:length(trial_list)){
    trial_i = trial %>%
      filter(use_trial_name %in% trial_list[i])
    myplot <- ggplot(trial_i, aes(x=use_col_number, y= use_row_number, fill=use_rep_number)) +
      geom_tile(color="black", size=0.5) +           # Black border on tiles
      labs(x="col_number", y="row_number", fill = "rep",title = trial_list[i]) +
      coord_fixed() +                                # Square tiles
      theme_minimal() +                              # Minimal theme, no grey background
      theme(panel.grid=element_blank(),              # No underlying grid lines
            axis.text.x=element_text(                # Vertical text on x axis
              angle=0,vjust=0.5,hjust=0))
    print(myplot)
  }
}



#
#
#

str(sel_data_kp)
trial_layout(trial = sel_data_kp)
dim(sel_data_kp)

#
#
#













# ----------- optional -------------  #
### 2.5 fill the gap in row and col   ---- required by ASREML, but not statGen
# it is optional
# check 1.2, row and column form a rectangle in each trial?
# check 1.3, any gap in the row and column combination?
# check 1.3.1 make the rectangle

if(FALSE) {
  # ---- function
  fill_gap = function (sel_data_kp) {
    
    # trials with duplicated rows and columns
    row_col_ct = sel_data_kp %>%
      count(use_trial_name, use_col_number, use_row_number, sort=TRUE) %>%
      filter(n>1) %>%
      arrange(use_row_number, use_col_number)
    
    # no including the trials with duplications
    sel_data_NOdup = sel_data_kp %>%
      filter(!use_trial_name %in% row_col_ct$use_trial_name)
    trial_list = unique(sel_data_NOdup$use_trial_name)
    
    trial_full = sel_data_NOdup[FALSE,]
    print("The trials are:")
    print(trial_list)
    
    for(i in 1:length(trial_list)){
      
      trial_i = sel_data_NOdup %>%
        filter(use_trial_name %in% all_of(trial_list[i]))
      max_col = max(trial_i$use_col_number)
      max_row = max(trial_i$use_row_number)
      all_row_col = data.frame(use_col_number = rep(1:max_col, max_row),
                               use_row_number = rep(1:max_row, each = max_col))
      # if there is missing col and row
      # provide the rows and colmns missing in the phenotypic data
      trial_i_lack = anti_join(all_row_col, trial_i, by = c("use_col_number", "use_row_number")) %>%
        select(use_row_number, use_col_number)
      
      if(nrow(trial_i_lack) == 0) {
        print(trial_list[i])
        print("Good, there is no missing in rows and columns for the trial")
        
        myplot <- ggplot(trial_i, aes(x=use_col_number, y= use_row_number, fill=use_rep_number)) +
          geom_tile(color="black", size=0.5) +           # Black border on tiles
          labs(x="col_number", y="row_number", fill = "rep",title = trial_list[i]) +
          coord_fixed() +                                # Square tiles
          theme_minimal() +                              # Minimal theme, no grey background
          theme(panel.grid=element_blank(),              # No underlying grid lines
                axis.text.x=element_text(                # Vertical text on x axis
                  angle=0,vjust=0.5,hjust=0))
        print(myplot)
        
        trial_i_full = trial_i
      }
      
      if(nrow(trial_i_lack) > 0) {
        print(trial_list[i])
        print("No, there are missing rows and columns for the trial")
        print(paste("There are ", nrow(trial_i_lack),
                    " missing combinations of rows and columns",
                    sep = ""))
        print("Here are the missing rows:")
        print(unique(trial_i_lack$use_row_number))
        print("Here are the missing columns:")
        print(unique(trial_i_lack$use_col_number))
        
        myplot <- ggplot(trial_i, aes(x=use_col_number, y= use_row_number, fill=use_rep_number)) +
          geom_tile(color="black", size=0.5) +           # Black border on tiles
          labs(x="col_number", y="row_number", fill = "rep",title = trial_list[i]) +
          coord_fixed() +                                # Square tiles
          theme_minimal() +                              # Minimal theme, no grey background
          theme(panel.grid=element_blank(),              # No underlying grid lines
                axis.text.x=element_text(                # Vertical text on x axis
                  angle=0,vjust=0.5,hjust=0))
        print(myplot)
        
        # fill the missing col and row with NA
        add_colname = colnames(trial_i)[!colnames(trial_i) %in% colnames(trial_i_lack)]
        trial_i_lack[,add_colname] = NA
        
        trial_no_underline = gsub("_.*", "" , trial_list[i])
        trial_i_lack$use_accession_name = paste(trial_no_underline, seq(1:nrow(trial_i_lack)), sep="-")
        trial_i_lack$use_plot_number = seq(1:nrow(trial_i_lack)) + 8000
        trial_i_lack$use_location = unique(trial_i$use_location)
        trial_i_lack$use_trial_name = unique(trial_i$use_trial_name)
        trial_i_lack$use_year =  unique(trial_i$use_year)
        trial_i_lack$use_plot_width = unique(trial_i$use_plot_width)
        trial_i_lack$use_plot_length = unique(trial_i$use_plot_length)
        # trial_i_lack$use_rep_number, keep it as NA?
        trial_i_full = rbind(trial_i, trial_i_lack)
      }
      trial_full = rbind(trial_full, trial_i_full)
      
    }
    return(trial_full)
  }
  
  #
  #
  #
  data_NOgap = fill_gap(sel_data_kp)
  # check whether the gaps were filled
  # show the missing plot with gray background
  data_NOgap = fill_gap(data_NOgap)
  
  data.frame(table(sel_data_kp$use_trial_name))
  data.frame(table(data_NOgap$use_trial_name))
  
  
}







### 2.6 check rep
data_NOgap = sel_data_kp


#### check and correct rep number
# anly wrong rep_number?
rep_ct = data_NOgap %>%
  count(use_trial_name, use_rep_number)  %>%
  arrange(use_trial_name)
print(rep_ct)
View(rep_ct)


# print plots with wrong rep number
data_wrong_rep = data_NOgap %>%
  filter(use_rep_number >3) %>%
  select(use_trial_name, use_plot_name, use_rep_number, use_row_number, use_col_number) %>%
  arrange(use_row_number, use_col_number)
print(data_wrong_rep)

#
#
#
data_fixREP = data_NOgap
# visualize the layout
trial_layout(trial = data_fixREP)





### 2.7 convert accession_name to standard names and add the check_test column

# ---- function
check_clone_name = function(clone_list,
                            new_names = NA,
                            add_check = NULL) {
  
  ## 1). list of released varieties
  released = read_excel("D:\\OneDrive - CGIAR\\01_2021_2021\\01_CassavaBase_data\\01_Cassava Varieties with multiple names_2021Jan.xlsx",
                        sheet = "varietyName", na="")
  cat("Released varieties:")
  print(sort(released$accession_name)) # the correct name format
  
  ## 2). accessions in genebank
  eGWAS = "D:\\OneDrive - CGIAR\\05_proposal\\09_Approved_e-GWAS\\"
  genebank = read_excel(paste(eGWAS, "CIAT_genebank_cassava.xlsx", sep = ""),
                        sheet = "CASSAVA-GRP-CIAT (1)", skip = 6, na = "")
  genebank_clone = unique(genebank$`Accession number`)
  
  ## 3. other know clones
  # we can add more clones   -------- flexibility
  known_clone = c("C19", "C243", "C33", "C39", "C413", "TME3", "HB60" , "KU50" )
  cat("The other known clones:")
  print(sort(known_clone))
  
  # all the clones in the file
  accession_out = data.frame("oldName" = unique(clone_list), "newName" = unique(clone_list))
  accession_out$newName = gsub(" ", "", accession_out$newName) # remove space in names
  accession_all = unique(accession_out$newName)  # list of clones after remove space
  
  variety_wrong = accession_all [!str_detect(accession_all, "-") &   # without "-" in name
                                   !accession_all %in% genebank_clone &    # not in genebank
                                   !accession_all %in% known_clone &       # not in known clones
                                   !accession_all %in% released$accession_name] # not in released list
  
  
  
  if(sum(!is.na(new_names)) ==0) {
    
    if(length(variety_wrong)>0){
      print("The clones below did not have the correct name.")
      print("Need give the new name to -- new_names, then re-run the function")
      print(variety_wrong)
    }
    
    if(length(variety_wrong)==0){
      print("Good, the released names were correctly used")
      
      # trial accessions in released collections
      trial_clone_released = released %>%
        filter(accession_name %in% accession_out$newName) %>%
        data.frame()
      
      # change the trial accession into standard names
      if(nrow(trial_clone_released) >=1) {
        for(j in 1:nrow(trial_clone_released)){
          
          accession_out = accession_out %>%
            mutate(newName =
                     ifelse(newName == trial_clone_released[j,1] ,
                            trial_clone_released[j,2], newName) )
        }
      }
      
      if(nrow(trial_clone_released) ==0) {
        accession_out$newName = accession_out$oldName
      }
      
      accession_all_format = unique(accession_out$newName)
      variety_format = accession_all_format [str_detect(accession_all_format, "_is_")]
      print("Now the standard names are used. Here are the released varieties")
      print(variety_format)
      
      # add check_released column
      
      accession_out$use_check_released = NA
      check_list = c(variety_format, add_check)
      # change the check column
      if(length(check_list) >0){
        accession_out = accession_out %>%
          mutate(use_check_released =
                   ifelse(newName %in% check_list ,
                          "check_released", "test") )
      }
      
      if(length(check_list) == 0){
        accession_out$use_check_released = "test"
      }
      
      check_after = accession_out %>%
        filter(use_check_released == "check_released") %>%
        select(newName) %>%
        unique()
      print("The check or released clones are:")
      print(check_after)
      
      names(accession_out) = c("accession_name_ori", "use_accession_name",
                               "use_check_released")
      
      
      return(accession_out)
      
    }
    
    
  }
  
  
  
  if(sum(!is.na(new_names)) >0) {
    
    old_new_name = data.frame(old_name = variety_wrong,   # with both wrong and correct names
                              new_name = new_names)
    
    for(i in 1:nrow(old_new_name)){
      accession_out = accession_out %>%
        mutate(newName =
                 ifelse(newName == old_new_name[i,1] ,
                        old_new_name[i,2], newName) )   # replace with standard format
    }
    
    accession_all_modi = accession_out$newName
    variety_2wrong =  accession_all_modi [!str_detect( accession_all_modi, "-") &   # without "-" in name
                                            ! accession_all_modi %in% genebank_clone &    # not in genebank
                                            ! accession_all_modi %in% known_clone &       # not in known clones
                                            ! accession_all_modi %in% released$accession_name] # not in released list
    
    if(length(variety_2wrong)>0){
      print("The clones below might not have the correct name.")
      print("Please double-check them!")
      print(variety_2wrong)
    }
    
    if(length(variety_2wrong)==0){
      print("Good, the released names were correctly used")
    }
    
    
    # trial accessions in released collections
    trial_clone_released = released %>%
      filter(accession_name %in% accession_out$newName) %>%
      data.frame()
    
    # change the trial accession into standard names
    if(nrow(trial_clone_released) >0) {
      for(j in 1:nrow(trial_clone_released)){
        
        accession_out = accession_out %>%
          mutate(newName =
                   ifelse(newName == trial_clone_released[j,1] ,
                          trial_clone_released[j,2], newName) )
        
      }
    }
    
    if(nrow(trial_clone_released) ==0) {
      accession_out$newName = accession_out$oldName
    }
    
    accession_all_format = unique(accession_out$newName)
    variety_format = accession_all_format [str_detect(accession_all_format, "_is_")]
    print("Now the standard names are used. Here are the released varieties")
    print(variety_format)
    
    # add check_released column
    
    accession_out$use_check_released = NA
    check_list = c(variety_format, add_check)
    # change the check column
    if(length(check_list) >0){
      accession_out = accession_out %>%
        mutate(use_check_released =
                 ifelse(newName %in% check_list ,
                        "check_released", "test") )
    }
    
    if(length(check_list) == 0){
      accession_out$use_check_released = "test"
    }
    
    check_after = accession_out %>%
      filter(use_check_released == "check_released") %>%
      select(newName) %>%
      unique()
    print("The check or released clones are:")
    print(check_after)
    
    names(accession_out) = c("accession_name_ori", "use_accession_name",
                             "use_check_released")
    return(accession_out)
    
    
  }
  
  
  
}




# the function loads the local file with the names of released varieties
# first run with NA

# -- check_clone_name is an independent function
cloneName_new_old = check_clone_name(clone_list = data_fixREP$use_accession_name,
                                     new_names = NA,
                                     add_check = NULL)
# 2nd run with new names
# if there are any other check clones, we need add them here.
if(FALSE) {
cloneName_new_old = check_clone_name(clone_list = data_fixREP$use_accession_name,
                                     new_names = c("Venezolana"),
                                     add_check = NULL)
}



trial_standard = data_fixREP %>%
  left_join(cloneName_new_old,
            by = c("use_accession_name" = "accession_name_ori") ) %>%
  select(-use_accession_name) %>%
  rename(use_accession_name = use_accession_name.y)
names(trial_standard)

#unique(trial_standard$use_accession_name)



### 2.8  accession duplications within rep
#### pay attention to clones with > 1 count within a rep.

accession_rep_ct = trial_standard %>%
  count(use_trial_name, use_accession_name, use_rep_number)  %>%
  arrange(use_trial_name) %>%
  filter(n>1)
print(accession_rep_ct)
#View(accession_rep_ct)



if(FALSE){
  trial_standard %>%
    select(use_trial_name, use_accession_name, use_rep_number, use_row_number, use_col_number) %>%
    filter(use_accession_name == "CG1141-1_is_Costena")
}









### 2.9 add GIS information 

# the list of location was downloaded from CassavaBase on June 06
# and then match the information with the sheet, Nelson



add_GIS = function(trial_data = trial_standard) {
  # read in GIS information from the "standard" file
  base_folder = "D:\\OneDrive - CGIAR\\01_2021_2021\\01_CassavaBase_data\\"
  loc_file = "001_standardization_trait_trial_location.xlsx"  # updated on June 7
  GIS_info = read_excel (paste(base_folder, loc_file, sep=""),
                         sheet = "CassavaBase", skip = 0,
                         na="")
  GIS_info = GIS_info %>%
    select("location_CassavaBase", "use_latitude" ,  "use_longitude",
           "use_altitude", "use_department", "use_country",  "use_ag_zone", "use_location_short"  )
  GIS_info$use_longitude = as.numeric(GIS_info$use_longitude )
  
  trial_loc = unique(trial_data$use_location)
  print("The locations are:")
  print(trial_loc)
  in_database = trial_loc %in% GIS_info$location_CassavaBase
  
  if(sum(in_database) != length(in_database)){
    trial_loc_database = data.frame(trial_loc_list = trial_loc,
                                    loc_in_CassavaBase = in_database)
    print("Some locations are not in the database, please add them in:")
    print("D:\\OneDrive - CGIAR\\01_2021_2021\\01_CassavaBase_data\\")
    print("001_standardization_trait_trial_location.xlsx")
    print("The sheet is -- CassavaBase --")
    print(trial_loc_database)
  }
  
  if(sum(in_database) == length(in_database)) {
    print("All locations are in the database.")
    trial_data = left_join(trial_data, GIS_info, by = c("use_location" = "location_CassavaBase"))
    
  }
  return(trial_data)
}


trial_standard = add_GIS( trial_data = trial_standard )







### 2.10  compile new traits **********************
#### germination, yield, yield_DM
#### plot size
trial_standard %>%
  select(use_plot_width, use_plot_length, use_trial_name) %>%
  distinct()
print("Double-check the plot size, which will be used for calculating yield.")

### 2.10.1  change the plot size of certain trial
if(FALSE){
  #trial_standard <- mutate(trial_standard, use_plot_length =
  #                           ifelse(str_detect(use_trial_name, "DVGST_momi") ,
  #                                  4.0, use_plot_length  ))  # change from 3.5 to 4.0
  trial_standard <-  mutate(trial_standard, use_plot_width =
                              ifelse(str_detect(use_trial_name, "LAEPR_polo") ,
                                     3.2, use_plot_length  ))
}



### 2.10.2   number of plants harvested
view_harvest_number = trial_standard %>%
  count(use_trial_name, obs_harvest_number) %>%
  arrange(use_trial_name)
print(view_harvest_number)
# there are some plots with more plants havested than we planned

if(FALSE) {
  trial_standard %>%
    filter(obs_harvest_number == 6  ) %>%
    select(use_trial_name, use_accession_name, use_rep_number,
           use_row_number, use_col_number, obs_harvest_number,
           obs_root_weight_plot, obs_plant_type, `obs_root_type1-5`)
  
  
  trial_standard %>%
    filter(is.na(obs_harvest_number)  ) %>%
    select(use_trial_name, use_accession_name, use_rep_number,
           use_row_number, use_col_number, obs_harvest_number,
           obs_root_weight_plot, obs_plant_type, `obs_root_type1-5`)
}



# assign the number of plants we plan to havest
trial_standard$obs_harvest_number_plan = NA
trial_standard = trial_standard %>%
  mutate(obs_harvest_number_plan =
           ifelse(str_detect(use_trial_name, "2021") &  str_detect(use_trial_name , "DVGST") ,
                  4, obs_harvest_number_plan  ))



### 2.10.3   compile new traits
# 1) calculate germination %
trial_standard$obs_germination_perc = trial_standard$obs_germinated_number_plot/trial_standard$obs_planted_number_plot * 100

# 2) calculate the yield = plot weight x plot size /10000
trial_standard =  trial_standard %>% ungroup()
trial_standard$obs_yield_ha_v2 =
  trial_standard$obs_root_weight_plot  *10/ (trial_standard$use_plot_width * trial_standard$use_plot_length *0.6 )


# new yield and yield in CassavaBase
library(plotly)
p1 <- trial_standard %>%
  ggplot(aes(x = obs_yield_ha, y= obs_yield_ha_v2, color = use_trial_name))+geom_point()
ggplotly(p1)



if(FALSE) {
  trial_standard %>%
    filter(obs_yield_ha_v2 > 75) %>%
    select(use_plot_name, obs_yield_ha_v2, obs_yield_ha, obs_root_weight_plot)
}



# 3) calculate DM yield
trial_standard$obs_DM_yield = trial_standard$obs_DM_gravity * trial_standard$obs_yield_ha_v2/100


dim(trial_standard)




### 2.11  check whether all traits "obs_" are numerica 
# check whether all traits are numerica


#### function 6.1. check numeric of obs_ traits   ------------------------------ 6.1 6.1 6.1 6.1 6.1


is_numeric = function (trial_data) {
  
  all_trait = names(trial_data)[str_detect(names(trial_data), "obs_")]
  numeric_trait = names(select_if(trial_data[, all_trait], is.numeric))
  
  if(sum(all_trait%in% numeric_trait) == length(all_trait) ) {
    print("Good, all traits are numeric!")
  }
  if (sum(all_trait%in% numeric_trait) != length(all_trait) ) {
    print("The traits are not numeric. Need fix it!")
    print (all_trait [!all_trait%in% numeric_trait])
    print("After fixing the error, please re-run the function, is_numeric")
  }
  
}


is_numeric(trial_data = trial_standard)




### 2.12  get the tidy data  
#################### meta_infor, trait_list ###################################
#### select the informative columns
meta_info = names(trial_standard)[str_detect(names(trial_standard), "use_")]
meta_info = gsub("use_", "", meta_info)
meta_info
trial_tidy = trial_standard
names(trial_tidy)= gsub("use_", "", names(trial_standard))

trait_list = names(trial_tidy)[str_detect(names(trial_tidy), "obs_")]
trait_list = gsub("obs_", "", trait_list)
trait_list
names(trial_tidy)= gsub("obs_", "", names(trial_tidy))
trial_tidy = trial_tidy[c(meta_info, trait_list)]
# names(trial_tidy)

if(FALSE){
  trial_tidy%>%
    select(plant_date, harvest_date, trial_name, location) %>%
    distinct()
}
if(FALSE){
  write.csv(trial_tidy, paste(folder, "01_", year_interest, trial_interest,
                              "_tidy_data_", Sys.Date(),".csv", sep=""), row.names=FALSE)  
  #### 1) the tidy data the all the columns
}











### 2.13  visualize the difference among trials in all traits **********************


# ---- function
BOXPLOT_VAR = function (my_dat,
                        trait_wanted){
  
  # remove columns with all NA
  not_all_na = function(x) any(!is.na(x))
  my_dat_noNA = my_dat %>% select_if(not_all_na)
  
  # save as PDF, can adjust the figure size
  pdf(paste(folder, "01_", year_interest, trial_interest, "_boxplot_",
            Sys.Date(),".pdf", sep=""), width = 4, height = 6)
  
  for(i in 1: length(trait_wanted)){
    y_DATA = my_dat_noNA[[trait_wanted[i]]]   # data frame or vector?
    x_DATA = my_dat_noNA$trial_name
    my_DATA = my_dat_noNA
    y_LABEL = trait_wanted[i]
    x_LABEL = NULL
    TITLE = NULL
    y_MAX = max(y_DATA, na.rm = TRUE) * 1.2
    y_MIN = 0
    
    plot_box = ggplot(my_DATA, aes(x = x_DATA, y = y_DATA))+
      geom_violin(trim=FALSE, fill="gray")+
      geom_boxplot(width=0.3) +
      coord_cartesian(ylim = c(y_MIN,y_MAX))+
      theme(axis.text.x = element_text(face="bold", colour="black", size=12, angle = 45, hjust = 1),
            axis.text.y = element_text(face="bold", colour="black", size=12),
            axis.title.y=element_text(size=14,face="bold", angle = 90, vjust = 3) ,
            axis.title.x=element_text(size=14,face="bold", vjust = -0.5) ,
            plot.title = element_text(color="red", size=16, face="bold.italic", hjust = 0.5),
            plot.margin = unit(c(1,1,1,2), "cm"), # top, right, bottom, left
            legend.position = "none"
      )  +
      labs(y = y_LABEL , x = x_LABEL,
           title = TITLE)
    plot(plot_box)
  }
  dev.off()
}


#
#
#

dev.off()
BOXPLOT_VAR(my_dat = trial_tidy,
            trait_wanted = trait_list)










### 2.15   get the count info of clones  **********************
# the number of trials, locations, years for each clone


## ****************** FUNCTION START ************************************* ##
CLONE_CT = function(my_dat ){
  
  clones = unique(my_dat$accession_name)
  ### 1.3.1 total plots per clone
  clone_plotNUM = my_dat %>%
    count(accession_name) %>%
    arrange(accession_name) %>%
    rename(plot_ct = n )
  
  
  ### 1.3.2 total trials per clone
  clone_envNUM = my_dat %>%
    distinct(accession_name, trial_name) %>%
    count(accession_name) %>%
    arrange(accession_name) %>%
    rename(trial_ct = n ) %>%
    select(accession_name, trial_ct)
  
  
  ### 1.3.3 number trials per year per clone
  years = unique(my_dat$year)
  clone_yrNUM = data.frame(matrix(nrow = length(clones),
                                  ncol = 1+length(years)))
  colnames(clone_yrNUM) = c("accession_name", paste(years, "_ct", sep=""))
  env_yr_clone = subset(my_dat, select = c("accession_name", "trial_name", "year"))
  
  for(i in 1:length(clones)){
    env_clone_i = subset(env_yr_clone, accession_name == clones[i])
    clone_yrNUM[i,1] = clones[i]
    for(j in 1:length(years)){
      year_j = subset(env_clone_i, year ==years[j])
      clone_yrNUM[i,c(j+1)] =  length(unique(year_j$trial_name))
    }
  }
  
  ### 1.3.4 number trials per location per clone
  locations = unique(my_dat$location)
  clone_locNUM = data.frame(matrix(nrow = length(clones),
                                   ncol = 1+length(locations)))
  colnames(clone_locNUM) = c("accession_name", paste(locations, "_ct", sep=""))
  env_loc_clone = subset(my_dat, select = c("accession_name", "trial_name", "location"))
  
  for(i in 1:length(clones)){
    env_clone_i = subset(env_loc_clone, accession_name == clones[i])
    clone_locNUM[i,1] = clones[i]
    for(j in 1:length(locations)){
      location_j = subset(env_clone_i, location ==locations[j])
      clone_locNUM[i,c(j+1)] =  length(unique(location_j$trial_name))
    }
  }
  
  ### 1.3.5 merge 1.3.1, 1.3.2, 1.3.3, 1.3.4
  ct_1 = merge(clone_plotNUM, clone_envNUM, by="accession_name")
  ct_2 = merge(ct_1, clone_yrNUM, by="accession_name")
  clone_ct_info = merge(ct_2, clone_locNUM, by = "accession_name") %>%
    arrange( desc(plot_ct) )
  
  write.csv(clone_ct_info, paste(folder, "01_", year_interest, trial_interest,
                                 "_clone_ct_", Sys.Date(),".csv", sep=""), row.names=FALSE)
}

CLONE_CT(my_dat = trial_tidy)







### 2.16   select traits for analysis *******************

# mean, SD  by trials

# ---- function
remove_no_var = function(my_dat) {
  
  # remove columns with all NA
  not_all_na = function(x) any(!is.na(x))
  my_dat_noNA = my_dat %>% select_if(not_all_na)
  
  mean_trial = my_dat[, c("trial_name", "rep_number", all_of(analysis_trait))] %>%
    group_by(trial_name, rep_number) %>%
    summarise_all(mean, na.rm=TRUE)
  
  sd_trial = my_dat[, c("trial_name", "rep_number", all_of(analysis_trait))] %>%
    group_by(trial_name, rep_number) %>%
    summarise_all(sd, na.rm=TRUE)
  
  sd_mean = colMeans(sd_trial[, c( analysis_trait)] , na.rm = TRUE)
  sd_mean = data.frame (sd_mean) %>%
    rownames_to_column(var = "trait") %>%
    rename(mean_of_sd = sd_mean)
  
  print("The mean of SD of each trait:")
  print(sd_mean[order(sd_mean$mean_of_sd),])
  
  sd_mean_0 = sd_mean %>%
    filter(mean_of_sd == 0 )
  
  if (nrow(sd_mean_0) ==0) {
    print("Good, no traits without variance.")
    return(my_dat)
  }
  
  if (nrow(sd_mean_0) >0) {
    print("The traits without variation:")
    print(sd_mean_0)
    print("Remove the traits from the trial data")
    
    
    analysis_trait = analysis_trait[!analysis_trait %in% sd_mean_0$trait]
    
    my_dat = my_dat %>%
      select(all_of(meta_info), all_of(analysis_trait))
    return(my_dat)
  }
  
}

#
#
#

no_analysis_trait = c("harvest_number_plan", "harvest_number",          # yield calculation
                      "root_weight_air", "root_weight_water",           # DM calculation
                      "planted_number_plot","germinated_number_plot",   # germination calculation
                      "root_weight_plot", "yield_ha" ,                  # yield calculation
                      "CBB_3mon", "shoot_weight_plot"
)

analysis_trait = trait_list[!trait_list %in% no_analysis_trait]    # ----- updated below

# remove traits without variation
trial_tidy = remove_no_var(my_dat = trial_tidy)
# update analysis_trait
analysis_trait = analysis_trait[analysis_trait %in% names(trial_tidy)]
# re-run to double check
trial_tidy = remove_no_var(my_dat = trial_tidy)



write.csv(trial_tidy, paste(folder, "01_", year_interest, trial_interest,
                            "_tidy_data4analysis_", Sys.Date(),".csv", sep=""),
          row.names=FALSE)    #### 2) the tidy data will reduced number of columns

dim(trial_tidy)












