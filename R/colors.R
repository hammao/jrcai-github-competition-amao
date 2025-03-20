################## PREP ###########################
# Color selection



ls_colors_1<- c( "#55A958", # Section A
               "#643C8B", # Section B
               "#E28048", # Section D
               "#4FABE6", # Section F
               "grey50", # Section G
               "red",
               "black",
               "#7DAB57", # Section C
               "#F5BE45", # Section E
               "#FBE3A3", # Section H
               "#BED5EC" , # Section I
               "#51ABE1"
               )

ls_colors_2<- c( "#55A958", # Section A
               "#643C8B", # Section B
               "red",
               "#E28048", # Section D
               "#F5BE45", # Section E
               "#4FABE6", # Section F
               "#B2AFB0", # Section G
               "#7DAB57", # Section C
               "#FBE3A3", # Section H
               "#BED5EC"  # Section I
               )

colors_3 <- c("#FDAE61", # Orange
              "#D9EF8B", # Light green
              "#66BD63") # Darker green

colors_4 <- c("#51ABE1", # AB light blue
              "#54AD5A", # AB green
              "#673D92") # AB purple
              
base_family = "Times New Roman"

if(!dir.exists("figures")){dir.create("figures")} else{print("dir already exists!")}

ls_colors_1
ls_colors_2
colors_3
colors_4
#ifelse(!dir.exists("./figures"), dir.create("./figures"), "dir already exists!")