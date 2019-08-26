library(dplyr)
library(stringr)
library(stringi)

# Date: 7/5/2019
# Last Edit: 7/9/2019
# Author: Yiran Jia
# Objective: This will help identify which Account Executive doesn't have Large Strategic Partnership (LSP) opportunity assigned. It will compare the whole list of AE
# in North America with the AE in LSP Control Tower SharePoint site. It will list if the AE has LSP oppy assigned or not. If yes, how many, and what are the opportunity name. 

# We only want to know if an AE has been assigned on an active (01 -05 stage). This prepares for later filter
active_stages = c("05 - Negotiation","04 - Propose", "03 - Develop","02 - Qualify","01 - Pre-Qual")

# donwload Aaron's report. We only need AE name, Zone, and customer number. The final report doesn't include customer number, but I think the leaders might be interested 
# in the future. This report provides the full list of AE in Norther America.
setwd("C:/Users/320024909/Desktop/AE_Analysis")
sfdc <- read.csv("AE_All.csv", stringsAsFactors = FALSE, na.strings = c("","NA")) %>% 
  filter(!is.na(Zone)) %>% 
  select(Zone, AE = Full.Name) %>% 
  group_by(Zone, AE) %>% 
  summarise(Customer.Number = n()) %>% 
  ungroup() %>% 
  filter(!grepl("Open", AE)) %>% 
  mutate(UpperCaseAE = toupper(AE)) %>% 
  select(UpperCaseAE, AE, Zone, Customer.Number)
 
# download the control tower report and filter to only active deals. We need to format the name in the same way that Aaron's report did, so that we can use that to do a left.join.
setwd("C:/Users/320024909/Desktop")
ct <- read.csv("Control_Tower.csv", stringsAsFactors = FALSE, na.strings = c("","NA")) %>% 
  filter(!is.na(Account.Executive),Zone != "Govt", Stage %in% active_stages) %>% 
  group_by(Account.Executive) %>% 
  summarise(LSP.Number = n()) %>% 
  select(Account.Executive, LSP.Number)
test.ct <- as.data.frame(str_split_fixed(ct$Account.Executive, ",",2)) %>% select(first.name = V2, last.name = V1)
test.ct$first.name <- stri_sub(test.ct$first.name, 2)
ct <- ct %>% mutate(paste(test.ct$first.name, test.ct$last.name, sep = " ")) %>% select(-Account.Executive)
colnames(ct)[2] <- "AE"
ct <- ct %>% mutate(UpperCaseAE = toupper(AE)) %>% select(UpperCaseAE,AE, LSP.Number)


# left.join the sfdc, or Aaron's report, with the control tower. It tells us how many oppys each of AE has. But it doesn't say which oppy exactly. Will tackle in later code.
all.sfdc <- left_join(sfdc, ct, by = "UpperCaseAE") %>% select(-AE.y,-Customer.Number, AE = AE.x, Zone, LSP.Number)
# this is for testing. Some of AE are in control tower, but not in Aaron's list. We should be able to find all AE's in Aaron's list. Export this and ask CM or Phil or Aaron to 
# manually fix it. Then run the entier code again to make sure we include and will compare all AE.
all.ct <- left_join(ct,sfdc, by = "UpperCaseAE") %>% select(-UpperCaseAE, -AE.y,-Customer.Number, AE = AE.x, Zone, LSP.Number)

# this helps us fine the opportunity names each of the AE has. Since some AE has more than 1, we previously knew that the maximum number of oppy an AE will have is 4, 
# therefore I generate a datafram with four oppy fields. It will go over each of the AE, and then find and list the oppy to the dataframe. 
setwd("C:/Users/320024909/Desktop")
ct.ae.oppy <- read.csv("Control_Tower.csv", stringsAsFactors = FALSE, na.strings = c("","NA")) %>% 
  filter(!is.na(Account.Executive),Zone != "Govt", Stage %in% active_stages) %>% 
  select(Opportunity.Name, Account.Executive)
unique.ae <- unique(ct.ae.oppy$Account.Executive)

for(i in 1:length(unique.ae)){
  temp <- data.frame("AE" = unique.ae[i], "Opportunity1" = NA, "Opportunity2" = NA, "Opportunity3" = NA, "Opportunity4" = NA)
  oppy <- ct.ae.oppy[ct.ae.oppy$Account.Executive ==  unique.ae[i], "Opportunity.Name"]
  for(j in 1:length(oppy)){
    temp[1, 1+j] <- oppy[j]
  }
  if(!exists("stack_ae_oppy")){
    stack_ae_oppy <- temp
  } else{
    stack_ae_oppy <- rbind(stack_ae_oppy, temp)
  }
}

# since we are comparing with the names on sfdc, and some letters are lower in one place but up in another place. therefore we add another column with upper case name, and use
# use that to do a left join with the all_sfdc, which has AE name, zone, number of LSP they are having. After join, we will list what deals they are working at exactly. 
test_stack_ae_oppy <- as.data.frame(str_split_fixed(stack_ae_oppy$AE, ",",2)) %>% select(first.name = V2, last.name = V1)
test_stack_ae_oppy$first.name <- stri_sub(test_stack_ae_oppy$first.name, 2)
stack_ae_oppy <- stack_ae_oppy %>% mutate(paste(test_stack_ae_oppy$first.name, test_stack_ae_oppy$last.name, sep = " ")) %>% select(-AE)
colnames(stack_ae_oppy)[5] <- "AE"
stack_ae_oppy <- stack_ae_oppy %>%
  mutate(UpperCaseAE = toupper(AE)) %>% 
  select(UpperCaseAE, AE, everything())

# left join with the previous all.sfdc by using the uppercase AE name. This is the final table we need. 
all.sfdc.oppy <- left_join(all.sfdc, stack_ae_oppy, by = "UpperCaseAE") %>% 
  select(-UpperCaseAE, -AE.y, AE = AE.x, Zone, LSP.Number, Opportunity1, Opportunity2, Opportunity3, Opportunity4)

# export the final table. 
setwd("C:/Users/320024909/Desktop/AE_Analysis")
write.csv(all.sfdc.oppy, "AE_Analysis.csv", row.names = FALSE)
# write.csv(all.ct,"Missing_AE.csv", row.names = FALSE)
