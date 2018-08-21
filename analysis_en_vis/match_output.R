library(plotly)
library(dplyr)

#kaiser_matches_df <- read.table("kaiser_matches.txt", header=TRUE, sep="\t", comment="", stringsAsFactors = FALSE, quote="")

kaiser_matches_df <- read.table("kaiser_matching_30705741.txt", header=FALSE, sep="\t", comment="", stringsAsFactors = FALSE, quote="")
kaiser_matches_df$V26<-NULL
teach_type_vec = c("aard","bio","duits","frans", "ict","natuur","ned","wis")

head(kaiser_matches_df,5)
names(kaiser_matches_df) = c("id","title","education_level","salary_min","salary_max", "sector"
                             ,"org_size",paste(teach_type_vec,"_max",sep=""), "type_max"
                             ,paste(teach_type_vec,"_mean",sep=""), "type_mean")

job_df = read.table("..\\output.30705741_no_duplicates.txt", header=FALSE, sep="\t", quote="", comment.char = "", stringsAsFactors = FALSE)
job_df_col_names =c("id","date","title","organization_name","job_location"
                    ,"job_location_id","job_location_latitude","job_location_longitude"
                    ,"jobfeed_profession","jobfeed_profession_id","jobfeed_profession_group"
                    ,"jobfeed_profession_group_id","jobfeed_profession_class","jobfeed_profession_class_id"
                    ,"source_url","source_website","source_type","source_type_id"
                    ,"education_level",	"education_level_id",	"employment_type",	"employment_type_id"
                    ,"contract_type",	"contract_type_id",	"working_hours",	"working_hours_id",	"hours_per_week_min",	"hours_per_week_max"
                    ,"salary_min",	"salary_max",	"experience_min",	"experience_max",	"via_intermediary",	"advert_name"
                    ,"advert_street","advert_house_no","advert_postal_code","advert_city"
                    ,"contact_person",	"phone",	"fax",	"email",	"website",	"ref_no"
                    ,"expired_at",	"expired",	"status",	"duplicate",	"guid",	"vac_uid"
                    ,"region",	"region_id",	"sector",	"sector_id",	"industry_sector_id",	"sic",	"sic_descr",	"org_size"
                    ,"org_size_id",	"apply_date")
names(job_df)=job_df_col_names
job_df = job_df[,c("id","job_location","jobfeed_profession","jobfeed_profession_group"
                   ,"jobfeed_profession_class","employment_type","contract_type"
                   ,"hours_per_week_min","hours_per_week_max")]

kaiser_matching_df <- merge(kaiser_matches_df, job_df, by="id")
write.table(kaiser_matching_df, file="kaiser_matching_df_complete.txt",quote=FALSE, sep="\t"
            , row.names = FALSE, col.names=TRUE)

kaiser_match_df = read.table("kaiser_matching_df_complete.txt", header=TRUE, sep="\t", comment="", stringsAsFactors = FALSE, quote="")

#kaiser_aard <- kaiser_matches_df %>% filter(jobtypeAve=="wis" & percentNed < .10)
#kaiser_aard <- kaiser_matches_df %>% filter( jobtypeAve=="wis")

#kaiser_aard <- kaiser_matches_df

#kaiser_aard_sub <- kaiser_aard[sample(1:nrow(kaiser_aard),100, replace=FALSE),]

#aard_df <- kaiser_aard_sub %>% select(functionrole, functionclass, educ) %>% group_by(functionclass, educ) %>% summarise(Count = n())

#aard_df <- kaiser_aard_sub %>% group_by(functionclass,functionrole,educ, jobtitle) %>% summarise(Count = n()) %>% 
#  mutate(Freq = paste0(round(100*Count/sum(Count),0),"%")) %>% 
#  as.data.frame()

aard_df_functclass <- kaiser_aard %>% filter(!educ %in% c("VMBO","HAVO","VWO","Elementair","HAVO/VWO","MAVO/HAVO","MAVO/VMBO")) %>%
  group_by(functionclass) %>%  
  summarise(Count = n(),meanpercenwis= mean(percentWis), meancosine = mean(aggrecosWis)) %>% 
  arrange(desc(Count)) %>%
  as.data.frame()

industry <-aard_df_functclass[1:10,1]

aard_df <- kaiser_aard %>% filter(!educ %in% c("VMBO","HAVO","VWO","Elementair","HAVO/VWO","MAVO/HAVO","MAVO/VMBO")) %>%
  group_by(functionclass,functionrole, educ, jobtitle) %>%  
  summarise(Count = n(),   meanpercenwis= mean(percentWis), meancosine = mean(aggrecosWis)) %>% 
  arrange(desc(meanpercenwis)) %>% ungroup() %>%
  filter(functionclass %in% industry) %>%
    as.data.frame()




head(aard_df,10)
dim(aard_df)

# arrange(desc(avecos)) %>%

p0<- plot_ly(aard_df_functclass, x = ~meanpercenwis, y=~meancosine, hoverinfo='text', text=~functionclass, type="scatter",  mode='markers', 
             size=~Count,
             #sizes=c(10,50),
             marker = list(opacity = 0.5, sizemode='diameter')) %>%
  layout(title = '',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend=FALSE)
#,
#             hoverinfo='text',
#             text=~paste('Job Title:', functionclass))

p0


p <- plot_ly(aard_df, x = ~meanpercenwis, y=~meancosine, type="scatter",mode="markers", size=~Count, color=~functionclass,colors="Spectral",
             #sizes=c(10,50),
             marker = list(opacity = 0.5, sizemode = 'diameter'),
             hoverinfo='text',
             text=~paste('Job Title:', jobtitle,'<br>Max Cosine:', meancosine,'<br>Max Percent:', meanpercenwis))
             


p0


#filter(aard_df, functionclass=="Administratie en klantenservice")

head(aard_df,20)
dim(aard_df)


filter(aard_df, functionclass=="Overig")
