#Source figure creation code for Nat Comms "Coupling comprehensive pesticide-wide association study to iPSC dopaminergic neuron screening identifies and classifies Parkinson-relevant pesticides". Some figures are not included as they were created using other software (see methods)

library(pacman)
pacman::p_load(ggplot2, ggpubr, dplyr, tibble, Hmisc, data.table, formattable, tidyr, tidyverse, broom, ggrepel, hrbrthemes, readxl, viridis, gridExtra, dendextend, scales, ggh4x, reshape2, stringr, openxlsx, ggupset)


setwd('/Volumes/Extreme SSD/PWAS/Comb Paper/Nat Comms/Code/Source_Data')

########################################################################################################
#Figure 1

##########
#Figure 1B
Fig1B<-read.csv('Source_dat_Fig1B.csv')

type_names <- list('AI'="Total number of different pesticide AIs applied per year",'lbs'="Total reported lbs of pesticide AI applied per year")
type_labeller <- function(variable,value){ return(type_names[value]) }
million <- function(x) { ifelse(x >= 1e6, paste0(x/1e6," Million"), x) }

F1B<-ggplot(Fig1B, aes(x=year, y=y))  + geom_line()   +
  facet_wrap(facet  ~ ., scales='free_y', ncol=1, labeller = labeller(.cols = type_labeller)) + 
  theme_minimal() + 
  scale_color_viridis(discrete = TRUE, option = "A") + 
  ggtitle('PUR-Reported Pesticide AI Application Across Tri-Counties\nKern, Fresno, & Tulare; 1974-2017') + 
  xlab('') + ylab('') + 
  theme(strip.text = element_text(hjust = 1), 
        plot.title.position = "plot") + 
  geom_vline(xintercept=1989, linetype='dashed', color='grey', size=0.5) +
  scale_y_continuous(labels = million) +
  expand_limits(y=0)
F1B

# png(filename="Fig1B.png", units="in", width=6,  height=4,  res=300)
# print(F1B)
# dev.off()

##########
#Figure 1C
Fig1C<-read.csv('Source_dat_Fig1C.csv')

F1C<-ggplot(Fig1C, aes(x=Year, y=avg_sum_total_lbs_AI, color=as.factor(PD) ))  +
  geom_line() +
  facet_wrap(facet  ~ ., scales='free_y', ncol=1) + 
  theme_minimal()  +
  scale_colour_manual(values = c("#0072B2", "#D55E00"), labels = c("No","Yes"), name = "PD") +
  ylab("Average reported lbs AI applied") +
  ggtitle('Average of the total pounds of pesticide AI applied per acre per person', subtitle='Application within 500m of PEG participants residence and workplace') + 
  ylab('Average lbs/acre applied') + 
  theme(strip.text = element_text(hjust = 0), 
        plot.title.position = "plot", 
        axis.title.x = element_text(hjust = 1)) + 
  geom_vline(xintercept=1989, linetype='dashed', color='grey', size=0.5)  +
  expand_limits(y=0)
F1C

# png(filename="Fig1C.png",  units="in", width=6,  height=4, res=300)
# print(F1C)
# dev.off()


##########
#Figure 1D
Fig1D<-read.csv('Source_dat_Fig1D.csv')

F1D<-ggplot(Fig1D, aes(x = Year, y = Pest_reg)) +
  geom_line()+
  geom_point()+
  theme_ipsum() + ylab('Number of pesticides') +
  ggtitle('Number of PWAS-implicated pesticides registered with US EPA by year') + ylim(0,70) +
  geom_text_repel(data = Fig1D,aes(label = Annotate),size = 5,box.padding = unit(2, "lines"), point.padding = unit(0.3, "lines"), nudge_y = -Fig1D$Year/100, direction="y", segment.color = 'gray') + 
  labs(caption = "Annotation indicates year the specific pesticide's registration was cancelled or withdrawn\nAll implicated pesticides were registered prior to 1986") + 
  theme(plot.caption = element_text(hjust = 0, face= "plain", size=16),
        plot.title.position = "plot", 
        plot.caption.position =  "plot",
        axis.text.y = element_text(size=16),
        axis.text.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16)) +
  xlim(1984,2017)
F1D

# png(filename="Fig1D.png",  units="in", width=10,  height=4, res=300)
# F1D
# dev.off()


########################################################################################################
#Figure 2

##########
#Figure 2A
Fig2A<-read.csv('Source_dat_Fig2A.csv')

F2A_1<-ggplot(Fig2A, aes(x=as.factor(chem_ind), y=logp, color=Use_Type_PAN_FIRST)) + 
  geom_hline(yintercept=2, color='red') + geom_rug( alpha = 1/2, outside = TRUE, sides = "br") +
  coord_cartesian(clip = "off") + geom_point() + 
  theme_ipsum() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14),
        legend.position="right") +
  labs(color='Type (PAN)') + ylab("-logp") + xlab('Pesticides')

legend <- get_legend(F2A_1)

type_names <- list('Adjuvant'="",'Defoliant'="", 'Fumigant'="" ,'Fungicide'="Fungicide",'Herbicide'="Herbicide",' Insect Growth Regulator'='',  'Insecticide'="Insecticide", 'Microbiocide'="",'Other'="", 'Pheromone'="",'Plant Growth Regulator'="",'Rodenticide'="", 'Solvent'=""
)

type_labeller <- function(variable,value){
  return(type_names[value])
}

F2A_2<- ggplot(Fig2A, aes(x=as.factor(chem_ind), y=logp, color=Use_Type_PAN_FIRST)) + 
  geom_hline(yintercept=2.04, color='red') + geom_point() +
  theme_minimal() + 
  labs(color='Type (PAN)') + ylab("-logp") + xlab('Pesticides') +
  facet_grid(. ~ Use_Type_PAN_FIRST, scales="free", space = "free", labeller = labeller(.cols = type_labeller)) +
  geom_rug( alpha = 1/2, outside = TRUE, sides = "b") + 
  coord_cartesian(clip = "off") + 
  ggtitle("Pesticides-wide Association Study of PD: Manhattan Plot by Pesticide Type") +
  geom_text_repel(data= subset(Fig2A, Fig2A$fixed.p < 0.0001),aes(label = chem_name),nudge_y = 0.2, size = 4, point.padding = 0.5, direction='both') + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 16), 
        axis.title.x=element_text(size = 16), 
        axis.title.y=element_text(size = 16), 
        plot.title = element_text(size = 18, face = "plain"),
        legend.position="none", 
        plot.margin=unit(c(0.25,0,0.25,1),"cm"),
        plot.caption = element_text(hjust = 0, face= "plain", size=16),
        plot.title.position = "plot",
        plot.caption.position =  "plot") + 
  labs(y=expression(-log["10"](P)))

grid.arrange(F2A_2,legend,ncol=2, widths =c(2.3, 0.6))

# png(filename="Fig2A.png", units="in", width=11,  height=5.5,  res=300)
# grid.arrange(p2,legend,ncol=2, widths =c(2.3, 0.6))
# dev.off()


##########
#Figure 2B
Fig2B<- read.csv('Source_dat_Fig2B.csv')

Fig2B$chemname2<-reorder(Fig2B$chemname2,Fig2B$meta_OR)

fdr_facet <- list(
  'A'="FDR<0.01",
  'B'="FDR 0.01-0.05",
  'C'="FDR 0.05-0.10"
)

facet_labeller2 <- function(variable,value){
  return(fdr_facet[value])
}

F2B<- ggplot(Fig2B, aes(x = meta_OR, y = chemname2))  +
  geom_errorbar(aes(xmin=LCL_exp, xmax=UCL_exp), width=.1, alpha=0.3) + 
  geom_point(aes(size=R_Percent_ever, color = fixed.p), alpha = 0.90) + 
  scale_y_discrete(label = function(x) stringr::str_trunc(x, 40))  +
  scale_colour_gradient(low="red", name = "P-value") +
  theme_ipsum()   +
  labs(size="% Exposed") +
  xlab("OR for PD risk") + ylab("") + 
  ggtitle("PWAS: PD and Pesticide Meta Associations", subtitle='') + 
  theme(plot.title = element_text(size = 18, face = "plain"),
        plot.subtitle = element_text(size =14), 
        axis.text.x = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.y = element_text(size=12),
        plot.caption = element_text(hjust = 0, face= "plain", size=16),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        legend.title = element_text(size=14),
        legend.text = element_text(size=14), 
        strip.text.y = element_text(size = 16),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  facet_grid(y_facet~., scales = 'free_y', space='free_y', labeller=facet_labeller2)

F2B

# png(filename="Fig2B.png",  units="in",  width=6, height=12.5, res=300)
# pt1.dot
# dev.off()

##########
#Figure 2C

Fig2C<-read.csv('Source_dat_Fig2C.csv')

Fig2C$Group<-reorder(Fig2C$Group,Fig2C$estimate)
Fig2C$Type_f = factor(Fig2C$Type, levels=c('Toxicity Group','Use Type','Chemical Class'))

F2C<-ggplot(Fig2C, aes(x = estimate, y = Group)) + geom_errorbar(aes(xmin=conf.low, xmax=conf.high), width=.1, alpha=0.3) + 
  geom_point(aes(size=Hits_InCat, color = ORA.fdr), alpha = 0.90)  +
  scale_colour_gradient(low="red", name = "ORA FDR", limits=c(0,0.1), na.value="lightblue") +
  theme_ipsum() +
  labs(size="Number of Pesticide\nHits in Group") + 
  xlab("OR") + ylab("") + 
  ggtitle("PD-Associated Pesticide Overrepresentation Analysis", subtitle='') +  
  geom_segment(aes(x = LCL_l, xend = UCL_l, y = Group , yend = Group ), arrow = arrow(length = unit(0.2, "cm")), alpha=0.4)+ xlim(0, 15) + geom_vline(xintercept = 1) +
  facet_grid(Type_f~., scales = 'free', space='free_y')  + 
  scale_y_discrete(labels = c(
    "PAN.Bad.Actor" = "PAN Bad Actor",
    "Highly.Hazardous.Pesticide" = "Highly Hazardous Pesticide",
    "Drift.Prone2" = "Drift Prone",
    "Acute.Toxicity2" = "Acute Toxicity",
    "Ground.Water2" = "Ground Water",
    "Carcinogen2" = "Carcinogen",
    "Endocrine.Disruptor" = "Endocrine Disruptor",
    "Cholinesterase.Inhibitor" = "Cholinesterase Inhibitor",
    "PlantGrowthRegulator" = "Plant Growth Regulator"))  + 
  labs(caption = "Associated pesticide set includes all PWAS-assocaited pesticides at FDR<0.05 (n=53 pesticides)") +
  theme(plot.title = element_text(size = 20, face = "plain"), 
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(hjust = 0, face= "plain", size=16), 
        plot.title.position = "plot", 
        plot.caption.position =  "plot",
        axis.text.y = element_text(size=17), 
        axis.text.x = element_text(size=18), 
        strip.text.y = element_text(size = 18), 
        axis.title.x = element_text(size=18), 
        legend.title = element_text(size=14),
        legend.text = element_text(size=14))

F2C

# png(filename="Fig2C.png", units="in", width=7,  height=10,  res=300)
# F2C
# dev.off()


##########
#Figure 2D

Fig2D<-read.csv('Source_dat_Fig2D.csv')
Fig2D$Type_f = factor(Fig2D$Type, levels=c('Toxicity Group','Use Type','Chemical Class'))
Fig2D$Group<-reorder(Fig2D$Group,Fig2D$Percent_InHitvTotHit)

F2D<-ggplot(Fig2D, aes(y=Percent_InHitvTotHit, x= Group, fill=Percent_InHitvTotHit)) + 
  geom_bar(stat='identity') + coord_flip() +
  geom_text(aes(label=round(Percent_InHitvTotHit,2.0)),vjust=0.5,hjust=0, size = 5.5) + 
  scale_fill_gradient2(high="coral4") +
  ggtitle('Percent of Pesticides in Each Group Associated with PD in PWAS') + 
  theme_ipsum() + 
  theme(axis.title.y = element_blank(), 
        axis.text = element_text(size = 16),
        plot.title.position = "plot", 
        plot.caption.position =  "plot", 
        legend.position = "none",
        plot.title = element_text(size = 20, face = "plain"), 
        plot.subtitle = element_text(size = 10), 
        strip.text.y = element_text(size = 18),
        axis.text.y = element_text(size=17),
        axis.text.x = element_text(size=18),
        axis.title.x = element_text(size=17)) + 
  scale_x_discrete(labels = c(
    "PAN.Bad.Actor" = "PAN Bad Actor",
    "Highly.Hazardous.Pesticide" = "Highly Hazardous Pesticide",
    "Drift.Prone2" = "Drift Prone",
    "Acute.Toxicity2" = "Acute Toxicity",
    "Ground.Water2" = "Ground Water",
    "Carcinogen2" = "Carcinogen",
    "Endocrine.Disruptor" = "Endocrine Disruptor",
    "Cholinesterase.Inhibitor" = "Cholinesterase Inhibitor",
    "PlantGrowthRegulator" = "Plant Growth Regulator")) +
  facet_grid(Type_f~., scales = 'free', space='free_y') +
  xlab("") +ylab("Percent") + 
  labs(caption = " ") + 
  geom_text(aes(y=110, label=paste0(Hits_InCat,"/", Total_InCat) ), vjust=0.5,hjust="inward", size = 5.5)

F2D

# png(filename="F2D.png",units="in", width=7.2, height=10, res=300)
# print(F2D)
# dev.off()


########################################################################################################
#Figure 4

##########
#Figure 4A
Fig4A <- read_excel('Source_dat_Fig4A.xlsx')

#uppercase first letter of chemical
Fig4A$toxicant<-str_to_title(Fig4A$toxicant)

#clean up MSMA/DSMA to all caps
Fig4A$toxicant<-ifelse(Fig4A$toxicant=='Dmso','DMSO',ifelse(Fig4A$toxicant=='Msma','MSMA',Fig4A$toxicant)) 

#set up indicator for negative control (DSMO, water), positive control (rotenone, ziram), and PWAS pesticides
Fig4A$type<-ifelse(Fig4A$toxicant %in% c('Water','DMSO'), '(-) Controls',
            ifelse(Fig4A$toxicant %in% c('Rotenone','Ziram'), '(+) Controls','PWAS Pesticides'))

#set order so same as dot plot
Fig4A$toxicant2 <- factor(Fig4A$toxicant, levels = c('DMSO','Water','Rotenone','Ziram',"Sodium Chlorate","Dicofol","Prometryn",'Methomyl','Chlorpyrifos','Dimethoate','1,3-Dichloropropene','Phorate','Diuron','Copper Sulfate (Pent)','Malathion','Acephate','1,2-Dichloropropane','Trifluralin','MSMA','Bromoxynil Octanoate','Oxydemeton-Methyl','Dicloran','Fluazifop-Butyl','Carbaryl','Propargite','Diquat Dibromide','Glyphosate','Endothall','Ethephon','Carbofuran','Methidathion','Crude Oil','Methamidophos','Dinoseb','2,4-D, Isopropyl Ester','Azinphos-Methyl','Iprodione','Copper Sulfate (Basic)','Endosulfan','Naled','S,S,S-Tributyl Phosphorotrithioate (Aka Tribufos)','Oryzalin','Parafin Oil','Folpet','Propyzamide','Chlorthal-Dimethyl','Xylene','Permethrin'))

F4A<-ggplot(data = Fig4A, aes(x=toxicant2, y= THplus_cells, color=type)) + geom_point(position = position_jitter(w = 0.25, h = 0)) + theme_ipsum() + ylab('# of THtdt+ cells') + xlab('') +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=14), axis.text.y=element_text(size=14),axis.title.y=element_text(size=16), legend.text=element_text(size=16), legend.position="top", legend.justification='left') +  
  facet_nested(. ~ type + toxicant2, scales="free", space = "free") +  
  theme(strip.text.x = element_blank(), panel.spacing.x=unit(0, "lines")) + 
  geom_hline(yintercept=45.6, color = "gold", size=2.5) + 
  scale_color_manual(name='', values=c("#00BA38", "#F8766D", "#619CFF"))

F4A

# png(filename="F4A.png",  units="in", width=12, height=9, res=300)
# print(F4A)
# dev.off()


##########
#Figure 4C

Fig4C_indiv<-as.data.frame(read_excel('Source_dat_Fig4C.xlsx', sheet=1))
Fig4C_indiv$ind<-'Ind'

Fig4C_avg <- as.data.frame(read_excel('Source_dat_Fig4C.xlsx', sheet=2))
Fig4C_avg$ind<-'Avg'

Fig4C<-rbind(Fig4C_indiv, Fig4C_avg)

Fig4C$Th_ind<-ifelse(Fig4C$ind=='Ind', Fig4C$`TH+ cells`, NA)
Fig4C$Th_avg<-ifelse(Fig4C$ind=='Avg', Fig4C$`TH+ cells`, NA)

#create indicator for AZM (negative example)
Fig4C$AZM<-ifelse(Fig4C$toxicant=='Azinphos-Methyl','Y','N')

#reorder facets
Fig4C$toxicant<-reorder(Fig4C$toxicant, Fig4C$`TH+ cells`)
#put AZM at end 
Fig4C$toxicant <-forcats::fct_relevel(Fig4C$toxicant, "Azinphos-Methyl", after = Inf)

#clean up names
Fig4C$concentration <- Fig4C$`concentration (in micromolar)`
Fig4C$TH<-Fig4C$`TH+ cells`

#reorder concentration (keep it factor)
Fig4C$concentration <- factor(Fig4C$concentration, levels=c("0.2", "1", "6", "30"))

F4C<-ggplot(Fig4C, aes()) +
  geom_jitter(aes(x=concentration,y=Th_ind), color='#802582FF', width=0.05)  + 
  theme_minimal() + 
  ylab("#THtd+ cells") + xlab("[pesticide] in micromolar") + 
  geom_line(aes(x=concentration,y=Th_avg, color=AZM, group=ind))  + ylim(1,125) + 
  facet_wrap(toxicant~., nrow=2, scales = "free") + scale_color_manual(values=c('#E85362FF','blue'))  + theme (legend.position = "none") 

F4C
 
# png(filename="Fig4C.png", units="in",  width=10,  height=4, res=300)
# F4C
# dev.off()




########################################################################################################
#Figure 5

##########
#Figure 5A

Fig5A <- data.matrix(read.csv("Source_dat_Fig5A.csv", header = TRUE, row.names = 1, sep = ","))
colnames(Fig5A)<-row.names(Fig5A)


#heatmap of modules
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[rev(hc$order),hc$order]
}

#bold DA hits
#https://stackoverflow.com/questions/39694490/highlighting-individual-axis-labels-in-bold-using-ggplot2

colorado <- function(src, boulder) {
  if (!is.factor(src)) src <- factor(src)  
  src_levels <- levels(src)         
  brave <- boulder %in% src_levels    
  if (all(brave)) {                                      
    b_pos <- purrr::map_int(boulder, ~which(.==src_levels)) 
    b_vec <- rep("plain", length(src_levels))         
    b_vec[b_pos] <- "bold"                        
    b_vec                                          
  } else {
    stop("All elements of 'boulder' must be in src")
  }
}

#order by distance
Fig5A <- reorder_cormat(Fig5A)

melted_cormat <- melt(Fig5A, na.rm = TRUE)

F5A<- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+ geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", space = "Lab", name="Pearson\nCorrelation", midpoint = 0.1, limits=c(0, 0.75), oob=squish ) +
  theme_minimal() + 
  coord_fixed() + ylab("") + xlab("") + 
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 25)) +
  ggtitle("Residential Pesticide Exposure Correlation Heatmap",subtitle ="PD-associated pesticides") + 
  theme(plot.title = element_text(size = 24), 
        plot.subtitle =  element_text(size = 20), 
        legend.position="right",  
        axis.text.y=element_text(size = 14, hjust = 1, face='bold'), 
        legend.title = element_text(size=18), 
        legend.text = element_text(size=16),
        axis.text.x=element_text(angle = -45, vjust = 1, size = 10, hjust = 0, face=colorado(melted_cormat$Var2, c("Dicofol",'Copper Sulfate (Basic)','Copper Sulfate (Pentahydrate)','Trifluralin','Propargite','Diquat Dibromide','Endothall, Mono [N,N-Dimethyl Alkylamine] Salt','Endothall, Mono (N,N-Diethyl Alkylamine) Salt','Endosulfan', "Naled",'Folpet')))) + 
  scale_y_discrete(
    breaks=c("Dicofol",'Copper Sulfate (Basic)','Copper Sulfate (Pentahydrate)','Trifluralin','Propargite','Diquat Dibromide','Endothall, Mono [N,N-Dimethyl Alkylamine] Salt','Endothall, Mono (N,N-Diethyl Alkylamine) Salt','Endosulfan', "Naled",'Folpet'),
    label = function(x) stringr::str_trunc(x, 35), position = "right")


F5A

# png(filename="F5A.png", units="in", width=16, height=14, res=300)
# print(F5A)
# dev.off()

########################################################################################################
#Figure 6

##########
#Figure 6A
Fig6A<-read.csv('Source_dat_Fig6A.csv')

# Fig6A dataset is a correlation matrix
# Correlation is a measure of similarity so we’ll turn it to into a measure of dissimilarity before passing it to the as.dist function. turn the correlation into a distance measure by subtracting it from 1 (so perfectly positively correlated variables have distance 0) and passing it to the as.dist function
Fig6A.dist <- as.dist(1 - Fig6A)
#hclust for hierarchical clustering,  method = 'average' calculates the average distance between clusters before merging
Fig6A.tree <- hclust(Fig6A.dist, method="average")
Fig6A.dend <- as.dendrogram(Fig6A.tree) # create dendrogram object

nleaves(Fig6A.dend)  # number of leaves (ie pesticides) in tree
## [1] 68
nnodes(Fig6A.dend)  # number of nodes (=leaves + joins) in tree
## [1] 135

#cut tree to get clusters at height of 0.55 (correlation of 0.45)
clusters <- cutree(Fig6A.dend, h=0.55)
table(clusters)

#supplemental table 12, residential exposure cluster details
TS12<-data.frame(chemname2 = names(clusters), cluster = clusters)

# pdf("Fig6A.pdf",width=10,height=7)

#plot horizontal
par(mar=c(12,5,2,2))
Fig6A.dend %>%
  set("labels_cex", 0.75) %>%
  set("branches_k_color", h=0.55)%>% 
  hang.dendrogram  %>% 
  plot(main="Residential Pesticide Exposure Correlation Cluster Dendrogram", sub='', ylab='Height (1-Correlation)', xlab='', cex.lab=1.4, cex.axis=1.4, cex.main = 1.6) 
panel.first = 
  c(abline(h = c(0.0,0.2,0.4,0.6,0.8,1.0), lty = 3, lwd = 0.8, col = 'grey') ) 
panel.first = 
  c(abline(h = c(0.55), lty =1, lwd = 0.8, col = 'red') ) 
Fig6A.dend %>% rect.dendrogram(h=0.55, which=c(25),  border = 0, lty = 3, lwd = 0, lower_rect = -0.55, upper_rect = -0.001, col= rgb(0,0,1.0,alpha=0.1))

# dev.off()


##########
#Figure 6B
Fig6B<-read.csv('Source_dat_Fig6B.csv')
Fig6B$Chem<-reorder(Fig6B$Chem, Fig6B$Percent)

F6B<-ggplot(Fig6B, aes(y=Percent, x=Chem, fill=Percent)) + 
  geom_bar(stat='identity') + coord_flip() +
  geom_text(aes(label=round(Percent,2.0)),vjust=0.5,hjust="inward", size = 6) + 
  scale_fill_gradient2(high="coral4") +
  ggtitle('Percent of Total PUR-Reported Application on Cotton') +  theme_minimal()+ 
  theme(axis.title.y = element_blank(), 
        axis.text = element_text(size = 18),  plot.title = element_text(size = 20),
        plot.title.position = "plot",
        plot.caption.position =  "plot", legend.position = "none")
F6B

# png(filename="Fig6B.png",units="in", width=9, height=6, res=300)
# print(F6B)
# dev.off()

##########
#Figure 6B



Fig6D_rep1<-read.xlsx("Source_dat_Fig6D.xlsx", sheet=2)

Fig6D_rep1_factor<-Fig6D_rep1 %>% mutate(Aldicarb = ifelse(!is.na(Aldicarb), "Aldicarb", NA)) %>%
  mutate(Promethryn  = ifelse(!is.na(Promethryn), "Promethryn", NA)) %>%
  mutate(Trifluralin  = ifelse(!is.na(Trifluralin), "Trifluralin", NA)) %>%
  mutate(Ethelphon  = ifelse(!is.na(Ethelphon), "Ethephon", NA)) %>%
  mutate(Tribufos  = ifelse(!is.na(Tribufos), "Tribufos", NA)) %>%
  mutate(Phorate  = ifelse(!is.na(Phorate), "Phorate", NA)) %>%
  mutate(DMSO = ifelse(!is.na(DMSO), "DMSO", NA))


Fig6D_rep2<-read.xlsx("Source_dat_Fig6D.xlsx", sheet=3)

Fig6D_rep2_factor<-Fig6D_rep2 %>% mutate(Aldicarb = ifelse(!is.na(Aldicarb), "Aldicarb", NA)) %>%
  mutate(Promethryn  = ifelse(!is.na(Promethryn), "Promethryn", NA)) %>%
  mutate(Trifluralin  = ifelse(!is.na(Trifluralin), "Trifluralin", NA)) %>%
  mutate(Ethelphon  = ifelse(!is.na(Ethelphon), "Ethephon", NA)) %>%
  mutate(Tribufos  = ifelse(!is.na(Tribufos), "Tribufos", NA)) %>%
  mutate(Phorate  = ifelse(!is.na(Phorate), "Phorate", NA)) %>%
  mutate(DMSO = ifelse(!is.na(DMSO), "DMSO", NA))


Fig6D_rep3<-read.xlsx("Source_dat_Fig6D.xlsx", sheet=4)

Fig6D_rep3_factor<-Fig6D_rep3 %>% mutate(Aldicarb = ifelse(!is.na(Aldicarb), "Aldicarb", NA)) %>%
  mutate(Promethryn  = ifelse(!is.na(Promethryn), "Promethryn", NA)) %>%
  mutate(Trifluralin  = ifelse(!is.na(Trifluralin), "Trifluralin", NA)) %>%
  mutate(Ethelphon  = ifelse(!is.na(Ethelphon), "Ethephon", NA)) %>%
  mutate(Tribufos  = ifelse(!is.na(Tribufos), "Tribufos", NA)) %>%
  mutate(Phorate  = ifelse(!is.na(Phorate), "Phorate", NA)) %>%
  mutate(DMSO = ifelse(!is.na(DMSO), "DMSO", NA))


Fig6D_rep4<-read.xlsx("Source_dat_Fig6D.xlsx", sheet=5)


Fig6D_rep4_factor<-Fig6D_rep4 %>% mutate(Aldicarb = ifelse(!is.na(Aldicarb), "Aldicarb", NA)) %>%
  mutate(Promethryn  = ifelse(!is.na(Promethryn), "Promethryn", NA)) %>%
  mutate(Trifluralin  = ifelse(!is.na(Trifluralin), "Trifluralin", NA)) %>%
  mutate(Ethelphon  = ifelse(!is.na(Ethelphon), "Ethephon", NA)) %>%
  mutate(Tribufos  = ifelse(!is.na(Tribufos), "Tribufos", NA)) %>%
  mutate(Phorate  = ifelse(!is.na(Phorate), "Phorate", NA)) %>%
  mutate(DMSO = ifelse(!is.na(DMSO), "DMSO", NA))


Fig6D<-bind_rows(Fig6D_rep1_factor, Fig6D_rep2_factor, Fig6D_rep3_factor, Fig6D_rep4_factor)

Fig6D_mean_thb<-Fig6D %>% group_by(Aldicarb, Promethryn,Trifluralin,Ethelphon,Tribufos,Phorate,DMSO) %>%
  dplyr::summarize(THB=mean(THbright3))

Fig6D_mean_thb_exp<-Fig6D_mean_thb %>% unite(., col = "Expression",  Aldicarb, Promethryn,Trifluralin,Ethelphon,Tribufos,Phorate, na.rm=TRUE, sep = "&")

Fig6D_mean_thb_exp_tib<-Fig6D_mean_thb_exp %>% slice(-n())

Fig6D_mean_thb_exp_tib<- Fig6D_mean_thb_exp_tib%>% arrange(desc(THB))
Fig6D_mean_thb_exp_tib$Expression<-factor(Fig6D_mean_thb_exp_tib$Expression, levels=Fig6D_mean_thb_exp_tib$Expression)

F6D<-Fig6D_mean_thb_exp_tib %>%
  ggplot(aes(x=Expression, y=THB, fill=THB)) +
  geom_bar(stat="identity") +
  scale_fill_viridis(option="plasma") +
  axis_combmatrix(sep="&") +
  theme_bw() +
  theme(plot.margin=grid::unit(c(5,5,0,10), "mm"))+
  labs(x="Toxicant Combination", y="Mean #THtdtomato cells", fill="") 

F6D

# ggsave("Fig6D.pdf", height=7, width=11, F6D)

########################################################################################################
#Supplemental Figure 1

##########
#Figure S1A

FigS1A<- read.csv('Source_dat_FigS1A.csv')

FS1A<-ggplot(FigS1A, aes(x=x, fill = as.factor(PD), group=as.factor(PD))) +
  geom_histogram(binwidth=3, alpha = 0.5, position = "identity", aes(y = (..count..)/sum(..count..)))  +
  facet_wrap(facet  ~ ., scales='free_y', ncol=1)  + 
  theme_minimal() + scale_color_viridis(discrete = TRUE, option = "A") + 
  ggtitle('Total number of different pesticide AIs applied near PEG participants', subtitle='1974-PEG Enrollment (2000-2012)') + 
  scale_y_continuous(labels = percent) +
  ylab("Percent of participants")+ xlab('Number of unique pesticide AIs') + 
  theme(strip.text = element_text(hjust = 1), 
        plot.title.position = "plot", 
        axis.title.x = element_text(hjust = 1)) + 
  scale_fill_manual(values = c("#0072B2", "#D55E00"), labels = c("No","Yes"), name='PD')

FS1A

# png(filename="FigS1A.png",
#    units="in",
#    width=6,
#    height=6,
#    res=300)
# print(FS1A)
# dev.off()

##########
#Figure S1B
FigS1B<- read.csv('Source_dat_FigS1B.csv')

FS1B<-ggplot(FigS1B, aes(x=Year, y=avg_total_num_AI_yr, color=as.factor(PD)))  +
  geom_line()  +
  geom_errorbar(aes(ymin = 0, ymax = u90_total_num_AI_yr), alpha=0.25) +
  facet_wrap(facet  ~ ., scales='free_y', ncol=1) + 
  theme_minimal()  +
  scale_colour_manual(values = c("#0072B2", "#D55E00"), labels = c("No","Yes"), name='PD') + 
  ylab("Total number of different AI applied within the buffer")+ 
  ggtitle('Total number of different pesticide AI applied near PEG participants, by year', subtitle='Application within 500m of PEG participants residence and workplace') + 
  theme(strip.text = element_text(hjust = 0),
        plot.title.position = "plot",
        axis.title.x = element_text(hjust = 1)) + 
  geom_vline(xintercept=1989, linetype='dashed', color='black', size=0.5, alpha=0.5) 

FS1B

# png(filename="FigS1B.png", units="in", width=6.5, height=6.5, res=300)
# print(FS1B)
# dev.off()

########################################################################################################
#Supplemental Figure 2

FigS2<- read.csv('Source_dat_FigS2.csv')


FS2<-ggplot(FigS2, aes(x=Year, y=med_sum_total_lbs_AI, color=as.factor(PD)))  +
  geom_line() +
  facet_wrap(facet  ~ ., scales='free_y', ncol=1) + 
  theme_minimal()  +
  scale_colour_manual(values = c("#0072B2", "#D55E00"), labels = c("No","Yes"), name='PD')+ 
  ylab("Median of total reported lbs AI applied")+ 
  ggtitle('Median of the total pounds of pesticide AI applied per acre', subtitle='Application within 500m of PEG participants residence and workplace') + ylab('Median of total lbs/acre applied') + 
  theme(strip.text = element_text(hjust = 0), 
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, size=7),
        axis.title.x = element_text(hjust = 1) ) + 
  geom_vline(xintercept=1989, linetype='dashed', color='grey', size=0.5)  + 
  labs(caption = "Median among those with any pounds applied within buffer") 

FS2

########################################################################################################
#Supplemental Figure 3

FigS3_1 <- read_excel('Source_dat_FigS3.xlsx' , sheet=1)
FigS3_2 <- read_excel('Source_dat_FigS3.xlsx' , sheet=2)
FigS3_3 <- read_excel('Source_dat_FigS3.xlsx' , sheet=3)

FigS3_1$chemname2<-factor(FigS3_1$chemname2, levels = levels(Fig2B$chemname2))
FigS3_2$chemname2<-factor(FigS3_2$chemname2, levels = levels(Fig2B$chemname2))
FigS3_3$chemname2<-factor(FigS3_3$chemname2, levels = levels(Fig2B$chemname2))

#create facet names
facet_names <- list(
  'Residence'="",
  'Workplace'="",
  'W Workplace'="Workplace",
  'R Residence'="Residence",
  'A'='',
  'B'='',
  'C'=''
)

facet_labeller <- function(variable,value){
  return(facet_names[value])
}

FigS3_1$UCL_l<-ifelse(FigS3_1$UCL_l=='NA' ,'', FigS3_1$UCL_l)
FigS3_1$LCL_l<-ifelse(FigS3_1$LCL_l=='NA' ,'', FigS3_1$LCL_l)
FigS3_1$UCL_l<-as.numeric(FigS3_1$UCL_l)
FigS3_1$LCL_l<-as.numeric(FigS3_1$LCL_l)


S3_1<- ggplot(Fig2B, aes(x = meta_OR, y = chemname2))  +
  geom_errorbar(aes(xmin=LCL_exp, xmax=UCL_exp), width=.1, alpha=0.3) + 
  geom_point(aes(size=R_Percent_ever, color = fixed.p), alpha = 0.90) + 
  scale_y_discrete(label = function(x) stringr::str_trunc(x, 40))  +
  scale_colour_gradient(low="red", name = "P-value") +
  theme_ipsum() + labs(size="% Exposed") +
  xlab("OR for PD risk") + ylab("") + 
  ggtitle("PD and Pesticide Meta Associations",subtitle='') + 
  theme(axis.text.y = element_text(size=9), 
        plot.title = element_text(size = 12, face = "plain"), 
        plot.subtitle = element_text(size =6),
        plot.margin=unit(c(1,-0.25,1,1),"cm"),
        panel.spacing.y = unit(0.5, "lines")) +
  facet_grid(y_facet~., scales = 'free_y', space='free_y', labeller=facet_labeller2) 

S3_2<-  ggplot(FigS3_1, aes(x = OR, y = chemname2, color = PEG))  +  
  geom_errorbar(data=FigS3_2, aes(xmin=LCL_exp, xmax=UCL_exp), width=.3, alpha=0.4)+
  geom_errorbar(data=FigS3_3, aes(xmin=LCL_exp, xmax=UCL_exp), width=.3, alpha=0.9) +  
  geom_segment(aes(x = LCL_l, xend = UCL_l, y = chemname2 , yend = chemname2 ), arrow = arrow(length = unit(0.2, "cm")), width=.3, alpha=0.4) + 
  geom_vline(xintercept=1, color='gray') +
  geom_point(alpha=0.6)  +
  facet_grid(y_facet ~ Location, scales = 'free_y', space='free_y', labeller=facet_labeller)  +
  theme_ipsum() + 
  scale_y_discrete(label = function(x) stringr::str_trunc(x, 40)) +
  xlab("OR for PD risk") + ylab("") +
  ggtitle("Estimates by Study Wave and Exposure Location") +
  theme(axis.text.y = element_text(size=9),
        plot.title = element_text(size = 12, face = "plain"), 
        plot.subtitle = element_text(size = 10),
        panel.spacing.x = unit(c(0.6,1.5,0.6), "lines"), 
        panel.spacing.y = unit(0.5, "lines"),
        plot.margin=unit(c(1,1,1,0),"cm"))  + 
  xlim(0.75, 2) +
  scale_color_manual(values=c("Combined"='#7b7b7b',"PEG1"='#FDAE61','PEG2'='#9970AB'), name="")  


grid.arrange(S3_1,S3_2,ncol=2, widths =c(2.5, 5))


# png(filename="FigS3.png", units="in", width=16.25, height=12, res=300)
# grid.arrange(pt1.2,pt2.2,ncol=2, widths =c(2.5, 5))
# dev.off()

########################################################################################################
#Supplemental Figure 4

FigS4<- read.csv('Source_dat_FigS4.csv')
FigS4$chemname2<-reorder(FigS4$chemname2,FigS4$meta_OR)

fdr_facet <- list(
  'A'="FDR<0.01",
  'B'="FDR 0.01-0.05",
  'C'="FDR 0.05-0.10",
  'Men'='Men',
  'Women'='Women',
  '1:PWAS Analysis'='PWAS Analysis'
)

facet_labeller2 <- function(variable,value){
  return(fdr_facet[value])
}

FS4<-ggplot(FigS4, aes(x = meta_OR, y = chemname2)) + 
  geom_vline(xintercept = 1) +
  geom_errorbar(aes(xmin=LCL_exp, xmax=UCL_exp), width=.1, alpha=0.3) + 
  geom_point(aes(color = fixed.p), alpha = 0.90) + 
  scale_y_discrete(label = function(x) stringr::str_trunc(x, 40))  +
  scale_colour_gradient(low="red", name = "P-value") +
  theme_ipsum() +
  labs(size="% Exposed") +
  xlab("OR for PD risk") + ylab("") + 
  ggtitle("PD and Pesticide Meta Associations", subtitle='') + 
  theme(axis.text.y = element_text(size=9),
        plot.title = element_text(size = 12, face = "plain"), 
        plot.subtitle = element_text(size =10),
        plot.margin=unit(c(1,1,1,1),"cm"),
        panel.spacing.y = unit(0.5, "lines"),
        legend.position="none") +
  facet_grid(y_facet~Sex.fac, scales = 'free_y', space='free_y', labeller=facet_labeller2)
FS4

# png(filename="FigS4.png", units="in", width=11.5, height=9, res=300)
# print(FS4)
# dev.off()

  

########################################################################################################
#Supplemental Figure 7

##########
#Figure S7A

#load individual experiment data
FigS7A_indiv <- as.data.frame(read_excel("Source_dat_FigS7A.xlsx", sheet=1))
FigS7A_indiv$ind<-'Ind'

#clean up variable names
FigS7A_indiv$neurite<-FigS7A_indiv$`Nuclei Selected - Total Neurite Length - Sum per Well`
FigS7A_indiv$`Nuclei Selected - Total Neurite Length - Sum per Well`<-NULL

#load average across experiments
FigS7A_avg <- as.data.frame(read_excel("Source_dat_FigS7A.xlsx", sheet=2))
FigS7A_avg$ind<-'Avg'

#clean up variable names
FigS7A_avg$neurite<-FigS7A_avg$`Average Total Neurite Length`
FigS7A_avg$`Average Total Neurite Length`<-NULL

#combine
FigS7A<-rbind(FigS7A_indiv, FigS7A_avg)

FigS7A$Neu_ind<-ifelse(FigS7A$ind=='Ind', FigS7A$neurite, NA)
FigS7A$Neu_avg<-ifelse(FigS7A$ind=='Avg', FigS7A$neurite, NA)

#create indicator for AZM (negative example)
FigS7A$AZM<-ifelse(FigS7A$toxicant=='Azinphos-Methyl','Y','N')

#reorder facets
FigS7A$toxicant<-reorder(FigS7A$toxicant, FigS7A$neurite)
#put AZM at end
FigS7A$toxicant <-forcats::fct_relevel(FigS7A$toxicant, "Azinphos-Methyl", after = Inf)

#reorder concentration (keep it factor)
FigS7A$concentration <- factor(FigS7A$concentration, levels=c("0.2", "1", "6", "30"))

FS7A <-  ggplot(FigS7A, aes()) +
  geom_jitter(aes(x=concentration,y=Neu_ind), color='#802582FF', width=0.05)  + 
  theme_minimal() + 
  ylab("neurite length per well in microns") + xlab("[pesticide] in micromolar") + 
  geom_line(aes(x=concentration,y=Neu_avg, group=ind, color=AZM))  + ylim(0,150000) + 
  facet_wrap(toxicant~., nrow=2, scales = "free") + scale_color_manual(values=c('#E85362FF','blue'))  + theme (legend.position = "none") 

FS7A

# png(filename="FigS7A.png", units="in", width=11, height=4, res=300)
# FS7A
# dev.off()


##########
#Figures S7B & S7C

FigS7BC <- read_excel('Source_dat_FigS7BC.xlsx')

#remove blank rows
FigS7BC <- FigS7BC[complete.cases(FigS7BC), ]

#uppercase first letter of each word
FigS7BC$toxicant<-str_to_title(FigS7BC$toxicant)

#clean up DMSO / MSMA
FigS7BC$toxicant<-ifelse(FigS7BC$toxicant=='Dmso','DMSO',ifelse(FigS7BC$toxicant=='Msma','MSMA',FigS7BC$toxicant)) 

#set up indicator for negative control (DSMO, water), positive control (rotenone, ziram), and PWAS pesticides
FigS7BC$type<-ifelse(FigS7BC$toxicant %in% c('Water','DMSO'), '(-) Controls',
                 ifelse(FigS7BC$toxicant %in% c('Rotenone','Ziram'), '(+) Controls','PWAS Pesticides'))

#change colors, red + control, green - control

# New facet label names 
FigS7BC$day2 <- factor(FigS7BC$day, levels = c(0, 7), labels = c("Day 0", "Day 7"))

#sort by Dot plot
FigS7BC$toxicant2 <- factor(FigS7BC$toxicant, levels = c('DMSO','Water','Rotenone','Ziram',"Sodium Chlorate","Dicofol","Prometryn",'Methomyl','Chlorpyrifos','Dimethoate','1,3-Dichloropropene','Phorate','Diuron','Copper Sulfate (Pent)','Malathion','Acephate','1,2-Dichloropropane','Trifluralin','MSMA','Bromoxynil Octanoate','Mcpa, Dimethylamine Salt','Oxydemeton-Methyl','Dicloran','Fluazifop-Butyl','Dicamba, Dimethylamine Salt','Carbaryl','Propargite','Diquat Dibromide','Glyphosate','Endothall','Ethephon', 'Carbofuran','Methidathion','Crude Oil','Methamidophos','Dinoseb','Calcium Hydroxide','2,4-D, Isopropyl Ester','Azinphos-Methyl','Iprodione','Copper Sulfate (Basic)','Endosulfan','Naled','S,S,S-Tributyl Phosphorotrithioate (Aka Tribufos)','Oryzalin','Sodium Cacodylate','Parafin Oil','Folpet','2,4-D, Dimethylamine Salt','Propyzamide','Chlorthal-Dimethyl','Xylene','Permethrin'))


FS7B<-ggplot(data = FigS7BC[FigS7BC$day==0, ], aes(x=toxicant2, y= value, color=type)) + geom_point(position = position_jitter(w = 0.25, h = 0)) + theme_ipsum() + ylab('# of THtdt+ cells') + xlab('') +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=14), axis.text.y=element_text(size=14),axis.title.y=element_text(size=16), legend.text=element_text(size=16), legend.position="top", legend.justification='left') +  
  facet_nested(. ~ type + toxicant2, scales="free", space = "free") +  
  theme(strip.text.x = element_blank(), panel.spacing.x=unit(0, "lines")) + ggtitle('Day 0') +
  scale_color_manual(name='', values=c("#00BA38", "#F8766D", "#619CFF")) + expand_limits(y = 0
                                                                                         )
FS7B

# png(filename="FigS7B.png", units="in",width=12, height=9, res=300)
# print(FS7B)
# dev.off()


FS7C<-ggplot(data = FigS7BC[FigS7BC$day==7, ], aes(x=toxicant2, y= value, color=type)) + geom_point(position = position_jitter(w = 0.25, h = 0)) + theme_ipsum() + ylab('# of THtdt+ cells') + xlab('') +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=14), axis.text.y=element_text(size=14),axis.title.y=element_text(size=16), legend.text=element_text(size=16), legend.position="top", legend.justification='left') +  
  facet_nested(. ~ type + toxicant2, scales="free", space = "free") +  
  theme(strip.text.x = element_blank(), panel.spacing.x=unit(0, "lines")) + ggtitle('Day 7') +
  scale_color_manual(name='', values=c("#00BA38", "#F8766D", "#619CFF"))+ expand_limits(y = 0)

FS7C

# png(filename="FigS7C.png", units="in",width=12, height=9, res=300)
# print(FS7C)
# dev.off()


##########
#Figures S7D & S7E

FigS7DE <- read_excel('Source_dat_FigS7DE.xlsx')

#uppercase first letter of each word
FigS7DE$toxicant<-str_to_title(FigS7DE$toxicant)

#clean up DMSO / MSMA
FigS7DE$toxicant<-ifelse(FigS7DE$toxicant=='Dmso','DMSO',ifelse(FigS7DE$toxicant=='Msma','MSMA',FigS7DE$toxicant)) 


#set up indicator for negative control (DSMO, water), positive control (rotenone, ziram), and PWAS pesticides
FigS7DE$type<-ifelse(FigS7DE$toxicant %in% c('Water','DMSO'), '(-) Controls',
                 ifelse(FigS7DE$toxicant %in% c('Rotenone','Ziram'), '(+) Controls','PWAS Pesticides'))


#sort by Dot plot
FigS7DE$toxicant2 <- factor(FigS7DE$toxicant, levels = c('DMSO','Water','Rotenone','Ziram',"Sodium Chlorate","Dicofol","Prometryn",'Methomyl','Chlorpyrifos','Dimethoate','1,3-Dichloropropene','Phorate','Diuron','Copper Sulfate (Pent)','Malathion','Acephate','1,2-Dichloropropane','Trifluralin','MSMA','Bromoxynil Octanoate','Mcpa, Dimethylamine Salt','Oxydemeton-Methyl','Dicloran','Fluazifop-Butyl','Dicamba, Dimethylamine Salt','Carbaryl','Propargite','Diquat Dibromide','Glyphosate','Endothall','Ethephon', 'Carbofuran','Methidathion','Crude Oil','Methamidophos','Dinoseb','Calcium Hydroxide','2,4-D, Isopropyl Ester','Azinphos-Methyl','Iprodione','Copper Sulfate (Basic)','Endosulfan','Naled','S,S,S-Tributyl Phosphorotrithioate (Aka Tribufos)','Oryzalin','Sodium Cacodylate','Parafin Oil','Folpet','2,4-D, Dimethylamine Salt','Propyzamide','Chlorthal-Dimethyl','Xylene','Permethrin'))

FS7D<-ggplot(data = FigS7DE, aes(x=toxicant2, y= `Nucleus Area [µm²] - Mean per Well`, color=type)) + geom_point(position = position_jitter(w = 0.25, h = 0)) + theme_ipsum() + xlab('') +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=14), axis.text.y=element_text(size=14),axis.title.y=element_text(size=16), legend.text=element_text(size=16), legend.position="top", legend.justification='left') +  
  facet_nested(. ~ type + toxicant2, scales="free", space = "free") +  
  theme(strip.text.x = element_blank(), panel.spacing.x=unit(0, "lines")) + 
  #  geom_hline(yintercept=45.6, color = "gold", size=2.5) + 
  scale_color_manual(name='', values=c("#00BA38", "#F8766D", "#619CFF"))

FS7D

# png(filename="FigS7D.png", units="in",width=12, height=9, res=300)
# print(FS7D)
# dev.off()


FS7E<-ggplot(data = FigS7DE, aes(x=toxicant2, y= `THtdT Pixel intensity - Mean per Well`, color=type)) + geom_point(position = position_jitter(w = 0.25, h = 0)) + theme_ipsum() + xlab('') +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=14), axis.text.y=element_text(size=14),axis.title.y=element_text(size=16), legend.text=element_text(size=16), legend.position="top", legend.justification='left') +  
  facet_nested(. ~ type + toxicant2, scales="free", space = "free") +  
  theme(strip.text.x = element_blank(), panel.spacing.x=unit(0, "lines")) + 
  #  geom_hline(yintercept=45.6, color = "gold", size=2.5) + 
  scale_color_manual(name='', values=c("#00BA38", "#F8766D", "#619CFF"))+ expand_limits(y = 0)

FS7E

# png(filename="FigS7E.png", units="in",width=12, height=9, res=300)
# print(FS7E)
# dev.off()


########################################################################################################
#Supplemental Figures 8-13 were not plotted with R (see methods). Source data are provided.

