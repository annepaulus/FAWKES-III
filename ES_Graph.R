
ES_Graph<-function(input,GRAPH){
  
  
  
  NewNames<-c("CultCrop","WildPlants","PlantMaterialAg","Fibre","PlantEnergy","RearedAnimal","WildAnimal","Aquaculture","SurfWaterDrink",
                   "SurfWaterNonDrink","GroundWaterDrink","GroundWaterNonDrink","ErosionPrevent","HydroMaintain","FloodProtect","GHGReduction",
                   "Experiential","Physical")
CurrentNames<-c("Cultivated.crops","Wild.plants..algae.and.their.outputs","Materials.from.plants..algae.and.animals.for.agricultural.use",
                "Fibres.and.other.materials.from.plants..algae.and.animals.for.direct.use.or.processing","Plant.based.energy.resources",
                "Reared.animals.and.their.outputs","Wild.animals.and.their.outputs","Animals.from.in.situ.aquaculture","Surface.water.for.drinking",
                "Surface.water.for.non.drinking.purposes","Ground.water.for.drinking","Ground.water.for.non.drinking.purposes",
                "Erosion.prevention.and.mass.stabilisation","Hydrological.cycle.and.water.flow.maintenance","Flood.protection","Greenhouse.gas.reduction",
                "Experiential.use.of.plants..animals.and.land..seascapes.in.different.environmental.settings",
                "Physical.use.of.land..seascapes.in.different.environmental.settings")
  
  
Coords<-layout.circle(make_ring(18))
rownames(Coords)<-NewNames

# Create a vector of simplified service groups 
ServiceList<-unique(input$SERVICE)

ServiceBySite<-matrix(ncol=length(ServiceList),nrow=length(unique(input$SITECODE)))
rownames(ServiceBySite)<-unique(input$SITECODE)
colnames(ServiceBySite)<-ServiceList
# This is the ugly bit, and takes ca. 15 minutes to run
for(x in 1:nrow(ServiceBySite)){
  # For each unique site ID code, extract a list of threats that have corresponding services
  SiteServices<-na.omit(subset(input,input$SITECODE==rownames(ServiceBySite)[x])$SERVICE)
  # For each service group, check whether the site had a threat that indicates that service is present
  for(y in 1:ncol(ServiceBySite)){
    if(colnames(ServiceBySite)[y] %in% SiteServices) {ServiceBySite[x,y]<-1} else {ServiceBySite[x,y]<-0}
  }
}
ServiceFreq<-colSums(ServiceBySite)
names(ServiceFreq)<-NewNames[match(names(ServiceFreq),CurrentNames)]
colnames(ServiceBySite)<-NewNames[match(colnames(ServiceBySite),CurrentNames)]

# Create a co-occurrence matrix of threats
ServiceCount<-numeric(length=length(ServiceList)*length(ServiceList))
for (x in 1:length(ServiceList)){
  for(y in 1:length(ServiceList)){
    TempDat<-subset(ServiceBySite,ServiceBySite[,x]!="0" & ServiceBySite[,y]!="0")
    ServiceCount[(x-1)*length(ServiceList)+y]<-nrow(TempDat)
  }
}
ServiceBySiteEdgeTable<-data.frame(Service1=rep(colnames(ServiceBySite),each=length(ServiceList)),
                                   Service2=rep(colnames(ServiceBySite),length(ServiceList)),
                                   Weight=ServiceCount)

# Remove duplicates
NoDup<-t(combn(colnames(ServiceBySite),m=2))
NoDupWeight<-numeric(length=nrow(NoDup))
for(x in 1:nrow(NoDup)){
  NoDupWeight[x]<-subset(ServiceBySiteEdgeTable,ServiceBySiteEdgeTable$Service1==NoDup[x,1] 
                         & ServiceBySiteEdgeTable$Service2==NoDup[x,2])[,3]
}
ServiceBySiteEdgeTable<-data.frame(Service1=NoDup[,1],Service2=NoDup[,2],Weight=NoDupWeight)

  ServiceBySiteEdgeTable2<-subset(ServiceBySiteEdgeTable,ServiceBySiteEdgeTable$Weight>(nrow(input)/100))
  graph <- graph.data.frame(ServiceBySiteEdgeTable2, directed = FALSE)
  E(graph)$width <- E(graph)$Weight/((max(E(graph)$Weight))/10) # Set edge width based on weight
  ServiceFreq2<-ServiceFreq[names(ServiceFreq)%in%names(V(graph))] # extract frequencies from subset of services
  V(graph)$size <- ServiceFreq2/(max(ServiceFreq2)/20) # Set node size based on frequency of service
  GraphCoords<-Coords[match(names(V(graph)),rownames(Coords)),]
  if(GRAPH==TRUE) {plot(graph,layout=GraphCoords)}

  return(list(graph, ServiceBySiteEdgeTable,GraphCoords,ServiceBySite))
}