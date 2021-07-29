library(raster);library(rgdal);library(rgeos);library(maptools)

wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/ipbes_regions_mesoregions_shape"
wd_simp_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/Simplified_shp"

shp <- readOGR("IPBES_mesoregions",dsn = wd_shp)

shp_simp <- gSimplify(shp, tol = 0.01, topologyPreserve = T)

shp_mesoregion <- shp_simp

shp_mesoregion$MesoRegion <- shp$Mes_Rgn

shp_region <- shp_simp

shp_region$region <- shp$Region

shp_subregion <- shp_simp

shp_subregion$SubRegion <- shp$Sub_Rgn

### IPBES Regs

ipbes_regs <- unique(shp_region$region)


for(i in 3:length(ipbes_regs))
{
  reg <- shp_region[which(shp_region$region == ipbes_regs[i]),]
  reg2 <- gSimplify(reg,tol = 0.01)
  reg3 <- gUnaryUnion(reg2)
  reg3$Region <- ipbes_regs[i]
  
  if(i == 1){
    
    reg3 <- spChFIDs(reg3,paste(1))
    simp_shp_reg <- reg3
    
  }else{
    
    reg3 <- spChFIDs(reg3,paste(nrow(simp_shp_reg)+1))
    simp_shp_reg <- spRbind(simp_shp_reg,reg3)
  }
}

plot(simp_shp_reg)

writeOGR(shp_region, layer = "IPBES_Region", driver = "ESRI Shapefile",
         dsn = wd_simp_shp)

### IPBES SubRegs

ipbes_subregs <- unique(shp_subregion$SubRegion)


for(i in 1:length(ipbes_subregs))
{
  reg <- shp_subregion[which(shp_subregion$SubRegion == ipbes_subregs[i]),]
  reg2 <- gSimplify(reg,tol = 0.01)
  reg3 <- gUnaryUnion(reg2)
  reg3$Region <- ipbes_subregs[i]
  
  if(i == 1){
    
    reg3 <- spChFIDs(reg3,paste(1))
    simp_shp_subreg <- reg3
    
  }else{
    
    reg3 <- spChFIDs(reg3,paste(nrow(simp_shp_subreg)+1))
    simp_shp_subreg <- spRbind(simp_shp_subreg,reg3)
  }
}

plot(simp_shp_subreg)

writeOGR(simp_shp_subreg, layer = "IPBES_SubRegion", driver = "ESRI Shapefile",
         dsn = wd_simp_shp)



writeOGR(shp_mesoregion, layer = "IPBES_MesoRegion", driver = "ESRI Shapefile",
         dsn = wd_simp_shp)



simp_shp_subreg2 <- gSimplify(simp_shp_subreg,tol=0.01)

plot(simp_shp_subreg2)
