#' Extract necessary data from DARDAR-Nice L2 filenames (ta and icnc, mainly)
#' Data fame is adapted to run on some in situ functions
#' @param fn name of DARDAR-Nice L2 file
#' @param dir.rds save output as an rds file if not NULL (default is NULL)
#' @export
l2.extract <- function(fn,dir.rds=NULL,only.ice=TRUE) {
    
    if(!is.null(dir.rds)) {
        fn.out <- paste0(dir.out,"/",basename(fn),".rds")
        if(!dir.exists(dir.out)) dir.create(dir.out)
        
        if(file.exists(fn.out)) return(NULL)
    }
    
    bins.lat.lev <- c(-90,-67.7,-23.3,23.3,67.7,90)
    bins.lat.lab <- c("Antarctica","Mid-lat South","Tropics","Mid-lat North","Arctic")        

    nc <- ncdf4::nc_open(fn)
    nheight <- nc$dim$height$len; ntime <- nc$dim$time$len
    tmp <- data.frame(expand.grid(idx.height=1:nheight,
                                 idx.time=1:ntime),
                     tc=as.numeric(ncdf4::ncvar_get(nc,"ta")),
                     icnc_5um=as.numeric(ncdf4::ncvar_get(nc,"icnc_5um")),
                     clm=as.numeric(ncdf4::ncvar_get(nc,"clm")),
                     flag.mixed=as.numeric(ncdf4::ncvar_get(nc,"mixedphase_flag")),
                     iwc=as.numeric(ncdf4::ncvar_get(nc,"iwc")) ## Bug in DARDAR-Nice v1.1: reff and iwc swapped
                     ) %>%
        dplyr::filter(!is.na(tc) & !is.na(icnc_5um) & !is.na(clm)) %>%
        dplyr::mutate(lat=ncdf4::ncvar_get(nc,"lat")[idx.time] %>% as.numeric(),
                      lon=ncdf4::ncvar_get(nc,"lon")[idx.time] %>% as.numeric(),
                      height=ncdf4::ncvar_get(nc,"height")[idx.height] %>% as.numeric(),
                      time=ncdf4::ncvar_get(nc,"dtime")[idx.time] + ncdf4::ncvar_get(nc,"base_time") + as.POSIXct("1970-01-01"),
                      flag.iter=ncdf4::ncvar_get(nc,"iteration_flag")[idx.time] %>% as.numeric(),
                      nightday_flag=ncdf4::ncvar_get(nc,"nightday_flag")[idx.time] %>% as.numeric(),
                      tc=tc-273.15,
                      icnc_5um=icnc_5um*1E-3) %>%
        dplyr::filter(flag.iter==1) %>%
        dplyr::mutate(flag.ice= (icnc_5um > 0 & iwc > 1E-8 & clm%in%c(1,2,3) & flag.mixed==0),
                      icnc_5um=replace(icnc_5um,!flag.ice,0)) 
    
    if(only.ice) {tmp  <- tmp %>% dplyr::filter(flag.ice)}
    
    df <- tmp %>%
        dplyr::select(-c(clm,flag.mixed,flag.iter,idx.height,idx.time,flag.ice)) %>%
        plotutils::bin(lat,bins.lat.lev) %>%
        plotutils::bin(nightday_flag,c(0,0.5,1)) %>%
        dplyr::mutate(region=factor(lat_bin,levels=(dplyr::lead(bins.lat.lev)+bins.lat.lev)/2,labels=bins.lat.lab),
                      season=baseutils::time2season(time),
                      nightday=factor(nightday_flag_bin,levels=c(0.25,0.75),labels=c("day","night"))) %>%
        dplyr::select(-c(lat_bin,lat_width,nightday_flag_bin,nightday_flag_width,nightday_flag))
    
    ncdf4::nc_close(nc)
    
    if(!is.null(dir.rds)) {
        saveRDS(df,fn.out)
        rm(df); gc()
        return()
    } else {
        return(df)
    }
    
}
