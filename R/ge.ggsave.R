ge.ggsave <- function(name, scale = 1, dpi = 300) {
        ## write data.frame as csv and rds file into data folder using df name itselt
now <- format(Sys.time(), "_%y%m%d_%H%M%S")
## eps/ps, tex (pictex), pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).
pngName  <- paste0(deparse(substitute(name)), now, ".png")
tifName  <- paste0(deparse(substitute(name)), now, ".tiff")
pdfName  <- paste0(deparse(substitute(name)), now, ".pdf")
ggsave(pngName,scale = scale, dpi = 300)
ggsave(tifName,scale = scale, dpi = 300)
ggsave(pdfName,scale = scale, dpi = 300)
setwd(wd)
}
