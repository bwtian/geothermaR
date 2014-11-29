ge.ggsave <- function(gg, scale = 1, dpi = 300, ...) {
now <- format(Sys.time(), "_%y%m%d_%H%M%S")
## eps/ps, tex (pictex), pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).
#pngName  <- paste0(deparse(substitute(name)), now, ".png")
# unit is inch
# if (paper == 1) {
#         width = 6.83
# } else if(paper == 2){
#         width = 3.27
# } else if (paper  == 3 ){
#         width = 2.09
# }
fileName  <- deparse(substitute(gg))
pngName  <- paste0(fileName, now, ".png")
tifName  <- paste0(fileName, now, ".tiff")
pdfName  <- paste0(fileName, now, ".pdf")
ggsave(pdfName, plot = gg, scale = scale, dpi = dpi)
ggsave(pngName, plot = gg, scale = scale, dpi = dpi)
ggsave(tifName, plot = gg, scale = scale, dpi = dpi)
}
