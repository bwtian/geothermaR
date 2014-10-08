ge.ggsave <- function(name, scale = 1, dpi = 300) {
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
pngName  <- paste0(deparse(substitute(name)), ".png")
tifName  <- paste0(deparse(substitute(name)), ".tiff")
pdfName  <- paste0(deparse(substitute(name)), ".pdf")
ggsave(pngName,scale = scale, dpi = dpi)
ggsave(tifName,scale = scale, dpi = dpi)
ggsave(pdfName,scale = scale, dpi = dpi)
}
