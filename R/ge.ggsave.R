ge.ggsave <- function(name, width = 1024,  height = 1024/0.707, scale = 1, dpi = 300) {
now <- format(Sys.time(), "_%y%m%d_%H%M%S")
## eps/ps, tex (pictex), pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).
#pngName  <- paste0(deparse(substitute(name)), now, ".png")
pngName  <- paste0(deparse(substitute(name)), ".png")
tifName  <- paste0(deparse(substitute(name)), ".tiff")
pdfName  <- paste0(deparse(substitute(name)), ".pdf")
ggsave(pngName,width = width, height = height, scale = scale, dpi = dpi)
ggsave(tifName,width = width, height = height, scale = scale, dpi = dpi)
ggsave(pdfName,width = width, height = height, scale = scale, dpi = dpi)
}
ggsave
