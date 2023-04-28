wob_full <- readRDS("data/db/wob_full.rds")


wob_de <- subset(wob_full, `Country EU Code` == "DE")[ ,c("Prices in force on", "Product Name", "Weekly price with taxes")]
yrange <- range(wob_de$`Weekly price with taxes`, na.rm = TRUE)
ylim <- c(0, ceiling(yrange[2] * 10 ^ -floor(log10(yrange[2])) * 2) / 2 * 10 ^ floor(log10(yrange[2])))

wob_de <- split(wob_de, wob_de$`Product Name`)




png(filename = "fig/wob_germany.png", bg = "white", width = 9, height = 6, units = "in", res = 300)


par(bty = "l")
par(cex.sub  = 0.75)
plot(
  wob_de[[1]]$`Prices in force on`,
  wob_de[[1]]$`Weekly price with taxes`,
  xlab = colnames(wob_de[[1]])[1],
  ylab = paste(colnames(wob_de[[1]])[3], " [EUR]"),
  type = 'l',
  ylim = ylim)

title(main = "Weekly Oil Bulletin Database - Germany",
  sub = sprintf("Updated on %s", format(Sys.Date(), "%d %b, %Y")),
  adj = 0)

lapply(seq_along(wob_de)[-1],
  function(x) {

    lines(
      wob_de[[x]]$`Prices in force on`,
      wob_de[[x]]$`Weekly price with taxes`,
      col = x,
      lty = x)
  })

legend(x = "topleft", legend = names(wob_de), col = 1:4, lty = 1:4, bty = "n")



dev.off()

