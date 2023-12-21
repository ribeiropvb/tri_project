
library("BradleyTerry2")

data("citations", package = "BradleyTerry2")
citations

citations.sf <- countsToBinomial(citations)
names(citations.sf)[1:2] <- c("journal1", "journal2")
citations.sf

citeModel <- BTm(
  cbind(win1, win2)
  , journal1, journal2, ~ journal
  , id = "journal", data = citations.sf
)
citeModel

update(citeModel, refcat = "JASA")
update(citeModel, br = TRUE) # Bias-reduced estimates


options(show.signif.stars = FALSE)
data("flatlizards", package = "BradleyTerry2")
flatlizards
lizModel <- BTm(1, winner, loser, ~ SVL[..] + (1|..), data = flatlizards)
summary(lizModel)

# Missing Values

lizModel2 <- BTm(1, winner, loser, ~ throat.PC1[..] + throat.PC3[..] +  head.length[..] + SVL[..] + (1|..), data = flatlizards)
summary(lizModel2)
