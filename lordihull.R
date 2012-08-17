# Changes colors of hulls to be more specific about graphic output
# uses groups out of Wilson's Creek data

nmds.scores <- scores(nmds.3)
ordiplot(nmds.scores, type='t', display='sites')
hull1 <- chull(nmds.scores[wicrmap$mapped==1,])
scores1 <- nmds.scores[wicrmap$mapped==1,])
polygon(scores1[hull1,], border="red", lty = 2)
# lyt 0 = blank, 1=solid, 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash
# font 1=plain, 2=bold, 3=italics, 4= bold italics
ordiplot(nmds.scores, type='t', display='sites')

hullplot = function(lgroup, lcolor, llinetype){
	polygon(nmds.scores[wicrmap$mapped==lgroup,][chull(nmds.scores[wicrmap$mapped==lgroup,]),], border=lcolor, lty=llinetype)
	text(nmds.scores[wicrmap$mapped==lgroup,1], nmds.scores[wicrmap$mapped==lgroup,2], rownames(nmds.scores[wicrmap$mapped==lgroup,]), col=lcolor, cex=0.7)
	}
	
	
ordiplot(nmds.scores, type='n', xlab='NMDS1', ylab='NMDS2', display='sites')
hullplot(1, "orange", 2)
hullplot(2, "forestgreen", 1)
hullplot(3, "blue", 1)
hullplot(5, "red", 1)
hullplot(6, "red", 2)
hullplot(7, "blue", 2)
hullplot(8, "forestgreen", 2)
hullplot(9, "black", 1)
points(nmds.scores["E1",1], nmds.scores["E1", 2], pch=1, cex = 2.5, col="black") 
text(nmds.scores["E1",1], nmds.scores["E1",2], "E1", col="black", cex=0.7)
points(nmds.scores["K2",1], nmds.scores["K2", 2], pch=1, cex = 2.5, col="red")
text(nmds.scores["K2",1], nmds.scores["K2",2], "K2", col="red", cex=0.7)


#polygon(nmds.scores[wicrmap$mapped==1,][chull(nmds.scores[wicrmap$mapped==1,]),], border="orange", lty=2)
#text(nmds.scores[wicrmap$mapped==1,1], nmds.scores[wicrmap$mapped==1,2], rownames(nmds.scores[wicrmap$mapped==1,]), col="orange", cex=0.7)
#polygon(nmds.scores[wicrmap$mapped==2,][chull(nmds.scores[wicrmap$mapped==2,]),], border="forestgreen", lty=1)
#text(nmds.scores[wicrmap$mapped==2,1], nmds.scores[wicrmap$mapped==2,2], rownames(nmds.scores[wicrmap$mapped==2,]), col="forestgreen", cex=0.7)
#polygon(nmds.scores[wicrmap$mapped==3,][chull(nmds.scores[wicrmap$mapped==3,]),], border="blue", lty=1)
#text(nmds.scores[wicrmap$mapped==3,1], nmds.scores[wicrmap$mapped==3,2], rownames(nmds.scores[wicrmap$mapped==3,]), col="blue", cex=0.7)
#polygon(nmds.scores[wicrmap$mapped==5,][chull(nmds.scores[wicrmap$mapped==5,]),], border="red", lty=1)
#text(nmds.scores[wicrmap$mapped==5,1], nmds.scores[wicrmap$mapped==5,2], rownames(nmds.scores[wicrmap$mapped==5,]), col="red", cex=0.7)
#polygon(nmds.scores[wicrmap$mapped==6,][chull(nmds.scores[wicrmap$mapped==6,]),], border="red", lty=2)
#text(nmds.scores[wicrmap$mapped==6,1], nmds.scores[wicrmap$mapped==6,2], rownames(nmds.scores[wicrmap$mapped==6,]), col="red", cex=0.7)
#polygon(nmds.scores[wicrmap$mapped==7,][chull(nmds.scores[wicrmap$mapped==7,]),], border="blue", lty=2)
#text(nmds.scores[wicrmap$mapped==7,1], nmds.scores[wicrmap$mapped==7,2], rownames(nmds.scores[wicrmap$mapped==7,]), col="blue", cex=0.7)
#polygon(nmds.scores[wicrmap$mapped==8,][chull(nmds.scores[wicrmap$mapped==8,]),], border="forestgreen", lty=2)
#text(nmds.scores[wicrmap$mapped==8,1], nmds.scores[wicrmap$mapped==8,2], rownames(nmds.scores[wicrmap$mapped==8,]), col="forestgreen", cex=0.7)
polygon(nmds.scores[wicrmap$mapped==9,][chull(nmds.scores[wicrmap$mapped==9,]),], border="black", lty=1)
text(nmds.scores[wicrmap$mapped==9,1], nmds.scores[wicrmap$mapped==9,2], rownames(nmds.scores[wicrmap$mapped==9,]), col="black", cex=0.7)

polygon(nmds.scores[wicrmap$mapped==9,][chull(nmds.scores[wicrmap$mapped==9,]),], border="orange", lty=1) 

hullplot = function(lgroup, lcolor, llinetype){
	polygon(nmds.scores[wicrmap$mapped==lgroup,][chull(nmds.scores[wicrmap$mapped==lgroup,]),], border=lcolor, lty=llinetype)
	text(nmds.scores[wicrmap$mapped==lgroup,1], nmds.scores[wicrmap$mapped==lgroup,2], rownames(nmds.scores[wicrmap$mapped==lgroup,]), col=lcolor, cex=0.7)
	}
