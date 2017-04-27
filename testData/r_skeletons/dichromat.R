##
## Exported symobls in package `dichromat`
##

## Exported package methods

dichromat <- function (colours, type = c("deutan", "protan", "tritan")) 
{
    colours <- t(col2rgb(colours))
    colnames(colours) <- c("r", "g", "b")
    type <- match.arg(type)
    if (type == "deutan") {
        nred <- predict(redd, newdata = colours)
        ngreen <- predict(greend, newdata = colours)
        nblue <- predict(blued, newdata = colours)
    }
    else if (type == "protan") {
        nred <- predict(redp, newdata = colours)
        ngreen <- predict(greenp, newdata = colours)
        nblue <- predict(bluep, newdata = colours)
    }
    else if (type == "tritan") {
        nred <- predict(redt, newdata = colours)
        ngreen <- predict(greent, newdata = colours)
        nblue <- predict(bluet, newdata = colours)
    }
    nred <- pmax(0, pmin(1, nred/255))
    ngreen <- pmax(0, pmin(1, ngreen/255))
    nblue <- pmax(0, pmin(1, nblue/255))
    rgb(nred, ngreen, nblue)
}


colorschemes <- structure(list(BrowntoBlue.10 = c("#663000", "#996136", "#CC9B7A", 
"#D9AF98", "#F2DACE", "#CCFDFF", "#99F8FF", "#66F0FF", "#33E4FF", 
"#00AACC"), BrowntoBlue.12 = c("#331A00", "#663000", "#996136", 
"#CC9B7A", "#D9AF98", "#F2DACE", "#CCFDFF", "#99F8FF", "#66F0FF", 
"#33E4FF", "#00AACC", "#007A99"), BluetoDarkOrange.12 = c("#1F8F99", 
"#52C4CC", "#99FAFF", "#B2FCFF", "#CCFEFF", "#E6FFFF", "#FFE6CC", 
"#FFCA99", "#FFAD66", "#FF8F33", "#CC5800", "#994000"), BluetoDarkOrange.18 = c("#006666", 
"#009999", "#00CCCC", "#00FFFF", "#33FFFF", "#66FFFF", "#99FFFF", 
"#B2FFFF", "#CCFFFF", "#E6FFFF", "#FFE6CC", "#FFCA99", "#FFAD66", 
"#FF8F33", "#FF6E00", "#CC5500", "#993D00", "#662700"), DarkRedtoBlue.12 = c("#2A0BD9", 
"#264EFF", "#40A1FF", "#73DAFF", "#ABF8FF", "#E0FFFF", "#FFFFBF", 
"#FFE099", "#FFAD73", "#F76E5E", "#D92632", "#A60021"), DarkRedtoBlue.18 = c("#2400D9", 
"#191DF7", "#2957FF", "#3D87FF", "#57B0FF", "#75D3FF", "#99EBFF", 
"#BDF9FF", "#EBFFFF", "#FFFFEB", "#FFF2BD", "#FFD699", "#FFAC75", 
"#FF7857", "#FF3D3D", "#F72836", "#D91630", "#A60021"), BluetoGreen.14 = c("#0000FF", 
"#3333FF", "#6666FF", "#9999FF", "#B2B2FF", "#CCCCFF", "#E6E6FF", 
"#E6FFE6", "#CCFFCC", "#B2FFB2", "#99FF99", "#66FF66", "#33FF33", 
"#00FF00"), BluetoGray.8 = c("#0099CC", "#66E6FF", "#99FFFF", 
"#CCFFFF", "#E6E6E6", "#999999", "#666666", "#333333"), BluetoOrangeRed.14 = c("#085AFF", 
"#3377FF", "#5991FF", "#8CB2FF", "#BFD4FF", "#E6EEFF", "#F7FAFF", 
"#FFFFCC", "#FFFF99", "#FFFF00", "#FFCC00", "#FF9900", "#FF6600", 
"#FF0000"), BluetoOrange.10 = c("#0055FF", "#3399FF", "#66CCFF", 
"#99EEFF", "#CCFFFF", "#FFFFCC", "#FFEE99", "#FFCC66", "#FF9933", 
"#FF5500"), BluetoOrange.12 = c("#002BFF", "#1A66FF", "#3399FF", 
"#66CCFF", "#99EEFF", "#CCFFFF", "#FFFFCC", "#FFEE99", "#FFCC66", 
"#FF9933", "#FF661A", "#FF2B00"), BluetoOrange.8 = c("#0080FF", 
"#4CC4FF", "#99EEFF", "#CCFFFF", "#FFFFCC", "#FFEE99", "#FFC44C", 
"#FF8000"), LightBluetoDarkBlue.10 = c("#E6FFFF", "#CCFBFF", 
"#B2F2FF", "#99E6FF", "#80D4FF", "#66BFFF", "#4CA6FF", "#3388FF", 
"#1A66FF", "#0040FF"), LightBluetoDarkBlue.7 = c("#FFFFFF", "#CCFDFF", 
"#99F8FF", "#66F0FF", "#33E4FF", "#00AACC", "#007A99"), Categorical.12 = c("#FFBF80", 
"#FF8000", "#FFFF99", "#FFFF33", "#B2FF8C", "#33FF00", "#A6EDFF", 
"#1AB2FF", "#CCBFFF", "#664CFF", "#FF99BF", "#E61A33"), GreentoMagenta.16 = c("#005100", 
"#008600", "#00BC00", "#00F100", "#51FF51", "#86FF86", "#BCFFBC", 
"#FFFFFF", "#FFF1FF", "#FFBCFF", "#FF86FF", "#FF51FF", "#F100F1", 
"#BC00BC", "#860086", "#510051"), SteppedSequential.5 = c("#990F0F", 
"#B22D2D", "#CC5252", "#E67E7E", "#FFB2B2", "#99700F", "#B28B2D", 
"#CCA852", "#E6C77E", "#FFE8B2", "#1F990F", "#3CB22D", "#60CC52", 
"#8AE67E", "#BCFFB2", "#710F99", "#8B2DB2", "#A852CC", "#C77EE6", 
"#E9B2FF", "#990F20", "#B22D3C", "#CC5260", "#E67E8A", "#FFB2BC"
)), .Names = c("BrowntoBlue.10", "BrowntoBlue.12", "BluetoDarkOrange.12", 
"BluetoDarkOrange.18", "DarkRedtoBlue.12", "DarkRedtoBlue.18", 
"BluetoGreen.14", "BluetoGray.8", "BluetoOrangeRed.14", "BluetoOrange.10", 
"BluetoOrange.12", "BluetoOrange.8", "LightBluetoDarkBlue.10", 
"LightBluetoDarkBlue.7", "Categorical.12", "GreentoMagenta.16", 
"SteppedSequential.5"))




## Package Data

dalton <- dichromat::dalton		## Effects of Daltonism (Red-Green Color Blindness)



## Package Info

.skeleton_package_title = "Color Schemes for Dichromats"

.skeleton_package_version = "2.0-0"

.skeleton_package_depends = "stats"

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF