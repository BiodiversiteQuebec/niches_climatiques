
library(exiftoolr)
library(stringi)
library(magick)

#Sys.setlocale("LC_ALL","English")

descQC <- read.csv("https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/qc/description.csv")
descNA <- read.csv("https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/na/description.csv")

desc <- merge(descNA[, c("collection", "variable")], descQC, all = TRUE)

if(FALSE){
  lf <- list.files("/home/frousseu/Downloads/niches_climatiques/results/graphics", full = TRUE)#, pattern = "compare")
  lf <- lf[-grep("_pic.png", lf)]
  #lf <- lf[-grep(" ", lf)]
  lapply(lf, function(i){
    print(i)
    gc() # image magick problem fills the cache!?
    im <- image_read(i) |>
      image_trim() |>
      image_scale("x1200") |>
      image_write(i)
  }) |> invisible()
}

lf <- list.files("/home/frousseu/Downloads/niches_climatiques/results/graphics", full = TRUE)
info <- read.csv("/home/frousseu/Downloads/niches_climatiques/results/niches_climatiques_info.csv")
#lf <- lf[-grep(" ", lf)]

species <- sapply(strsplit(basename(lf) ,"_|\\."),function(i){
  paste0(i[1:3], collapse = "_") 
}) |> trimws() |> table() 
species<- sort(names(species)[species >= 6])

ordre <- c("climat", "gam", "habitat", "climat + habitat", "climat (habitat)", "small", "gam (habitat)", "climat (small)", "gam (small)")
size <- c("large", "large", "large", "large", "large", "small", "large", "small", "small")
nom <- c("climat", "climatGAM", "habitatNA", "climat + habitatNA", "habitatNA (climat)", "habitatQC", "habitatNA (climatGAM)", "habitatQC (climat)", "habitatQC (climatGAM)")

o <- c(1, 2, 3, 6, 4, 5, 7, 8, 9)
ordre <- ordre[o]
size <- size[o]
nom <- nom[o]



ref <- species
sp <- sapply(strsplit(species, "_"), function(i){paste(i[1:2], collapse = " ")})
model <-sapply(strsplit(species, "_"), function(i){paste(i[3:length(i)], collapse = " ")}) |>
  factor(levels = ordre)
vars <- "variables"

toc <- data.frame(ref, species = sp, model, vars, div = species)
toc$scale <- size[match(toc$model, ordre)]
toc$nom <- nom[match(toc$model, ordre)]
toc$vernaculaire <- info$nom[match(toc$species, info$species)]
toc$period <- info$period[match(toc$species, info$species)]
toc$period <- ifelse(is.na(toc$period), "", paste("Nidification", toc$period))
toc <- toc[order(toc$species, toc$model), ]

toc <- lapply(split(toc, toc$species), function(i){
  a <- i[1, ] |>
    lapply(function(i){gsub("climat", "data", i)}) |>
    as.data.frame()
  b1 <- i[1, ] |>
    lapply(function(i){gsub("climat", "compare", i)}) |>
    as.data.frame()
  b2 <- i[1, ] |>
    lapply(function(i){gsub("climat", "compare_localized", i)}) |>
    as.data.frame()
  
  x <- rbind(a, i, b1, b2)
  
  x$nom <- gsub("data", "données", x$nom)
  x$nom <- gsub("compare", "comparaison", x$nom)
  x$nom <- gsub("_localized", " localisée", x$nom)
  
  x <- x[c(1, 1:nrow(x)), ] 
  x$display <- ifelse(duplicated(x$ref, fromLast = TRUE), paste0("<b>", x$vernaculaire, "</b>"), paste("&nbsp", x$nom))
  x
}) |> do.call("rbind", args = _)




#src<-"https://object-arbutus.cloud.computecanada.ca/bq-io/niches_climatiques/figures"
src<-"/home/frousseu/Downloads/niches_climatiques/results/graphics"

#species<-function(sp,url,copyright,ebirdurl,common,period,n){


set_models <- function(sp, url = "url", copyright = "copyright", ebirdurl = "ebird", common = "sp", n = "23"){
  
  spname <- sapply(strsplit(sp, "_"), "[", 1:2) |> paste(collapse = "_")
  modelscale <- paste0("_", size[match(sapply(strsplit(sp, "_"), "[", 3), ordre)])
  
  paste0("

<hr class=\"vspace\"> 
<section id=\"",sp,"\" class=\"section\">
<h2 class=\"h2\">", common,"</h2> 

  <div class=\"header\">
    <button class=\"showmore\" onclick=\"showmorefirst('",sp,"','panel1","')\">&nbsp&nbsp&nbsp&nbspSDM actuel&#8628</button>
    <button class=\"showmore\" onclick=\"showmorefirst('",sp,"','panel2","')\">SDM projeté &#8628</button>
  </div>
  <div class=\"row\" id=\"",sp,"panelfirst\">
    <div class=\"col1\" id=\"",sp,"panel1\">
      <img loading=\"lazy\" style=\"height: 30vw; padding: 0px;\" src=\"",file.path(src, sp),"_sdm", modelscale, ".png\" alt=\"\">
    </div>
    <div class=\"col2\" id=\"",sp,"panel2\">
      <img loading=\"lazy\" style=\"height: 30vw; padding: 0px;\" src=\"",file.path(src, sp),"_sdm_proj", modelscale, ".png\" alt=\"\">
    </div>
  </div>
</section>


<section class=\"section\">
  <div class=\"header\">
    <button class=\"showmore\" onclick=\"showmoresecond('",sp,"','panel3","')\">&nbsp&nbsp&nbsp&nbspRange actuel &#8628</button>
    <button class=\"showmore\" onclick=\"showmoresecond('",sp,"','panel4","')\">Range projeté &#8628</button>
  </div>
  <div class=\"row\" id=\"",sp,"panelsecond\">
    <div class=\"col1\" id=\"",sp,"panel3\">
      <img loading=\"lazy\" style=\"height: 30vw; padding: 0px;\" src=\"",file.path(src, sp),"_range", modelscale, ".png\" alt=\"\">
    </div>
    <div class=\"col2\" id=\"",sp,"panel4\">
      <img loading=\"lazy\" style=\"height: 30vw; padding: 0px;\" src=\"",file.path(src, sp),"_range_proj", modelscale, ".png\" alt=\"\">
    </div>
  </div>
</section>


<section class=\"section\">
  <div class=\"header\">
    <button class=\"showmore\" onclick=\"showmorethird('",sp,"','panel5","')\">&nbsp&nbsp&nbsp&nbspDifférence sdm &#8628</button>
    <button class=\"showmore\" onclick=\"showmorethird('",sp,"','panel6","')\">Différence range &#8628</button>
  </div>
  <div class=\"row\" id=\"",sp,"panelthird\">
    <div class=\"col1\" id=\"",sp,"panel5\">
      <img loading=\"lazy\" style=\"height: 30vw; padding: 0px;\" src=\"",file.path(src, sp),"_sdm_diff", modelscale, ".png\" alt=\"\">
    </div>
    <div class=\"col2\" id=\"",sp,"panel6\">
      <img loading=\"lazy\" style=\"height: 30vw; padding: 0px;\" src=\"",file.path(src, sp),"_range_diff", modelscale, ".png\" alt=\"\">
    </div>
  </div>
</section>


<section class=\"section\">
  <div class=\"header\"><!--<header>-->
    <button class=\"showmore\" onclick=\"showmorefourth('",sp,"','panel7","')\">&nbsp&nbsp&nbsp&nbspEffets variables &#8628</button>
    <button class=\"showmore\" onclick=\"showmorefourth('",sp,"','panel8","')\"></button>
  </div><!--</header>-->
  <div class=\"row\" id=\"",sp,"panelfourth\">
    <div class=\"col1\" id=\"",sp,"panel7\">
      <img loading=\"lazy\" style=\"width: 200%; padding: 0px;\" src=\"",file.path(src, sp),"_marginal_effects.png\" alt=\"\">
    </div>
    <div class=\"col2\" id=\"",sp,"panel8\">
    </div>
  </div>
</section>

  ")
}

set_species <- function(sp, url = "url", copyright = "copyright", ebirdurl = "ebird", common = "sp", period = "model 1", n = "23"){
  
  spname <- sapply(strsplit(sp, "_"), "[", 1:2) |> paste(collapse = "_")
  
  paste0("

<hr class=\"vspace\"> 
<section id=\"",sp,"\" class=\"section\">
<h2 class=\"h2\">", common,"</h2>  
<!-- <section class=\"section\"></section> -->
<div class=\"subheader\"></div>

  <div class=\"header\">
    <div class=\"top-left\" style=\"max-width: 24vw;\"></div>
    <div class=\"top-left\">Observations</div>
    <div class=\"top-left\">
      <a target=\"_blank\" href=\"", ebirdurl,"\">Québec</a>
    </div>
  </div>
  <div class=\"rowshow\">
    <div class=\"col3\" style=\"width: 24vw;\">
      <!-- <a target=\"_blank\" href=\"",url,"\"> -->
      <div class=\"container\" style=\"border: 0px solid red;\">
        <a class=\"aim\" target=\"_blank\" href=\"", url,"\">
          <div class=\"infotop\">", period, "<br>scénario = ", "RPC XY.Z", "</div>
          <!-- <a class=\"aim\" target=\"_blank\" href=\"",url,"\"> -->
          <img loading=\"lazy\" style=\"height: 16vw; padding-top: 5%; padding-left: 0%; border: 0px solid green;\" src=\"", file.path(src, spname), "_pic.png\" alt=\"\">
        <div class=\"middle\" style=\"border: 0px solid red;\">
          <div class=\"text\" style=\"border: 0px solid green;\">", copyright,"</div>
        </div>
        <!-- </a> -->
        <div class=\"infobottom\"><i>", gsub("_"," ", spname),"</i></div>
        </a>
      </div>
    </div>
    <div class=\"col4\">
      <img loading=\"lazy\" style=\"height: 25vw; padding: 1vh;\" src=\"", file.path(src, spname),"_na.png\" alt=\"\">
    </div>
    <div class=\"col3\">
      <img loading=\"lazy\" style=\"height: 25vw; padding: 1vh;\" src=\"", file.path(src, spname),"_quebec.png\" alt=\"\">
    </div>
  </div>
  <div class=\"header\">
    <div class=\"top-left\" style=\"max-width: 25vw;\">Précision</div>
    <div class=\"top-left\">Observations utilisées</div>
    <div class=\"top-left\">Observations utilisées</div>
  </div>
  <div class=\"rowshow\">
    <div class=\"col4\">
      <img loading=\"lazy\" style=\"height: 15vw; padding: 1vh;\" src=\"", file.path(src, spname),"_uncertainty.png\" alt=\"\">
    </div>
    <div class=\"col4\">
      <img loading=\"lazy\" style=\"height: 20vw; padding: 1vh;\" src=\"", file.path(src, spname),"_na_used.png\" alt=\"\">
    </div>
    <div class=\"col3\">
      <img loading=\"lazy\" style=\"height: 25vw; padding: 1vh;\" src=\"", file.path(src, spname),"_quebec_used.png\" alt=\"\">
    </div>
  </div>
</section>

")
  
}

set_compare <- function(sp, url = "url", copyright = "copyright", ebirdurl = "ebird", common = "sp", n = "23"){
  
  spname <- sapply(strsplit(sp, "_"), "[", 1:2) |> paste(collapse = "_")
  
  paste0("

<hr class=\"vspace\"> 
<section id=\"",sp,"\" class=\"section\">
<h2 class=\"h2\">", common,"</h2> 
  <div class=\"header\">
    <button class=\"showmore\" onclick=\"showmorezero('",sp,"','panel0","')\">Comparaison entre les différentes méthodes SDM&#8628</button>
  </div>
  <div class=\"row\" id=\"",sp,"panelzero\">
    <div class=\"col1\" id=\"",sp,"panel0\">
      <img loading=\"lazy\" style=\"width: 100%; padding: 0px;\" src=\"",file.path(src, gsub("compare", "sdm_compare", sp)), ".png\" alt=\"\">
    </div>
  </div>
</section>

  ")
}

css<-function(){cat(paste0("

<!DOCTYPE html>
<html>
<head>
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">
<link href='https://fonts.googleapis.com/css2?family=Roboto+Mono:wght@400' rel='stylesheet'>

<style>

html {
  /* max-width: 1950px !important; */
  background-color: white;
}

/* img {
  loading: lazy;
} */

body {
  background-color: white;
  max-width: 95vw;
  margin-left: auto;
  margin-right: auto;
  font-family: 'Roboto Mono';
  font-size: 2.25vmin;
}

div.left {
  position: -webkit-sticky;
  position: sticky;
  top: 3vh;
  bottom: 3vh;
  width: 18%; 
  height: 94vh; 
  float: left; 
  display: inline-block; 
  padding-right: 0.0%; 
  overflow-y: scroll;
}



div.right {
  width: 80%; 
  float: left; 
  display: inline-block; 
  padding-left: 1vw;
}


a:link, a:visited, a:hover, a:active {
 color: #000000;
 text-decoration: none;
}

a:hover{
  opacity: 0.50;
}

a.toc {
  font-size: 1.75vmin;
  color: black;
}

.aim:hover{
  opacity: 1;
}

a.text{
  font-size: 2.25vmin;
  font-weight: 600;
}

a.text:link, a.text:visited, a.text:hover, a.text:active {
 color: #000000;
 text-decoration: none;
}

a.text:hover{
  opacity: 0.50;
}

#toc a.active {
  text-decoration: underline;
  font-weight: bold;
}

.vspace {
  margin-top: 2vh;
  margin-bottom: 12vh;
  padding:0;
  border: 0;
  background-color: white;
}
.subheader {
  height: 1vh;
  margin: 0vh;
  padding: 0vh;
  border-left: 0.25vmin solid seagreen;
}

.infotop {
  padding-top: 10%;
  font-size: 1.75vmin;
}

.infobottom {
  padding-top: 5%;
  font-size: 1.75vmin;
}

.container {
  position: relative;
  text-align: center;
  color: black;
}

.container2 {
  position: relative;
  text-align: center;
  color: black;
}

.top-left {
  display: flex;
  flex: 1;
  align-items: center;
  justify-content: center;
  padding: 0vmin;
  margin: 0vmin;
  border: 0px solid green;
}

h1 {
  color: seagreen;
}

.h2 {
  margin: 0vh;
  padding: 0vh;
  color: seagreen;
  /* border-bottom: 2px solid seagreen; */
  /* border-right: 2px solid seagreen; */
}

.section{
  border-left: 0.25vmin solid seagreen;
  padding-left: 2vw;
  /* border-bottom: 2px solid seagreen; */
  /* border-right: 2px solid seagreen; */
}

/* .hide {
  display: none;
  padding-top: 2vw;
  padding-bottom: 0vw;
} 

.show {
  padding-top: 0vw;
  font-weight: 1200;
  font-size: 2.5vmin;
}

.show:hover + .hide {
  display: block;
} */

.shown {
  display: block;
  padding-top: 0vw;
  border-left: 0.25vmin solid seagreen;
  <!-- border-right: 2px solid seagreen; -->
}

.row {
  display: flex;  /* aligns all child elements (flex items) in a row */
}

.header {
  display: flex;  /* aligns all child elements (flex items) in a row */
  justify-content: space-around;
  padding: 0vmin;
  margin: 0vmin;
}

.row {
  display: flex;  /* aligns all child elements (flex items) in a row */
  justify-content: space-around;
  padding-top: 1vh;
  padding-bottom: 1vh;
  border: 0px solid red;
}

.rowshow {
  display: flex;  /* aligns all child elements (flex items) in a row */
  padding-top: 0vh;
  padding-bottom: 1vh;
  border: 0px solid red;
}

.showmore {
  background: none;
  border: none; /* none */
  padding-bottom: 3vh;
  text-align: center;
  text-decoration: none;
  display: flex;
  flex: 1;
  font-size: 2.25vmin;
  font-weight: 1200;
  font-family:'Roboto Mono'; 
  cursor: pointer;
  height: 2vmin;
}

.showmore:hover {
  opacity: 0.50;
  filter: alpha(opacity=100);
}

.col1, .col2 {
  display: flex;
  flex: 1;
  visibility: visible;
  flex-flow: wrap;
  border: 0px solid red;
}

.col3 .col4 {
  display: flex;
  flex: 1;
  visibility: visible;
  transition: transform .2s;
  border: 0px solid blue;
}

/* .col4:hover {
  -ms-transform: scale(1.1); /* IE 9 */
  -webkit-transform: scale(1.1); /* Safari 3-8 */
  transform: scale(1.1); 
} */



.ID {
  display: none;
}

#####
.container {
  position: relative;
  width: 50%;
  border: 0px solid red;
}

.container2 {
  position: relative;
  width: 100%;
  border: 0px solid red;
  opacity: 1;
}

.container3 {
  position: relative;
  /* width: 50%; */
  /* border: 1px solid red; */
}

.image {
  opacity: 1;
  display: block;
  width: 100%;
  height: auto;
  transition: .2s ease;
  /* backface-visibility: hidden; */
}

.middle {
  transition: .2s ease;
  opacity: 0;
  position: absolute;
  top: 70%;
  left: 50%;
  width: 60%;
  transform: translate(-50%, -50%);
  -ms-transform: translate(-50%, -50%);
  text-align: center;
}

.middle2 {
  opacity: 1;
  position: absolute;
  background-color: #FFFFFF00;
  top: 25%;
  left: 15%;
  width: 80%;
  text-align: left;
}

.middle3 {
  opacity: 1;
  position: absolute;
  background-color: #FFFFFF00;
  top: 17%;
  left: 2%;
  width: 98%;
  text-align: left;
  /* border: 1px solid red; */
}

.container:hover .image {
  opacity: 0.3;
}

.container:hover .middle {
  opacity: 1;
}

.text {
  background-color: #FFFFFFAA;
  color: #000000;
  font-size: 1.25vmin;
  padding: 0.25vmin;
  border-radius: 1vmin;
  line-height: 1;
}

.text2 {
  background-color: #FFFFFF00;
  color: #00000033;
  opacity: 1;
  font-size: 1.75vmin;
  padding: 1vmin;
  border-radius: 1vmin;
  line-height: 1.1;
  /* text-align:justify; */
  text-justify:inter-character;
}

.container2:hover .image2 {
  opacity: 0.4;
}

.container2:hover .image3 {
  opacity: 0.2;
}

.container2:hover .text2 {
  color: #000000;
  background-color: #FFFFFF55;
}

.container3:hover .image2 {
  opacity: 0.4;
}

.container3:hover .image3 {
  opacity: 0.2;
}

.container3:hover .text2 {
  color: #000000;
  background-color: #FFFFFF55;
}

</style>
</head>
<body>
<div style=\"display:inline-block; width:100%;\">
<div class=\"left\">
<nav id=\"toc\">"
,paste(paste0("<a class=\"toc\" href=\"#", toc$ref, "\">", toc$display, "</a>"), collapse = "<br>\n"),
"</nav>
</div>
<div class=\"right\">
<h1>Niches climatiques</h1>



<p>Modèles et résultats</p>

<h4>climat</h4>
<p>Modèle avec uniquement des variables de climat</p>
<h4>climatGAM</h4>
<p>Modèle de climat plus flexible, mais forcé concave</p>
<h4>habitatNA</h4>
<p>Modèle avec des variables d'habitat stables uniquement (Amérique du Nord)</p>
<h4>habitatQC</h4>
<p>Modèle avec des variables d'habitat stables uniquement (Québec)</p>
<h4>climat + habitatNA</h4>
<p>Modèle avec les variables de climat et d'habitat (Amérique du Nord) dans le même modèle</p>
<h4>habitatNA (climat)</h4>
<p>Modèle basé sur l'habitat uniquement clippé par le modèle de climat</p>
<h4>habitatNA (climatGAM)</h4>
<p>Modèle basé sur l'habitat uniquement clippé par le modèle de climat GAM</p>
<h4>habitatQC (climat)</h4>
<p>Modèle des variables d'habitat locales clippées avec le modèle de climat</p>
<h4>habitatQC (climatGAM)</h4>
<p>Modèle des variables d'habitat locales clippées avec le modèle de climat GAM</p>


<hr class=\"vspace\"> 

"))}

script<-function(){cat(paste0("

<script>
    var buttons = document.getElementsByClassName('showmore');
    var divs = document.getElementsByClassName('ID');
    
    for (var i = 0; i < buttons.length; i++) {
      var butt = buttons[i];
      var div = divs[i];
      // and attach our click listener for this image.
      buttsssssss.onclick = function(evt) {
        console.log(evt);
        if (div.style.display == \"flex\") {
          div.style.display = \"none\";
        } else {
          div.style.display = \"flex\";
        }
      }
    }
    
    
    
    function showmorezero(sp,panel) {
      var id = sp+panel
      var x = document.getElementById(id);
      var div = document.getElementById(sp+\"panelzero\");
      var div1 = document.getElementById(sp+\"panel0\");
      var status = \"hidden\";
      if (x.style.visibility == \"visible\") {
        x.style.visibility = \"hidden\";
      } else {
        x.style.visibility = \"visible\";
        status = \"visible\";
      }
      if (div1.style.visibility == \"visible\" || status == \"visible\") {
        div.style.display = \"flex\";
      } else {
        div.style.display = \"none\";
      }
    }
    
    
    function showmorefirst(sp,panel) {
      var id = sp+panel
      var x = document.getElementById(id);
      var div = document.getElementById(sp+\"panelfirst\");
      var div1 = document.getElementById(sp+\"panel1\");
      var div2 = document.getElementById(sp+\"panel2\");
      var status = \"hidden\";
      if (x.style.visibility == \"visible\") {
        x.style.visibility = \"hidden\";
      } else {
        x.style.visibility = \"visible\";
        status = \"visible\";
      }
      if (div1.style.visibility == \"visible\" || div2.style.visibility == \"visible\" || status == \"visible\") {
        div.style.display = \"flex\";
      } else {
        div.style.display = \"none\";
      }
    }
    
    function showmoresecond(sp, panel) {
      var id = sp+panel
      var x = document.getElementById(id);
      var div = document.getElementById(sp+\"panelsecond\");
      var div3 = document.getElementById(sp+\"panel3\");
      var div4 = document.getElementById(sp+\"panel4\");
      var status = \"hidden\";
      if (x.style.visibility == \"visible\") {
        x.style.visibility = \"hidden\";
      } else {
        x.style.visibility = \"visible\";
        status = \"visible\";
      }
      if (div3.style.visibility == \"visible\" || div4.style.visibility == \"visible\" || status == \"visible\") {
        div.style.display = \"flex\";
      } else {
        div.style.display = \"none\";
      }
    }
    
    function showmorethird(sp, panel) {
      var id = sp+panel
      var x = document.getElementById(id);
      var div = document.getElementById(sp+\"panelthird\");
      var div5 = document.getElementById(sp+\"panel5\");
      var div6 = document.getElementById(sp+\"panel6\");
      var status = \"hidden\";
      if (x.style.visibility == \"visible\") {
        x.style.visibility = \"hidden\";
      } else {
        x.style.visibility = \"visible\";
        status = \"visible\";
      }
      if (div5.style.visibility == \"visible\" || div6.style.visibility == \"visible\" || status == \"visible\") {
        div.style.display = \"flex\";
      } else {
        div.style.display = \"none\";
      }
    }
    
    function showmorefourth(sp, panel) {
      var id = sp+panel
      var x = document.getElementById(id);
      var div = document.getElementById(sp+\"panelfourth\");
      var div7 = document.getElementById(sp+\"panel7\");
      var div8 = document.getElementById(sp+\"panel8\");
      var status = \"hidden\";
      if (x.style.visibility == \"visible\") {
        x.style.visibility = \"hidden\";
      } else {
        x.style.visibility = \"visible\";
        status = \"visible\";
      }
      if (div7.style.visibility == \"visible\" || div8.style.visibility == \"visible\" || status == \"visible\") {
        div.style.display = \"flex\";
      } else {
        div.style.display = \"none\";
      }
    }
    
    
    document.addEventListener(\"DOMContentLoaded\", () => {
      console.log('tetststststststststs');
      const sections = document.querySelectorAll(\"section\");
      const navLinks = document.querySelectorAll(\"#toc a\");
      const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
          if (entry.isIntersecting) {
            const id = entry.target.id;
            navLinks.forEach(link => {
              link.classList.toggle(\"active\", link.getAttribute(\"href\") === `#${id}`);
            });
          }
        });
      }, {
        root: null,
        rootMargin: \"0px\",
        threshold: 0.6  // Adjust based on how much of the section must be visible
      });
      sections.forEach(section => observer.observe(section));
    });
 
</script>

"))}

con <- file("/home/frousseu/Documents/github/niches_climatiques/results.html", open = "w+b")#, encoding = "UTF-8")
sink(con)

css()  

dashit <- function(x){sub("^(([^ ]* )[^\ ]*) ", "\\1 \u2014\u2014 ", x)}
#dashit(gsub("_", " ", toc$div[i]))
#dashit(paste(toc$species[i], toc$nom[i]))


invisible(lapply(1:nrow(toc), function(i){
  if(toc$model[i] == "data" & !grepl("<b>", toc$display[i])){
    pic <- paste0(gsub(" ", "_", toc$species[i]), "_pic.png")
    copyright <- exif_read(file.path("/home/frousseu/Downloads/niches_climatiques/results/graphics", pic), tags = "Copyright")$Copyright
    ans <- set_species(toc$div[i], common = paste(toc$vernaculaire[i], gsub("_", " ", toc$nom[i]), sep = " \u2014\u2014 "), period = toc$period[i], copyright = copyright)
    stri_write_lines(ans, con = con)
  }
  if(toc$model[i] %in% c("compare", "compare_localized")){
    ans <- set_compare(toc$div[i], common = paste(toc$vernaculaire[i], gsub("_", " ", toc$nom[i]), sep = " \u2014\u2014 "))
    stri_write_lines(ans, con = con)
  }
  if(!toc$model[i] %in% c("compare", "compare_localized", "data")){
    ans <- set_models(toc$div[i], common = paste(toc$vernaculaire[i], gsub("_", " ", toc$nom[i]), sep = " \u2014\u2014 "))
    stri_write_lines(ans, con = con)
  }
}))

script()

cat(paste0("
</body>
</html>
"))


close(con)

#file.show("/home/frousseu/Downloads/website.html")

if(FALSE){
  
  
  getCC0links<-function(species,license=c("cc0")){  
    sp<-gsub(" ","%20",species)
    cc<-paste(license,collapse="0%2C")
    urlsearch<-paste0("https://api.inaturalist.org/v1/taxa?q=",sp,"&order=desc&order_by=observations_count")
    x<-fromJSON(urlsearch)$results
    taxonid<-x$id[1]
    x<-fromJSON(paste0("https://api.inaturalist.org/v1/observations?photo_license=",cc,"&taxon_id=",taxonid,"&&quality_grade=research&order=desc&order_by=created_at"))
    if(x$total_results==0){
      return(NA)
    }else{
      x<-x$results
    }
    users<-x$user[,c("login","name")]
    pics<-do.call("rbind",lapply(seq_along(x$observation_photos),function(i){
      res1<-x$observation_photos[[i]]$photo[,c("url","license_code","attribution")]
      res2<-x$observation_photos[[i]]$photo$original_dimensions[,c("width","height")]
      res<-cbind(res1,res2)
      #res<-res[which(res$width>205 & res$height>205),]
      #if(nrow(res)>0){
      res<-res[1,] # keep first one
      #}
      cbind(id=x$id[i],res,users[rep(i,nrow(res)),])
    })) 
    pics$url<-gsub("/square","/medium",pics$url)
    pics<-pics[which(pics$width>205 & pics$height>205),]
    pics
  }
  
  #getCC0links("Melinis repens") 
  
  roundIm<-function(url,file,path,link=NULL,license=NULL,author=NULL,open=FALSE){
    if(url%in%c("",NA)){
      im<-image_blank(width=500, height=500, color = "")
    }else{
      im <- image_read(url)
    }
    wh<-c(image_info(im)$width,image_info(im)$height)
    mi<- min(wh)
    wm<-which.max(wh)
    if(wh[1]!=wh[2]){
      if(wm==1){
        geom<-paste0(mi, "x", mi, "+",abs(diff(wh))/2,"+","0")
      }else{
        geom<-paste0(mi, "x", mi, "+","0","+",abs(diff(wh))/2)
      }
      im<-image_crop(im, geometry=geom,repage=TRUE)
    }
    wh<-c(image_info(im)$width,image_info(im)$height)
    mask <- image_draw(image_blank(wh[1],wh[2]))
    symbols(wh[1]/2,wh[2]/2,circles=(wh[1]/2)-2,bg="#000000",inches=FALSE,add=TRUE)
    dev.off()
    organism<-image_composite(im,mask, operator="copyopacity") 
    organim<-image_trim(organism)
    h<-image_info(organism)$height
    organism<-image_border(organism,"grey95","50x50",operator="over")
    #organism<-image_draw(organism)
    #symbols(image_info(organism)$height/2,image_info(organism)$width/2,circles=h/2,bg="transparent",fg="grey99",inches=FALSE,lwd=20,add=TRUE)
    #dev.off()
    organism<-image_fill(organism,"none",point="+5+5",fuzz=2)
    organism<-image_trim(organism,fuzz=0)
    image_write(organism,file.path(path,paste0(file,"_pic.png")),format="png",comment=link)
    tags<-c("-overwrite_original",paste0("-ImageDescription=",link),paste0("-Copyright=",license))
    #print(tags)
    exif_call(path=file.path(path,paste0(file,"_pic.png")),args=tags)
  }
  
  #lapply(seq_along(pics),function(i){
  #  roundIm(url=pics[[i]],file=names(pics)[i],path=path,open=FALSE)
  #})
  
  #spp<-
  #speciesPics<-function(species=spp)
  
  library(jsonlite)
  library(magick)
  
  path<-"/home/frousseu/Downloads/niches_climatiques/results/graphics"
  sps <- unique(sapply(strsplit(species, "_"), function(i){paste(i[1:2], collapse = " ")}))#,gsub("_", " ", species)
  
  lapply(sps,function(i){
    print(i)
    links<-getCC0links(i,license=c("cc0"))
    #Sys.sleep(0.05)
    if(identical(NA,links)){
      url<-"https://inaturalist-open-data.s3.amazonaws.com/photos/246765575/large.jpg"
      url<-""
      link<-"https://www.inaturalist.org/observations/143844420"
      link<-""
      author<-""
      license<-""
      
    }else{
      url<-links$url[1]
      link<-paste0("https://www.inaturalist.org/observations/",links$id[1])
      if(links$license_code[1]=="cc0"){
        if(links$name[1]%in%c("",NA)){
          author<-links$login[1]
        }else{
          author<-links$name[1]
        }
        license<-paste0("(c) ",author,", ",links$attribution[1]," (CC0)")
      }else{
        license<-links$attribution[1]
      } 
      
    }
    roundIm(url=url,file=gsub(" ","_",i),path=path,link=link,license=license,open=FALSE)
    print(i)
  })
  
  
  
  #con <- file("C:/Users/God/Downloads/test.html",open="w+b")#, open = "wt", encoding = "latin1")
  #sink(con)
  
  #css() 
  #ans<-species(species_list$sp[1],species_list$url[1],species_list$copyright[1])
  #stri_write_lines(ans,con=con)
  #script()
  #cat(paste0("
  #</div>
  #</div>
  #</body>
  #</html>
  #"))
  
  #  close(con)
  
  #  file.show("C:/Users/God/Downloads/test.html")
  
  
  
  
}


