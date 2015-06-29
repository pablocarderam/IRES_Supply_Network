fillCodDep <- function(pRow) {
  row <- pRow;
  if (row["DEPARTAMENTO"] == "EXTERIOR") {
    row["COD_DEP"] = 0;
  } 
  else if (row["DEPARTAMENTO"] == "PROCEDENCIA DESCONOCIDA") {
    row["COD_DEP"] = -1;
  } 
  else {
  #if (is.na(row["COD_DEP"])) {
    depIndeces = which(ColMun$NOMBRE_DPT == row["DEPARTAMENTO"]);
    dep = ColMun$DPTO[ depIndeces[1] ];
    #print(depIndeces[1]);
    row["COD_DEP"] = dep;
  }
  return(row);
}

# ext <- function(pRow) {
#   row <- pRow;
#   if (row["DEPARTAMENTO"] == "EXTERIOR") {
#     row["COD_DEP"] <- 0;
#   }
#   return(row);
# }
# 
# procDesc <- function(pRow) {
#   row <- pRow;
#   if (row["DEPARTAMENTO"] == "PROCEDENCIA DESCONOCIDA") {
#     row["COD_DEP"] <- -1;
#   }
#   return(row);
# }

crctNames <- function(pDf) {
  df = pDf
  df$DEPARTAMENTO[df$DEPARTAMENTO == "GUAJIRA"] <- "LA GUAJIRA";
  df$DEPARTAMENTO[df$DEPARTAMENTO == "NORTE SANTANDER"] <- "NORTE DE SANTANDER";
  df$DEPARTAMENTO[df$DEPARTAMENTO == "SAN ANDRES"] <- "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA";
  df$DEPARTAMENTO[df$DEPARTAMENTO == "VALLE"] <- "VALLE DEL CAUCA";
  df$DEPARTAMENTO = gsub("\\.", "", df$DEPARTAMENTO);
  
  df$MUNICIPIO[df$MUNICIPIO == "ANIMAS"] <- "UNION PANAMERICANA";
  df$MUNICIPIO[df$MUNICIPIO == "ANTIOQUIA"] <- "* ANTIOQUIA MUNICIPIO DESCONOCIDO";
  df$MUNICIPIO[df$MUNICIPIO == "BETE"] <- "MEDIO ATRATO";
  df[df$MUNICIPIO == "BOLIVAR" & df$DEPARTAMENTO == "ANTIOQUIA", ]$MUNICIPIO <- "CIUDAD BOLIVAR" # mas de un municipio con nombre, se especifica dept.
  df$MUNICIPIO[df$MUNICIPIO == "BRICENO"] <- "BRICEÑO";
  df$MUNICIPIO[df$MUNICIPIO == "CANTON DE SAN PABLO (MANAGRU)"] <- "CANTON DEL SAN PABLO (MANAGRU)";
  df$MUNICIPIO[df$MUNICIPIO == "CARMEN DE VIBORAL"] <- "EL CARMEN DE VIBORAL";
  df$MUNICIPIO[df$MUNICIPIO == "CARTAGENA DE INDIAS"] <- "CARTAGENA";
  df$MUNICIPIO[df$MUNICIPIO == "COCONUCO"] <- "PURACE";
  df$MUNICIPIO[df$MUNICIPIO == "CUBARRAL"] <- "SAN LUIS DE CUBARRAL";
  df$MUNICIPIO[df$MUNICIPIO == "CURBARADO"] <- "CARMEN DEL DARIEN";
  df$MUNICIPIO[df$MUNICIPIO == "EL PENON"] <- "EL PEÑON";
  df$MUNICIPIO[df$MUNICIPIO == "EL PINON"] <- "EL PIÑON";
  df$MUNICIPIO[df$MUNICIPIO == "GUACHENE"] <- "CALOTO"; # En realidad si es municipio, el mapa esta desactualizado
  df$MUNICIPIO[df$MUNICIPIO == "LA HORMIGA"] <- "VALLE DEL GUAMUEZ";
  df$MUNICIPIO[df$MUNICIPIO == "LA MONTANITA"] <- "LA MONTAÑITA";
  df$MUNICIPIO[df$MUNICIPIO == "LITORAL DEL BAJO SAN JUAN"] <- "EL LITORAL DEL SAN JUAN";
  df$MUNICIPIO[df$MUNICIPIO == "MANAURE BALCON DEL CESAR"] <- "MANAURE";
  df$MUNICIPIO[df$MUNICIPIO == "NARINO"] <- "NARIÑO";
  df$MUNICIPIO[df$MUNICIPIO == "NOROSI"] <- "RIO VIEJO"; # En realidad si es municipio, el mapa esta desactualizado
  df$MUNICIPIO[df$MUNICIPIO == "OCANA"] <- "OCAÑA";
  df$MUNICIPIO[df$MUNICIPIO == "OSPINA PEREZ"] <- "VENECIA";
  df$MUNICIPIO[df$MUNICIPIO == "PALMAS SOCORRO"] <- "PALMAS DEL SOCORRO";
  df$MUNICIPIO[df$MUNICIPIO == "PIJINO DEL CARMEN (PIJINO)"] <- "PIJIÑO DEL CARMEN";
  df$MUNICIPIO[df$MUNICIPIO == "POLO NUEVO"] <- "POLONUEVO";
  df$MUNICIPIO[df$MUNICIPIO == "PUERTO CARRENO"] <- "PUERTO CARREÑO";
  df$MUNICIPIO[df$MUNICIPIO == "PUERTO INIRIDA"] <- "INIRIDA";
  df$MUNICIPIO[df$MUNICIPIO == "PUNTA DE PIEDRAS"] <- "PEDRAZA";
  df$MUNICIPIO[df$MUNICIPIO == "PURISIMA DE LA CONCEPCION"] <- "PURISIMA";
  df$MUNICIPIO[df$MUNICIPIO == "RAFAEL REYES (APULO)"] <- "APULO";
  df$MUNICIPIO[df$MUNICIPIO == "RIOQUITO"] <- "RIO QUITO";
  df$MUNICIPIO[df$MUNICIPIO == "SALDANA"] <- "SALDAÑA";
  df$MUNICIPIO[df$MUNICIPIO == "SAN ANDRES DE CUERQUIA"] <- "SAN ANDRES";
  df$MUNICIPIO[df$MUNICIPIO == "SAN JOSE DE URE"] <- "PUERTO LIBERTADOR"; # Se les olvido este municipio, es viejisimo
  df$MUNICIPIO[df$MUNICIPIO == "SAN FRANCISCO DE QUIBDO"] <- "QUIBDO";
  df$MUNICIPIO[df$MUNICIPIO == "SAN PEDRO DE LOS MILAGROS"] <- "SAN PEDRO";
  df$MUNICIPIO[df$MUNICIPIO == "SAN SEBASTIAN DE MARIQUITA"] <- "MARIQUITA";
  df$MUNICIPIO[df$MUNICIPIO == "SANTA CRUZ DE LORICA"] <- "LORICA";
  df$MUNICIPIO[df$MUNICIPIO == "SANTA RITA"] <- "RIO IRO";
  df$MUNICIPIO[df$MUNICIPIO == "SANTA ROSA DE LIMA"] <- "SANTA ROSA";
  df$MUNICIPIO[df$MUNICIPIO == "SANTIAGO DE CALI"] <- "CALI";
  df[df$MUNICIPIO == "SANTUARIO" & df$DEPARTAMENTO == "ANTIOQUIA", ]$MUNICIPIO <- "EL SANTUARIO" # mas de un municipio con nombre, se especifica dept.
  df$MUNICIPIO[df$MUNICIPIO == "SITIO NUEVO"] <- "SITIONUEVO";
  df$MUNICIPIO[df$MUNICIPIO == "TUCHIN"] <- "SAN ANDRES DE SOTAVENTO"; # En realidad si es municipio, el mapa esta desactualizado
  df$MUNICIPIO[df$MUNICIPIO == "VILLARICA"] <- "VILLA RICA";
  
  df$MUNICIPIO = gsub("^\\s+|\\s+$", "", df$MUNICIPIO);
  df$MUNICIPIO = gsub("\\.", "", df$MUNICIPIO);
  df$MUNICIPIO = as.character(df$MUNICIPIO);
  #df = t(df);
  #df = as.data.frame(df, stringsAsFactors = F);
  return(df);
}

fillCodMun <- function(pRow) {
  row <- pRow;
  if (regexpr( "\\*", row["MUNICIPIO"] )[1] > 0 || row["COD_DEP"] == 0) {
    row["COD_MUN"] = 0;
  }
  else {#if (is.na(row["COD_MUN"])) {
    munIndeces = which(ColMun$NOMBRE_MPI == row["MUNICIPIO"] );
    if (length(munIndeces) > 0) {
      for (index in munIndeces) {
        mun = ColMun$MPIO[index];
        #print( c( row["COD_DEP"], AdminColombia$ID_1[index] ) );
        if (row["COD_DEP"] == ColMun$DPTO[index]) {
          row["COD_MUN"] = mun;
        }
      }
    }
    else {
      munIndeces = which(ColMun$NOMBRE_CAB == row["MUNICIPIO"] );
      if (length(munIndeces) > 0) {
        for (index in munIndeces) {
          mun = ColMun$MPIO[index];
          #print( c( row["COD_DEP"], AdminColombia$ID_1[index] ) );
          if (row["COD_DEP"] == ColMun$DPTO[index]) {
            row["COD_MUN"] = mun;
          }
        }
      }
      else {
        altName = row["MUNICIPIO"];
        pIndex = regexpr( "\\(", altName )[1];
        
        if (!is.na(pIndex) && pIndex > 0) {
          altName = substr(altName, 1, pIndex-1);
          #print(c(altName, pIndex));
          altName = gsub("^\\s+|\\s+$", "", altName);
        }
        
        munIndeces = which( ColMun$NOMBRE_MPI == altName );
        if (length(munIndeces) > 0) {
          for (index in munIndeces) {
            mun = ColMun$MPIO[index];
            if (row["COD_DEP"] == ColMun$DPTO[index]) {
              row["COD_MUN"] = mun;
            }
          }
        }
        else {
          munIndeces = which( ColMun$NOMBRE_CAB == altName );
          if (length(munIndeces) > 0) {
            for (index in munIndeces) {
              mun = ColMun$MPIO[index];
              if (row["COD_DEP"] == ColMun$DPTO[index]) {
                row["COD_MUN"] = mun;
              }
            }
          }
          else {
          #         if (!is.na(pIndex) && pIndex > 0) {
          #           altName = row["MUNICIPIO"];
          #           altName = substr(altName, pIndex+1, pIndex-1);
          #           print(c(altName, pIndex));
          #           altName = gsub("^\\s+|\\s+$", "", altName);
          #         }
            print(as.character(row["MUNICIPIO"]));
            unsorted <<- c(unsorted, as.character(row["MUNICIPIO"]));
          }
        }
      }
    }
  }
  return(row);
}


library(rgdal);
ColMun <- readOGR("/Users/Palo/Downloads/mpio", "mpio", stringsAsFactors = F);

ColMun$NOMBRE_MPI <- gsub("¥", "Ñ", ColMun$NOMBRE_MPI);
ColMun$NOMBRE_MPI <- gsub("  ", " ", ColMun$NOMBRE_MPI);
ColMun$NOMBRE_MPI <- gsub("^\\s+|\\s+$", "", ColMun$NOMBRE_MPI);
ColMun$NOMBRE_MPI <- gsub("\\.", "", ColMun$NOMBRE_MPI);
ColMun$NOMBRE_CAB <- gsub("¥", "Ñ", ColMun$NOMBRE_CAB);
ColMun$NOMBRE_CAB <- gsub("  ", " ", ColMun$NOMBRE_CAB);
ColMun$NOMBRE_CAB <- gsub("^\\s+|\\s+$", "", ColMun$NOMBRE_CAB);
ColMun$NOMBRE_CAB <- gsub("\\.", "", ColMun$NOMBRE_CAB);
ColMun$NOMBRE_DPT <- gsub("¥", "Ñ", ColMun$NOMBRE_DPT);
ColMun$NOMBRE_DPT <- gsub("  ", " ", ColMun$NOMBRE_DPT);
ColMun$NOMBRE_DPT <- gsub("^\\s+|\\s+$", "", ColMun$NOMBRE_DPT);
ColMun$NOMBRE_DPT <- gsub("\\.", "", ColMun$NOMBRE_DPT);

ColMun$NOMBRE_MPI[ColMun$NOMBRE_MPI == "ARIGUAINI"] <- "ARIGUANI";
ColMun$NOMBRE_MPI[ColMun$NOMBRE_MPI == "SANTAFE DE ANTIOQUIA"] <- "SANTA FE DE ANTIOQUIA";
ColMun$NOMBRE_MPI[ColMun$NOMBRE_MPI == "MIRITI-PARANA"] <- "MIRITI PARANA";

plot(ColMun);
View(ColMun);

SnakebiteColombia <- read.csv("~/Downloads/SnakebiteColombia.csv", encoding="UTF-8", stringsAsFactors=FALSE);
View(SnakebiteColombia);

SnakebiteColombia <- crctNames(SnakebiteColombia);

# SnakebiteColombia <- apply(SnakebiteColombia, 1, procDesc);
# SnakebiteColombia <- t(SnakebiteColombia);
# SnakebiteColombia <- as.data.frame(SnakebiteColombia);
# 
# SnakebiteColombia <- apply(SnakebiteColombia, 1, ext);
# SnakebiteColombia <- t(SnakebiteColombia);
# SnakebiteColombia <- as.data.frame(SnakebiteColombia);

SnakebiteColombia <- apply(SnakebiteColombia, 1, fillCodDep);
SnakebiteColombia <- t(SnakebiteColombia);
SnakebiteColombia <- as.data.frame(SnakebiteColombia);

unsorted = list();
SnakebiteColombia <- apply(SnakebiteColombia, 1, fillCodMun);
SnakebiteColombia <- t(SnakebiteColombia);
SnakebiteColombia <- as.data.frame(SnakebiteColombia);
print(length(unsorted));
unsorted = unique(t(as.data.frame(unsorted)));
View(unsorted);
#which(AdminColombia$NAME_2 == SnakebiteColombia$MUNICIPIO[2]);
