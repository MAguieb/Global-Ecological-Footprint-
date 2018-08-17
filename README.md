# Global Ecological Footprint
L’empreinte écologique recueille des données sur les atouts écologiques dont une population a besoin pour produire les ressources naturelles qu’elle consomme, notre jeux de données présentent l’empreinte écologique par pays, ceci est une analyse de données exploratoire sur l’ensemble de nos variables.

D’une part l’empreinte écologique qui représente l’écarte entre la consommation des ressources naturelles et la capacité d’absorber les déchets produits d’une population donnée.

Il est défini par :

Total Ecological Foorprint = Cropland Footprint + Grazing Footprint + Forest Footprint + Carbon Footprint + Fish Footprint + Built-up Footprint

D’autre part cette variable est comparé au Total Biocapacity de la région, qui est défini par: Total Biocapacité = Cropland + Grazing Land + Forest Land + Fishing Water + Urban Land

On aussi la variable Biocapacity (Reserve or Deficit) qui définit par:

BioCapacity = Total Biocapacity - Total Ecological Foorprint

Si BioCapacity > 0, la région est appelée une réserve.

Si BioCapacity < 0, la région est appelée Déficit, ce qui signifie qu’elle doit importer des actifs pour soutenir sa population.

N.B: Notre jeux de données ne contient pas la variable Built-up Footprint, mais nous pouvons l’inférer en soustrayant le Total Ecological Foorprint des autres caractéristiques (Cropland Footprint, Grazing Footprint, Forest Footprint, Carbon Footprint, Fish Footprint)

Quelques Réflexions:

Quels pays ont les plus grands déficits ou réserves écologiques?

Existe-t-il une relation entre le statut d’un pays (Réserve, Déficit) et sa Population, GPD, HDI (Indice de Développement Humain)?
