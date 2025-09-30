

handmatig aan path toevoegen
freeglut downloaden [link ooit] dll in path zetten


3 verschillende repo's inspiratie uithalen en eigen intepretatie.
  - wie doet wat en waarom?
  - Waarom kies ik optie a over b of c?
  - Doe ik iets anders? waarom?

3 verschillende staten van gridcells:
- Levend ()
- Dood (voormalig levend)
- Levenloos (nooit levend of dood geweest)

# Inleiding


# Onderzoek

## Wat wil ik dat de applicatie wel en niet doet?
### MOSCOW
#### Must
- Speelveld met een grid daarop
- Cellen die inladen en leven binnen dat speelveld aan de hand van een algoritme (tbd)
- Een lopende simulatie die doorgaat tot er geen levende cellen meer zijn

#### Should
- Een teller met daarin de hoeveelheid levende cellen in het veld
- Een teller met daarin de hoeveelheid aan gelopen generaties
- Laat 'dode' cellen zien

#### Could
- Speel/pauze knop
- keypress voor volgende generatie
- (HELE GROTE COULD) vorige generatie knop
- Met muis plaatsbare cellen
- gridsize + aantal initial cellen als parameter meegeven?
- 

#### Won't
- /













# Implementatie

### Window aanmaken


### Grid tekenen


### Cellen Genereren

#### Duplicate cellen probleem
Omdat de code random nummers tussen 0 en gridWidth - 1 of gridHeight - 1 genereert om coordinaten aan te maken gaan duplicate coordinaten voor cellen voorkomen. Om dit tegen te gaan en een cel niet onnodig 2x te tekenen (en onnodig veel te berekenen adhv het algoritme) zullen deze uit de lijst gehaald moeten worden.
De eerste oplossing die ik tegenkwam zou uitgewerkt deze vorm hebben: 

> removeDuplicates :: Eq x => [x] -> [x] removeDuplicates = nub

DMV

>import Data.List (nub)

Deze oplossing heeft echter de O(n^2) complexiteit en gaat (vooral met grotere grids) onnodig veel processorkracht gebruiken, op [StackExchange](https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem) ben ik een andere oplossing tegengekomen die er uitgewerkt zo uitziet: 

> removeDuplicates :: (Ord x) => [x] -> [x] removeDuplicates = map head . group . sort

DMV

>import Data.List (sort, group)

Deze oplossing heeft de O(n log n) complexiteit en past beter bij de specifieke toepassing die deze functie heeft. Deze oplossing verwijderd duplicates door te sorteren, maar dat is in deze context onbelangrijk aangezien de volgorde van de data in de state irrelevant is. 

Nu is er uiteindelijk een nóg betere oplossing die de lijst niet hoeft te sorteren voordat er duplicates uit verwijderd worden. Deze functie loopt 1 voor 1 door de lijst en verwijdert coördinaten die hij al heeft gezien uit de lijst. Door `set` te gebruiken in plaats van steeds door de hele lijst te lopen is deze code een stuk sneller dan `nub`

>removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = go Set.empty
  where
    go _   []     = []
    go set (x:xs)
      | x `Set.member` set = go set xs
      | otherwise          = x : go (Set.insert x set) xs
  
DMV

>import qualified Data.Set as Set










# Reflectie


# Conclusie


# Bronvermelding


## Bronnen:


https://github.com/alexbooth/Game-of-Life-Haskell
(Algoritme en het idee om Gloss te gebruiken)




## Prompts:
### Claude:



### ChatGPT:




