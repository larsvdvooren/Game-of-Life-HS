___
# Inleiding
>Doel van de opdracht en korte uitleg over de gekozen programmeertaal.

In deze opdracht ga ik een zelfbedachte challenge ontwerpen en implementeren in een zuiver functionele programmeertaal, een paar van de gegeven opties waren:
- Haskell
- Erlang
- Clojure
- Elixir

Het doel van de opdracht is:
- De werking van functionele programmeertalen te begrijpen en functionele concepten in de praktijk te brengen.
- Een oplossing te programmeren die gebaseerd is op het functionele paradigma.
- Functionele concepten bewust te koppelen aan specifieke onderdelen van mijn uitgeprogrammeerde algoritme.
- Mijn bevindingen helder te verwoorden in een rapport dat voldoet aan de AIM-controlekaart.

De door mij gekozen programmeertaal is **Haskell**, een *zuiver functionele*, *statisch getypeerde* programmeertaal die bekend staat om sterke ondersteuning van functionele paradigma. In het onderzoek ga ik verder in op wat deze concepten inhouden, en waarom Haskell een goede keuze is voor deze opdracht.

# Onderzoek
>Functionele concepten en kenmerken van de gekozen taal.

Haskell dwingt je om op een declaratieve manier te denken: je beschrijft wat het resultaat moet zijn in plaats van stap voor stap te beschrijven hoe iets moet gebeuren (zoals in een imperatieve taal). Dit dwingt het gebruik van Immutability, Recursie, hogere orde functies en vooral zuivere functies af, dit zijn allemaal kernconcepten binnen functioneel programmeren.


> Functioneel programmeren:
> "At the heart of Haskell is the concept of purity. In functional programming, purity refers to the idea that functions should behave like mathematical functions: they always produce the same output given the same input and cause no side effects. This property simplifies reasoning about code, leading to more predictable and testable software. Throughout this book, you will see how Haskell’s commitment to purity leads to elegant solutions for complex problems, fostering code that is more modular, easier to debug, and naturally parallelizable" (Edet, 2024).

> Statische Typering: 
> "Static Typing: Haskell uses static typing, meaning that types are checked at compile-time. This leads to early detection of type errors and enhances program reliability." (Edet, 2024).


# Challenge
>  Beschrijving van de bedachte challenge en waarom deze uitdagend is.

De door mij uitgekozen challenge is: het uitprogrammeren van de klassieker: **Conway's Game of Life.** in Haskell.
Het hiervan uitwerken leek mij interessanter (en een iets grotere uitdaging) dan het uitwerken van een van de voorgegeven opties. Na het voorgesteld te hebben aan mijn docent heb ik de go-ahead gekregen om te beginnen aan deze challenge.

### Conway's Game of Life:
Is bedacht door britse wiskundige John Horton Conway in 1970, als een cellular automation simulatie waarin 'cellen' worden 'geboren', 'leven' en 'sterven' aan de hand van een voorgegeven set aan regels (algoritmen), zonder input van buitenaf. 

Het gebruik van (vrij eenvoudige) algoritmes om een systeem te beheren dat soms vrij complexe entiteiten kan creëren sprak me aan om dit te kiezen voor deze challenge.

Waarom haskell hiervoor?

> "Haskell’s functional paradigm offers unique advantages for game development. This module covers building real-time interactive games, working with game engines, and leveraging Haskell’s strengths in concurrency and functional purity. Case studies highlight successful Haskell-based game projects that emphasize performance and reliability." (Edet, 2024).

### Om dit goed te kunnen doen heb ik voor mijzelf een lijst aan requirements opgezet volgende de MoSCoW methode:

#### Must
- Weergave van een speelveld met grid.
- Een initial state die random gegenereerd wordt.
- Cellen volgens het algoritme leven en sterven.

#### Should
- Een teller met daarin de hoeveelheid levende cellen in het veld.
- Een teller met daarin de hoeveelheid aan gelopen generaties.
- De simulatie stopt zodra er geen verandering meer is (loop van 3 of 4 generaties?)
- Een visualisatie van 'dode' cellen

#### Could
- Speel/pauze knop
- Keypress voor volgende generatie
- (HELE GROTE COULD) vorige generatie knop
- Met muis plaatsbare cellen
- gridsize + aantal initial cellen als parameter meegeven?
- Wipe veld en genereer nieuwe random cellen met keypress

#### Won't
- /

Een groot deel van deze requirements heb ik niet uit kunnen werken helaas.


Online heb ik 3 implementaties van Conway's GoL gevonden die ik als inspiratie wil gaan gebruiken:
- [Game-of-Life-Haskell](https://github.com/alexbooth/Game-of-Life-Haskell) van Alexander Booth
- [Game-of-Life-Haskell](https://github.com/rst0git/Game-of-Life-Haskell) van Radostin Stoyanov
- [game-of-life](https://github.com/marcusbuffett/game-of-life/tree/master?tab=readme-ov-file) van Marcus Buffet

Van deze drie heb ik de meeste inspiratie (en de basis van de algoritmen) gehaald uit de uitwerking van Alexander Booth.

# Implementatie
>Korte samenvatting van de implementatie en gebruikte functionele concepten.

## Gebruikte Functionele Concepten
#### Immutability:
Immutable data structures staan aan de kern van mijn implementatie. De `GameState` wordt nooit aangepast en iedere generatie opnieuw gegenereerd.

#### Pure functies:
Alle functies zijn pure functies, ze geven altijd dezelfde output bij dezelfde input en hebben geen neveneffecten.

#### Higher order Functions
De implementatie maakt veel gebruik van Higher-order functies uit de standaard library:
- `map` zet elke cell in de lijst om naar een Picture object:
> `drawCells board = pictures (map drawCell board)`

- `filter` Selecteert de juiste elementen waar nodig, bijvoorbeeld: 
> `aliveNeighbors = filter (`elem` board) neighborCoords`
of
`deadNeighbors = filter (\cell -> cell `notElem` board) neighborCoords`

- `replicateM` voert een actie n keer uit:
> `generateRandomCoords n = replicateM n getRandomCoords`

#### Type safety
Bijvoorbeeld:
> `type Cell = (Int, Int) `
> `type Board = [Cell] `
> `type GameState = Board`

#### Lazy Evaluation
Door hoe Haskell omgaat met data worden waarden pas berekend wanneer zij nodig zijn, dat helpt bijvoorbeeld hier: 
> `neighborCoords = [(x + dx, y + dy) | (x, y) <- board, dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0,0)]`

#### Monads (IO Monad)
Voor random generatie en I/O operaties gebruik ik de IO monad, bijvoorbeeld: 
> `initialState :: IO GameState
`initialState = generateRandomCoords amountOfStartingCells`

## Een paar functies en beslissingen uitgelicht:

### Window aanmaken
Voor het laten zien van de Game of Life aan de gebruiker heb ik meerdere opties overwogen, de drie methoden die gebruikt zijn voor de drie hiervoor genoemde gevonden voorbeelden:
- [Gloss](https://hackage.haskell.org/package/gloss) (Booth A. 2016) 
- Terminal (Stoyanov R. 2017)
- [ncurses](https://hackage.haskell.org/package/hscurses) (hscurses) (Buffett M. 2017)

Van de drie sprak Gloss me het meeste aan hoewel ncurses een bepaalde charme heeft. De implementatie in terminal heb ik achter de hand gehouden als fallback mocht ik in tijdsnood komen.

Hoewel ik tegen wat problemen ben aangelopen met het instellen van Gloss (uitleg rondom freeGlut liet te wensen over) werkte het uiteindelijk prima en draait de GoL in een eigen grafisch window, wat veel eleganter over kan komen dan een terminal applicatie.

### Cellen Genereren

#### Duplicate cellen probleem
Omdat de code random nummers tussen `0` en `gridWidth - 1` of `gridHeight - 1` genereert om coordinaten aan te maken gaan duplicate coordinaten voor cellen voorkomen. Om dit tegen te gaan en een cel niet onnodig 2x te tekenen (en onnodig veel te berekenen adhv het algoritme) zullen deze uit de lijst gehaald moeten worden.
De eerste oplossing die ik tegenkwam zou uitgewerkt deze vorm hebben: 

> removeDuplicates :: Eq x => [x] -> [x] 
> removeDuplicates = nub

DMV

>import Data.List (nub)
___

Deze oplossing heeft echter de O(n^2) complexiteit en gaat (vooral met grotere grids) onnodig veel processorkracht gebruiken, op [StackExchange](https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem) ben ik een andere oplossing tegengekomen die er uitgewerkt zo uitziet: 

> removeDuplicates :: (Ord x) => [x] -> [x] 
> removeDuplicates = map head . group . sort

DMV

>import Data.List (sort, group)
___

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
___

Deze implementatie heb ik eventjes gebruikt maar uiteindelijk vervangen omdat ik het onnodig complex vond. Verder heb ik er geen vertrouwen in dat ik helemaal doorheb wat ik aan het doen ben met `Data.Set`. Vandaar de uiteindelijke implementatie met gebruik van `Data.List`.

### Leven-Dood cyclus algoritmen

Mijn algoritme is gebaseerd op deze uitwerking:

> `nextAlive alive (a,b)`
	`| numAliveNeighbors alive (a,b) == 3 = True`
	`| alive (a,b) && numAliveNeighbors alive (a,b) == 2 = True`
	`| otherwise = False`
(Booth, 2016)

Zoals te zien is hier bededen bereikt mijn code in principe hetzelfde doel, maar is het makkelijker onderhoudbaar (en minder efficiënt)

>`calculateNextGeneration :: GameState -> GameState
`calculateNextGeneration board = removeDuplicates $ survivors ++ births
  `where
    `-- If a cell has either 2 or 3 neighbors that cell survives to the next generation
    `survivors = [cell | cell <- board, let c = countCellNeighbors cell board, c == 2 || c == 3]  
    `-- Gives a list of all coordinates adjacent to live cells
    `neighborCoords = [(x + dx, y + dy) | (x, y) <- board, dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0,0)]
    `-- Filters the list of gridsquares that border alive cells for currently occupied gridsquares
    `deadNeighbors = filter (\cell -> cell `notElem` board) neighborCoords
    `-- Checks if potential new cell gridsquares are out of bounds (inside (0,0) and (gridWidth,gridHeight))
    `checkIfInBounds = filter (\(x, y) -> x >= 0 && y >= 0 && x < toInt gridWidth && y < toInt gridHeight) deadNeighbors
    `-- Checks if the previously filterered gridsquares border 3 live cells exactly, if so, those cells come to life
    `births = [cell | cell <- checkIfInBounds, countCellNeighbors cell board == 3]`
(app/Main.hs)

Het algoritme loopt als volgt 1 generatie per oproepping door:
1. Het kijkt of een nu levende cell de volgende generatie haalt door te kijken of die cell aan 2 of 3 andere levende cellen grenst, zo niet dan sterft de cel.
2. Het zoekt waar nieuwe cellen kunnen ontstaan door te kijken naar alle lege plekken rondom cellen, voor al die plekken telt het de buren op, zijn dit er precies 3, dan komt die dode cel tot leven.
3. Het maakt de nieuwe generatie aan door de cellen die de huidige generatie hebben overleeft te combineren met de nieuwe cellen.
4. Het loopt door de lijst heen om eventuele 'dubbele' cellen te verwijderen (twee of meerdere cellen die dezelfde coördinaat hebben).

#### Slechte prestaties bij grotere playfields:
###### N.B. Dit is niet geïmplementeerd I.V.M. gebrek aan tijd en kennis.
##### Probleem: schaalbaarheid bij lijsten (en slechte duplicate policeing)
Op dit moment gebruikt het bord een lijst aan coördinaten om bij te houden welke gridSquares in leven zijn. Hoewel dit prima werkt in kleinere grids met weinig levende cellen, is er duidelijke slowdown wanneer het speelveld groter wordt (of er meer cellen actief zijn). 
Dit gebeurt omdat:
- Er voor iedere levende cell iedere generatie gekeken wordt naar de 8 cellen waar deze aan grenst. Dit leidt tot heel veel lijst doorzoekingen met O(n) complexiteit.
- Hij removeDuplicates doorloopt, iedere generatie, voor de hele lijst. (geloof me dit is nodig.)


##### Oplossing: gebruik maken van `Data.Set` ipv een lijst
Een Set gebruiken:
Een `Data.Set` hiervoor gebruiken heeft meerdere voordelen over de huidige implementatie:
- Snelheid (O(log n) in plaats van O(n))
- Automatisch unieke elementen (geen removeduplicates nodig)

Hiermee zou dus niet alleen de efficiëntie waarmee het programma generaties berekent vergroten, ook zou het een stap uit die berekening zelf halen.

De code staat nu in comments onder de huidige `removeDuplicates` functie, maar is nu dus net in gebruik.

>Misschien toch proberen?)

N.B. Een uiteindelijke aesthetische beslissing heeft ervoor gezorgd dat de functie `drawGridlines` niet meer in gebruik is en dus in comments staat. Mocht je deze lijnen toch willen zien dan zal je de functie uit comments moeten halen, de achtergrondkleur van de applicatie (onderin main) óf de kleur van de gridlines aanpassen (in de functie zelf)

# Reflectie
>Wat heb je geleerd over het functionele paradigma? Wat werkte goed of juist niet?


Deze opdracht heeft mij er toe gezet om een fundamenteel andere manier van programmeren te gebruiken dan ik gewend ben. Waar je bij 'normaal' programmeren (Imperatief) stap voor stap instructies geeft dwingt Haskell je om met transformaties en datatypes te denken. In het begin (en nu nog steeds soms) was dit vrij frustrerend, vooral het niet echt kunnen debuggen met `printLn`'s.

Immutability was eventjes wennen, vooral het nut ervan inzien duurde eventjes. Maar de voordelen zijn mij nu duidelijk, door steeds een nieuw bord te tekenen kan je nooit vergeten een waarde te updaten of de verkeerde cell aanpassen (als je het goed hebt geschreven). Hierdoor wordt de code veel voorspelbaarder.

Ik liep wel vast hier en daar, vooral bij het gebruik van `Data.set` hier wil ik in mijn vrije tijd nog aan verder werken aangezien ik wel zie hoe dat de applicatie vele malen sneller/lighter kan maken.

Het was een redelijke leerweg, vooral het begin, maar uiteindelijk verliep het bedenken van oplossingen en het implementeren daarvan vrij goed.

De challenge heeft me niet overgehaald om over te stappen naar functioneel programmeren, maar ik zie het nu wel als een optie. Ik denk dat ik bij een toekomstig project nu goed kan inzien of een functionele optie misschien een betere match is dan een imperatieve, en daarbij het vertrouwen kan hebben om die optie uit te werken (of in ieder geval met prototypen te bewijzen).

# Conclusie
>Samenvatting van de belangrijkste leerpunten.

Dit Game of Life project vormde een waardevolle introductie in functioneel programmeren met Haskell. In de implementatie heb ik kernconcepten zoals immutability, pure functies en higher-order functies gebruikt om de challenge aan te pakken.

De belangrijkste dingen die ik heb geleerd zijn: de kracht van *immutability* voor bug-preventie, de elegantie van *function composition* en *list comprehensions*, en de waarde van Haskell's type systeem als failsafe. Ook de prestaties van de code (waar het niet inefficiënt door mij geschreven is) is erg indrukwekkend.

Functioneel programmeren is niet noodzakelijk beter dan imperatief programmeren, maar het heeft wel zijn plaats en biedt een goede kans tot leren. Conway's Game of Life bleek een ideaal probleem voor Functioneel Programmeren: elke generatie is een pure transformatie in een immutable state. Wat hier perfect uitkomt.

Het meest waardevolle dat ik uit deze challenge heb gehaald met Haskell is niet de kennis over de taal zelf, maar de denkpatronen die toepasbaar zijn in andere talen: voorkeur voor immutability, kleine, pure functies maken code meer modulair en daardoor herbruikbaarder wordt én voorspelbaarder in wat de code doet. 

Conway's Game of Life is een passende metafoor: simpele regels kunnen leiden tot verrassend complexe patronen. De basisconcepten van functioneel programmeren zijn elegant, maar hun toepassing en implicaties zijn rijk en soms onverwacht. Met deze challenge heb ik veel kunnen leren en een beter perspectief gekregen op functioneel programmeren.

# Bronvermelding

## Bronnen:


Booth, A. (alexbooth). (2016, 22 december). Game-of-Life-Haskell. Github. Geraadpleegd op 30 September 2025, van:
https://github.com/alexbooth/Game-of-Life-Haskell

Stoyanov R. (rts0git). (2017, 22 april). Game-of-Life-Haskell. Github, geraadpleegd op 1 Oktober 2025, van:
https://github.com/rst0git/Game-of-Life-Haskell

Buffett M. (marcusbuffett). (2016, 24 oktober). game-of-life. Github. Geraadpleegd op 30 september 2025, van:
https://github.com/marcusbuffett/game-of-life/tree/master?tab=readme-ov-file


Edet T. (2024). # Haskell Programming: Pure Functional Language with Strong Typing for Advanced Data Manipulation and Concurrency (Mastering Programming Languages Series). CompreQuest Books.

## Prompts:
### Claude:
"Wat heb ik verpest? Zitten er spelfoiuten in? Lopen de zinnen een beetje? Verder nog opmerkingen?"
(Vorige variant van dit document bijgevoegd)
https://claude.ai/share/f3d01064-21db-4673-91dd-ea72affa9a84
