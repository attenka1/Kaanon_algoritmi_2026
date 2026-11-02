# Kaanonit ja kontrapunktit – ensimmäinen toteutus

R-ohjelma etsii 2–4-äänisiä kaanoneita ja eri teemojen kontrapunkteja. Uusi toteutus on itsenäinen eikä tarvitse vanhaa MusicTheoryScriptsForR.R-tiedostoa. Alkuperäistä algoritmia ei muuteta.

## Käynnistys

Avaa R tässä projektikansiossa ja suorita:

```r
shiny::runApp('.')
```

Käyttöliittymässä voi ladata MIDI-, MusicXML- tai pakatun MXL-tiedoston, valita teemajaon, äänimäärän, I/R/RI-muunnokset, rytmikertoimet, viiveet, transpositiot ja korjausasetukset. O on aina käytössä. Tulokset näkyvät taulukkona ja sävelkorkeuskuvana. Kaikki palautetut ratkaisut voi ladata yhdessä MIDI- tai MusicXML-tiedostossa. Raportin voi ladata erikseen. RStudiossa voi käyttää myös **Tallenna MusicXML projektikansioon** -painiketta: se kirjoittaa oikean `.musicxml`-tiedoston suoraan projektin `output/`-kansioon ja näyttää tiedoston koko polun. Yksilöllinen tiedostonimi estää vanhojen tulosten korvaamisen. Selainlataukset käyttävät erikseen määritettyä tiedostotyyppiä, ja ne rekisteröidään myös painikkeiden ollessa näkymän ulkopuolella.

Tarvitaan R, Shiny-paketti ja komentopolusta löytyvä Python 3. MusicXML-vienti ei tarvitse Python 3.9:n `math.lcm`- tai `ElementTree.indent`-funktioita, joten myös vanhempi RStudion löytämä Python toimii. Tiedostojen luku ja vienti käyttävät vain Pythonin vakiokirjastoa. Käyttöliittymä tekee haun paikallisessa R-prosessissa, joten se odottaa haun valmistumista aikarajaan asti.

## Säkeet peräkkäisinä kaanoneina

Valinta **Säkeet peräkkäisinä kaksiäänisinä kaanoneina** käsittelee yhden syötemelodian säkeet niiden alkuperäisessä järjestyksessä. Anna **Tahteja säejaksossa**, esimerkiksi 5: säkeet alkavat tahdeista 1, 6, 11 jne. Lopputauot säilyvät jaksoissa. Kokonaan tyhjät jaksot ohitetaan kuten tavallisessa teemajaossa. Valitse monistemmaisesta tiedostosta yksi melodialinja.

Valinta **Sama alkuintervalli kaikissa säkeissä** on oletuksena käytössä. Kun sen poistaa, kullekin säkeelle kokeillaan erikseen transpositiolistan alkuintervalleja. Jokainen säe on edelleen oma kaksiääninen kaanoninsa, mutta kokonaisuus ei silloin ole yhden kiinteän transposition kaanon. Säekohtaiset transpositiot kirjataan asetuksiin ja MusicXML-/MIDI-merkintöihin. Muuten tasavertaisten ketjujen järjestyksessä suositaan pienempää äänten sävelkorkeusliikettä säerajojen yli. Tämä on pehmeä tasapistesääntö, ei uusi hylkäysperuste. Sisääntuloviive, sallittu O/I/R/RI-muoto ja rytmikerroin voivat vaihdella säkeittäin. Tila käyttää aina kahta ääntä ja kaanonhakua; tavalliset ääni- ja hakuvalinnat väistyvät tämän tilan ajaksi.

**Sallittuja harmoniavirheitä kussakin säkeessä** on säekohtainen raja. Jos raja on 2 ja säkeitä 4, kokonaisuuden virhesumma saa olla esimerkiksi 8, kun jokaisessa säkeessä on enintään 2 virhettä. Muut hyväksymissäännöt sekä mahdolliset sävelkorjaukset tarkastetaan kullekin säkeelle erikseen. Säerajojen välisiä melodisia hyppyjä ja äänenkuljetusliikkeitä ei tässä tilassa arvioida yhtenä jatkuvana kaanonina.

Säkeet yhdistetään yhdeksi MIDI- tai MusicXML-ratkaisuksi ilman ylimääräisiä välitaukoja, kun vastaääni mahtuu säejaksoon. Jos vastaäänen viive tai augmentaatio pidentää säettä jakson yli, seuraava säe aloitetaan vasta edellisen molempien äänten loputtua seuraavalta vapaalta tahtirajalta. Näin nuotit eivät mene saman stemman sisällä päällekkäin. Alkuperäisiä nuottikestoja ei lyhennetä. Ratkaisujen välinen vientitauko koskee kokonaisia säeketjuja.

Kaikille säkeille on löydyttävä ratkaisu valitun intervalliperiaatteen mukaan; keskeneräistä ketjua ei palauteta kokonaisena ratkaisuna. Kokonaisaika ja ehdokasraja jaetaan säehakujen kesken. Aikarajalla tyhjä tulos ei todista, ettei yhteistä transpositiota voisi löytyä. Yhdistelmiä pidetään rajattu, hyvyysjärjestyksessä valittu määrä: tämä on löytyneiden säeratkaisujen yhdistely, ei kaikkien mahdollisten ketjujen tyhjentävä optimointi. `phrase_search_complete` tarkoittaa säekohtaisten hakujen valmistumista valituilla rajoilla; `search_limit` kertoo kesken jääneistä hauista.

R-rajapinta: `kaanoni_phrase_chain(syote, bars=5, options=kaanoni_options(voices=2, mode='canon', max_harmony_errors=2), same_transposition=FALSE)` (TRUE pitää alkuintervallin yhteisenä). Tulos on tavallinen vientikelpoinen tulosolio; kunkin kokonaisuuden `phrases`-kenttä sisältää erillisten säkeiden ajat, asetukset ja arviot. MusicXML- ja MIDI-merkinnät näyttävät myös myöhempien säkeiden alut. Kaikki uudet käyttöliittymävalinnat tallentuvat asetuksiin.

## Päällekkäiset äänet viennissä

**Kirjoita samanaikaiset identtiset äänet vain kerran** poistaa ratkaisun kokonaiset unisonokahdennukset nuotinnoksesta. Valinta **Yhdistä myös samanaikaiset oktaavikahdennukset** tekee saman, kun säveljonojen ero on koko ajan sama määrä oktaaveja. Käyttöliittymässä molemmat ovat oletuksena käytössä; R-viennissä vastaavat parametrit ovat `merge_duplicates=TRUE` ja `merge_octaves=FALSE` (oktaavien yhdistäminen vaatii erillisen TRUE-arvon).

Verrataan koko äänen alkuaikoja, kestoja ja sävelkorkeuksia. Pelkkä sama teeman nimi tai osittainen samankaltaisuus ei riitä. Eri alkuajalla tuleva kaanonääni säilyy. Ensimmäinen vastaavista äänistä jää kirjoitettavaksi alkuperäiseen rekisteriinsä; jäljelle jäävät äänet numeroidaan peräkkäin. Vanhasta hakutuloksesta vientiin tehtävä pienempiääninen alivariantti säilyy. Uusi haku ei enää hyväksy unisono- tai oktaavikaksinnusta erillisen äänen paikalle. Jos samassa tiedostossa on myös aidosti neliäänisiä ratkaisuja, yhteisessä partituurissa voi edelleen olla neljä viivastoa, mutta kahdennettua ääntä ei kirjoiteta uudelleen.

Tämä muutos koskee MIDI- ja MusicXML-vientiä. Haku, hyvyysjärjestys sekä virhe- ja muutosluvut perustuvat edelleen alkuperäiseen ratkaisuun. Vientimerkintä näyttää kirjoitetun ja alkuperäisen äänimäärän sekä yhdistettyjen äänten kartan. RDS-raportissa alkuperäinen aineisto säilyy `notes`-kentässä ja kirjoitettava aineisto `export_notes`-kentässä. Valinnat tallentuvat asetuksiin.

## Hyvyysjärjestys ja parhaiden vienti

Kaikki hyväksyttävät ratkaisut järjestetään ennen näytettävän enimmäismäärän rajaamista. Vähemmän harmoniavirheitä on aina parempi. Sen jälkeen voi valita painotuksen:

- **Säilytä teema mahdollisimman tarkasti** (`ranking='balanced'`): vähemmän muutettuja lähdesäveliä, pienempi kokonaismuutos, suurempi kestopainotettu konsonanssi ja pidempi kaikkien äänten yhteissointi.
- **Suosi konsonoivaa yhteissointia** (`ranking='consonance'`): suurempi kestopainotettu konsonanssi, vähemmän lähdesävelmuutoksia, pienempi kokonaismuutos ja pidempi yhteissointi.
- **Suosi pitkää kaikkien äänten yhteissointia** (`ranking='overlap'`): pidempi yhteissointi, vähemmän lähdesävelmuutoksia, pienempi kokonaismuutos ja suurempi konsonanssi.

Kriteerit ovat tärkeysjärjestyksessä: aiempi kriteeri ratkaisee ennen seuraavaa. Painotus valitaan ennen hakua. Täysin tasavertaiset ratkaisut säilyttävät löytöjärjestyksensä. Hyvyys tarkoittaa näitä mitattavia ominaisuuksia; arvio ei ole yleinen sävellyksellisen laadun mittari.

**Näytä enintään parasta ratkaisua** rajaa hausta palautettavan listan. Viennin erillinen **Vie enintään näin monta parasta ratkaisua** (oletus 10) määrää MIDI-, MusicXML- ja raporttitulosteiden määrän listan alusta. Vientimäärää voi muuttaa ilman uutta hakua. R-rajapinnassa vastaava valinta on `kaanoni_write(tulos,polku,best_n=10)`. Jos ratkaisuja on vähemmän, viedään kaikki saatavilla olevat.

Ratkaisumäärään pysäyttävä nopea haku kannattaa jättää pois, kun halutaan vertailla laajempaa joukkoa. Parhaat tarkoittavat aina parhaimpia haussa löytyneiden joukosta, eivät todistetusti koko hakuavaruuden parhaita.

## Nopeutettu haku

Lähtöteemoja käsitellään vuorotellen noin 0,1 sekunnin / enintään 100 muunnoskokeilun erissä. Jokaisen teeman hakupino ja seuraava kokeiltava muunnos säilyvät vuorojen välillä. Kaikille teemoille annetaan vuoro, jos kokonaisaika tai ehdokasraja ei täyty jo ennen sitä.

Teemamuunnokset muodostetaan laiskasti ja säilytetään muistissa. Jo valittujen äänten yhteinen soiva aika tarkastetaan ennen harmoniaa: jos se on liian lyhyt, haara ei voi kelvata lisä-ääniäkään lisäämällä. Ääniparien harmonia- ja äänenkuljetustulokset tallennetaan välimuistiin, ja uusia ääniä liitetään vain yhteisen harmoniavirhebudjetin sallimissa rajoissa. Lopullinen kokonaisuus käy aina läpi alkuperäisen kokonaisarvioinnin, mukaan lukien sointujoukkorajoitukset ja yhteissointien lukumäärä. Sävelkorjaushaussa parien alkuperäisiä virheitä ei käytetä ehdottomaan karsintaan, koska ne voivat korjaantua.

Valintaruutu **Pysäytä, kun enimmäismäärä ratkaisuja on löytynyt** (`stop_after_results=TRUE`) lopettaa haun heti ratkaisumäärän täyttyessä. Tila on tällöin `result_limit`. Oletuksena valinta on pois päältä, jolloin haku käyttää annetut rajat ja järjestää löydetyt ratkaisut kuten aiemmin. Nopea pysäytys ei lupaa löytää koko hakuavaruuden parhaita ratkaisuja.

Tulosolion `stats` kertoo käydyt lähtöteemat, muunnos- ja parivälimuistien osumat sekä aikakarsintojen määrän. Automaattinen vertailutesti varmistaa pienissä 2–4-äänisissä hauissa samat ratkaisut kuin kaikkien kokonaisten yhdistelmien suorassa läpikäynnissä.

## Haun seuranta

Oletusaikaraja on 240 sekuntia. Haun aikana päivittyvä Shiny-mittari näyttää vaiheen (teemayhdistelmät, sävelkorjaukset tai järjestäminen), kuluneen ajan, aikarajan, lähtöteeman numeron, tutkittujen kokonaisten ehdokkaiden määrän ja löytyneet ratkaisut. Korjausvaiheessa näytetään myös käsiteltyjen korjausehdokkaiden määrä. Palkki kuvaa käytettyä aikabudjettia; se ei ole hakupuun valmistumisprosentti eikä arvio jäljellä olevasta työajasta. Haku voi valmistua ennen aikarajaa.

R-rajapinnassa `kaanoni_generate(..., progress=function(p) {...})` välittää samat tiedot ilman Shiny-riippuvuutta. Päivityksiä rajoitetaan noin viiteen sekunnissa vaiheenvaihtojen lisäksi.

## Asetusten säilyminen

Kaikki haku- ja vientiasetukset tallentuvat automaattisesti projektikansion `kaanoni-asetukset.json`-tiedostoon heti valintojen muuttuessa. Tallennus ei vaadi hakua eikä ohjelman hallittua sulkemista. Asetukset palautuvat myös RStudion tai tietokoneen uudelleenkäynnistyksen jälkeen. Viimeisin muutettu asetusvalikoima on yhteinen tämän projektin ikkunoille. Nuottitiedosto valitaan uudelleen; hakutuloksia ei palauteta automaattisesti.

Jos asetustiedosto puuttuu tai on vioittunut, sovellus käynnistyy oletuksilla.

## Käyttö R-funktioilla

```r
source('R/kaanoni.R')
syote <- kaanoni_read('/polku/teemat.musicxml')
teemat <- kaanoni_themes(syote, bars=4)
# Yksi pitkä melodia kokonaisuudessaan: bars=NULL.
# Valitse tarvittaessa syötteen stemmat: kaanoni_read(polku, voices=c(1,2)).

asetukset <- kaanoni_options(
  voices=3, mode='canon', forms=c('I','R','RI'),
  factors=c(0.5,1,2), delays=seq(1,8,by=0.5),
  transpositions=c(-12,-7,0,7,12),
  repair=TRUE, repair_mode='strict', max_shift=2,
  max_changes=4, repair_nodes=500,
  max_harmony_errors=0, max_results=10, seconds=30
)
tulos <- kaanoni_generate(teemat,asetukset)
print(tulos)
if(length(tulos$results)) {
  kaanoni_write(tulos,'tulokset.musicxml',gap_bars=1,time=c(4,4),tempo=60)
  kaanoni_write(tulos,'tulokset.mid')
}
```

`examples/demo.R` muodostaa pienen esimerkin ilman syötetiedostoa. Suorita `Rscript examples/demo.R`. Esimerkkitulosteet ovat `examples/`-kansiossa.

## Musiikilliset periaatteet

- Aika mitataan neljäsosina; lähtötiedoston tempo ei vaikuta rytmien suhteisiin. MIDI-viennin tarkkuus on 9600 ticksiä neljäsosaa kohti.
- Kiinteä teemajako käyttää nuotinnoksen todellisia tahtirajoja. Esimerkiksi neljän tahdin teemat alkavat tahdeista 1, 5, 9 jne. Lopputauot säilyvät teemajakson pituudessa. Kokonaan tyhjä jakso ohitetaan. Teemarajan ylittävä nuotti katkaistaan rajan kohdalta.
- Yhden teeman tilassa koko valitun stemman ajallinen kehys säilyy. Monistemmaisessa syötteessä jokainen stemma muodostaa oman teemansa.
- I on kromaattinen inversio ensimmäisen sävelen ympäri. Akseli pysyy korjauksessa kiinteänä. R peilaa nuotit kestoineen koko teemajakson sisällä, jolloin lopputauko siirtyy alkuun. RI yhdistää nämä.
- Ensimmäinen ääni käyttää alkuperäistä teemaa alkuperäisellä korkeudella ja rytmillä. Muiden äänten parametrit ovat suhteessa siihen. Kaanonissa kaikki äänet ovat saman lähdeteeman johdannaisia; kontrapunktihaussa niitä voidaan valita eri teemoista.
- Saman säveljonon samanaikaiset unisono- ja oktaavikaksinnukset katsotaan yhdeksi teemaksi. Identtiset alkuajat ja kestot sekä koko ajan vakio, 12:lla jaollinen sävelkorkeusero tunnistetaan riippumatta teeman nimestä tai muunnostunnuksesta. Kaksinnus ei täytä erillisen äänen paikkaa: neliäänisessä haussa vaaditaan neljä näin erotettavaa ääntä. Eri alkuajalla tuleva kaanonääni säilyy erillisenä. Sama sääntö tarkastetaan myös korjausten jälkeen, eikä harmoniavirheiden salliminen ohita sitä. Oktaavitranspositiot valitaan eksplisiittisesti transpositiolistasta; ohjelma ei siirrä ääniä automaattisesti rekisteriin.
- Harmonia tarkastetaan kaikissa alkamis- ja päättymiskohdissa. Nuotti soi aikavälillä `[alku, loppu)`. Jokainen äänipari tarkastetaan.
- Oletuskonsonanssit ovat intervalliluokat 0, 3, 4, 7, 8 ja 9. Neljäsosametrin heikot loma- ja sivusävelet voidaan sallia R-rajapinnan `dissonances=TRUE`-valinnalla: asteittainen liike, konsonoivat päätepisteet ja liikkeen ajan paikallaan pysyvä toinen ääni vaaditaan. Pidätyssääntöjä ei ole vielä toteutettu.
- Käyttöliittymän **Salli epäiskujen intervallit asteikkoliikkeessä** (`allow_weak_steps=TRUE`) hyväksyy dissonoivan intervallin, kun jommankumman äänen kyseinen nuotti on yhtenäisen nousevan tai laskevan asteikkoliikkeen keskellä: tulo ja lähtö ovat kumpikin 1–2 puolisävelaskelta samaan suuntaan ilman taukoja. Intervallin lajia, toisen äänen paikallaan pysymistä tai ympäröivien intervallien konsonanssia ei tällöin vaadita. Sääntö toimii kaikissa äänipareissa 2–4-äänisessä satsissa, eikä hyväksyttyä dissonanssia lasketa harmoniavirheeksi.
- Epäiskut määritetään oletuksena automaattisesti (`metric_mode='auto'`) ensimmäisen äänen lähdeteeman tahtilajeista ja tahtirajoista. 3/4-, 3/8- ja muissa kolmen yksikön tahdeissa toinen ja kolmas isku ovat heikkoja; 4/4-tahdissa toinen ja neljäs isku ovat heikkoja. 6/8- ja 12/8-tahtien vahvat iskut ovat pisteellisten neljäsosien aluissa; 9/8:ssa ensimmäinen pääisku on vahva ja kaksi seuraavaa heikkoja. Alijaot ovat heikkoja. Teemajaossa metritiedot kohdistetaan teeman alkuun, vajaa alkutahti käsitellään kohotahtina ja tahtilajinvaihdokset huomioidaan. Jos satsi jatkuu lähdeteeman tahtikartan loppua pidemmälle, viimeistä tahtilajia jatketaan.
- Valinnalla `metric_mode='manual'` voi yhä määrittää oman vahvojen iskujen välin `strong_beat_period`-arvolla neljäsosina ensimmäisen äänen alusta. Arvo 2 vastaa 4/4-tahdin iskuja 1 ja 3; arvo 3 vastaa 3/4-tahdin ensimmäisiä iskuja. Jos suoraan `kaanoni_evaluate`-funktiota kutsuttaessa ei anneta `meter`-tahtikarttaa, käytetään tätä säännöllistä ruudukkoa. Epäiskun nuotti ei saa jatkua seuraavan vahvan iskun yli. Sivusäveliä, hyppyjä tai tauon katkaisemaa liikettä sääntö ei hyväksy.
- Äänenkuljetusrajoitukset (esimerkiksi rinnakkaiskvintit) ja mahdollinen koko sävelluokkajoukon `allowed_sets`-rajaus ovat edelleen voimassa. Optio koskee intervallien harmoniavirheitä; se ei kytke näitä erillisiä rajoja pois.
- Rinnakkaiset kvintit ja oktaavit tunnistetaan samansuuntaisesta liikkeestä. Edellinen soiva intervalli muistetaan ääniparikohtaisesti myös nuottien päättymistapahtumien yli. MIDI-artikulaation lyhyet katkot eivät siten nollaa kieltoa: oletustoleranssi on 0,125 neljäsosaa (`parallel_gap_tolerance` R-rajapinnassa). Pidempi tauko katkaisee vertailun. Tämä tunnistus on erillinen epäiskudissonanssien hyväksymisestä, eikä harmoniavirheiden salliminen ohita rinnakkaiskieltoa. Rekisteri, suurin hyppy ja valinnainen risteämiskielto ovat ehdottomia sääntöjä. Risteämiskiellossa ääni 1 on ylin.
- `min_sonorities` määrittää yhteissointien vähimmäislukumäärän. Oletus 0 poistaa lukumäärärajan. Kaksiäänisessä lasketaan eri soivat nuottiparit, myös uusi nuotti aiemmin alkaneen, edelleen soivan nuotin kanssa. Pitkä muuttumaton sointi lasketaan kerran. Saman sävelkorkeuden uudelleen soittaminen on uusi nuotti ja muodostaa uuden parin.
- Kolmi- ja neliäänisessä `sonority_mode='intervals'` laskee kaikki eri nuottiparit, myös osittaisen yhteissoinnin aikana. `sonority_mode='chords'` laskee tilanteet, joissa kaikki valitun äänimäärän äänet soivat: kolmiäänisessä kolme, neliäänisessä neljä. Kunkin nuottiyhdistelmän lasketaan esiintyvän kerran; pelkkä sävelen päättyminen ei lisää jo lasketun parin määrää. Neljän äänen yhtäaikainen uusi sointu tuottaa yhden sointitapahtuman tai kuusi intervalliparia valinnan mukaan. Luku kertoo nuottiyhdistelmien määrän, ei erilaisten intervalliluokkien määrää. Harmonian hyväksyttävyys ja sallittu virheraja tarkastetaan edelleen erikseen.
- Sekä lukumäärärajan että kestorajan on täytyttävä. Lukumäärää ei yritetä kasvattaa sävelkorkeuksia korjaamalla, koska korjaukset eivät muuta rytmiä.
- `min_overlap` vaatii, että kaikki valitut äänet soivat samanaikaisesti vähintään annetun ajan. Tauoilla ei voi kiertää tätä vaatimusta.
- R-rajapinnassa `allowed_sets` rajaa koko soivan harmonian. Avaimet muodostetaan `kaanoni_pc_key(c(60,64,67))`-funktiolla. Luokitus on transposition suhteen sama; se ei samaista inversioita eikä käytä vanhan ohjelman FNID-tunnuksia. Rajaus koskee myös kahden sävelluokan harmonioita.
- Yhtenäinen sama kielletty intervalli samassa ääniparissa on yksi harmoniavirhe. Eri ääniparien rikkomukset lasketaan erikseen. Myös mahdollinen kielletty kokonaisjoukko lasketaan erikseen. Raportti näyttää tyypin, ajan, äänet ja nuottirivit.

## Korjausten toiminta ja haun rajat

Tiukassa korjauksessa lähdeteeman samaa säveltä muutetaan kaikissa sen johdannaisissa. Inversion muutossuunta kääntyy. Rytmit, kestot ja sisääntulot pysyvät samoina. Vapaassa tilassa äänten nuotteja voi muuttaa erikseen; muutettu kaanon merkitään `modified_counterpoint`-tyyppiseksi.

Haku tarkastaa ensin muunnosvaihtoehdot ilman sävelkorjauksia. Korjaustilassa tähän varataan enintään 60 % kokonaisajasta. Sen jälkeen korjattavia ehdokkaita käsitellään pienissä vuorottelevissa erissä: hakutila säilytetään erien välillä. Ensimmäinen vaikea ehdokas ei näin kuluta koko haun aikaa. Tilasto kertoo löydettyjen ratkaisujen kokonaismäärän myös silloin, kun palautettujen tulosten enimmäismäärä rajaa listaa.

Korjaushaku laajentaa ongelmakohtiin osallistuvien sävelten muutoksia yhden puolisävelaskelen askelin molempiin suuntiin. Kaikki muutoksen vaikutukset arvioidaan uudelleen. Tilojen järjestys suosii ensin ehdottomien rikkomusten poistamista, sitten harmoniavirherajaa, muutettujen lähdesävelten määrää, kokonaismuutosta ja suurinta muutosta.

**Korjaus on heuristinen:** ensimmäinen löydetty hyväksyttävä korjaus palautetaan ehdokasta kohti. Se ei todista pienintä mahdollista korjausta, eikä kaikkia välivaiheessa virheettömiksi muuttuneita nuotteja enää laajenneta. `max_shift`, `max_changes`, `repair_nodes` ja yhteinen aikaraja rajoittavat hakua. Muutosehdokkaiden laajennus ja yhden tilan arviointi voivat ylittää aikarajan hieman.

Haun tila on:

- `exhaustive`: valitut geometriset vaihtoehdot on käsitelty ilman korjaushakua.
- `geometry_exhausted_heuristic_repair`: muunnosvaihtoehdot on käsitelty, mutta korjaukset olivat heuristisia.
- `search_limit`: aikaraja tai kokonaisien ehdokkaiden raja pysäytti haun.

Tyhjä tulos aikarajalla tai heuristisessa korjaushaussa **ei todista ratkaisun mahdottomuutta**. Hakujärjestys voi vaikuttaa aikarajalla löydettyihin tuloksiin.

Tulokset järjestetään harmoniavirheiden määrän, muutettujen lähdesävelten määrän, kokonaismuutoksen ja lopuksi konsonanssin perusteella. Konsonanssi tarkoittaa tässä kestopainotettua hyväksyttyjen intervalliparien osuutta. Se ei ole vanhan ohjelman HUCV-mittari. Saman nuottiaineiston kaksoiskappaleet poistetaan; erillistä vaihtoehtojen monipuolisuusoptimointia ei vielä ole.

## Tiedostomuotojen rajaukset

MusicXML-luku tukee partwise-muotoa, tahtilajinvaihdoksia, taukoja, moniäänisen notaation erillisiä stemmoja, sidoksia, kohotahteja ja transponoivien instrumenttien soivia sävelkorkeuksia. Kussakin valitussa melodiastemmassa saa soida vain yksi sävel kerrallaan. Eri osien tahtirajojen pitää vastata toisiaan. Korunuotit ja mikrointervallit hylätään selkeällä ilmoituksella.

MIDI-luku tukee formaatteja 0/1 ja nuottipohjaista PPQ-aikaa. Sustain-pedaalin käyttö hylätään, koska se muuttaisi soivien sävelten kestoja. Ohjelma ei tulkitse pitch bend -viestejä, sävelasteikkoja tai MIDI-instrumenttien sointivärejä. Syötteen on oltava sävelkorkeuksiltaan tavallista 12-sävelistä melodia-aineistoa.

MusicXML-vienti kirjoittaa enintään neljä yksiviivastoista osaa, tauot ja tahtien yli sidotut nuotit sekä ratkaisujen kuvaukset. Jokaisen ratkaisun alussa ylimmällä viivastolla on harjoitusmerkki (esim. Ratkaisu 1) ja samat generointitiedot kuin MIDI-merkkitekstissä: muutosmäärät, virheet ja kaikkien äänten teema/muoto/rytmikerroin/viive/transpositio. Sävelten enharmoniset kirjoitusasut, dynamiikat, artikulaatiot ja alkuperäinen sivuasettelu eivät vielä säily. Vienti käyttää valittua kiinteää tahtilajia ja tempoa. Nuottien kestot kirjoitetaan täsmällisinä MusicXML-kestoina; vaativan rytmin graafinen ryhmittely jää nuotinnusohjelmalle.

R-rajapinnan vienti tallentaa lisäksi tekstiraportin ja koko tulosolion `.rds`-muodossa. Seuraava ratkaisu alkaa tahtirajalta viimeisen soivan nuotin päättymisen ja valitun tauon jälkeen.

## Tarkistukset

```sh
Rscript tests/test_kaanoni.R
Rscript tests/test_korjaukset.R
Rscript tests/test_app.R
Rscript tests/test_search_fairness.R
Rscript tests/test_settings.R
Rscript tests/test_sonorities.R
Rscript tests/test_weak_steps.R
python3 tests/test_python_compat.py
Rscript tests/test_progress.R
Rscript tests/test_search_optimized.R
Rscript tests/test_ranking.R
Rscript tests/test_export_voices.R
Rscript tests/test_octave_rule.R
Rscript tests/test_phrase_chain.R
Rscript tests/test_parallel_gaps.R
```

Testit kattavat teemajaon ja lopputauot, O/I/R/RI-muunnokset, rytmikertoimet, harmoniaa ja rinnakkaisliikettä, tiukan kaanonin korjauksen, 2–4 ääntä sekä MIDI- ja MusicXML-takaisinluvun. Automaattiset testit eivät vielä korvaa MusicXML:n ulkoista nuotinnusohjelmassa tehtävää visuaalista tarkistusta.
