##############################
###                        ###
### KONTRAPUNKTI-ALGORITMI ###
###                        ###
##############################

# OutputNimi="Teemoja_260624" ; system(paste("/Applications/midicsv-1.1/midicsv /Users/attetenkanen/VIRRET/",OutputNimi,".mid"," /Users/attetenkanen/VIRRET/",OutputNimi,".csv",sep=""))

# for(OutputNimi in "TonaalisetSoinnut_ja_Uudet_soinnut_in_C_121119g") system(paste("/Applications/midicsv-1.1/midicsv /Users/attetenkanen/SAVELLYS/SOINTUJA/",OutputNimi,".mid"," /Users/attetenkanen/SAVELLYS/SOINTUJA/",OutputNimi,".csv",sep=""))
# 
# source("/Users/attetenkanen/Skriptit/MusicTheoryScriptsForR.R"); library(beepr); library(sna); library(colorspace); library(gtools)
options(max.print=99999)
options(scipen = 999) # Ei tieteellisia numeromerkintoja kuten "8E+05".

###############
# PARAMETREJA #
###############

# Teemaan liittyvia ja kasin valittavia kiinteita vakioita. 

TEEMATAHTIMAARA=4 # TAHTEJA NUOTTIRIVILLA!
YHTEISIA_SAVELIA=4 # KONTRAPUNKTIEN HAKUUN, SAADA TATA!!!
TICKSEJATAHDISSA=8*512
TAHTILAJI=4 # funktio "nuotinnos"
OsoittajaTL=4; NimittajaTL=4 # funktio "nuotinnos2"
YHTEISIA_SAVELLUOKKIA=12 # Kontrapunkteissa hyvaksyttava savelluokkien kokonaismaara.
Sallittuja_3_1_joukkoja=0
SallittujenVirheidenMaara=0
Testataanko_3_1_melodiassa=FALSE
Tehdaan_vain_Kaanoneita=FALSE # 
Tehdaan_konsonanssisuusjarjestys=TRUE
TEHDAAN_INVERSIOTEEMAT=TRUE
TRANSPS=c(0:11)
DIRECTORY_VIRRET="/Users/attetenkanen/VIRRET/"
DIRECTORY_GENEROIDUT="/Users/attetenkanen/SAVELLYS/GENEROIDUT/"
kpja_4x_max=30 # Kuinka monta 4x kp:n maksimissaan nuotinnetaan.

KaikkiLahtoTeemat=TRUE
KaikkiVastaAiheet=TRUE

if(KaikkiLahtoTeemat==FALSE)
{
	LahtoteemojenNumerot=c(1,3,4,8,11,12,13,15)
}

if(KaikkiVastaAiheet==FALSE)
{
	Vasta_aiheidenNumerot=c(1,3,4,8,11,12,13,15)
}

# Teemat
OutputNimi="Teemoja_260624" ; system(paste("/Applications/midicsv-1.1/midicsv /Users/attetenkanen/VIRRET/",OutputNimi,".mid"," /Users/attetenkanen/VIRRET/",OutputNimi,".csv",sep=""))
Compo=readmidicsvfile_Dorico(DIRECTORY_VIRRET, paste(OutputNimi,".csv",sep="")) # Tahan voisi vain LISATA rivin savelluokat!

# AUGMENTAATIOKERTOIMET:
#           1 2 3 4 5 6 7 8 9 101112131415161718192021222324252627282930313233343536373839404142434445
Osoittaja=c(4,3,2,1,5,3,1,8,5,4,2,1,3,1,6,4,3,2,1,5,3)
Nimittaja=c(1,1,1,1,2,2,2,3,3,3,3,3,4,4,5,5,5,5,5,8,8)
# length 45
Kertoimet=rep(0,length(Osoittaja))
KertoimetNimet=rep(NA,length(Osoittaja))

for(i in 1:length(Osoittaja))
{
  Kertoimet[i]=Osoittaja[i]/Nimittaja[i]
  KertoimetNimet[i]=paste(Osoittaja[i],Nimittaja[i],sep="")
}

ValitutAugmentaatiotAbs=Kertoimet[c(3:4)] #c(4,7,8)

TSHIFT2=TSHIFT1=seq(0, TEEMATAHTIMAARA*TICKSEJATAHDISSA, by=2*512) # Rajat teemojen max-pituuden mukaan, esim. 6 4/4-tahtia on 6*4096 tai vahemman.
TSHIFT1=TSHIFT1[TSHIFT1<=(8*1024)] # Rajoitetaan siirtymaa, vain tiiviit kp:t mukaan.

Hyvaksyttyja=0

HUCVs=c()
HUCVs[1]=2
for(i in 2:69)
{
	HUCVs[i]=hucv(i)
}

ConsonantSETs=which(HUCVs>=0.4)

Hyvaksyttyja=0

POSSIBLE_SETS=unique(as.numeric(c(
	subsets("6-34B",4)[,3],
	subsets("6-34B",5)[,3],
	subsets("6-34B",6)[,3],
	subsets("5-21A",3)[,3],
	subsets("5-21A",4)[,3],
	subsets("6-Z28",4)[,3],
	subsets("6-Z28",5)[,3],
	subsets("6-Z28",6)[,3])))

POSSIBLE_SETS2=unique(as.numeric(c(
	subsets("6-20",2)[,3],
	subsets("6-20",3)[,3],
	subsets("6-20",4)[,3],
	subsets("6-20",5)[,3],
	subsets("6-20",6)[,3])))

HUCVs=c()
for(i in 1:length(POSSIBLE_SETS))
{
	HUCVs[i]=hucv(POSSIBLE_SETS[i])
}

HUCVs2=c()
for(i in 1:length(POSSIBLE_SETS2))
{
	HUCVs2[i]=hucv(POSSIBLE_SETS2[i])
}

# Kaikkein konsonoivimmat joukot:

HUCVs_cons=rep(0,69)

for(i in 2:69)
{
	HUCVs_cons[i]=hucv(i)
}

Order_Cons_Sets=rev(order(HUCVs_cons))

# Fnid1=13, mollin septimipidätys 0,4,11 -> 0,4,9*, * on liikkuva savelluokka
# Fnid2=18, duurin septimipidätys 0,3,10 -> 0,3,9* tai 0,3,8*
# Fnid3=22, mollin kvarttipidätys 0,5,7 -> 0,3*,7
# Fnid3=22, duurin kvarttipidätys 0,5,7 -> 0,4*,7

#SallitutJoukot_4x_Kp=unique(c(1,POSSIBLE_SETS2[which(HUCVs2>=0.1)])) # Vain 6-Z28 -osajoukot.
# SallitutJoukot_4x_Kp=unique(c(c(1,4:6,19,24:26,64:67),POSSIBLE_SETS2[which(HUCVs>=0.0)]))
SallitutJoukot_4x_Kp=SallitutJoukot_4x_Kp2=c(1,4:6,19,24:26,64:67) # 24=mollisointu, 25=duurisointu
# SallitutJoukot_4x_Kp=c(1,Order_Cons_Sets[1:16])
# SallitutJoukot_4x_Kp=as.vector(na.omit(SallitutJoukot_4x_Kp))

Teematiedot_4x_Kp_Taulukko=c()

###############################
###### Lisaa parametreja ######
###############################
Teemanro=1
i10=1

PVM=format(Sys.time(), "%a_%b_%d_%H%M_%Y")

GenerointiNro=0
Kombinaatiot=c()

Pitches_TOT_All=c()
Alkuticks_TOT_All=c()
Durations_TOT_All=c()
Velocities_TOT_All=c()

Pitches_TOT_All_CompoA=c()
Alkuticks_TOT_All_CompoA=c()
Durations_TOT_All_CompoA=c()
Velocities_TOT_All_CompoA=c()

Pitches_TOT_All_CompoB=c()
Alkuticks_TOT_All_CompoB=c()
Durations_TOT_All_CompoB=c()
Velocities_TOT_All_CompoB=c()

Kaytetyt_Teemat=c()
ExtraTicks=0
LahtoT_A=0
OtetaanSavellykseen=0
Hyvaksyttyja=0
Kaikki_KP_tiedot=c()
VERSIO=1
LahtoteemaNro=1
CompoMatInd=0
CompoMatInd0=1
AUGMENTAATIO=1
CompoMatLahtoteemat=list()
CompoMatKaikki=list()
EdLahtoTeemaA=1 # Ensimmaisen lahtoteeman nro.

KV=5; GoodPitchIntervals=c(1,0,0,1,1,1,0,1,1,1,0,0); INT=-12 # Jos yla-aanelle luodaan bassoa, 5 on hyva.

################################
##     Teemat listassa        ##
################################

PVM=format(Sys.time(), "%a_%b_%d_%H%M_%Y")

KaikkiTeemat=list() # Ks. miten kaytetty ...
KaikkiTeemat2=list() # Ks. miten kaytetty ...

TeemojaLKM=ceiling(max(Compo[,1])/TEEMATAHTIMAARA)
Laskuri_i1=0

for(i1 in 0:(TeemojaLKM-1))
{
	TeemanRivit=which(Compo[,1]>=((i1*TEEMATAHTIMAARA)+1) & Compo[,1]<=((i1+1)*TEEMATAHTIMAARA))
	KaikkiTeemat[[i1+1]]=Compo[TeemanRivit,]
	TicksLisays=KaikkiTeemat[[i1+1]][1,2]%%1024
	KaikkiTeemat[[i1+1]][,2]=KaikkiTeemat[[i1+1]][,2]-KaikkiTeemat[[i1+1]][1,2]+TicksLisays
	
	TeemanKeskikorkeus=round(mean(KaikkiTeemat[[i1+1]][,5]))
	InversionSavelet=2*TeemanKeskikorkeus-KaikkiTeemat[[i1+1]][,5]
	KaikkiTeemat[[i1+2]]=KaikkiTeemat[[i1+1]]
	KaikkiTeemat[[i1+2]][,5]=InversionSavelet
	Laskuri_i1=Laskuri_i1+1
	KaikkiTeemat2[[Laskuri_i1]]=KaikkiTeemat[[i1+1]]
	KaikkiTeemat2[[Laskuri_i1+TeemojaLKM]]=KaikkiTeemat[[i1+2]] # Inversioteemat listan loppupuolelle.
}

if(TEHDAAN_INVERSIOTEEMAT==TRUE)
{
	KaikkiTeemat=KaikkiTeemat2
} else KaikkiTeemat=KaikkiTeemat[-length(KaikkiTeemat)]

if(KaikkiLahtoTeemat==TRUE)
{
	LahtoteemojenNumerot=c(1:length(KaikkiTeemat))
}

if(KaikkiVastaAiheet==TRUE)
{
	Vasta_aiheidenNumerot=c(1:length(KaikkiTeemat)) 
}


####################################################################################
# TimeShift-Augmentaatio-Transpositio-taulukko, parametrit viittaavat vasta-aiheen #
# ominaisuuksiin. Mahdollistetaan kaikki kombinaatiot:                             #
####################################################################################

if(TEHDAAN_INVERSIOTEEMAT==TRUE & KaikkiLahtoTeemat==FALSE)
{
	LenKaikkiTeemat=length(KaikkiTeemat)
	LahtoteemojenMaara=LenKaikkiTeemat/2
	LahtoteemojenNumerot2=c(LahtoteemojenNumerot,LahtoteemojenMaara+LahtoteemojenNumerot)
	Vasta_aiheidenNumerot2=c(Vasta_aiheidenNumerot,LahtoteemojenMaara+Vasta_aiheidenNumerot)
	LahtoteemojenNumerot=LahtoteemojenNumerot2
	Vasta_aiheidenNumerot=Vasta_aiheidenNumerot2
}

LenLAHTOTEEMAT=length(LahtoteemojenNumerot)
LenVASTA_AIHEET=length(Vasta_aiheidenNumerot)
LenTSHIFT1=length(TSHIFT1)
LenTRANSPS=length(TRANSPS)
LenValitutAugmentaatiotAbs=length(ValitutAugmentaatiotAbs)
LenKaikkiYhdistelmat=LenLAHTOTEEMAT*LenVASTA_AIHEET*LenTSHIFT1*LenTRANSPS*LenValitutAugmentaatiotAbs
LenKaikkiYhdistelmatTod=LenKaikkiYhdistelmat
AugmTshiftTransp_Taulukko=array(NA,dim=c(LenKaikkiYhdistelmat,5))
RiviLaskuri1=0

for(Lahtoteema1 in LahtoteemojenNumerot)
{
	for(Vasta_aihe1 in Vasta_aiheidenNumerot)
	{
		for(Augm1 in ValitutAugmentaatiotAbs)
		{
			for(Tshift1 in TSHIFT1)
			{
				for(Transp1 in TRANSPS)
				{
					RiviLaskuri1=RiviLaskuri1+1
					AugmTshiftTransp_Taulukko[RiviLaskuri1,]=c(Lahtoteema1,Vasta_aihe1,Augm1,Tshift1,Transp1)
				}
			}
		}
	}
}

# Jos tehdään vain kaanoneita.:
if(Tehdaan_vain_Kaanoneita==TRUE)
{
	# Inversiot mukaan: "... | abs(...)
	AugmTshiftTransp_Taulukko_KaanonRivit=which((AugmTshiftTransp_Taulukko[,1]-AugmTshiftTransp_Taulukko[,2])==0 | abs(AugmTshiftTransp_Taulukko[,1]-AugmTshiftTransp_Taulukko[,2])==TeemojaLKM)
	AugmTshiftTransp_Taulukko=AugmTshiftTransp_Taulukko[AugmTshiftTransp_Taulukko_KaanonRivit,]
	LenKaikkiYhdistelmatTod=length(AugmTshiftTransp_Taulukko[,1])
	LenKaikkiYhdistelmat=LenKaikkiYhdistelmatTod
}

########################
# VARSINAINEN LASKENTA #
########################

Kierroksia=0
#beep(sound = 1, expr = NULL)
LahtoteemaNro_Vasta_aiheNro1=c(1,1)
LahtoteemaNro_Vasta_aiheNro2=c(1,1)

for(i_Kaikki in 1:LenKaikkiYhdistelmat) # KAYDAAN LAPI KAIKKI TEEMAKOMBINAATIOT!
{
	LahtoteemaNro_Vasta_aiheNro2=LahtoteemaNro_Vasta_aiheNro1
	Kierroksia=Kierroksia+1
	
	VertailuA=LahtoteemaNro
	
	LahtoteemaNro=AugmTshiftTransp_Taulukko[i_Kaikki,1]
	if(LahtoteemaNro!=VertailuA) {Kombinaatiot=c(); CompoMatInd=0}
	
	CompoA=KaikkiTeemat[[LahtoteemaNro]]
	#Lisaticksit_alusta=CompoA[1,2]
	if(length(CompoA[,2])<YHTEISIA_SAVELIA){next}# Jos A-savelma liian lyhyt, ei tehda.
	SavelluokatA=unique(CompoA[,5]%%12)
	SavelluokatAMaara=length(SavelluokatA)
	if(SavelluokatAMaara>YHTEISIA_SAVELLUOKKIA){next}
	
	Vasta_aiheNro=AugmTshiftTransp_Taulukko[i_Kaikki,2] 
	LahtoTeemaB=Vasta_aiheNro
	CompoB=KaikkiTeemat[[Vasta_aiheNro]]
	if(length(CompoB[,2])<YHTEISIA_SAVELIA){next}# Jos B-savelma liian lyhyt, ei tehda.
	
	LahtoteemaNro_Vasta_aiheNro1=c(LahtoteemaNro,Vasta_aiheNro)
	if(identical(LahtoteemaNro_Vasta_aiheNro1, LahtoteemaNro_Vasta_aiheNro2)==FALSE) {Kombinaatiot=c()} # Valtetaan pitkaa Kombinaatioiden muistilistaa.
	
	AUGMENTAATIO=AugmTshiftTransp_Taulukko[i_Kaikki,3]
	TRANSP=AugmTshiftTransp_Taulukko[i_Kaikki,5]
	AIKASIIRTYMA=AugmTshiftTransp_Taulukko[i_Kaikki,4]
	
	# JOS EI HALUTA SAMOJA MELODIAKOMBINAATIOITA UUDESTAAN: 
	
	Kombinaatio=as.character(paste(LahtoteemaNro, Vasta_aiheNro, VERSIO, "_", round(AUGMENTAATIO,digits=2),"_", AIKASIIRTYMA, TRANSP, sep=""))
	if(is.element(Kombinaatio, Kombinaatiot)){next} # print("Tuli sama kombinaatio!: "); print(Kombinaatio); Klikkaa talla paalle ja pois.
	Kombinaatiot=c(Kombinaatiot, Kombinaatio) # VOISIKO TEHDA TOISIN?
	
	CompoAugm=CompoB; CompoAugm[,c(2,3)]=CompoB[,c(2,3)]*AUGMENTAATIO
	if(AIKASIIRTYMA==0 & LahtoteemaNro==Vasta_aiheNro & AUGMENTAATIO==1){next} # EI rinnakkaisia kontrapunkteja.
	Compo3=CompoAugm # Compo3:n alkamisaikaa ja transpositiota tullaan muuttamaan.
	Compo3[,2]=CompoAugm[,2]+AIKASIIRTYMA
	
	Compo3[,5]=CompoAugm[,5]+TRANSP+INT
	SavelluokatB=unique(Compo3[,5]%%12)
	SavelluokatBMaara=length(SavelluokatB)
	if(SavelluokatBMaara>YHTEISIA_SAVELLUOKKIA)
	{
		print(paste("Vasta-aiheessa nro", Vasta_aiheNro,"yli", YHTEISIA_SAVELLUOKKIA,"savelluokkaa")); next
	}
	
	YhteisetSavelluokat=unique(c(SavelluokatA, SavelluokatB))
	YhteisetSavelluokatMaara=length(YhteisetSavelluokat)
	if(YhteisetSavelluokatMaara>YHTEISIA_SAVELLUOKKIA){next}
	
	# Merkitaan yhden (1) ticksin paassa olevat samaksi. Esim. trioleissa 341=342...
	CommonTicksXtraA=intersect(CompoA[,2]+1,Compo3[,2])
	CommonTicksXtra3=intersect(CompoA[,2],Compo3[,2]+1)
	if(length(CommonTicksXtraA)>0)
	{
		for(i in 1:length(CommonTicksXtraA))
		{
			CompoA[which(CompoA[,2]==(CommonTicksXtraA[i]-1)),2]=CompoA[which(CompoA[,2]==(CommonTicksXtraA[i]-1)),2]+1
		}						
	}
	if(length(CommonTicksXtra3)>0)
	{
		for(j in 1:length(CommonTicksXtra3))
		{
			Compo3[which(Compo3[,2]==(CommonTicksXtra3[j]-1)),2]=Compo3[which(Compo3[,2]==(CommonTicksXtra3[j]-1)),2]+1
		}						
	}
	CommonTicks=intersect(CompoA[,2],Compo3[,2])
	YhtAikSoivSavMaara=length(CommonTicks)
	if(YhtAikSoivSavMaara<(YHTEISIA_SAVELIA)){next}# Jos ei tarpeeksi yhteisia tickseja, palataan ed. looppiin.
	
	Pitches_TOT=c()# NAMA POIS PAALTA, JOS TEHDAAN KOKO SATSISTA VAIN YKSI NUOTTI
	Alkuticks_TOT=c()
	Durations_TOT=c()
	Velocities_TOT=c()
	
	Pitches_TOT_CompoA=c()# NAMA POIS PAALTA, JOS TEHDAAN KOKO SATSISTA VAIN YKSI NUOTTI
	Alkuticks_TOT_CompoA=c()
	Durations_TOT_CompoA=c()
	Velocities_TOT_CompoA=c()
	
	Pitches_TOT_CompoB=c()# NAMA POIS PAALTA, JOS TEHDAAN KOKO SATSISTA VAIN YKSI NUOTTI
	Alkuticks_TOT_CompoB=c()
	Durations_TOT_CompoB=c()
	Velocities_TOT_CompoB=c()	
	
	
	###################
	
	if(max(Compo3[,5])>88){Compo3[,5]=Compo3[,5]-12}# Pudotetaan tarvittaessa oktaavilla.
	if(min(Compo3[,5])<28){Compo3[,5]=Compo3[,5]+12}# Nostetaan tarvittaessa 
	
	# Intervallien laskenta ja hyvyyskuvaaja:
	
	PcInts=rep(0,YhtAikSoivSavMaara)
	CompoAPitches=rep(0,YhtAikSoivSavMaara)
	Compo3Pitches=rep(0,YhtAikSoivSavMaara)
	HyviaIntervalleja=rep(0,YhtAikSoivSavMaara)
	
	for(i in 1: YhtAikSoivSavMaara)
	{
		CompoAPitches[i]=CompoA[which(CompoA[,2]==CommonTicks[i]),5]
		Compo3Pitches[i]=Compo3[which(Compo3[,2]==CommonTicks[i]),5]
	}
	
	# Jos kaksi perakkaista dissonanssia, ei lasketa:
	TestiIntervallit=((CompoAPitches-Compo3Pitches)%%12)
	if(is.element(1,diff(which(is.element(TestiIntervallit,c(1,2,6,10,11)))))){next}
	if(length(subvector(c(7,7),TestiIntervallit))>0){next} # RINNAKKAISET oktaavit, kvartit ja kvintit turhia.
	if(length(subvector(c(5,5),TestiIntervallit))>0){next}
	if(length(subvector(c(0,0),TestiIntervallit))>0){next}
	
	for(t0 in 1:YhtAikSoivSavMaara)# samaan aikaan soivien savelten ticksit
	{
		TEE_GRAAF_NUOTTI=0
		
		PcInts[t0]=(CompoAPitches[t0]-Compo3Pitches[t0])%%12# Yhtaaikaa soiv. sav. valiset intervallit
		
		if(is.element(PcInts[t0],c(0,3,4,7,8,9)))# Mutta enta jos ei ole naita, miten huomioidaan?
		{# Jos oktaaviin tai kvinttiin tullaan ok.
			if(t0==1 & is.element(PcInts[t0],c(0,3,4,7,8,9))){HyviaIntervalleja[t0]=1}# Nama automaattisesti hyvia
			if(PcInts[t0]!=0 & PcInts[t0]!=KV){HyviaIntervalleja[t0]=1}# Nama automaattisesti hyvia
			if(t0>1) 
			{
				EdCompoASavel=CompoA[rev(which(CompoA[,2]<CommonTicks[t0]))[1],5]
				EdCompo3Savel=Compo3[rev(which(Compo3[,2]<CommonTicks[t0]))[1],5]
				
				if((PcInts[t0]==0 | PcInts[t0]==KV) & ((Compo3Pitches[t0]-EdCompo3Savel)==0 | (CompoAPitches[t0]-EdCompoASavel)==0 | ((Compo3Pitches[t0]-EdCompo3Savel)>0 & (CompoAPitches[t0]-EdCompoASavel)<0) | ((Compo3Pitches[t0]-EdCompo3Savel)<0 & (CompoAPitches[t0]-EdCompoASavel)>0)))
				{
					HyviaIntervalleja[t0]=1 # Merkitaan ykkoseksi.
				}
			}
		}
		
		# HYVAKSYTYT PIDATYSLIIKKEET JA APPOGIATUURAT:			
		
		if(HyviaIntervalleja[t0]==0 & t0>1 & t0<YhtAikSoivSavMaara)
		{
			# suuresta sekunnista alaspainen purkaus, viimeinen ehto takaa alaspaisen liikkeen:
			if((CompoAPitches[t0]-Compo3Pitches[t0])%%12==2 &
				 ((CompoAPitches[t0+1]-Compo3Pitches[t0+1])%%12==3 |
				  (CompoAPitches[t0+1]-Compo3Pitches[t0+1])%%12==4) &
				 (CompoAPitches[t0]==CompoAPitches[t0+1]) &
				 (Compo3Pitches[t0-1]==Compo3Pitches[t0]) &
				 is.element((CompoAPitches[t0-1]-Compo3Pitches[t0-1])%%12,c(0,3,4,5,7,8,9))
			)
			{
				HyviaIntervalleja[t0]=1
				TEE_GRAAF_NUOTTI=1
			}
			# # pienesta sekunnista alaspainen purkaus pieneen terssiin:
			if((CompoAPitches[t0]-Compo3Pitches[t0])%%12==1 &
				 (CompoAPitches[t0+1]-Compo3Pitches[t0+1])%%12==3 &
				 (CompoAPitches[t0]==CompoAPitches[t0+1]) &
				 (Compo3Pitches[t0-1]==Compo3Pitches[t0]) &
				 is.element((CompoAPitches[t0-1]-Compo3Pitches[t0-1])%%12,c(0,3,4,5,7,8,9))
			)
			{
				HyviaIntervalleja[t0]=1
				TEE_GRAAF_NUOTTI=1
			}
			
			# pienesta septimista alaspainen purkaus:
			if((CompoAPitches[t0]-Compo3Pitches[t0])%%12==10 &
				 ((CompoAPitches[t0+1]-Compo3Pitches[t0+1])%%12==8 |
				  (CompoAPitches[t0+1]-Compo3Pitches[t0+1])%%12==9) &
				 (Compo3Pitches[t0]==Compo3Pitches[t0+1]) &
				 (CompoAPitches[t0-1]==CompoAPitches[t0]) &
				 is.element((CompoAPitches[t0-1]-Compo3Pitches[t0-1])%%12,c(0,3,4,5,7,8,9)))
			{
				HyviaIntervalleja[t0]=1
				TEE_GRAAF_NUOTTI=1
			}
			
			# suuresta septimista alaspainen purkaus suureen sekstiin:
			if((CompoAPitches[t0]-Compo3Pitches[t0])%%12==11 &
				 (CompoAPitches[t0+1]-Compo3Pitches[t0+1])%%12==9 &
				 (CompoAPitches[t0-1]==CompoAPitches[t0]) &
				 (Compo3Pitches[t0]==Compo3Pitches[t0+1]) &
				 is.element((CompoAPitches[t0-1]-Compo3Pitches[t0-1])%%12,c(0,3,4,5,7,8,9)))
			{
				HyviaIntervalleja[t0]=1
				TEE_GRAAF_NUOTTI=1
			}
			
			# # suuresta septimista ylospainen tai alaspainen purkaus oktaaviin:
			if((CompoAPitches[t0]-Compo3Pitches[t0])%%12==11 &
				 (CompoAPitches[t0+1]-Compo3Pitches[t0+1])%%12==0 &
				 (CompoAPitches[t0-1]==CompoAPitches[t0]))
			{
				HyviaIntervalleja[t0]=1
			}
			
			# # tritonuksesta ylospainen purkaus puhtaaseen kvinttiin
			if((CompoAPitches[t0]-Compo3Pitches[t0])%%12==6 &
				 (CompoAPitches[t0+1]-Compo3Pitches[t0+1])%%12==7 &
				 (CompoAPitches[t0-1]==CompoAPitches[t0]))
			{
				HyviaIntervalleja[t0]=1
			}
		}
	}
	
	if (sum(HyviaIntervalleja)==0){next}
	#print(sum(HyviaIntervalleja))
	
	##############################
	# SEURAAVAT HYVAKSYTAAN !!!: #		
	##############################
	
	# Lopussa loysempi ehto, yksi dissonanssi sallitaan.
	if(length(table(PcInts))!=1 & sum(HyviaIntervalleja)>=(length(HyviaIntervalleja)-SallittujenVirheidenMaara))
	{
		if(CompoMatInd==0 & CompoMatInd0==0)
		{
			CompoMatKaikki[[EdLahtoTeemaA]]=CompoMatLahtoteemat
			CompoMatLahtoteemat=list()
		} 
		
		CompoMatInd=CompoMatInd+1
		
		Hyvaksyttyja=Hyvaksyttyja+1
		OtetaanSavellykseen=1
		CompoA_Check=LahtoT_A
		
		PcInts=c(PcInts,0:11)
		
		print(paste("Kierroksia:", Kierroksia, "Tehty:", 100*round(Kierroksia/LenKaikkiYhdistelmatTod,digits=3), "%", "Hyvaksyttyja:", Hyvaksyttyja))
		
		CompoMat=Sortmat.R(rbind(CompoA,Compo3), 2, decreasing=FALSE) # Tassa yhdistetaan teemojen savelet listaksi.
		
		# Ticksien kvantisointi:
		CompoMat[,2]=round(CompoMat[,2])
		
		# Jos joukkoja 3-1, ei mukaan:
		
		# LenCompTest=length(CompoMat[,2])
		# LastTick=max(apply(CompoMat[,c(2,3)],1,sum))
		# PC_taulukko_Test=array(0,dim=c(LastTick,12))
		# 
		# for(k2 in 1:LenCompTest)
		# {
		#   PCtest=CompoMat[k2,5]%%12
		#   PC_taulukko_Test[CompoMat[k2,2]:(CompoMat[k2,2]+CompoMat[k2,3]),PCtest+1]=1			
		# }
		# 
		# if(Testataanko_3_1_melodiassa==TRUE)
		# {
		#   k3=which(CompoMat[,2]==0)[length(which(CompoMat[,2]==0))] # Ei testata Ticksia 0.
		#   Test_3_1_mel=TRUE
		#   
		#   while(k3<LenCompTest & Test_3_1_mel==TRUE)
		#   {
		#     k3=k3+1
		#     Test_3_1_mel=(nCVm[fnid(which(PC_taulukko_Test[CompoMat[k3,2],]==1)-1),8]==0) # Onko 3-1 osajoukkona?
		#   }
		#   if(Test_3_1_mel==FALSE){print("Ei hyvaksyta!"); next}
		# }
		
		# * * * #
		
		# +1:t
		CompoMat[which((CompoMat[,2]%%512)==1),2]=(CompoMat[,2][which((CompoMat[,2]%%512)==1)])-1
		# -1:t
		CompoMat[which((CompoMat[,2]%%512)==511),2]=(CompoMat[,2][which((CompoMat[,2]%%512)==511)])+1
		# +2:t
		CompoMat[which((CompoMat[,2]%%512)==2),2]=(CompoMat[,2][which((CompoMat[,2]%%512)==2)])-2
		# -2:t
		CompoMat[which((CompoMat[,2]%%512)==510),2]=(CompoMat[,2][which((CompoMat[,2]%%512)==510)])+2
		
		CompoMatLahtoteemat[[CompoMatInd]]=CompoMat
		EdLahtoTeemaA=LahtoteemaNro
		CompoMatInd0=0 # Tarvitaan vain ensimmaista if-luuppia varten varten. Vain ihan aluksi arvo on 1.
		
		PitchesAll=CompoMat[,5]
		AlkuticksAll=CompoMat[,2]
		DurationsAll=CompoMat[,3]
		#VelocitiesAll=CompoMat[,6]
		
		# Pelkka Lahtoteema An lista:
		PitchesAll_CompoA=CompoA[,5]
		AlkuticksAll_CompoA=CompoA[,2]
		DurationsAll_CompoA=CompoA[,3]
		#VelocitiesAll_CompoA=CompoA[,6]
		
		# Pelkka Lahtoteeman Bn lista:
		PitchesAll_CompoB=Compo3[,5]
		AlkuticksAll_CompoB=Compo3[,2]
		DurationsAll_CompoB=Compo3[,3]
		#VelocitiesAll_CompoB=Compo3[,6]
		
		if(length(Alkuticks_TOT)==0){TickLisays=0} else {TickLisays=(max(Alkuticks_TOT)+ Durations_TOT[length(Durations_TOT)])}
		Pitches_TOT=c(Pitches_TOT, PitchesAll)
		Alkuticks_TOT=c(Alkuticks_TOT, AlkuticksAll+TickLisays)
		Durations_TOT=c(Durations_TOT, DurationsAll)
		#Velocities_TOT=c(Velocities_TOT, VelocitiesAll)
		
		# Pelkkaan Lahtoteema An listaan:
		Pitches_TOT_CompoA=c(Pitches_TOT_CompoA, PitchesAll_CompoA)
		Alkuticks_TOT_CompoA=c(Alkuticks_TOT_CompoA, AlkuticksAll_CompoA+TickLisays)
		Durations_TOT_CompoA=c(Durations_TOT_CompoA, DurationsAll_CompoA)
		#Velocities_TOT_CompoA=c(Velocities_TOT_CompoA, VelocitiesAll_CompoA)
		
		# Pelkkaan Lahtoteema Bn listaan:
		Pitches_TOT_CompoB=c(Pitches_TOT_CompoB, PitchesAll_CompoB)
		Alkuticks_TOT_CompoB=c(Alkuticks_TOT_CompoB, AlkuticksAll_CompoB+TickLisays)
		Durations_TOT_CompoB=c(Durations_TOT_CompoB, DurationsAll_CompoB)
		#Velocities_TOT_CompoB=c(Velocities_TOT_CompoB, VelocitiesAll_CompoB)
		
		Kaytetyt_Teemat=rbind(Kaytetyt_Teemat,c(LahtoteemaNro, LahtoTeemaB)) # Otetaan kaytetyt teemat talteen.
		if(length(Alkuticks_TOT_All)==0)
		{
			TickLisaysAll=0} else {TickLisaysAll=(max(Alkuticks_TOT_All)+ Durations_TOT_All[length(Durations_TOT_All)])
			}
		Pitches_TOT_All=c(Pitches_TOT_All, PitchesAll)
		Pitches_TOT_All_CompoA=c(Pitches_TOT_All_CompoA, PitchesAll_CompoA)# Lahtoteemaan nuotinnosta varten.
		Pitches_TOT_All_CompoB=c(Pitches_TOT_All_CompoB, PitchesAll_CompoB)# LahtoteemaBn nuotinnosta varten.
		
		ExtraTicks=((2048)-(((CompoA[,2]+TickLisaysAll)[which(CompoA[,6]==max(CompoA[,6]))[1]])%%2048))# Matsataan Lahtoteemaan voimakkain isku tahtien puolinuotteihin, jos 2048.
		
		Alkuticks_TOT_All=c(Alkuticks_TOT_All, AlkuticksAll+TickLisaysAll+ExtraTicks-1024)#-2048)
		Durations_TOT_All=c(Durations_TOT_All, DurationsAll)
		#Velocities_TOT_All=c(Velocities_TOT_All, VelocitiesAll)
		
		# Lahtoteema An listaan:
		Alkuticks_TOT_All_CompoA=c(Alkuticks_TOT_All_CompoA, AlkuticksAll_CompoA+TickLisaysAll+ExtraTicks-1024)#-2048)
		Durations_TOT_All_CompoA=c(Durations_TOT_All_CompoA, DurationsAll_CompoA)
		#Velocities_TOT_All_CompoA=c(Velocities_TOT_All_CompoA, VelocitiesAll_CompoA)
		
		####################
		# KAIKKI KP-TIEDOT #
		####################
		
		Kaikki_KP_tiedot=rbind(Kaikki_KP_tiedot, c(LahtoteemaNro, Vasta_aiheNro, TRANSP, AUGMENTAATIO, AIKASIIRTYMA, YhtAikSoivSavMaara, length(YhteisetSavelluokat),round(((AlkuticksAll_CompoA+TickLisaysAll+ExtraTicks-1024)[1])/TICKSEJATAHDISSA,digits=2)))
		print(paste("LahtoTeema:", LahtoteemaNro, "VastaTeema:", Vasta_aiheNro, "TranspB:", TRANSP, "Augm:", round(AUGMENTAATIO,digits=2), "Tshift:", AIKASIIRTYMA, "YhtAikSoivSavMaara", YhtAikSoivSavMaara,"Savelluokkia:", length(YhteisetSavelluokat),"Lahtotahti:",round(((AlkuticksAll_CompoA+TickLisaysAll+ExtraTicks-1024)[1])/TICKSEJATAHDISSA)))
		
		# Lahtoteema Bn listaan:
		Alkuticks_TOT_All_CompoB=c(Alkuticks_TOT_All_CompoB, AlkuticksAll_CompoB+TickLisaysAll+ExtraTicks-1024) #-2048)
		Durations_TOT_All_CompoB=c(Durations_TOT_All_CompoB, DurationsAll_CompoB)
		#Velocities_TOT_All_CompoB=c(Velocities_TOT_All_CompoB, VelocitiesAll_CompoB)
		
		#print(length(Kombinaatiot))
	}
}

# Korjaus AIKASIIRTYMAan alla: -1024?

Velocities_TOT_All_CompoA=rep(64,length(Durations_TOT_All_CompoA))
Velocities_TOT_All_CompoB=rep(64,length(Durations_TOT_All_CompoB))
Velocities_TOT_All=rep(64,length(Durations_TOT_All))

KP_TiedostoNimiKaikki=paste("KONTRAPUNKTIT", PVM, sep="") # Kaytetaan alempana KP-nuotinnoksessa
# nuotinnos(TAHTILAJI, Pitches_TOT_All, Alkuticks_TOT_All, Durations_TOT_All, Velocities_TOT_All, KP_TiedostoNimiKaikki)
nuotinnos(TAHTILAJI, Pitches_TOT_All_CompoA, Alkuticks_TOT_All_CompoA-1024, Durations_TOT_All_CompoA, Velocities_TOT_All_CompoA, 1, paste("A_Lahtoteemat_",Teemanro, PVM,sep=""),CSV=FALSE)
nuotinnos(TAHTILAJI, Pitches_TOT_All_CompoB, Alkuticks_TOT_All_CompoB-1024, Durations_TOT_All_CompoB, Velocities_TOT_All_CompoB, 1, paste("B_Vasta_aiheet_",Teemanro, PVM,sep=""),CSV=FALSE)

##########
# 4x KPt #
##########

LAHTOTEEMAT4x=unique(Kaikki_KP_tiedot[,1])

Kontrapunktit4=c()
TickEnd4=0

KontrapunktitKaikki4=c()
TickEndKaikki4=0
KP_4x_tiedot=c()
Korjaukset_4x=list()
HYVAKSYTYT4=0

for(i11 in LAHTOTEEMAT4x[-length(LAHTOTEEMAT4x)]) # Alkuperaiset lahtoteemat.
{
	KPesiintymia4=length(CompoMatKaikki[[i11]]) # Kuinka monta vasta-aiheosumaa ko. lahtoteeman kohdalla kaikkiaan. Voi sisaltaa saman vastateeman uudelleen eri asemassa.
	if(KPesiintymia4<3){next}
	VastaTeemat4=Kaikki_KP_tiedot[which(Kaikki_KP_tiedot[,1]==i11),2:5] # Yhdistelmissa kaytetyt vastateemat. Sama teema voi olla useampaan kertaan. Pitaisi olla yhta monta kuin edella, koska samat esiintymat.
	
	Kombinaatiot_a4=combinations(KPesiintymia4,3,v=1:KPesiintymia4) # Lahtoteeman ja vasta-aiheiden kombinaatiot, abstrakti kombinaatiovaihtoehtojen jarjestysnumerointi. Ei teemojen numerointiin liittyva. Jos a4, r=3.
	Len_Kombinaatiot_a4=length(Kombinaatiot_a4[,1])
	
	for(k in 1:Len_Kombinaatiot_a4)
	{
		T1=VastaTeemat4[Kombinaatiot_a4[k,1],]
		T2=VastaTeemat4[Kombinaatiot_a4[k,2],]
		T3=VastaTeemat4[Kombinaatiot_a4[k,3],]
		
		Teema1=paste(T1,sep="",collapse="_")
		Teema2=paste(T2,sep="",collapse="_")
		Teema3=paste(T3,sep="",collapse="_")
		
		CompoMat1=CompoMatKaikki[[i11]][[Kombinaatiot_a4[k,1]]] # 2-aanisista yhdistelmista
		CompoMat2=CompoMatKaikki[[i11]][[Kombinaatiot_a4[k,2]]]
		CompoMat3=CompoMatKaikki[[i11]][[Kombinaatiot_a4[k,3]]]
		
		CompoMat_KPt4=Sortmat.R(rbind(CompoMat1, CompoMat2, CompoMat3),2) # a4 kpt.
		CompoMat_KPt4=unique(CompoMat_KPt4)
		
		rownames(CompoMat_KPt4)=1:length(CompoMat_KPt4[,1])
		CompoMat_KPt4[,4]=CompoMat_KPt4[,5]%%12 # Savelluokat savelkorkeuksista.
		Savelluokat4x=unique(CompoMat_KPt4[,4])
		
		CompoMat_KPt4[which(diff(CompoMat_KPt4[,2])==1),2]=CompoMat_KPt4[which(diff(CompoMat_KPt4[,2])==1),2]+1 # a4-compo
		IskullisetTicksit4=intersect(CompoMat_KPt4[,2],TSHIFT2)
		PCsets4=rep(0,length(IskullisetTicksit4))
		l2=0
			Korjaukset_tama=list()
			KPn_Joukot4=c()
		
		# Tassa voitaisiin korjata hylattavia sointuja toivotuilla sen mukaan, miten niihin pienimmin korjauksiin paastaan.
		# EI TOIMI(!) kuin kahden viivaston valinotaatioon, koska lopullinen partituuri a4 luodaan kayttaen alussa luotua KaikkiTeemat-listaa.
		for(l3 in IskullisetTicksit4)
		{
			l2=l2+1
			Pcs=unique(CompoMat_KPt4[which(CompoMat_KPt4[,2]==l3),4]) # Tarkasteltavan hetken sl-joukko
			PCsets4[l2]=fnid(Pcs)
			Hyvaksytaanko=is.element(PCsets4[l2], SallitutJoukot_4x_Kp)
			Hyvaksytaanko1=FALSE
			Hyvaksytaanko2=FALSE
			i5=0
			
			if(Hyvaksytaanko==FALSE) # Jos alkuperaista joukkoa ei hyväksyta:
			{
				while(i5<length(Pcs) & (Hyvaksytaanko1==FALSE & Hyvaksytaanko2==FALSE))
				{
					Pcs1=Pcs2=Pcs # Testattavat kopiot slj:sta.
					i5=i5+1
					Pcs1[i5]=Pcs[i5]+1 #muokataan yhden sl:n kohdalla puolisavelaskeleella.
					Pcs2[i5]=Pcs[i5]-1
					FNID1=fnid(unique(Pcs1)); Hyvaksytaanko1=is.element(FNID1, SallitutJoukot_4x_Kp)
					FNID2=fnid(unique(Pcs2)); Hyvaksytaanko2=is.element(FNID2, SallitutJoukot_4x_Kp)
				}
			}
			
                if(Hyvaksytaanko1 || Hyvaksytaanko2)
                {
                    Muutos=if(Hyvaksytaanko1) 1 else -1
                    # Pcs sisaltaa uniikit savelluokat, ei nuottirivien indekseja.
                    RivitKorjattavat=which(CompoMat_KPt4[,2]==l3 &
                                           CompoMat_KPt4[,4]==Pcs[i5])
                    Korjaukset_tama[[length(Korjaukset_tama)+1]]=c(l3,Pcs[i5],Muutos)
                    CompoMat_KPt4[RivitKorjattavat,5]=CompoMat_KPt4[RivitKorjattavat,5]+Muutos
                    CompoMat_KPt4[RivitKorjattavat,4]=CompoMat_KPt4[RivitKorjattavat,5]%%12
                    PCsets4[l2]=if(Hyvaksytaanko1) FNID1 else FNID2
                    Hyvaksytaanko=TRUE
                }

			KPn_Joukot4=c(KPn_Joukot4,Hyvaksytaanko) # Vain tietyt konsonanssit hyvaksytaan.
			#print(KPn_Joukot4)
		}
		
		#------------------------------------------------------------------------------------#
		# HYVAKSYTAAN ne vertikaalijoukot (puolinuotti-iskuilla), jotka maaritelty FNIDt:ssa #
		# ja jos kaikki hyvaksytaan, tehdaan nuotti:                                         #
		#------------------------------------------------------------------------------------#
		
		if(length(which(PCsets4>=8))<4){next}
		
		if(length(which(KPn_Joukot4==TRUE))<(length(KPn_Joukot4)-SallittujenVirheidenMaara)){next} # Jos kaikki ei TRUE eli hyvaksyttavia joukkoja.
		
		Table_PCsets4=table(PCsets4)
		
		SallitutJoukot_4x_Kp_Lista=rep(0,length(SallitutJoukot_4x_Kp))
		names(SallitutJoukot_4x_Kp_Lista)<-SallitutJoukot_4x_Kp
		
		for(i4 in names(Table_PCsets4))
		{
			SallitutJoukot_4x_Kp_Lista[i4]=Table_PCsets4[i4]
		}
		
		# i11 on alkuperaiset lahtoteemat (Nr)
		SJ_vektori_tama_kierros=c(i10,i11,SallitutJoukot_4x_Kp_Lista,c(round(TickEnd4/3072), VastaTeemat4[Kombinaatiot_a4[k,1],], VastaTeemat4[Kombinaatiot_a4[k,2],], VastaTeemat4[Kombinaatiot_a4[k,3],]))
		SJ_vektori_tama_kierros=round(SJ_vektori_tama_kierros,digits = 2)
		ColsNr_SallJoukotTaul=length(SJ_vektori_tama_kierros)
		ColsTeemat=c(2,ColsNr_SallJoukotTaul-11,ColsNr_SallJoukotTaul-7,ColsNr_SallJoukotTaul-3)
		TeematTamaKierros=SJ_vektori_tama_kierros[ColsTeemat]
		
		EiOtetaMukaan=FALSE
		for(i12 in 1:3)
		{
			Teema1a=round(TeematTamaKierros[i12])
			for(j12 in (i12+1):4)
			{
				Teema2a=round(TeematTamaKierros[j12])
				if(Teema1a==Teema2a)
				{
					if(i12==1)
					{
						Transp2=SJ_vektori_tama_kierros[round(ColsTeemat[j12])+1] #transp
						if(is.element(Transp2,c(5,7,0))){EiOtetaMukaan=TRUE}
					} else if (i12>1)
					{
						if(is.element(abs(SJ_vektori_tama_kierros[round(ColsTeemat[i12])+1]-SJ_vektori_tama_kierros[round(ColsTeemat[j12])+1]),c(5,7,0)))
						{
							EiOtetaMukaan=TRUE
						}
					}
				}
			}
		}
		if(EiOtetaMukaan==TRUE){next} # ei rinnakkaisia teemoja.
		
		Teematiedot_4x_Kp_Taulukko=rbind(Teematiedot_4x_Kp_Taulukko, c(i10,i11,c(round(TickEnd4/3072), VastaTeemat4[Kombinaatiot_a4[k,1],], VastaTeemat4[Kombinaatiot_a4[k,2],], VastaTeemat4[Kombinaatiot_a4[k,3],])))
		
		Korjaukset_4x[[length(Korjaukset_4x)+1]]=Korjaukset_tama
			HYVAKSYTYT4=HYVAKSYTYT4+1
		print(paste("HYVAKSYTTYJA:", HYVAKSYTYT4)) # TANNE ASTI paastaan vain, jos edella olevat ehdot eivat pompauta takaisin luuppiin.
		
		KP_4x_tiedot=rbind(KP_4x_tiedot,c(round(TickEndKaikki4/3072),i11, VastaTeemat4[Kombinaatiot_a4[k,1],], VastaTeemat4[Kombinaatiot_a4[k,2],], VastaTeemat4[Kombinaatiot_a4[k,3],])) # Jos a4.
		
		CompoMat_KPt4[,2]=CompoMat_KPt4[,2]+TickEnd4
		Kontrapunktit4=rbind(Kontrapunktit4, CompoMat_KPt4) # Kaikki samaan teemaan liittyvat generoinnit yhteen.
		TickEnd4=round(tail(Kontrapunktit4[,2],n=1L))
		while((TickEnd4%%1024)!=0){TickEnd4=TickEnd4+1}
		TickEnd4=TickEnd4+4*3072
		print(paste0("Teema:",i11," 4x kp:ien kombinaatiotesti nro: ",k,"/",Len_Kombinaatiot_a4))
	}
	
	if(length(Kontrapunktit4)>0)
	{
		#nuotinnos(TAHTILAJI, Kontrapunktit4[,5], Kontrapunktit4[,2], Kontrapunktit4[,3], Kontrapunktit4[,6],paste("4xKP_",i11,"_", PVM,"_",sep=""), CSV=FALSE)
		
		Kontrapunktit4[,2]=Kontrapunktit4[,2]+TickEndKaikki4
		KontrapunktitKaikki4=rbind(KontrapunktitKaikki4, Kontrapunktit4)
		
		TickEndKaikki4=round(tail(KontrapunktitKaikki4[,2],n=1L))
		while((TickEndKaikki4%%1024)!=0){TickEndKaikki4=TickEndKaikki4+1}
		TickEndKaikki4=TickEndKaikki4+4*3072
		Kontrapunktit4=c()
		TickEnd4=0
		HYVAKSYTYT4=0
	}
}

if(length(KontrapunktitKaikki4)>0)
{
	#beep(sound = 3, expr = NULL)
	# nuotinnos(TAHTILAJI, KontrapunktitKaikki4[,5], KontrapunktitKaikki4[,2], KontrapunktitKaikki4[,3], KontrapunktitKaikki4[,6],paste("4xKP_", PVM,"_",sep=""), CSV=FALSE)
	
	#------------------------------------------#
	# Nuotinnos-funktioon neljalle viivastolle #
	#------------------------------------------#
	
	KaikkiTeematJonossa4b=c()
	TickEndKaikki4b=0
	
	for(i in 1:length(KP_4x_tiedot[,1])) # 2,3,7, ja 11 alkup. teemoja.
	{
		Teema0n=KaikkiTeemat[[KP_4x_tiedot[i,2]]] 
		Teema1n=KaikkiTeemat[[KP_4x_tiedot[i,3]]] # 4 transp, 5 augm, 6 timeshift
		Teema2n=KaikkiTeemat[[KP_4x_tiedot[i,7]]]
		Teema3n=KaikkiTeemat[[KP_4x_tiedot[i,11]]]
		
		# 1. teeman kasittely:
		
		Teema0n[,2]=Teema0n[,2] -Teema0n[1,2] # TOIMIIKO TAMAN POISTON JALKEEN?
		Teema0n[,5]=Teema0n[,5]+12
		Teema0n[,1]=rep(2,length(Teema0n[,1])) # poistetaan tahtinrot, korvataan raidan nrolla.
		Teema0n[,4]=rep(1,length(Teema0n[,1])) # poistetaan pc:t, korvataan kanavan nrolla.
		colnames(Teema0n)<-c("Track","Tick","Dur","Channel","Pitch","Velo")
		
		# 2. teeman kasittely:
		
		Teema1n[,3]=(Teema1n[,3])*c(KP_4x_tiedot[i,5]) # augm/dimin.
		Teema1n[,2]=((Teema1n[,2]-Teema1n[1,2])*c(KP_4x_tiedot[i,5]))+c(KP_4x_tiedot[i,6])
		Teema1n[,5]=Teema1n[,5]+c(KP_4x_tiedot[i,4])	
		Teema1n[,1]=rep(3,length(Teema1n[,1])) # poistetaan tahtinrot, korvataan raidan nrolla.
		Teema1n[,4]=rep(2,length(Teema1n[,1])) # poistetaan pc:t, korvataan kanavan nrolla.
		colnames(Teema1n)<-c("Track","Tick","Dur","Channel","Pitch","Velo")
		
		# 3. teeman kasittely:
		
		Teema2n[,3]=(Teema2n[,3])*c(KP_4x_tiedot[i,9]) # augm/dimin.
		Teema2n[,2]=((Teema2n[,2]-Teema2n[1,2])*c(KP_4x_tiedot[i,9]))+c(KP_4x_tiedot[i,10])
		Teema2n[,5]=Teema2n[,5]+c(KP_4x_tiedot[i,8])-12
		Teema2n[,1]=rep(4,length(Teema2n[,1])) # poistetaan tahtinrot, korvataan raidan nrolla.
		Teema2n[,4]=rep(3,length(Teema2n[,1])) # poistetaan pc:t, korvataan kanavan nrolla.
		colnames(Teema2n)<-c("Track","Tick","Dur","Channel","Pitch","Velo")
		
		# 4. teeman kasittely:
		
		Teema3n[,3]=(Teema3n[,3])*c(KP_4x_tiedot[i,13]) # augm/dimin.
		Teema3n[,2]=((Teema3n[,2]-Teema3n[1,2])*c(KP_4x_tiedot[i,13]))+c(KP_4x_tiedot[i,14])
		Teema3n[,5]=Teema3n[,5]+c(KP_4x_tiedot[i,12])-24
		Teema3n[,1]=rep(5,length(Teema3n[,1])) # poistetaan tahtinrot, korvataan raidan nrolla.
		Teema3n[,4]=rep(4,length(Teema3n[,1])) # poistetaan pc:t, korvataan kanavan nrolla.
		colnames(Teema3n)<-c("Track","Tick","Dur","Channel","Pitch","Velo")
		
		KaikkiTeematJonossa4=rbind(Teema0n, Teema1n, Teema2n, Teema3n)
        # Siirra haussa tehdyt savelluokkakorjaukset lopullisiin stemmoihin.
        for(Korjaus in Korjaukset_4x[[i]]) {
            RivitKorjattavat=which(KaikkiTeematJonossa4[,2]==Korjaus[1] &
                                  KaikkiTeematJonossa4[,5]%%12==Korjaus[2])
            KaikkiTeematJonossa4[RivitKorjattavat,5]=KaikkiTeematJonossa4[RivitKorjattavat,5]+Korjaus[3]
        }
		KaikkiTeematJonossa4[,2]=KaikkiTeematJonossa4[,2]+TickEndKaikki4b
		KaikkiTeematJonossa4b=rbind(KaikkiTeematJonossa4b, KaikkiTeematJonossa4)
		
		TickEndKaikki4b=round(tail(KaikkiTeematJonossa4b[,2],n=1L))
		while((TickEndKaikki4b%%4096)!=0){TickEndKaikki4b=TickEndKaikki4b+1}
		TickEndKaikki4b=TickEndKaikki4b+4*3072
	}
	
	KaikkiTeematJonossa4b[,2]=round(KaikkiTeematJonossa4b[,2])
	KaikkiTeematJonossa4b[,3]=round(KaikkiTeematJonossa4b[,3])
	KaikkiTeematJonossa4b=cbind(KaikkiTeematJonossa4b,KaikkiTeematJonossa4b[,5]%%12) # Lisataan sl:t omaan sarakkeeseen (7).
	rownames(KaikkiTeematJonossa4b)=1:length(KaikkiTeematJonossa4b[,1])
	
	KaikkiTeematJonossa4b_vara=KaikkiTeematJonossa4b
	# Tasta mitataan myos dissonanssisuus:
	# nuotinnos2(KaikkiTeematJonossa4b,1,4,3,4,0,60, paste("4x_KPt_a4_viivastolle_",PCSn_alkusavel,"_", PVM, i10,sep=""),CSV=TRUE)
	# ToMidiFile2=nuotinnos2(KaikkiTeematJonossa4b,1,4,3,4,0,60, paste("4x_KPt_a4_viivastolle", PVM, i10,sep=""),CSV=FALSE)
	
	# Poistetaan mid-tiedosto
	# system(paste("/bin/rm -f /Users/attetenkanen/SAVELLYS/GENEROIDUT/",paste("4x_KPt_a4_viivastolle", PVM, i10,sep=""),".mid",sep=""),intern=FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = FALSE)
	
	KaikkiTeematJonossa4bJarjestetty=c() # Kaytetaan sl:ien korjauksessa
	for(i7 in 2:5)
	{
		Mat1=KaikkiTeematJonossa4b[which(KaikkiTeematJonossa4b[,1]==i7),]
		Mat2=Sortmat.R(Mat1,2)
		KaikkiTeematJonossa4bJarjestetty=rbind(KaikkiTeematJonossa4bJarjestetty,Mat2)
	}
	rownames(KaikkiTeematJonossa4bJarjestetty)=1:length(KaikkiTeematJonossa4bJarjestetty[,1])
	
	
	# JOS TASTA FILEESTA TEHDAAN SOINNUTUS, TAYTYY KO. FINALE-FILE KAANTAA ENSIN 0-TYYPPISEKSI MIDIKSI.
	# MIDI_0=Sortmat.R(KaikkiTeematJonossa4b,2)
	
	# nuotinnos(TAHTILAJI, MIDI_0[,5], MIDI_0[,2], MIDI_0[,3], MIDI_0[,6], KP_TiedostoNimiKaikki)
}

#print(Kaikki_KP_tiedot)

# 0-transpositiot:
#print(Kaikki_KP_tiedot[which(Kaikki_KP_tiedot[,3]==0),])
# transpositioiden maarat:
#print(table(Kaikki_KP_tiedot[,4]))


#############################
# Sointien konsonanssisuus: #
#############################

# Korjaa 3-1-klusterit ennen pisteytysta ja vientia. Aikavalit ovat [alku,loppu).
# Hyvaksy muutos vain, jos klusteritilanteiden kokonaismaara pienenee.
korjaa_31 <- function(nuotit) {
    hetket=sort(unique(nuotit[,2]))
    klusteri=function(p) {
        p=sort(unique(p%%12))
        any(vapply(p,function(x) all(c(x,(x+1)%%12,(x+2)%%12) %in% p),logical(1)))
    }
    aktiiviset=lapply(hetket,function(t) which(nuotit[,2]<=t & nuotit[,2]+nuotit[,3]>t))
    virheet=function(m) vapply(aktiiviset,function(r) klusteri(m[r,5]),logical(1))
    repeat {
        ennen=virheet(nuotit)
        parani=FALSE
        for(h in which(ennen)) {
            for(r in aktiiviset[[h]]) {
                for(d in c(-1,1)) {
                    koe=nuotit
                    koe[r,5]=koe[r,5]+d
                    jalkeen=virheet(koe)
                    if(sum(jalkeen)<sum(ennen) && !any(jalkeen & !ennen)) {
                        nuotit=koe
                        parani=TRUE
                        break
                    }
                }
                if(parani) break
            }
            if(parani) break
        }
        if(!parani) break
    }
    nuotit[,7]=nuotit[,5]%%12
    nuotit
}
KaikkiTeematJonossa4b=korjaa_31(KaikkiTeematJonossa4b)

KaikkiTeematJonossa4b_Teemojen_lopetus_rivit=cumsum(rle(KaikkiTeematJonossa4b[,1])$lengths)[c(FALSE,FALSE,FALSE,TRUE)]
KaikkiTeematJonossa4b_Teemojen_aloitus_rivit=KaikkiTeematJonossa4b_Teemojen_lopetus_rivit+1
KaikkiTeematJonossa4b_Teemojen_aloitus_rivit=c(1,KaikkiTeematJonossa4b_Teemojen_aloitus_rivit[-length(KaikkiTeematJonossa4b_Teemojen_aloitus_rivit)])

MaxTick_kaikki=max(KaikkiTeematJonossa4b[,2]+KaikkiTeematJonossa4b[,3])
MatrixTicks_a4_kaikki=array(NA,dim=c(MaxTick_kaikki,4))
KaikkiTeematJonossa4bInstanssit_kaikki=c()
TickLisays_seuraavaan_kaikki=0
Kaikkien_KP_ryhmien_konsonanssisuus=c()

# KaikkiTeematJonossa4b instansseja on i1 kpl.
SLlaskuri1=0;SLlaskuri2=0;SLlaskuri3=0
for(i1 in 1:length(KaikkiTeematJonossa4b_Teemojen_lopetus_rivit))
{
	KaikkiTeematJonossa4bInstanssi=KaikkiTeematJonossa4b[KaikkiTeematJonossa4b_Teemojen_aloitus_rivit[i1]:KaikkiTeematJonossa4b_Teemojen_lopetus_rivit[i1],]
	KaikkiTeematJonossa4bInstanssi[,2]=KaikkiTeematJonossa4bInstanssi[,2]-min(KaikkiTeematJonossa4bInstanssi[,2]) # Nollataan aloitustick.
	MaxTick=max(KaikkiTeematJonossa4bInstanssi[,2]+KaikkiTeematJonossa4bInstanssi[,3])
	KaikkiTeematJonossa4bInstanssi_jatko=KaikkiTeematJonossa4bInstanssi
	KaikkiTeematJonossa4bInstanssi_jatko[,2]=KaikkiTeematJonossa4bInstanssi_jatko[,2]+TickLisays_seuraavaan_kaikki
	KaikkiTeematJonossa4bInstanssit_kaikki=rbind(KaikkiTeematJonossa4bInstanssit_kaikki,KaikkiTeematJonossa4bInstanssi_jatko)
	TickLisays_seuraavaan=(TICKSEJATAHDISSA-MaxTick%%TICKSEJATAHDISSA)+MaxTick
	TickLisays_seuraavaan_kaikki=sum(TickLisays_seuraavaan_kaikki,TickLisays_seuraavaan)
	
	# Tapahtumahetket:
	UniqueTicks1234=sort(unique(KaikkiTeematJonossa4bInstanssi[,2])) # Tassa kaikki soinnun muutoshetket.
	MatrixTicks_a4=array(NA,dim=c(MaxTick,4))
	
	for(i2 in 2:5)
	{
		Rivit_Stemma_Instanssi=which(KaikkiTeematJonossa4bInstanssi[,1]==i2)
		for(i3 in Rivit_Stemma_Instanssi)
		{
			MatrixTicks_a4[seq.int(KaikkiTeematJonossa4bInstanssi[i3,2]+1, length.out=KaikkiTeematJonossa4bInstanssi[i3,3]),i2-1]=(KaikkiTeematJonossa4bInstanssi[i3,5])%%12
		}
	}
	
	# Jos halutaan muuttaa satsin sl:a ja s.korkeuksia, muutokset muuttujaan KaikkiTeematJonossa4b.
	
	Tapahtumien_Konsonanssiarvot=c()
	for(i6 in UniqueTicks1234)
	{
		SLlaskuri1=SLlaskuri1+1
		SLt=MatrixTicks_a4[i6+1,]
		SLjoukko=sort(unique(c(na.omit(SLt))),decreasing=FALSE)
		SLJfnid=fnid(SLjoukko)
		if(length(SLJfnid)==1) # Voi olla 0, eli kaikissa stemmoissa NA, ei savelta.
		{
			if(is.element(SLJfnid,SallitutJoukot_4x_Kp)){SLlaskuri3=SLlaskuri3+1} # onko mika tahansa sointi osa "Sallittuja joukkoja"?
		}
		if(length(SLjoukko)>0)
		{
			if(is.element(fnid(SLjoukko),c(8,27:29,31:35)))# onko 3-1 osajoukkona mukana soinnissa?
			{
				SLlaskuri2=SLlaskuri2+1

			}
		}
		if(length(SLjoukko)>2)
		{
			Tapahtumien_Konsonanssiarvot=c(Tapahtumien_Konsonanssiarvot,hucv2(SLjoukko)) # ; print(SLjoukko)
		}
	}
	
	Tapahtumien_Konsonanssiarvot=na.omit(Tapahtumien_Konsonanssiarvot)
	Tapahtumien_Konsonanssiarvot=Tapahtumien_Konsonanssiarvot[Tapahtumien_Konsonanssiarvot<1.5] # Unisonot pois.
	Kaikkien_KP_ryhmien_konsonanssisuus=c(Kaikkien_KP_ryhmien_konsonanssisuus,mean(Tapahtumien_Konsonanssiarvot))
}

KaikkiTeematJonossa4b[,7]=KaikkiTeematJonossa4b[,7]%%12

nuotinnos2(KaikkiTeematJonossa4bInstanssit_kaikki,1,4,OsoittajaTL,NimittajaTL,0,60, paste("4x_KPt_a4_viivastolle", PVM, i10,sep=""),CSV=FALSE)

###############################
# Neljasosaticksien harmoniat #
###############################

KaikkiNeljasosaticksit=seq(0, max(KaikkiTeematJonossa4bInstanssit_kaikki[,2]), by=1024)
PitchesSoinnut=c()
AlkuticksSoinnut=c()
KestotSoinnut=c()
VelocitiesSoinnut=c()

for(i9 in KaikkiNeljasosaticksit)
{
	SaveletSointuun=c()
	TicksRivit=which(KaikkiTeematJonossa4bInstanssit_kaikki[,2]==i9)
	if(length(TicksRivit)==4) # Jos neliaaninen kohta.
	{
		SaveletTicksissa=KaikkiTeematJonossa4bInstanssit_kaikki[TicksRivit,5]
		if(length(unique(SaveletTicksissa%%12))==4)
		{
			SaveletSointuun=taydennetty_6Z28_521A_joukko(SaveletTicksissa)
			PitchesSoinnut=c(PitchesSoinnut,SaveletSointuun)
			AlkuticksSoinnut=c(AlkuticksSoinnut,rep(i9,length(SaveletSointuun)))
			KestotSoinnut=c(KestotSoinnut,rep(1024,length(SaveletSointuun)))
			VelocitiesSoinnut=c(VelocitiesSoinnut,rep(64,length(SaveletSointuun)))
		}
	}
}
if(length(PitchesSoinnut)>0)
{
	# nuotinnos(TAHTILAJI, PitchesSoinnut, AlkuticksSoinnut, KestotSoinnut, VelocitiesSoinnut, "Sointuja",CSV=TRUE)
}

# TEHDAANKO KONSONANSSIJARJESTYS

if(Tehdaan_konsonanssisuusjarjestys==TRUE)
{
	
	KP_ryhmien_konsonanssisuus_jarjestetty=rev(order(Kaikkien_KP_ryhmien_konsonanssisuus))
	
	# Naista muuttujista tiedot JARJESTETTYa MIDIa varten:
	
	Seuraavan_kombinaation_ticks_lisays=0
	KaikkiTeematJonossa4b2=c()
	lenKPkons=length(KP_ryhmien_konsonanssisuus_jarjestetty)
	if(length(KP_ryhmien_konsonanssisuus_jarjestetty)>kpja_4x_max) {lenKPkons=kpja_4x_max}
	
	for(a1 in KP_ryhmien_konsonanssisuus_jarjestetty[1:lenKPkons]) # Teemojen jarjestysnumerot.
	{
		Teemakombinaatio=KaikkiTeematJonossa4b[KaikkiTeematJonossa4b_Teemojen_aloitus_rivit[a1]:KaikkiTeematJonossa4b_Teemojen_lopetus_rivit[a1],]
		Teemakombinaatio[,2]=Teemakombinaatio[,2]-Teemakombinaatio[1,2]+Seuraavan_kombinaation_ticks_lisays
		Seuraavan_kombinaation_ticks_lisays=(TICKSEJATAHDISSA-max(Teemakombinaatio[,2])%%TICKSEJATAHDISSA)+max(Teemakombinaatio[,2])
		KaikkiTeematJonossa4b2=rbind(KaikkiTeematJonossa4b2,Teemakombinaatio)
	}
	
	#KaikkiTeematJonossa4b2[,2]=KaikkiTeematJonossa4b2[,2]+Lisaticksit_alusta
	
	nuotinnos2(KaikkiTeematJonossa4b2,1,4,OsoittajaTL,NimittajaTL,0,60, paste("4x_KPt_a4_viivastolle_jarjestetty_Teemanro_",Teemanro,"_", PVM, i10,sep=""),CSV=FALSE)
	
	nuotinnos3(KaikkiTeematJonossa4b2,1,4,OsoittajaTL,NimittajaTL,0,60, paste("4x_KPt_a4_viivastolle_jarjestetty_Harmoniaa_",Teemanro,"_", PVM, i10,sep=""),CSV=FALSE, 4096)
	
	X=1:length(KaikkiTeematJonossa4b_Teemojen_lopetus_rivit)
	Y=round(sort(Kaikkien_KP_ryhmien_konsonanssisuus, decreasing=TRUE), digits=3)
	Y[which(is.na(Y))]=0
	
	jpeg(filename=paste(DIRECTORY_GENEROIDUT, paste("4x_KPt_a4_viivastolle_", max(Y),"_", PVM, i10,sep=""),"_",".jpeg",sep=""), width=800, height=800, quality = 99)
	plot(X, Y, type="l", col.axis="white", main=paste("4x_KPt_a4_viivastolle_", PVM, i10,"  max: ", max(Y),sep=""), ylim=c(-0.5, 1))
	axis(1,at=X)
	axis(2,at=seq(0,2,by=0.1))
	points(X, Y, cex = 2, col = "dark red")
	dev.off()
}


print(table(Teematiedot_4x_Kp_Taulukko[,2]))

# save.image("~/SAVELLYS/Sax_biisiin _k2021/WSpace210214a.RData")

EiKaytetty2x=(setdiff(1:TeemojaLKM,as.numeric(unique(names(table(c(Kaikki_KP_tiedot[,1],Kaikki_KP_tiedot[,2])))))))
EiKaytetty2x=do.call(paste, c(as.list(EiKaytetty2x), sep = " "))
print(paste("Naita teemoje ei kaytetty 2x kp:issa:",EiKaytetty2x))

EiKaytetty4x=setdiff(1:TeemojaLKM,unique(c(Teematiedot_4x_Kp_Taulukko[,c(2,4,8,12)])))
EiKaytetty4x=do.call(paste, c(as.list(EiKaytetty4x), sep = " "))
print(paste("Naita teemoje ei kaytetty 4x kp:issa:", EiKaytetty4x))

table(c(Teematiedot_4x_Kp_Taulukko[,c(2,4,8,12)]))

# teemaesiintymat yleisyysjarjestyksessa:
table(c(Teematiedot_4x_Kp_Taulukko[,c(2,4,8,12)]))[rev(order(table(c(Teematiedot_4x_Kp_Taulukko[,c(2,4,8,12)]))))]

# teemojen augmentaatiotiedot yleisyysjarjestyksessa:
KertoimetKaytetyt=table(c(Teematiedot_4x_Kp_Taulukko[,c(6,10,14)]))[rev(order(table(c(Teematiedot_4x_Kp_Taulukko[,c(6,10,14)]))))]
KertoimetKaytetyt2=round(as.numeric(names(KertoimetKaytetyt)), digits=5)
KertoimetKaikki=round(Kertoimet, digits=5)

KerroinInds=c()
for(i in 1:length(KertoimetKaikki))
{
	if(is.element(KertoimetKaikki[i],KertoimetKaytetyt2)) KerroinInds=c(KerroinInds,i)
}
