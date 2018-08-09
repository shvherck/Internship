##########################################################################################
## Experiment internship                                                                ##
## Paradigma uit:                                                                       ##
## Todd, J., Provost, A., Whitson, L. R., Cooper, G., & Heathcote, A. (2013).           ##
## Not so primitive: context-sensitive meta-learning about unattended sound sequences.  ##
## Journal of neurophysiology, 109(1), 99-105.                                          ##
##########################################################################################

## Importeren van de nodige packages
import numpy as np
import pandas as pd
from random import shuffle
import os

## Definieren van het proefpersoonnummer
## Andere randomisatie per persoon is op deze manier mogelijk
proefpersoonnr = 40

## Lege lijst definieren
## De waarden die wij nodig hebben worden dan in deze lijst weggeschreven
## -> welke order (1, 2, 3), welke sequence (slow of fast changing), hoeveelste sequence (van de 6: 3 slow, 3 fast), wat voor blok (welke toon is deviant?)
## (volgorde van tonen in volledige experiment)
order = []
SequenceName = []
SequenceNum = []
BlockType = []
BlockHalf = []

## Lengte van 1 sequence definieren
    ## In elke sequence zijn er 1920 tonen, zowel in de slow change als de fast change sequences
    ## In totaal 6 sequences: 3 slow change, en 3 fast change
    ## -> totalTrials = lengthSequences*6
lengthSequences = 1920
totalTrials = lengthSequences*4

## For loops die order 1, 2 of 3 weggeschrijven naar de lege list 'order'
## In totaal zal dus 3840 keer order1 in de list staan, 3840 order2 en 3840 order 3
    ## 1920*6 = 11520
    ## 11520/3 = 3840
for i in range(totalTrials):
    if i < int(totalTrials/2):
        order.append('order 1')
    if int(totalTrials/2) <= i < int(totalTrials):
        order.append('order 2')

i = 0
while i < 4:
    for j in range(lengthSequences): ## lengthSequences = 1920
        if i % 2 == 0: ## sequence 1, 3 en 5 zijn slow change
            SequenceName.append('Slow change')
        else: ## sequence 2, 4 en 6 zijn fast change 
            SequenceName.append('Fast change')
    i += 1 ## python begint altijd bij 0 ipv 1, dus de 1e sequence is in python eigenlijk 0

i = 0
while i < 4:
    for j in range(lengthSequences):
        SequenceNum.append('Sequence %d/4' %(i+1))
    i += 1

## Block type aanmaken: 60 deviant - 30 deviant
i = 0
if proefpersoonnr%2 == 0: 
    for i in range(totalTrials):
        if i <= 479:
            BlockType.append('30 deviant')
        if 479 < i <= 959:
            BlockType.append('60 deviant')
        if 960 <= i <= 1439:
            BlockType.append('30 deviant')
        if 1440 <= i <= 1919:
            BlockType.append('60 deviant')
        if 1920 <= i <= 2079:
            BlockType.append('30 deviant')
        if 2080 <= i <= 2239:
            BlockType.append('60 deviant')
        if 2240 <= i <= 2399:
            BlockType.append('30 deviant')
        if 2400 <= i <= 2559:
            BlockType.append('60 deviant')
        if 2560 <= i <= 2719:
            BlockType.append('30 deviant')
        if 2720 <= i <= 2879:
            BlockType.append('60 deviant')
        if 2880 <= i <= 3039:
            BlockType.append('30 deviant')
        if 3040 <= i <= 3199:
            BlockType.append('60 deviant')
        if 3200 <= i <= 3359:
            BlockType.append('30 deviant')
        if 3360 <= i <= 3519:
            BlockType.append('60 deviant')
        if 3520 <= i <= 3679:
            BlockType.append('30 deviant')
        if 3680 <= i <= 3839:
            BlockType.append('60 deviant')
        if 3840 <= i <= 4319:
            BlockType.append('60 deviant')
        if 4320 <= i <= 4799:
            BlockType.append('30 deviant')
        if 4800 <= i <= 5279:
            BlockType.append('60 deviant')
        if 5280 <= i <= 5759:
            BlockType.append('30 deviant')
        if 5760 <= i <= 5919:
            BlockType.append('60 deviant')
        if 5920 <= i <= 6079:
            BlockType.append('30 deviant')
        if 6080 <= i <= 6239:
            BlockType.append('60 deviant')
        if 6240 <= i <= 6399:
            BlockType.append('30 deviant')
        if 6400 <= i <= 6559:
            BlockType.append('60 deviant')
        if 6560 <= i <= 6719:
            BlockType.append('30 deviant')
        if 6720 <= i <= 6879:
            BlockType.append('60 deviant')
        if 6880 <= i <= 7039:
            BlockType.append('30 deviant')
        if 7040 <= i <= 7199:
            BlockType.append('60 deviant')
        if 7200 <= i <= 7359:
            BlockType.append('30 deviant')
        if 7360 <= i <= 7519:
            BlockType.append('60 deviant')
        if 7520 <= i <= 7679:
            BlockType.append('30 deviant')
else: 
    for i in range(totalTrials):
        if i <= 479:
            BlockType.append('60 deviant')
        if 479 < i <= 959:
            BlockType.append('30 deviant')
        if 960 <= i <= 1439:
            BlockType.append('60 deviant')
        if 1440 <= i <= 1919:
            BlockType.append('30 deviant')
        if 1920 <= i <= 2079:
            BlockType.append('60 deviant')
        if 2080 <= i <= 2239:
            BlockType.append('30 deviant')
        if 2240 <= i <= 2399:
            BlockType.append('60 deviant')
        if 2400 <= i <= 2559:
            BlockType.append('30 deviant')
        if 2560 <= i <= 2719:
            BlockType.append('60 deviant')
        if 2720 <= i <= 2879:
            BlockType.append('30 deviant')
        if 2880 <= i <= 3039:
            BlockType.append('60 deviant')
        if 3040 <= i <= 3199:
            BlockType.append('30 deviant')
        if 3200 <= i <= 3359:
            BlockType.append('60 deviant')
        if 3360 <= i <= 3519:
            BlockType.append('30 deviant')
        if 3520 <= i <= 3679:
            BlockType.append('60 deviant')
        if 3680 <= i <= 3839:
            BlockType.append('30 deviant')
        if 3840 <= i <= 4319:
            BlockType.append('30 deviant')
        if 4320 <= i <= 4799:
            BlockType.append('60 deviant')
        if 4800 <= i <= 5279:
            BlockType.append('30 deviant')
        if 5280 <= i <= 5759:
            BlockType.append('60 deviant')
        if 5760 <= i <= 5919:
            BlockType.append('30 deviant')
        if 5920 <= i <= 6079:
            BlockType.append('60 deviant')
        if 6080 <= i <= 6239:
            BlockType.append('30 deviant')
        if 6240 <= i <= 6399:
            BlockType.append('60 deviant')
        if 6400 <= i <= 6559:
            BlockType.append('30 deviant')
        if 6560 <= i <= 6719:
            BlockType.append('60 deviant')
        if 6720 <= i <= 6879:
            BlockType.append('30 deviant')
        if 6880 <= i <= 7039:
            BlockType.append('60 deviant')
        if 7040 <= i <= 7199:
            BlockType.append('30 deviant')
        if 7200 <= i <= 7359:
            BlockType.append('60 deviant')
        if 7360 <= i <= 7519:
            BlockType.append('30 deviant')
        if 7520 <= i <= 7679:
            BlockType.append('60 deviant')

## Block half aanmaken: first half - second half
i = 0
for i in range(totalTrials):
    if i <= 239:
        BlockHalf.append('first half')
    if 239 < i <= 479:
        BlockHalf.append('second half')
    if 479 < i <= 719:
        BlockHalf.append('first half')
    if 719 < i <= 959:
        BlockHalf.append('second half')
    if 959 < i <= 1199:
        BlockHalf.append('first half')
    if 1199 < i <= 1439:
        BlockHalf.append('second half')
    if 1439 < i <= 1679:
        BlockHalf.append('first half')
    if 1679 < i <= 1919:
        BlockHalf.append('second half')
    if 1919 < i <= 1999:
        BlockHalf.append('first half')
    if 1999 < i <= 2079:
        BlockHalf.append('second half')
    if 2079 < i <= 2159:
        BlockHalf.append('first half')
    if 2159 < i <= 2239:
        BlockHalf.append('second half')
    if 2239 < i <= 2319:
        BlockHalf.append('first half')
    if 2319 < i <= 2399:
        BlockHalf.append('second half')
    if 2399 < i <= 2479:
        BlockHalf.append('first half')
    if 2479 < i <= 2559:
        BlockHalf.append('second half')
    if 2559 < i <= 2639:
        BlockHalf.append('first half')
    if 2639 < i <= 2719:
        BlockHalf.append('second half')
    if 2719 < i <= 2799:
        BlockHalf.append('first half')
    if 2799 < i <= 2879:
        BlockHalf.append('second half')
    if 2879 < i <= 2959:
        BlockHalf.append('first half')
    if 2959 < i <= 3039:
        BlockHalf.append('second half')
    if 3039 < i <= 3119:
        BlockHalf.append('first half')
    if 3119 < i <= 3199:
        BlockHalf.append('second half')
    if 3199 < i <= 3279:
        BlockHalf.append('first half')
    if 3279 < i <= 3359:
        BlockHalf.append('second half')
    if 3359 < i <= 3439:
        BlockHalf.append('first half')
    if 3439 < i <= 3519:
        BlockHalf.append('second half')
    if 3519 < i <= 3599:
        BlockHalf.append('first half')
    if 3599 < i <= 3679:
        BlockHalf.append('second half')
    if 3679 < i <= 3759:
        BlockHalf.append('first half')
    if 3759 < i <= 3839:
        BlockHalf.append('second half')
    if 3839 < i <= 4079:
        BlockHalf.append('first half')
    if 4079 < i <= 4319:
        BlockHalf.append('second half')
    if 4319 < i <= 4559:
        BlockHalf.append('first half')
    if 4559 < i <= 4799:
        BlockHalf.append('second half')
    if 4799 < i <= 5039:
        BlockHalf.append('first half')
    if 5039 < i <= 5279:
        BlockHalf.append('second half')
    if 5279 < i <= 5519:
        BlockHalf.append('first half')
    if 5519 < i <= 5759:
        BlockHalf.append('second half')
    if 5759 < i <= 5839:
        BlockHalf.append('first half')
    if 5839 < i <= 5919:
        BlockHalf.append('second half')
    if 5919 < i <= 5999:
        BlockHalf.append('first half')
    if 5999 < i <= 6079:
        BlockHalf.append('second half')
    if 6079 < i <= 6159:
        BlockHalf.append('first half')
    if 6159 < i <= 6239:
        BlockHalf.append('second half')
    if 6239 < i <= 6319:
        BlockHalf.append('first half')
    if 6319 < i <= 6399:
        BlockHalf.append('second half')
    if 6399 < i <= 6479:
        BlockHalf.append('first half')
    if 6479 < i <= 6559:
        BlockHalf.append('second half')
    if 6559 < i <= 6639:
        BlockHalf.append('first half')
    if 6639 < i <= 6719:
        BlockHalf.append('second half')
    if 6719 < i <= 6799:
        BlockHalf.append('first half')
    if 6799 < i <= 6879:
        BlockHalf.append('second half')
    if 6879 < i <= 6959:
        BlockHalf.append('first half')
    if 6959 < i <= 7039:
        BlockHalf.append('second half')
    if 7039 < i <= 7119:
        BlockHalf.append('first half')
    if 7119 < i <= 7199:
        BlockHalf.append('second half')
    if 7199 < i <= 7279:
        BlockHalf.append('first half')
    if 7279 < i <= 7359:
        BlockHalf.append('second half')
    if 7359 < i <= 7439:
        BlockHalf.append('first half')
    if 7439 < i <= 7519:
        BlockHalf.append('second half')
    if 7519 < i <= 7599:
        BlockHalf.append('first half')
    if 7599 < i <= 7679:
        BlockHalf.append('second half')


## In de lijst zouden nu 7680 items moeten zitten
print(len(order))
print(len(SequenceName))
print(len(SequenceNum))
print(len(BlockType))

## Definieren van lege lijsten voor de slow change blokken, en de slow change blok van de tweede order (ander volgorde)
## Wat gebeurt er net in de volgende lijnen? 
soundSlowChange = []
unexp_slow = [] 

soundSlowChangeOrder2 = []
unexp_ph2_1 = []  

i = 0
standard = 'text'
deviant = 'text'

## SLOW CHANGE SEQUENCES
## while i < 4 omdat er 4 blokken zijn in de slow change sequence
## i start op 0, dus als je altijd + 1 doet zal je 4 blokken hebben voor i werkelijk 4 wordt
## afhankelijk van het feit of de restdeling van i gedeeld door 2 nul is wordt een andere geluidsfile 'standard'
## De slow change van order 2 is exact het omgekeerde van order 1, dit wordt hier ook gedaan (standard wordt deviant en deviant wordt standard)
## Daarna is er een random shuffle van de lijsten, zodat de deviants op een random plaats in de list terecht komen
## Lijsten worden samengevoegd tot een groot geheel en bewerkt zodat ze een grote lijst vormen
while i < 4: 
    if i%2 == 0: ## in de het 1e en 3e blok van de sequence (order 1 & 3) is 60 ms deviant
        standard = '30ms_sound.wav'
        deviant = '60ms_sound.wav'
    else: ## in het 2e en 4e blok van de sequence (order 1) is 30 ms deviant
        standard = '60ms_sound.wav'
        deviant = '30ms_sound.wav'

    neededOrder1_1 = [standard]*int((lengthSequences/4)*.875) ## = standards order 1
    neededOrder2_1 = [deviant]*int((lengthSequences/4)*.875) ## = standards order 2: in order 2 wordt de deviant standard

    neededOrder1_2 = [deviant]*int((lengthSequences/4)*.125) ## = deviants order 1
    neededOrder2_2 = [standard]*int((lengthSequences/4)*.125) ## = deviants order 2: in order 2 wordt de deviant standard 

    neededOrder1 = neededOrder1_1 + neededOrder1_2 ## standards + deviants order 1
    shuffle(neededOrder1) ## standards en deviants random door elkaar aanbieden
    soundSlowChange.append(neededOrder1)
    
    neededOrder2 = neededOrder2_1 + neededOrder2_2 ## standards + deviants order 2
    shuffle(neededOrder2) ## standards en deviants random door elkaar aanbieden
    soundSlowChangeOrder2.append(neededOrder2)

    i += 1

soundSlowChange = np.ravel(soundSlowChange)  
soundSlowChange = list(soundSlowChange)


soundSlowChangeOrder2 = np.ravel(soundSlowChangeOrder2)
SoundSlowChangeOrder2 = list(soundSlowChangeOrder2)

soundFastChange = []
soundFastChangeOrder2 = []

i = 0 
standard = '30ms_sound.wav'
deviant = '60ms_sound.wav'

## Zelfde principe als hierboven, maar nu zijn er 12 blokjes in plaats van 4, dit omdat er meer afwisseling is in de fast change
## Dit is de reden dat i weer op 0 werd gezet, en i kleiner dan 8 moet zijn
while i < 12:
    if i%2 == 0: ## in het 1e, 3e, 5e en 7e blok van de fast change sequence (order 1) is 60 ms deviant
        standard = '30ms_sound.wav'
        deviant = '60ms_sound.wav'
    else: ## in het 2e, 4e, 6e en 8e blok van de fast change sequence (order 1) is 30 ms  deviant
        standard = '60ms_sound.wav'
        deviant = '30ms_sound.wav'

    neededOrder1_1 = [standard]*int((lengthSequences/12)*.875) ## = standards order 1
    neededOrder2_1 = [deviant]*int((lengthSequences/12)*.875) ## = standards order 2: in order 2 wordt standard deviant

    neededOrder1_2 = [deviant]*int((lengthSequences/12)*.125) ## = deviants order 1
    neededOrder2_2 = [standard]*int((lengthSequences/12)*.125) ## = deviants order 2: in order 2 wordt standard deviant

    neededOrder1 = neededOrder1_1 + neededOrder1_2 ## standards + deviants order 1
    shuffle(neededOrder1) ## standards en deviants random door elkaar aanbieden
    soundFastChange.append(neededOrder1)
    
    neededOrder2 = neededOrder2_1 + neededOrder2_2 ## standards + deviants order 2
    shuffle(neededOrder2) ## standards en deviants random door elkaar aanbieden
    soundFastChangeOrder2.append(neededOrder2)
    
    i += 1

soundFastChange = np.ravel(soundFastChange) 
soundFastChange = list(soundFastChange)

soundFastChangeOrder2 = np.ravel(soundFastChangeOrder2)
soundFastChangeOrder2 = list(soundFastChangeOrder2)

## Common trials staat hier voor order 1
## Het bestaat uit de gewone slow en fast change blocks
commonTrials = soundSlowChange + soundFastChange

## Uncommon trials staat voor order 2, dat andere regels heeft dan order 1 en order 3
uncommonTrials = SoundSlowChangeOrder2 + soundFastChangeOrder2

## Verzameling van alle trials, in de volgorde gedefinieerd volgens Todd. et al
trials = commonTrials + uncommonTrials 
# als er moet gecontrabalanceerd worden, kunnen we doen
if proefpersoonnr % 2 == 0:
   trials = uncommonTrials + commonTrials
else:
   trials = commonTrials + uncommonTrials
## Op deze manier krijgen de oneven proefpersonen de gewone volgorde (order 1 - order 2)
## en de even proefpersonen de andere volgorde (order 2 - order 1)
# ook nog nodig voor contrabalancering: block type aanpassen, want klopt niet meer welke trials 30 deviant / 60 deviant zijn 

## Tone type wegschrijven: standard of deviant tone
ToneType = []

for i in range(len(trials)): ## nakijken
    if trials[i] == '60ms_sound.wav' and BlockType[i] == '60 deviant':
        ToneType.append('deviant')
    if trials[i] == '30ms_sound.wav' and BlockType[i] == '30 deviant':
        ToneType.append('deviant')
    if trials[i] == '60ms_sound.wav' and BlockType[i] == '30 deviant':
        ToneType.append('standard')
    if trials[i] == '30ms_sound.wav' and BlockType[i] == '60 deviant':
        ToneType.append('standard')


## Triggers wegschrijven voor EEG
## 4 soorten triggers nodig: order, sequence type, block type, tone type
## Order 1 = 1, Order 2 = 2, slow = 3, fast = 4, 60 deviant = 5, 30 deviant = 6, standard = 7, deviant = 8

Trigger = []

for i in range(len(trials)):
    if order[i] == 'order 1' and SequenceName[i] == 'Slow change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'standard':
        Trigger.append('21')
    if order[i] == 'order 1' and SequenceName[i] == 'Slow change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'deviant':
        Trigger.append('22')
    if order[i] == 'order 1' and SequenceName[i] == 'Slow change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'standard':
        Trigger.append('23')
    if order[i] == 'order 1' and SequenceName[i] == 'Slow change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'deviant':
        Trigger.append('24')
    if order[i] == 'order 1' and SequenceName[i] == 'Slow change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'standard':
        Trigger.append('25')
    if order[i] == 'order 1' and SequenceName[i] == 'Slow change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'deviant':
        Trigger.append('26')
    if order[i] == 'order 1' and SequenceName[i] == 'Slow change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'standard':
        Trigger.append('27')
    if order[i] == 'order 1' and SequenceName[i] == 'Slow change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'deviant':
        Trigger.append('28')
        
    if order[i] == 'order 1' and SequenceName[i] == 'Fast change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'standard':
        Trigger.append('31')
    if order[i] == 'order 1' and SequenceName[i] == 'Fast change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'deviant':
        Trigger.append('32')
    if order[i] == 'order 1' and SequenceName[i] == 'Fast change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'standard':
        Trigger.append('33')
    if order[i] == 'order 1' and SequenceName[i] == 'Fast change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'deviant':
        Trigger.append('34')
    if order[i] == 'order 1' and SequenceName[i] == 'Fast change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'standard':
        Trigger.append('35')
    if order[i] == 'order 1' and SequenceName[i] == 'Fast change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'deviant':
        Trigger.append('36')
    if order[i] == 'order 1' and SequenceName[i] == 'Fast change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'standard':
        Trigger.append('37')
    if order[i] == 'order 1' and SequenceName[i] == 'Fast change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'deviant':
        Trigger.append('38')
    
    
    if order[i] == 'order 2' and SequenceName[i] == 'Slow change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'standard':
        Trigger.append('41')
    if order[i] == 'order 2' and SequenceName[i] == 'Slow change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'deviant':
        Trigger.append('42')
    if order[i] == 'order 2' and SequenceName[i] == 'Slow change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'standard':
        Trigger.append('43')
    if order[i] == 'order 2' and SequenceName[i] == 'Slow change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'deviant':
        Trigger.append('44')
    if order[i] == 'order 2' and SequenceName[i] == 'Slow change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'standard':
        Trigger.append('45')
    if order[i] == 'order 2' and SequenceName[i] == 'Slow change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'deviant':
        Trigger.append('46')
    if order[i] == 'order 2' and SequenceName[i] == 'Slow change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'standard':
        Trigger.append('47')
    if order[i] == 'order 2' and SequenceName[i] == 'Slow change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'deviant':
        Trigger.append('48')
        
    if order[i] == 'order 2' and SequenceName[i] == 'Fast change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'standard':
        Trigger.append('51')
    if order[i] == 'order 2' and SequenceName[i] == 'Fast change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'deviant':
        Trigger.append('52')
    if order[i] == 'order 2' and SequenceName[i] == 'Fast change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'standard':
        Trigger.append('53')
    if order[i] == 'order 2' and SequenceName[i] == 'Fast change' and  BlockType[i] == '60 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'deviant':
        Trigger.append('54')
    if order[i] == 'order 2' and SequenceName[i] == 'Fast change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'standard':
        Trigger.append('55')
    if order[i] == 'order 2' and SequenceName[i] == 'Fast change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'first half' and ToneType[i] == 'deviant':
        Trigger.append('56')
    if order[i] == 'order 2' and SequenceName[i] == 'Fast change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'standard':
        Trigger.append('57')
    if order[i] == 'order 2' and SequenceName[i] == 'Fast change' and  BlockType[i] == '30 deviant' and BlockHalf[i] == 'second half' and ToneType[i] == 'deviant':
        Trigger.append('58')


## Wegschrijven van de data in kolommen
## Data opslaan als een data frame
##experimentData = {'Order number': order, 'Sequence type': SequenceName,'Sequence number': SequenceNum, 'Sound file': trials, 'Block Type': BlockType, 'Tone type': ToneType, 'Trigger Order': TriggerOrder}
##expData = pd.DataFrame(experimentData, columns = ['Order number', 'Sequence type','Sequence number', 'Sound file', 'Block Type', 'Tone type', 'Trigger order'])

experimentData = {'Order number': order, 'Sequence type': SequenceName, 'Sequence number': SequenceNum, 'Sound file': trials, 'Block Type': BlockType, 'Block Half': BlockHalf, 'Tone type': ToneType, 'Trigger': Trigger}
expData = pd.DataFrame(experimentData, columns = ['Order number', 'Sequence type', 'Sequence number', 'Sound file', 'Block Type', 'Block Half', 'Tone type', 'Trigger'])

## Data weggeschreven naar .txt file gescheiden door kolommen
## Aanpassen van de child directory
os.chdir(r'/Users/Shauni/Desktop/ExperimentShauni/randomisationFiles') 
expData.to_csv("expShauni_pp%02d.txt" %proefpersoonnr, sep = '\t')

#########
## END ##
#########