 ################################
## SOA: 300 ms                ##
## Tussen sequences: 1.5 min  ##
## Tussen orders: 5 min       ##
################################

from __future__ import division
from psychopy import sound, core, visual, event
## parallel port importeren 
#from psychopy import parallel 
## port adress invoegen 
#parallel.setPortAddress(0xC020)

#from psychopy import prefs
#prefs.general['audioLib'] = ['pygame']

#import pygame

import numpy as np
import pandas as pd

import csv

import os

from datetime import datetime
import time

#proefpersoonnr ingeven
proefpersoonnr = 1

win = visual.Window(size = (800,600), color='black')

os.chdir(r'/Users/Shauni/Desktop/ExperimentShauni/randomisationFiles') 

dataPP = []

with open("expShauni_pp%02d.txt" %proefpersoonnr, 'r') as f:
    reader = csv.reader(f, dialect = 'excel', delimiter = '\t')
    for row in reader:
        dataPP.append(row)

del(dataPP[0])

clean = []

for i in range(7680):
    nodig = dataPP[i]
    del(nodig[0])
    clean.append(nodig)

trial = clean

## Triggers wegschrijven voor EEG
## 4 soorten triggers nodig: order, sequence type, block type, tone type
## Order 1 = 1, Order 2 = 2, slow = 3, fast = 4, 60 deviant = 5, 30 deviant = 6, standard = 7, deviant = 8

Trigger = []

for i in range(7680):
    if trial[i][0] == 'order 1' and trial[i][1] == 'Slow change' and trial[i][4] == '60 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'standard':
        Trigger.append(21)
    if trial[i][0] == 'order 1' and trial[i][1] == 'Slow change' and trial[i][4] == '60 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'deviant':
        Trigger.append(22)
    if trial[i][0] == 'order 1' and trial[i][1] == 'Slow change' and trial[i][4] == '60 deviant' and trial[i][5] == 'second half' and trial[i][6] == 'standard':
        Trigger.append(23)
    if trial[i][0] == 'order 1' and trial[i][1] == 'Slow change' and trial[i][4] == '60 deviant' and trial[i][5] == 'second half' and trial[i][6] == 'deviant':
        Trigger.append(24)
    if trial[i][0] == 'order 1' and trial[i][1] == 'Slow change' and trial[i][4] == '30 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'standard':
        Trigger.append(25)
    if trial[i][0] == 'order 1' and trial[i][1] == 'Slow change' and trial[i][4] == '30 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'deviant':
        Trigger.append(26)
    if trial[i][0] == 'order 1' and trial[i][1] == 'Slow change' and trial[i][4] == '30 deviant' and trial[i][5] == 'second half' and trial[i][6] == 'standard':
        Trigger.append(27)
    if trial[i][0] == 'order 1' and trial[i][1] == 'Slow change' and trial[i][4] == '30 deviant' and trial[i][5] == 'second half' and trial[i][6] == 'deviant':
        Trigger.append(28)
        
    if trial[i][0] == 'order 1' and trial[i][1] == 'Fast change' and trial[i][4] == '60 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'standard':
        Trigger.append(31)
    if trial[i][0] == 'order 1' and trial[i][1] == 'Fast change' and trial[i][4] == '60 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'deviant':
        Trigger.append(32)
    if trial[i][0] == 'order 1' and trial[i][1] == 'Fast change' and trial[i][4] == '60 deviant' and trial[i][5] == 'second half' and trial[i][6] == 'standard':
        Trigger.append(33)
    if trial[i][0] == 'order 1' and trial[i][1] == 'Fast change' and trial[i][4] == '60 deviant' and trial[i][5] == 'second half' and trial[i][6] == 'deviant':
        Trigger.append(34)
    if trial[i][0] == 'order 1' and trial[i][1] == 'Fast change' and trial[i][4] == '30 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'standard':
        Trigger.append(35)
    if trial[i][0] == 'order 1' and trial[i][1] == 'Fast change' and trial[i][4] == '30 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'deviant':
        Trigger.append(36)
    if trial[i][0] == 'order 1' and trial[i][1] == 'Fast change' and trial[i][4] == '30 deviant' and trial[i][5] == 'second half' and trial[i][6] == 'standard':
        Trigger.append(37)
    if trial[i][0] == 'order 1' and trial[i][1] == 'Fast change' and trial[i][4] == '30 deviant' and trial[i][5] == 'second half' and trial[i][6] == 'deviant':
        Trigger.append(38)
    
    
    if trial[i][0] == 'order 2' and trial[i][1] == 'Slow change' and trial[i][4] == '60 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'standard':
        Trigger.append(41)
    if trial[i][0] == 'order 2' and trial[i][1] == 'Slow change' and trial[i][4] == '60 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'deviant':
        Trigger.append(42)
    if trial[i][0] == 'order 2' and trial[i][1] == 'Slow change' and trial[i][4] == '60 deviant' and trial[i][5] == 'second half' and trial[i][6] == 'standard':
        Trigger.append(43)
    if trial[i][0] == 'order 2' and trial[i][1] == 'Slow change' and trial[i][4] == '60 deviant' and trial[i][5] == 'second half' and trial[i][6] == 'deviant':
        Trigger.append(44)
    if trial[i][0] == 'order 2' and trial[i][1] == 'Slow change' and trial[i][4] == '30 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'standard':
        Trigger.append(45)
    if trial[i][0] == 'order 2' and trial[i][1] == 'Slow change' and trial[i][4] == '30 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'deviant':
        Trigger.append(46)
    if trial[i][0] == 'order 2' and trial[i][1] == 'Slow change' and trial[i][4] == '30 deviant' and trial[i][5] == 'second half' and trial[i][6] == 'standard':
        Trigger.append(47)
    if trial[i][0] == 'order 2' and trial[i][1] == 'Slow change' and trial[i][4] == '30 deviant' and trial[i][5] == 'second half' and trial[i][6] == 'deviant':
        Trigger.append(48)
        
    if trial[i][0] == 'order 2' and trial[i][1] == 'Fast change' and trial[i][4] == '60 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'standard':
        Trigger.append(51)
    if trial[i][0] == 'order 2' and trial[i][1] == 'Fast change' and trial[i][4] == '60 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'deviant':
        Trigger.append(52)
    if trial[i][0] == 'order 2' and trial[i][1] == 'Fast change' and trial[i][4] == '60 deviant' and trial[i][5] == 'second half' and trial[i][6] == 'standard':
        Trigger.append(53)
    if trial[i][0] == 'order 2' and trial[i][1] == 'Fast change' and trial[i][4] == '60 deviant' and trial[i][5] == 'second half' and trial[i][6] == 'deviant':
        Trigger.append(54)
    if trial[i][0] == 'order 2' and trial[i][1] == 'Fast change' and trial[i][4] == '30 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'standard':
        Trigger.append(55)
    if trial[i][0] == 'order 2' and trial[i][1] == 'Fast change' and trial[i][4] == '30 deviant' and trial[i][5] == 'first half' and trial[i][6] == 'deviant':
        Trigger.append(56)
    if trial[i][0] == 'order 2' and trial[i][1] == 'Fast change' and trial[i][4] == '30 deviant' and trial[i][5]== 'second half' and trial[i][6] == 'standard':
        Trigger.append(57)
    if trial[i][0] == 'order 2' and trial[i][1] == 'Fast change' and trial[i][4] == '30 deviant' and trial[i][5]== 'second half' and trial[i][6] == 'deviant':
        Trigger.append(58)

## time definieren
soundtime = []

for i in range(7680):
    if trial[i][3] == '60ms_sound.wav':
        soundtime.append(.06)
    else:
        soundtime.append(.03)

soundlength = soundtime[i]

print len(trial)
for i in range(10):
    print(trial[i])

for i in range(10):
    print(soundtime[i])

for i in range(10):
    print(Trigger[i])

mouse = event.Mouse(visible = False, win = win)

instr1 = 'Start experiment. Druk space'

time1 = '1'
time2 = '2'
time3 = '3'
time4 = '4'
time5 = '5'

Pauze = 'Pauze'

EndInstr1 = 'Het experiment is afgelopen. \n \nHartelijk bedankt om deel te nemen!.'

instruction1 = visual.TextStim(win, text=instr1,units='norm',height=0.12, color='White',pos=[0,0], alignHoriz='center',flipHoriz=False)

timing1 = visual.TextStim(win, text=time1,units='norm',height=0.12, color='White',pos=[0,0], alignHoriz='center',flipHoriz=False)
timing2 = visual.TextStim(win, text=time2,units='norm',height=0.12, color='White',pos=[0,0], alignHoriz='center',flipHoriz=False)
timing3 = visual.TextStim(win, text=time3,units='norm',height=0.12, color='White',pos=[0,0], alignHoriz='center',flipHoriz=False)
timing4 = visual.TextStim(win, text=time4,units='norm',height=0.12, color='White',pos=[0,0], alignHoriz='center',flipHoriz=False)
timing5 = visual.TextStim(win, text=time5,units='norm',height=0.12, color='White',pos=[0,0], alignHoriz='center',flipHoriz=False)

Pause = visual.TextStim(win, text=Pauze,units='norm',height=0.12, color='White',pos=[0,0], alignHoriz='center',flipHoriz=False)

EndInstruction = visual.TextStim(win, text=EndInstr1,units='norm',height=0.12, color='White',pos=[0,0], alignHoriz='center',flipHoriz=False)

core.wait(0.5)

while True:
    instruction1.draw()
    win.flip()
    event.waitKeys()

    break

while True:
    timing5.draw()
    win.flip()
    time.sleep(1)

    timing4.draw()
    win.flip()
    time.sleep(1)

    timing3.draw()
    win.flip()
    time.sleep(1)

    timing2.draw()
    win.flip()
    time.sleep(1)

    timing1.draw()
    win.flip()
    time.sleep(1)

    break

mouse = event.Mouse(visible = True, win = win)
rectangle = visual.Rect(win, width = .05, height = .05, pos=(.5,.5), fillColor = 'springgreen')
rectanglebreak = visual.Rect(win, width = .05, height = .05, pos=(-.5,.5), fillColor = 'red')

trialNumber = []
dateAtSound = []

os.chdir(r'/Users/Shauni/Desktop/ExperimentShauni') 

rectangle.draw()
rectanglebreak.draw()
win.flip()


for i in range(len(trial)):
    
    audioFile = trial[i][3]
    play_audio = sound.Sound(value = audioFile)
    
    dateNow = str(datetime.now())
    
    #parallel.setData(0)
    #parallel.setData(Trigger[i]) 
    #core.wait(.03)
    #parallel.setData(0)
    
    play_audio.play()
    
    #trialNumber.append(i)
    #dateAtSound.append(dateNow)

    if mouse.isPressedIn(rectangle):
        break
    
    if mouse.isPressedIn(rectanglebreak):
        event.waitKeys()
    
    if(i % 3839 == 0 and i % 7678 != 0 and i != 0):
        Pause.draw()
        win.flip()
        core.wait(300) ## 5 min pauze (300 seconden)
        win.flip()
        rectangle.draw()
        rectanglebreak.draw()
        win.flip()
    elif((i % 1919 == 0 or i % 5759 == 0) and i % 3838 != 0 and i % 5757 != 0 and i != 0): 
        Pause.draw()
        win.flip()
        core.wait(90) ## 1.5 min pauze (90 seconden)
        win.flip()
        rectangle.draw()
        rectanglebreak.draw()
        win.flip()
        
    core.wait(.3 - soundlength)
    
    
experimentData = {'Trial number': trialNumber, 'Time stamp': dateAtSound}
expData = pd.DataFrame(experimentData, columns = ['Trial number', 'Time stamp'])

os.chdir(r'/Users/Shauni/Desktop/ExperimentShauni/experimentRunFiles') 
expData.to_csv("expJudith_rundata_pp%02d.txt" %proefpersoonnr, sep = '\t')

while not event.getKeys():
    EndInstruction.draw()
    win.flip()
    core.wait(3)
    win.close()
    core.quit()


#########
## END ##
#########