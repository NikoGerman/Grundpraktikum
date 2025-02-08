# Worldbank Datensatz
**Modul Grundlegendes Praxisprojekt P11.1 Bachelor Statistik und Data Science (PO 2021)**

**Autoren:** Nikolai German, Wenxuan Liang, Thomas Witzani, Yanyu Zhao


## Allgemein
**Bitte lesen, bevor Sie ein Programm aus diesem Repository ausführen.**\
Hierbei handelt es sich um das Repository zum Grundlegenden Praxisprojekt zum
Thema *Worldbank*. Grundsätzlich sind hier alle nötigen Skripte vorhanden, 
um sowohl Präsentation als auch Grafiken und Executive Summary zu erzeugen.

## Anleitung

### Wiederherstellen der Umgebung
**R Version 4.4 oder höher benötigt!**\
Bitte führen sie zunächst den nachfolgenden R-Command aus, um die notwendigen
Pakete und Dependencies zu installieren:
```
renv::restore()
```

### Erzeugen der Präsentation
Um Speicherplatz zu sparen, muss die wurde darauf verzichtet die gerenderte
Präsentation abzuspeichern. Zum Erzeugen der Präsentation, öffnen Sie wahlweise
Presentation.qmd und klicken dort "Render" oder führen folgenden R-Command aus:
```
quarto::quarto_render("Presentation.qmd")
```
Die gerenderte Präsentation trägt dann den Namen Presentation.html,
Presentation_held.pdf ist die Präsentation, welche vorgetragen wurde. 
Zur Erzeugung dieser existiert kein gesonderter Code.

### Erzeugen der Executive Summary
Zur Erstellen der Executive Summary gehen Sie bitte analog wie beim Erzeugen der
Präsentation vor, bzw. führen folgenden R-Command aus:
```
quarto::quarto_render("ExecutiveSummary.qmd")
```
Den Output finden Sie dann im File ExecutiveSummary.pdf.

## Verzeichnisstruktur

### Figures
Beim Erzeugen der Präsentation werden die Grafiken direkt erzeugt und nicht
zwischengespeichert. Um trotzdem den Abgaberichtlinien genüge zu tun, liegen
alle in der Präsentation genutzen Grafiken im Verzeichnis Figures/.\ 
Sollten Sie diese neu erzeugen wollen, führen sie folgenden R-Command aus:
```
source(source_all.R)
save.all()
```

### Data
Das Verzeichnis Data/ enthält die genutzen Rohdaten im Subverzeichnis /raw, 
sowie die aufbereiteten Daten im Subverzeichnis /cleaned.\
Sollten Sie die Daten in /cleaned neu erzeugen wollen, führen Sie folgenden
R-Command aus:
```
source(source_all.R)
DataPrep(method = "save", colors = TRUE)
```

### R
Das Verzeichnis R/ enthält alle .R Skripte mit Ausnahme von settings.R und 
source_all.R.\
Die Skripte sind in der Regel nach ihrer angedachten Funktionalität
benannt.\
In utils.R finden sich Funktionen, die in verschiedenen anderen
Skripten Verwendung finden.\
Q1.R,..., Q5.R erzeugen jeweils die Grafiken für
Frage 1,..., Frage 5.\
Appendix.R erzeugt die im Anhang der
Präsentation genutzten Grafiken.

### Weitere Verzeichnisse
Die übrigen Verzeichnisse wurden entweder systemseitig bei der Versionierung
oder im Rahmen der Paket-Verwaltung erzeugt und genutzt.




