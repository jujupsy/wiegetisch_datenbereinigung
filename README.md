# wiegetisch_datenbereinigung


### Run app
shiny::runGitHub("jujupsy/wiegetisch_datenbereinigung", ref="main", destdir = getwd())

Die App wird im Arbeitsverzeichnis (getwd()) "installiert". Im Ordner "wiegetisch_datenbereinigung-main" befinden sich dann die Unterordner "raw_data", in welchen die Rohdaten kopiert werden m�ssen, und "exported_data", in dem die bereinigten Daten am Ende gespeichert werden.

### Daten
Die Daten m�ssen bereits auf einen Messwert pro Sekunde aggregiert (Theoretisch kann auch mit mehr Messpunkten pro Sekunde gearbeitet werden. Aufgrund der steigenden Dateigr��en wird die App dann aber sehr langsam!) und den Snacks zugeordnet vorliegen. Der Name der Spalten, die die Gewichte der Snacks enthalten, k�nnen in der App angepasst werden. Die Spalten "vp" und "time" m�ssen exisitieren. Das Format sollte beispielsweise so aussehen:

```r
vp time CCC KSU PME BKS BES LBS
1 0 927.98 805.63 835.31 939.06 885.01 359.57
1 1 927.90 805.76 835.37 939.30 885.02 360.09
1 2 927.88 805.76 835.28 939.33 885.03 360.09
1 3 927.99 805.76 835.43 939.31 885.03 360.09
...

```

### Datenaufbereitung

Die in der Anwendung implementierte Datenbereinigung f�r jeden einzelnen Gewichtsverlauf umfasst folgende Schritte: 
- Zuerst wird der Mittelwert der ersten 15 Sekunden berechnet und mit der dreifachen Standardabweichung in diesem Bereich multipliziert um einen oberen Schwellenwert zu errechnen. Alle Werte, die �ber diesem Schwellenwert liegen werden als unplausibel eingestuft. Durch die Multiplikation mit der gewichteten Standardabweichung wird sichergestellt, dass die Schwelle proportional zur Streuung in den ersten 15 Sekunden gesetzt wird und so die Unsicherheit bzw. Ungenauigkeit in diesem Messzeitraum ber�cksichtigt wird. Gleiches Vorgehen mit den Werten der letzten 15 Sekunden f�hrt zu einer unteren Schwelle. Auch hier werden alle Werte die darunter lagen als unplausibel eingestuft. Mit diesem ersten Schritt werden vor allem sehr extreme Gewichtsabweichungen eliminiert, die z.B. durch ein Anheben der Sch�ssel oder das Herunterdr�cken dieser mit der Hand zustande kommen. 
- Weiterhin unplausible Datenpunkte werden nun anhand eines gleitenden Mittels versucht zu identifizieren. Das gleitende Mittel umfasst 20 Datenpunkte (entspricht 20 Sekunden) und l�uft vom ersten zum letzten Datenpunkt. Da das Gewicht theoretisch nie zunehmen sollte, werden nach jedem Schritt des gleitenden Mittels alle nachfolgenden Werte, die mehr als eine Schwelle dar�ber liegen als unplausibel eingestuft. Auch diese Schwelle war proportional zur Standardabweichung der Werte des gleitenden Mittels und wurde mit 20 Gramm multipliziert, um keine falschen aber m�glichst viele unplausible Werte zu erkennen. Gleiches Vorgehen f�hrt nachfolgend mit einem gleitenden Mittel vom letzten zum ersten Messpunkt zur Eliminierung m�glichst vieler zu niedriger Messpunkte. Dieser Schritt wird mit einem schmaleren gleitenden Mittel (10 statt 20 Datenpunkte) und geringerer Gewichtung der Standardabweichung (10 statt 20 Gramm) ein zweites Mal wiederholt. Dadurch werden kleinere Abweichungen besser erkannt.
- Zuletzt werden die als unplausibel eingestuften Werte durch den nachfolgenden Wert ersetzt und es wird �berpr�ft, ob die Werte monoton abnehmen. Ist dies nicht der Fall, dann wird der Wert durch den nachfolgend niedrigeren ersetzt, um diese logische Regel durchzusetzen. 


Alle Parameter [in eckigen Klammern angegeben] k�nnen in der Shiny-Anwendung manuell korrigiert werden, wenn die Datenaufbereitung Fehler aufweist. Au�erdem ist es m�glich von Hand einzelne Punkte hinzuzuf�gen, um den tats�chlichen Gewichtsverlauf genau zu rekonstruieren. 

### Replizierbarkeit
Damit die Datenaufbereitung nachpr�fbar und replizierbar ist, werden alle Parameter f�r jeden Gewichtsverlauf in der Datei "para_[Dateiname].rds" gespeichert.

