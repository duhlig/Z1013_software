;;; KRT-Grafik-Demonstration für den Z1013
;;; Dietmar Uhlig, 2024

;;; ============================================================
	org  %0100

	jp   main

;;; ============================================================
;;; 2 Tabellen, die so liegen müssen, dass das High-Byte der
;;; Adressen gleich ist.
;;; 
;;; 1. Tabelle
bitab	defb %7F	; 1. Bit des Bytes
	defb %BF	; Die 1. Tab. ist für das
	defb %DF	; Setzen eines Pixels auf
	defb %EF	; Vordergrundfarbe.
	defb %F7
	defb %FB
	defb %FD
	defb %FE	; 8. Bit des Bytes
;;; 2. Tabelle, wird nur vom modifizierten Punkt-Algo. verwendet
bitab2	defb %80	; 2. Tabelle, 1. Bit
	defb %40	; Die 2. Tab. ist für das
	defb %20	; Rücksetzen eines Pixels
	defb %10	; auf Hintergrundfarbe
	defb %08	; und für das Invertieren
	defb %04	; eines Pixels.
	defb %02
	defb %01	; 8. Bit
	defb %00


;;; ============================================================


main:
	;; ----- Einleitung und Erklärung der Tasten -----
	call einlt
	ld   a,1	; 1 = Debug rndini, 0 = ohne
	call rndini
;	jp   debug

demo:
	call gein
	call cls
	call stern
	ld   a,50
	ld   (wzeit),a
	call warte
	call cls

	call erklt
	
	;; ----- Punkte -----
	call txpu
	call ptfeld
	call drei
	ld   a,5
	call tastto
	call cls

	;; ----- Ameise -----
	call txam1
	call gein
	call cls

	ld   a,250
	ld   (whalb),a
	ld   a,16
	ld   (wzeit),a
	call ameise
	call drei
	ld   a,5
	call tastto
	call cls

	call txam2
	call gein
	call cls

	call amstn
	ld   a,8
	ld   (whalb),a
	ld   a,10
	ld   (wzeit),a
	call ameise
	call drei
	ld   a,5
	call tastto
	call cls

	;; ----- Linien -----
	call txfa
	call gein
	call cls
	call stfa
	call drei
	ld   a,5
	call tastto
	call cls

	;; FA- und Bresenhamalgorithmus
	call txfabr
	call lifeld
	call drei
	ld   a,10
	call tastto
	call cls

	call txbsl
	call gein
	call cls
	call linbss
	call drei
	ld   a,5
	call tastto
	call cls
	
	;; ----- Kreise -----
	call txbr
	call gein
	call kreise
	call drei
	ld   a,5
	call tastto

	call txkbs
debug:	
	call gein
	call cls
	call krbss
	call drei
	ld   a,5
	call tastto
	call cls
	
	;; ----- Bild -----
	call txbi
	call gein
	call cls

	ld   hl,bild1
	call zbild

	call drei
	ld   a,5
	call tastto

	;; Bild einschieben
	ld   hl,bild2
	call bivus

	call drei
	ld   a,5
	call tastto
	
	;; ----- Drachenkurve
	call txdrmn
	call gein
	call cls
	call drku
	
	call drei
	ld   a,5
	call tastto

	;; Männl
	call maennl
	
	call drei
	ld   a,5
	call tastto
	call cls

	call txende
	;; ----- Wiederholung oder Abbruch -----
	or   a		; Z=0 wenn Taste gedrückt wurde
	jpz  demo
	
	rst  %20
	defb %02
	defb %8C	; CLS (Textmodus)

mend:	rst  %20
	defb %02
	defb %0D,"Bis bald!"
	defb %8D
	rst  %38
	
;;; ============================================================
;;; ein paar Hilfsroutinen
	

;;; Warten auf Tastendruck oder Timeout
;;; Parameter: a: Timeout ungefähr in sek.
;;; 1 s warten ~ %60 * Schleife:
tastto:
	push hl
	push de
	ld   h,0
	ld   l,a
	add  hl,hl
	add  hl,hl
	add  hl,hl
	add  hl,hl
	add  hl,hl
	ld   d,h
	ld   e,l
	add  hl,hl
	add  hl,de	; hl := %60 * a

tastt1:	push hl
	rst  %20
	defb %04	; INKEY
	pop  hl
	or   a
	jrnz tastt2
	dec  hl
	xor  a
	cmp  h		; Time-Out erreicht? Vergleich Teil 1
	jrnz tastt1
	cmp  l		; Vergleich Teil 2
	jrnz tastt1	; nein -> noch warten
	jr   tastt9

tastt2:	cmp  %0D	; Enter gedrückt?
	jrz  tastt9	; ja -> weiter im Programm
	rst  %20
	defb %01	; INCH, warte endlos auf Taste

tastt9: pop  de
        pop  hl
        ret

; ---------------------------------------------


;;; künstlich warten
whalb   defb 0	       ; Nach wieviel Aufrufen soll die Wartezeit halbiert werden?
whzae	defb 0	       ; Zähler für whalb
wzeit	defb 0	       ; Wartezeit in ~10ms bei 2 MHz

warte:
	ld   a,(wzeit)
	or   a		; setzt Carry=0 und Z entspr. Inhalt
	rz
	push hl
	push bc

;;; innere Schleife:
;;; sbc + jrnz = 15 + 12 = 27 Takte
;;; äußere Schleife:
;;; 2*ld + dec + jrnz = 2*10 + 4 + 12 = 36 Takte
;;; hl = 740 -> 740*27 + 36 = 20016 Takte ~ 0,01s bei 2MHz
warte1:	ld   hl,740
	ld   bc,1
warte2:	sbc  hl,bc	; Carry=0, siehe or
	jrnz warte2
	dec  a
	jrnz warte1

	ld   hl,whzae
	inc  (hl)
	ld   b,(hl)
	ld   a,(whalb)
	cmp  b
	jrnz warte9
	xor  a
	ld   (hl),a
	ld   a,(wzeit)
	srl  a
	ld   (wzeit),a
warte9:	
	pop  bc
	pop  hl
	ret

	
; ---------------------------------------------
	
;;; im Textmodus rechts unten ein Dreieck anzeigen
tdrei:
	ld   hl,%EFFD	; vorletztes Zeichen
	ld   (hl),%C0
	inc  hl
	ld   (hl),%97
	ret

; ---------------------------------------------

dreim	defb %FF,%BF,%8F,%83,%80,%83,%8F,%BF
	;; ................ FF
	;; ..##............ BF
	;; ..######........ 8F
	;; ..##########.... 83
	;; ..############## 80
	;; ..##########.... 83
	;; ..######........ 8F
	;; ..##............ BF
	;;  8 4 2 1 8 4 2 1

;;; im Grafikmodus rechts unten ein Dreieck anzeigen
drei:	push hl
	push de
	ld   hl,dreim
	ld   de,%EFFF
	xor  a
drei1:	out  deco
	exaf
	ld   a,(hl)
	ld   (de),a
	inc  hl
	exaf
	inc  a
	cmp  %08
	jrnz drei1
	pop  de
	pop  hl
	ret

leer:	push hl
	push de
	ld   hl,%EFFF
	ld   e,%FF
	xor  a
leer1:	out  deco
	ld   (hl),e
	inc  a
	cmp  %08
	jrnz leer1
	pop  de
	pop  hl
	ret


;;; ------------------------------------------------------------
;;; Xorshift+, siehe
;;; https://de.wikipedia.org/wiki/Xorshift#Xorshift+

rstat0	defs 8
rstat1	defs 8
rnx	defs 8
rny	defs 8
rnza	defb 0		; Anzahl neuer Zz. in rna
rna	defs 8		; rnza und rna müssen in dieser
			; Reihenfolge stehen

rndidb	defb 0		; Debug-Kennzeichen: <>0 = Debug
rndita	defb 0		; Zähler für Warten auf Tastendruck
	
;;; Initialisierung, muss vor der ersten Benutzung von
;;; random aufgerufen werden
rndini:
	push bc
	ld   (rndidb),a	; Debug-Kennzeichen
	
	ld   a,(rndita)
	or   a
	jrnz rndin1
	dec  a		; Verzweiflungstat, Startwert <> 0 !
rndin1:	ld   (rstat0+1),a
	
	ld   a,r
	jrnz rndin2
	ld   a,r
	jrnz rndin2
	dec  a		; Verzweiflungstat, Startwert <> 0 !
rndin2:	ld   (rstat0),a
	
	ld   a,(rndidb)	; Debug-Kennzeichen
	cmp  0
	jpz  rndin3
	
	rst  %20
	defb %02	; PRST7
	defb %0C,%0D
	defb " Initialisierung des",%0D
	defb " Zufallszahlengenerators",%8D
	call trenn
	rst  %20
	defb %02	; PRST7
	defb "Xorshift+, siehe",%0D
	defb "https://de.wikipedia.org/wiki/-",%0D
	defb "  Xorshift#Xorshift+",%0D,%0D
	defb "Seed: Bytes 1 und 0 in rstat0",%0D
	defb "  Werte: ",%A0
	
	ld   a,(rstat0+1)
	rst  %20
	defb 6		; OUTHX
	rst  %20
	defb %0E	; OUTSP
	ld   a,(rstat0)
	rst  %20
	defb 6		; OUTHX

	rst  %20
	defb %02	; PRST7
	defb %0D,"  Byte 1: Anzahl INKEY-Aufrufe",%0D
	defb "          vor 1. Tastendruck",%0D
	defb "  Byte 0: Refresh-Register",%0D,%0D
	defb "10 Iterationen bis zu brauch-",%0D
	defb "baren Zufallszahlen:",%0D,%8D
rndin3:	
	ld   b,11	; schlechter Startwert
			; wird durch mehrfachen Abruf einer
			; Pseudozufallszahl ausgebessert
rndin4:
	ld   a,(rndidb)	; Debug-Kennzeichen
	cmp  0
	canz pr8hex	; ja: Zufallswert ausgeben
	
	call random
	djnz rndin4
	ld   a,8
	ld   (rnza),a
	
	ld   a,(rndidb)	; Debug-Kennzeichen
	cmp  0
	jrz  rndin5
	call tdrei
	ld   a,20
	call tastto

rndin5:	pop  bc
	ret

;;; aktuelle Zufallszahl hexadezimal ausgeben
pr8hex:	push hl
	push de
	push bc

	ld   hl,rna+7
	ld   b,8
pr8h1:	ld   a,(hl)
	dec  hl
	rst  %20
	defb 6		; OUTHX
	rst  %20
	defb %0E	; OUTSP
	djnz pr8h1
	ld   a,%0D	; newline
	rst  %20
	defb 0		; OUTCH

	pop  bc
	pop  de
	pop  hl
	ret

;;; Generierung einer 64-bit-Zufallszahl
;;; Return: 8 Byte ab Adresse rna
random:
	push hl
	push de
	push bc

	ld   hl,rstat0
	ld   de,rnx
	ld   bc,16
	ldir		; x = s[0]; y = s[1];
	ld   hl,rny
	ld   de,rstat0
	ld   bc,8
	ldir		; s[0] = y;

	;; x ^= x << 23;
	ld   hl,rnx+5
	ld   a,(hl)	; Hier interessiert nur das LSB,
	rra		; das ins Carry-Flag geschoben wird.
	dec  hl
	ld   de,rna+7
	ld   b,5
	call ransr
	ld   a,0	; xor a würde das Carry-Flag löschen
	rra
	ld   (de),a
	dec  de
	xor  a
	ld   (de),a
	dec  de
	ld   (de),a	; rna = x << 23
	ld   de,rnx
	ld   hl,rna
	call randx8	; x ^= rna

	;; s[1] = x ^ y ^ (x >> 17) ^ (y >> 26);
	ld   hl,rnx
	ld   de,rstat1
	ld   bc,8
	ldir		; s[1] = x
	
	ld   hl,rny
	ld   de,rstat1
	call randx8	; s[1] ^= y
	
	xor  a
	ld   de,rna+7
	ld   (de),a
	dec  de
	ld   (de),a
	dec  de
	ld   hl,rnx+7
	ld   b,6
	call ransr	; rna = x >> 17
	ld   hl,rna
	ld   de,rstat1
	call randx8	; s[1] ^= (x >> 17)
	
	xor  a
	ld   de,rna+7
	ld   (de),a
	dec  de
	ld   (de),a
	dec  de
	ld   (de),a
	dec  de
	ld   hl,rny+7
	ld   b,5
	call ransr	; rna = y >> 25
	ld   hl,rna+7	; noch ein Bit verschieben
	ld   b,8
	or   a		; Carry = 0
rand1:	rr   (hl)
	dec  hl
	djnz rand1	; rna = y >> 26
	ld   hl,rna
	ld   de,rstat1
	call randx8	; s[1] ^= (y >> 26)

	;; return s[1] + y; -- hier nur 16 bit zurückgeben
	ld   hl,rstat1
	ld   de,rna
	ld   bc,8
	ldir		; a = s[1]
	ld   de,rna
	ld   hl,rny
	ld   b,8
rand2:	ld   a,(de)
	adc  (hl)
	ld   (de),a
	inc  de
	inc  hl
	djnz rand2	; a = s[1] + y

	pop  bc
	pop  de
	pop  hl
	ret

	;; DE_8 ^= HL_8
randx8:	ld   b,8
ranx81:	ld   a,(de)
	xor  (hl)
	ld   (de),a
	inc  de
	inc  hl
	djnz ranx81	; x = x ^ rna
	ret

	;; DE_B = HL_B >> 1
ransr:	ld   a,(hl)
	dec  hl
	rra
	ld   (de),a
	dec  de
	djnz ransr
	ret

	;; DE_8 = HL_8 >> N; N in (17, 25)
ransx:	ld   (de),a	; Annahme: a=0
	dec  de
	ld   (de),a
	dec  de
	ld   hl,rnx
	call ransr	; rna = x >> 17
	ld   hl,rna
	ld   de,rstat1
	call randx8	; s[1] ^= (x >> 17)
	ret

;;; ------------------------------------------------------------
;;; 1 Byte aus dem "Cache" rna liefern
;;; Return: A
randb:
	push hl
	push de

	ld   hl,rnza
	ld   a,(hl)
	or   a
	jrnz randb1
	call random
	ld   (hl),8
randb1:	dec  (hl)
	ld   e,a
	ld   d,0
	add  hl,de
	ld   a,(hl)

	pop  de
	pop  hl
	ret

;;; ------------------------------------------------------------

;;; 2 positive 8-bit-Zahlen multiplizieren
;;; Parameter: H, E: Faktoren
;;; Return: HL: Produkt
;;; Quelle: https://tutorials.eeems.ca/Z80ASM/part4.htm
mul8b:
	push bc
	ld   d,0	; clearing D and L
	ld   l,d
	ld   b,8	; we have 8 bits
m8loop:	add  hl,hl	; advancing a bit
	jpnc m8skip	; if zero, we skip the addition
			; (jp is used for speed)
	add  hl,de	; adding to the product if necessary
m8skip:	djnz m8loop
	pop  bc
	ret

	
;;; ------------------------------------------------------------
	
rndvbo	defb 0		; Zwischenspeicher für Offset

;;; 8-bit-Pseudozufallszahl zwischen D und D+E liefern
;;; Return: A: Zufallszahl
rndvb:
	push hl
	ld   a,d
	ld   (rndvbo),a
	call randb
	ld   h,a
	call mul8b	; HL = H * E
	ld   a,(rndvbo)
	add  h		; L wird nicht gebraucht

	pop  hl
	ret
	
;;; ============================================================
;;; grundlegende Grafikroutinen

;;; Grafik einschalten
;;; mal ganz ohne Code-Modifizierung
geinp:	defb %08	; KRT: Port 8, FA: Port 0CH
gein:	push bc
	ld   a,(geinp)
	ld   c,a
	ld   a,%08
	out  a
	pop  bc
	ret

;;; Grafik ausschalten
gausp:	defb %08	; KRT: Port 8, FA: Port 10H
gaus:	push bc
	ld   a,(gausp)
	ld   c,a
	ld   a,%09
	out  a
	pop  bc
	ret
	
; ---------------------------------------------

;;; Bildschirm löschen
cls:	push hl
	push de
	push bc
	xor  a		; Block 0
cls1:	out  deco	; Block aktivieren
	ld   hl,%EC00	; BWS Anfang
	ld   (hl),%FF	; erste 8 Pixel auf Hintergrund"farbe"
	ld   d,h
	ld   e,l
	inc  de		; Beginn des überlappenden Bereichs
	ld   bc,%03FF	; Länge_BWS - 1
	ldir		; Hintergrund über BWS schmieren
	inc  a
	cmp  %08	; Anzahl Blöcke
	jrnz cls1
	pop  bc
	pop  de
	pop  hl
	ret


;;; ============================================================
;;; Grafikroutine von A. Köhler aus Funkamateur 7/91

vars	defs %17	; Bereich für Arbeitszellen
pixlx	equ  vars	; letzter X-Wert, Geradenanfang X
pixly	equ  pixlx+5	; letzter Y-Wert, Geradenanfang Y
pixex	equ  pixlx+8	; Endwert X, Geradenende X1
pixey	equ  pixlx+13	; Endwert Y, Geradenende Y
pixsp	equ  pixlx+%10	; Pixelspalte 0 x 255
pixx1	equ  pixsp+1	; Spaltennummer X1
pixze	equ  pixsp+4	; Pixelzeile
pixy1	equ  pixsp+5	; Zeilennummer Y1
deco	equ  %08

;;; bitab und bitab2 siehe Dateianfang
	
; ---------------------------------------------

point:	push hl
	push de
	push bc
	push ix
	ld   a,(pixsp)
	srl  a		; Berechnung
	srl  a		; von Wert
	srl  a		; X1
	ld   (pixx1),a	; X1 ablegen
	ld   a,(pixze)
	srl  a
	srl  a
	srl  a
	ld   (pixy1),a	; Y1 merken
	ld   hl,(pixy1)	; hl=Zeile
	add  hl,hl
	add  hl,hl	; Wert von Y1
	add  hl,hl	; Y1=32_Zeichen*Y1
	add  hl,hl
	add  hl,hl
	ld   de,(pixx1)
	ld   bc,%EC00	; Bildschirmanfang
	add  hl,de	; BS Posi.+Zeilenzahl X1
	add  hl,bc	; BS Posi.+Spaltenzahl
	ld   a,(pixze)
	and  %07	; nur die 3 niedrigsten Bit für Block
	out  deco	; Ausgabe Block
	ld   a,(pixsp)
	and  %07	; nur Low-Teil (3 Bit)
	ld   (pix+2),a	; Indexreg.-Befehl modifizieren
	ld   ix,bitab	; ix=Anfang Bittabelle
	ld   a,(hl)	; zu veränderndes Bit lesen
pix	and  (ix+3)	; Bildbyte ändern
	ld   (hl),a	; in BWS schreiben
	pop  ix
	pop  bc
	pop  de
	pop  hl
	ret

; ---------------------------------------------
; Programm zum Zeichnen einer Geraden
; Es wird die Routine POINT über die Arbeitszellen
; für X und Y verwendet.
draw:	push hl
	push de
	push bc
	push af
	ld   a,(pixlx)	; Parameterübergabe
	ld   e,a	; e=letzter X-Wert
	ld   a,(pixly)
	ld   d,a	; d=letzter Y-Wert
	ld   a,(pixex)
	ld   l,a	; l=Endwert X
	ld   a,(pixey)
	ld   h,a	; h=Endwert Y
	ex   de,hl
	ld   a,e	; dx berechnen
	sub  l		; dx=xend-xanf
	defb %06	; b=...
	inc  l		; ...Befehlscode für inc l
	jrnc draw2	; xend > x0, rechts-links
	defb %06	; b=...
	dec  l		; ...Befehlscode für dec l
	neg		; dx= -dx = abs(dx)
draw2:	push af		; dx merken
	ld   a,b
	ld   (xstep),a	; Schrittrichtung x einstellen
draw3:	ld   a,d	; dy berechnen
	sub  h		; dy=yend-yanf
	defb %06	; b=...
	inc  h		; ...Befehlscode für inc h
	ld   c,%FF	; c=-1, <0
	jrz  draw4	; wenn dy=0
	inc  c		; d=0
	jrnc draw4
	defb %06	; b=...
	dec  h		; ...Befehlscode für dec h
	neg		; dy= -dy = abs(dy)
draw4:	push af		; dy merken
	ld   a,b
	ld   (ystep),a	; Schrittrichtung y einstellen
	ld   a,c
	add  %00	; S-Flag initialisieren: waagerecht -> 1
	exx		; Koordinaten sichern
	ld   h,a	; Fehlervariable D initialisieren:
	ld   l,a	;  waagerechte Linie: -1, sonst 0
	exaf		; Vorzeichen D (S-Flag) sichern
	pop  af		; dy
	ld   e,a
	pop  af		; dx
	ld   c,a
	ld   d,%00	; Reg. de = dy auf 16 bit
	ld   b,d	; Reg. bc = dx auf 16 bit
	exaf		; Vorzeichen von D holen
next:	exaf		; Vorzeichen von D sichern
	exx		; Koord. holen: hl=aktuell, de=Ende
	ld   a,l
	ld   (pixsp),a	; Parameterübergabe X
	ld   a,h
	ld   (pixze),a
	call point
	ld   a,l	; a=xwert
	cmp   e		; x=xend?
	jpnz noend	; nein
	ld   a,h	; a=ywert
	cmp   d		; y=yend?
	jrz  dend	; ja
noend:	exaf		; Vorzeichen von D holen
	jpp  ystep	; wenn D>=0
xstep:	inc  l		; bei dx=0, sonst dec l
	exx		; D, dx und dy holen
	or   a		; C-Flag auf 0
	adc  hl,de	; D=D+dy
	jp   next	; nächster Punkt
ystep:	inc  h		; bei dy=0, sonst dec h
	exx		; D, dx und dy holen
	or   a		; C-Flag auf 0
	sbc  hl,bc	; D=D-dx
	jp   next	; nächster Punkt
dend:	pop  af
	pop  bc
	pop  de
	pop  hl
	ret

;;; ============================================================
	
; ---------------------------------------------
;;; Köhlers Punkt-Algorithmus, erweitert um
;;; löschen, invertieren und testen

; Punkt setzen/löschen/invertieren
; Parameter: d = y-Koord., e = x-Koord.
spixl:	push hl
	push de

;	call yx2ad
	;; ----- yx2ad-----
	;; Einbettung spart 27 Takte
;yx2ad:	
	ld   a,d	; y-Koord.
	and  %F8	; Bits 7-3 sind Teil der BWS-Adr.
	ld   l,a
	ld   h,%00
	add  hl,hl
	add  hl,hl	; y[7..3] -> BWS[9..5]
	ld   a,e	; x-Koord.
	and  %F8	; Bits 7-3 sind Teil der BWS-Adr.
	rrca
	rrca
	rrca
	or   l		; l[4..0] ist 0
	ld   l,a	; x[7..3] -> BWS[4..0]
	ld   a,h
	add  %EC
	ld   h,a	; --> hl += %EC00
	ld   a,d	; a:=(pixze)
	and  %07	; y[2..0] -> G-BWS-Block/-Seite
	out  deco
	ld   a,e	; a:=(pixsp)
	and  %07	; x[2..0] ist Bitpos. im BWS-Byte
;	ret
	;; ----- Ende von yx2ad-----
btb12:	ld   de,bitab
	add  e		; 8-bit-Arith. reicht
	ld   e,a
	ld   a,(hl)	; Byte aus BWS holen
	ex   de,hl
pxop:	and  (hl)	; Opcode wird modifiziert
	ld   (de),a	; in BWS zurückschreiben
	
	pop  de
	pop  hl
	ret
	
;;; Punkt testen
;;; Parameter: d = y-Koord., e = x-Koord.
;;; Rückgabe: Z-Flag, 0=gesetzt
tpixl:	push hl
	push de

;	call yx2ad
	;; ----- yx2ad-----
	;; Einbettung spart 27 Takte
;yx2ad:	
	ld   a,d	; y-Koord.
	and  %F8	; Bits 7-3 sind Teil der BWS-Adr.
	ld   l,a
	ld   h,%00
	add  hl,hl
	add  hl,hl	; y[7..3] -> BWS[9..5]
	ld   a,e	; x-Koord.
	and  %F8	; Bits 7-3 sind Teil der BWS-Adr.
	rrca
	rrca
	rrca
	or   l		; l[4..0] ist 0
	ld   l,a	; x[7..3] -> BWS[4..0]
	ld   a,h
	add  %EC
	ld   h,a	; --> hl += %EC00
	ld   a,d	; a:=(pixze)
	and  %07	; y[2..0] -> G-BWS-Block/-Seite
	out  deco
	ld   a,e	; a:=(pixsp)
	and  %07	; x[2..0] ist Bitpos. im BWS-Byte
;	ret
	;; ----- Ende von yx2ad-----
	ld   de,bitab2	; bitab2-Offset einrechnen
	add  e		; 8-bit-Arith. reicht
	ld   e,a
	ld   a,(hl)
	ex   de,hl
tpop:	and  (hl)	; Opcode wird modifiziert

	pop  de
	pop  hl
	ret
	
;;; Koordinaten in BWS-Adresse umrechnen,
;;; Speicherseite schalten,
;;; Pixelspalte in Reg. a zurückgeben
yx2ad:	
	ld   a,d	; y-Koord.
	and  %F8	; Bits 7-3 sind Teil der BWS-Adr.
	ld   l,a
	ld   h,%00
	add  hl,hl
	add  hl,hl	; y[7..3] -> BWS[9..5]
	ld   a,e	; x-Koord.
	and  %F8	; Bits 7-3 sind Teil der BWS-Adr.
	rrca
	rrca
	rrca
	or   l		; l[4..0] ist 0
	ld   l,a	; x[7..3] -> BWS[4..0]
	ld   a,h
	add  %EC
	ld   h,a	; --> hl += %EC00
	ld   a,d	; a:=(pixze)
	and  %07	; y[2..0] -> G-BWS-Block/-Seite
	out  deco
	ld   a,e	; a:=(pixsp)
	and  %07	; x[2..0] ist Bitpos. im BWS-Byte
	ret
	
; ---------------------------------------------
;;; "Farben" einstellen

setcc:	jrnc setcb
; Vordergrund-"farbe" festlegen
setcf:	ld   a,low(bitab)
	ld   (btb12+1),a
	ld   a,%A6	; Opcode f. and (hl)
	ld   (pxop),a
	ret
; Hintergrund-"farbe" festlegen
setcb:	ld   a,low(bitab2)
	ld   (btb12+1),a
	ld   a,%B6	; Opcode f. or (hl)
	ld   (pxop),a
	ret
; Pixel invertieren
setci:	ld   a,low(bitab2)
	ld   (btb12+1),a
	ld   a,%AE	; Opcode f. xor (hl)
	ld   (pxop),a
	ret
	
; ---------------------------------------------

; Parameter für die Prozeduren linie und kreis
px1	defb 0
py1	defb 0
px2	defb 0		; bei kreis: Radius
py2	defb 0

; Linie in eingestellter "Farbe" zeichnen
; FA-Algorithmus umgemodelt nach
; https://de.wikipedia.org/wiki/Bresenham-Algorithmus#BASIC-Implementierung
; Parameter: px1 - py2
linie:
	; Registersatz 1
	;  HL = fehler
	;  DE = deltafastdirection
	;  BC = deltaslowdirection
	; Registersatz 2
	;  HL =
	;  DE = y, x 
	;  BC = countfast, ?
	push hl
	push de
	push bc

	ld   de,(px1)	; d = yanf, e = xanf
	ld   hl,(px2)	; h = yend, l = xend
	ld   a,h
	sub  d		; a: dy
	ld   h,%14	; Code von inc d
	jrnc lini1	; yanf < yend
	ld   h,%15	; Code von dec d
	neg		; ady := abs(dy)
lini1:	ld   b,a	; b: ady
	ld   a,l
	sub  e		; a: dx
	ld   l,%1C	; Code von inc e
	jrnc lini2	; xanf < xend
	ld   l,%1D	; Code von dec e
	neg		; adx := abs(dx)
lini2:	ld   c,a	; c: adx
	cmp  b		; slow/fast?
	jrc  lini3
	ld   c,b	; c <-> b
	ld   b,a	; b: dfast, c: dslow
	ld   a,h	; l <-> h
	ld   h,l
	ld   l,a	; h: para, l: diag
lini3:	ld   a,h
	ld   (lpara),a
	ld   a,l
	ld   (ldiag),a
	push bc
	exx  		; Reg-satz 2 sichern
	pop  bc		; Reg-satz 1 aufbauen
	ld   e,b
	ld   b,0	; bc: dslow
	ld   d,b	; de: dfast
	ld   h,d
	ld   l,e
	srl  l		; hl: feh := dfast/2

	exx
	call spixl
	xor  a
	cmp  b		; Linie nur 1 Punkt lang?
	jrz  lini9	; ja -> Ende
lizei:	exx		; Reg-satz 1 holen
	or   a		; C-Flag := 0
	sbc  hl,bc	; feh:=feh-dslow
	jrnc lini5
	or   a
	adc  hl,de	; feh:=feh+dfast
	exx		; Reg-satz 2 holen
ldiag:	inc  e		; Befehl wird überschrieben
	exx
lini5:	exx		; Reg-satz 2 holen
lpara:	inc  d		; Befehl wird überschrieben
	call spixl
	djnz lizei

lini9:	pop  bc
	pop  de
	pop  hl
	ret

;;; ------------------------------------------------------------

;;; Linie zeichnen, Zeiger auf Parameter in HL
linhl:
	push hl
	push de
	push bc

	ld   de,px1
	ldi
	ldi
	ldi
	ldi
	call linie

	pop  bc
	pop  de
	pop  hl
	ret

; ---------------------------------------------
	
; Kreis in eingestellter "Farbe" zeichnen
; https://de.wikipedia.org/wiki/Bresenham-Algorithmus#Kreisvariante_des_Algorithmus ,
; Variante rasterCircle in C
; Parameter: px1, py2, px2(=Radius)
kreis:
	; Registersatz 1
	;  HL = fehler
	;  DE = deltaY
	;  BC = deltaX
	; Registersatz 2: Koordinaten
	;  HL = ykreis, xkreis = relativ zum Mittelpunkt
	;  DE = y, x = Bildkoordinaten
	;  BC = ymittel, xmittel
	push hl
	push de
	push bc

	ld   a,(px2)
	exx		; Reg-satz 2 füllen
	; h und l zunächst vertauscht
	ld   l,a	; l: y:=radius
	ld   h,0	; h: x:=0
	ld   bc,(px1)	; b: ymittel, c: xmittel
	exx		; Reg-satz 1 füllen
	ld   e,a	; de: radius
	xor  a		; a:=0, Carry=0
	ld   d,a
	ld   h,a
	ld   l,a	; hl: 0
	sbc  hl,de	; hl: -radius
	ld   d,h
	ld   e,l	; de: -radius
	add  hl,hl
	ex   de,hl	; de: ddf_y:=-2*radius
	inc  hl		; hl: fehler:=1-radius
	ld   b,0
	ld   c,b	; bc: ddf_x:=0

	; 4 Pixel hor/vert zu Mitte
	exx		; Reg-satz 2 holen
	ld   d,b
	call kre13a	; (ym,xm+r) und (ym,xm-r)
	ld   h,l	; h und l zurücktauschen
	ld   l,0

	ld   e,c	; e=c: xmittel
	ld   a,b
	add  h		; a: ymittel+ykreis
	jrc  kre14	; ausserhalb
	ld   d,a
	call spixl	; ym+r, xm
kre14:	ld   a,b
	sub  h		; a: ymittel-ykreis
	jrc  krenxt	; ausserhalb
	ld   d,a
	call spixl	; ym-r, xm

	ld   a,l	; a: xkreis
	cmp  h		; xkreis<ykreis?
	jrnc kreend	; nein -> Ende
krenxt:	exx		; Reg-satz 1 holen, fehler
	xor  a
	cmp  h		; hl: feh>=0?
	jrc  krexx
	inc  de
	inc  de		; de: deltaY+=2
	add  hl,de	; hl: feh+=deltaY
	exx		; Reg-satz 2, Koord.
	dec  h		; ykreis-=1
	exx		; Reg-satz 1, fehler
krexx:	inc  bc
	inc  bc		; bc: deltaX+=2
	add  hl,bc
	inc  hl		; hl: feh:=deltaX+1
	exx		; Reg-satz 2, Koord.
	inc  l		; xkreis+=1

	; while (xkreis<ykreis)
	ld   a,h	; a: ykreis
	cmp  l		; ykreis<xkreis?
	jrc  kreend	; ja -> Ende
	
	call kre4px
	ld   a,l
	cmp  h		; nicht doppelt zeichnen...
	jrz  kreend	; ...bei invers
	
	ld   l,h
	ld   h,a
	call kre4px
	ld   a,l
	ld   l,h
	ld   h,a

	jr   krenxt

kreend:	
	pop  bc
	pop  de
	pop  hl
	ret

; 4 Pixel des Kreises zeichnen falls sichtbar
; Registersatz 2 muss eingestellt sein
kre4px:	
	ld   a,b	; a=b: ymittel
	add  h		; a: ymittel+ykreis
	jrc  kre43	; ausserhalb
	ld   d,a
	ld   a,c
	add  l		; a: xmittel+xkreis
	jrc  kre42	; ausserhalb
	ld   e,a
	call spixl	; ym+yk, xm+xk
kre42:	ld   a,c
	sub  l		; a: xmittel-xkreis
	jrc  kre43	; ausserhalb
	ld   e,a
	call spixl	; ym+yk, xm-xk
kre43:	ld   a,b	; a=b: ymittel
	sub  h		; a: ymittel-ykreis
	rc		; ausserhalb
	ld   d,a
kre13a:	ld   a,c
	add  l		; a: xmittel+xkreis
	jrc  kre44	; ausserhalb
	ld   e,a
	call spixl	; ym-yk, xm+xk
kre44:	ld   a,c
	sub  l		; a: xmittel-xkreis
	rc		; ausserhalb
	ld   e,a
	call spixl	; ym-yk, xm-xk
	ret


;;; ============================================================
;;; erläuternde Texte
	
;;; 0-terminierte Zeichenkette ausgeben
;;; - kann Grafikzeichen enthalten
;;; - wiederholende Zeichen können mit %01 (Escape-Zeichen) und
;;;   Längenbyte zusammengefasst werden (lohnt sich also ab 4)
;;; Parameter: hl: Anfangsadresse
prstr0:
	ld   a,(hl)
	or   a		; a=0?
	rz		; ja -> Ende
	cmp  1		; a=1: Escape-Code
	jrz  prst01
	rst  %20
        defb 0
        inc  hl
        jp   prstr0
prst01: inc  hl
	ld   b,(hl)
	inc  hl
        ld   a,(hl)
prst02: rst  %20
	defb 0
	djnz prst02
        inc  hl
        jp   prstr0

; ---------------------------------------------

trennl	defb %0D,1,32,%9E,0
	
;;; waagerechte Trennlinie im Textmodus
trenn:	ld   hl,trennl
	call prstr0
	ret

; ---------------------------------------------

banner  defb %0C,%0D
	defb ' ',%AE,1,28,%9E,%AD,%0D
	defb ' ',%9F,1,6,' ',"KRT-Grafik-Demo",1,7,' ',%C0,%0D
	defb ' ',%AB,1,28,%F8,%AC,%0D,%0D,%0D,%0D,0

;;; Einleitung
einlt:
	ld   hl,banner
	call prstr0
	rst  %20
	defb %02	; PRST7
	defb %0D
	defb "Welche Variante,",%0D
	defb "(1) KRT, (2) FA, (0) keine?"
	defb %A0
fr1:
	ld   hl,rndita	; zur Initialisierung des Zufallsz.-gen.
	inc  (hl)
	rst  %20
	defb %04	; INKEY
	or   a
	jrz  fr1
	cmp  '0'
	jpnz einlt1
	rst  %20
	defb %02	; PRST7
	defb %0D
	defb "Schade.",%8D
	jp  mend
einlt1:	cmp   '1'
	ld   bc,%0808	; ein: Port 8, aus: Port 8
	jrz  antw1
	cmp   '2'
	jrnz fr1
	ld   bc,%0C10	; ein: Port 0CH, aus: Port 10H
antw1:	ld   hl,geinp
	ld   (hl),b
	ld   hl,gausp
	ld   (hl),c
	ret

;;; erkläre die Tasten
erklt:	call gaus
	ld   hl,banner
	call prstr0
	rst  %20
	defb %02	; PRST7
	defb %0D,%0D,%0D
	defb "Wenn rechts unten ein Dreieck",%0D
	defb "erscheint, dann:",%0D,%0D
	defb "- Enter: setzt das Programm fort",%0D
	defb "- nichts tun: Nach ein paar",%0D
	defb "  Sekunden geht es weiter.",%0D,%0D
	defb "- Leertaste: Die Anzeige bleibt",%0D
	defb "  bis zum naechsten Tastendruck",%0D
	defb "  stehen.",%8D
	call tdrei
	ld   a,20
	call tastto
	ret

; ---------------------------------------------

txpu:	call gaus
	rst  %20
	defb %02	; PRST7
	defb %0C,%0D
	defb " Punktalgorithmus aus dem Funk-",%0D
	defb " amateur 7/91 sowie Erweiterung",%8D
	call trenn
	rst  %20
	defb %02	; PRST7
	defb %0D
	defb "Der Punktalgorithmus von Koehler"
	defb "kann nur Punkte in der Vorder-",%0D
	defb "grundfarbe setzen. Er stellt per"
	defb "Code-Modifikation im Befehl",%0D,%0D
	defb "    and (ix+N)",%0D,%0D
	defb "mit der Bitposition N ein Muster"
	defb "ein, das das richtige Bit auf 0",%0D
	defb "setzt.",%0D,%0D
	defb "Die Erweiterung aendert das",%0D
	defb "'and' auf 'or' fuer Hintergrund",%0D
	defb "oder 'xor' fuer Invertieren.",%0D,%0D
	defb "Mit weiteren Anpassungen dauert",%0D
	defb "das Punktsetzen nur noch",%0D
	defb "218 Takte (weitere 17 f. CALL).",%0D
	defb "Die Farbe festzulegen dauert",%0D
	defb "50 Takte.",%0D,%0D
	defb "Es folgt ein Geschwindigkeits-",%0D
	defb "vergleich.",%8D

	call tdrei
	ld   a,20
	call tastto
	ret


; ---------------------------------------------

txam1:	call gaus
	rst  %20
	defb %02	; PRST7
	defb %0C,%0D
	defb " Der Ameisen-Algorithmus von",%0D
	defb " Christopher Langton (1986)",%8D
	call trenn
	rst  %20
	defb %02	; PRST7
	defb %0D
	defb "In einem quadratischen Gitter",%0D
	defb "sitzt eine Ameise (hier unsicht-"
	defb "bar) mit Blick nach ",%22,"unten",%22,".",%0D,%0D
	defb "1. Auf einem weissen Feld dreht",%0D
	defb "   sie sich 90 Grad nach rechts,"
	defb "   auf schwarz nach links.",%0D,%0D
	defb "2. Sie wechselt die Feldfarbe.",%0D,%0D
	defb "3. Sie geht zum naechsten Feld",%0D
	defb "   in Blickrichtung.",%0D,%0D
	defb "In den ersten ca. 500 Schritten",%0D
	defb "treten wiederholt symmetrische",%0D
	defb "Muster auf. Danach bildet die",%0D
	defb "Ameise in rund 10.000 Schritten",%0D
	defb "ein chaotisches Muster.",%0D,%0D
	defb "Schliesslich baut sie eine",%0D
	defb "regelmaessige Struktur (",%22,"Amei-",%0D
	defb "senstrasse",%22,") bis ins Unendliche.",%0D
	defb "              [Text: Wikipedia]",%8D

	call tdrei
	ld   a,20
	call tastto
	ret



; ---------------------------------------------

txam2:	call gaus
	rst  %20
	defb %02	; PRST7
	defb %0C,%0D
	defb " Ein Hindernis fuer die Ameise",%0D,%8D
	call trenn
	rst  %20
	defb %02	; PRST7
	defb %0D,%0D
	defb "Was passiert eigentlich,",%0D
	defb "wenn der Ameise",%0D
	defb "ein Stein im Weg liegt?",%0D,%0D
	defb "Der Stein wird an eine zufaelli-"
	defb "ge Stelle auf den Weg gelegt.",%0D
	defb "Beim naechsten Versuch wird die",%0D
	defb "Ameise vielleicht einen anderen",%0D
	defb "Weg gehen.",%8D

	call tdrei
	ld   a,20
	call tastto
	ret


; ---------------------------------------------

txfa:	call gaus
	rst  %20
	defb %02	; PRST7
	defb %0C,%0D
	defb " Linienalgorithmus",%0D
	defb " aus dem Funkamateur 7/91",%8D
	call trenn
	rst  %20
	defb %02	; PRST7
	defb "Von Bresenham stammt der bekann-"
	defb "teste Linienalgorithmus. Er legt"
	defb "eine schnelle und eine langsame",%0D
	defb "Richtung fest und berechnet mit",%0D
	defb "einer Fehlervariablen, ob ein",%0D
	defb "Gerad- oder Diagonalschritt",%0D
	defb "getan werden muss, um den",%0D
	defb "naechsten Punkt zu setzen.",%0D,%0D
	defb "Koehler (FA 7/91) verwendet",%0D
	defb "ebenfalls eine Fehlervariable,",%0D
	defb "unterscheidet die Richtungen",%0D
	defb "aber nicht und macht keinen",%0D
	defb "Diagonal- sondern einen",%0D
	defb "Senkrechtschritt. Dadurch sind",%0D
	defb "diagonale Linien dicker und das",%0D
	defb "Zeichnen entsprechend langsamer.",%0D,%0D

	defb "Gezeigt werden Linien in allen",%0D
	defb "Standard- und Grenzfaellen des",%0D
	defb "Algorithmus von Koehler.",%8D

	call tdrei
	ld   a,30
	call tastto
	ret

; ---------------------------------------------

txfabr:	call gaus
	rst  %20
	defb %02	; PRST7
	defb %0C,%0D
	defb " Linienalgorithmen im Vergleich",%0D,%8D
	call trenn
	rst  %20
	defb %02	; PRST7
	defb "Im direkten Vergleich sieht man",%0D
	defb "die etwas hoehere Geschwindig-",%0D
	defb "keit von Bresenham und die",%0D
	defb "dickeren Diagonalen von Koehlers"
	defb "Linienalgorithmus.",%0D,%0D
	defb "Die Implementation hier ist",%0D
	defb "eine Anpassung der Vorlage aus",%0D
	defb "dem Funkamateur 7/91 an",%0D
	defb "https://de.wikipedia.org/wiki/-",%0D
	defb "  Bresenham-Algorithmus#-",%0D
	defb "  BASIC-Implementierung.",%8D

	call tdrei
	ld   a,20
	call tastto
	ret


; ---------------------------------------------

txbsl:	call gaus
	rst  %20
	defb %02	; PRST7
	defb %0C,%0D
	defb " Bildschirmschoner ",%22,"Linien",%22,%0D,%8D
	call trenn
	rst  %20
	defb %02	; PRST7
	defb %0D,%0D,%0D,%0D
	defb "Wer Linien ",%22,"kann",%22,",",%0D
	defb "der kann auch Bildschirmschoner.",%8D

	call tdrei
	ld   a,5
	call tastto
	ret
	

; ---------------------------------------------

txbr:	call gaus
	rst  %20
	defb %02	; PRST7
	defb %0C,%0D
	defb " Linien und Kreise",%0D,%8D
	call trenn
	rst  %20
	defb %02	; PRST7
	defb "Die Idee von Bresenham eignet",%0D
	defb "sich nicht nur fuer Linien,",%0D
	defb "sondern auch fuer Kreise. Die",%0D
	defb "vorliegende Implementierung",%0D
	defb "orientiert sich an",%0D,%0D
	defb "https://de.wikipedia.org/wiki/-",%0D
	defb "  Bresenham-Algorithmus#-",%0D
	defb "  Kreisvariante_des_Algorithmus,"
	defb "Variante rasterCircle in C.",%0D,%0D
	defb "Jeder Kreis vom Radius 0 bis 255"
	defb "wird erst invertierend gezeich-",%0D
	defb "net und dann geloescht.",%8D

	call tdrei
	ld   a,20
	call tastto
	ret

; ---------------------------------------------

txkbs:	call gaus
	rst  %20
	defb %02	; PRST7
	defb %0C,%0D
	defb " Noch 'n Bildschirmschoner",%0D,%8D
	call trenn
	rst  %20
	defb %02	; PRST7
	defb %0D,%0D,%0D,%0D
	defb "Jetzt mit Kreisen.",%0D
	defb %0D,%0D
	defb "Kann mit <Enter>",%0D
	defb "abgebrochen werden.",%8D

	call tdrei
	ld   a,5
	call tastto
	ret

; ---------------------------------------------

txbi:	call gaus
	rst  %20
	defb %02	; PRST7
	defb %0C,%0D
	defb " 2 Fotos",%0D,%8D
	call trenn
	rst  %20
	defb %02	; PRST7
	defb "Im Hauptspeicher liegen 2 Fotos",%0D
	defb "als Bitmaps. Das erste wird",%0D
	defb "durch direkte Kopie in den BWS",%0D
	defb "angezeigt. Noch schneller ginge",%0D
	defb "es, wenn die Bildzeilen nach den"
	defb "Speicherbloecken des KRT-BWS",%0D
	defb "geordnet waeren.",%0D
	defb "Das zweite Bild wird von unten",%0D
	defb "eingeschoben.",%8D

	call tdrei
	ld   a,20
	call tastto
	ret
	


; ---------------------------------------------

txdrmn:	call gaus
	rst  %20
	defb %02	; PRST7
	defb %0C,%0D
	defb " Drachenkurve",%0D
	defb " und Maennl",%8D
	call trenn
	rst  %20
	defb %02	; PRST7
	defb "Die Drachenkurve ist ein",%0D
	defb "einfaches geometrisches Fraktal.",%0D
	defb "Sie wird bis zur 12. Iteration",%0D
	defb "gezeichnet.",%0D,%0D
	defb "Die Drachenkurve dient nur als",%0D
	defb "Hintergrund fuer eine Folge von",%0D
	defb "16x16 Pixel grossen Bildern mit",%0D
	defb "Maske (",%22,"Alpha-Kanal",%22,").",%0D,%0D
	defb "Die Bilder sollen eine einiger-",%0D
	defb "massen fluessige Folge ergeben.",%8D

	call tdrei
	ld   a,10
	call tastto
	ret

; ---------------------------------------------

txende:	call gaus
	rst  %20
	defb %02	; PRST7
	defb %0C,%0D
	defb " Ende der Vorstellung",%0D,%8D
	call trenn
	rst  %20
	defb %02	; PRST7
	defb %0D,%0D,%0D,%0D
	defb "<Enter>: Programmende",%0D,%0D
	defb "nichts tun: nochmal von vorn",%8D

	call tdrei
	ld   a,10
	call tastto
	ret
	

	;;    .........|.........|.........|..


;;; ============================================================

;;; Bilder mit Worten

;;; _-_-_-_|_-_-_-_|_-_-_-_|_-_-_-_|_-_-_-_|_
;;; .........................................
;;; .............#........#...............#..
;;; ......................................#..
;;; ..###..#.##..#...##.#.#..#.##...##.#..#..
;;; .#...#.##..#.#..#..##.#..##..#.#..##..#..
;;; .#...#.#.....#..#...#.#..#...#.#...#..#..
;;; .#...#.#.....#...####.#..#...#.#...#..#..
;;; ..###..#......#.....#..#.#...#..###.#..#.
;;; .................###.....................
;;; .........................................
tborig	defb 41,10	; Größe in Pixeln
	defb %FF,%FF,%FF,%FF,%FF,%FF
	defb %FF,%FB,%FD,%FF,%FD,%FF
	defb %FF,%FF,%FF,%FF,%FD,%FF
	defb %C6,%9B,%95,%A7,%2D,%FF
	defb %BA,%6B,%65,%9A,%CD,%FF
	defb %BA,%FB,%75,%BA,%ED,%FF
	defb %BA,%FB,%85,%BA,%ED,%FF
	defb %C6,%FD,%F6,%BB,%16,%FF
	defb %FF,%FF,%8F,%FF,%FF,%FF
	defb %FF,%FF,%FF,%FF,%FF,%FF

;;; _-_-_-_|_-_-_-_|_-_-_-_|_-_-_-_|_-_-_-_|_-_-_-_|
;;; ................................................
;;; .........................#...#...............#..
;;; .............................#...............#..
;;; ..###..#.##..#.#.#..###..#..###..###..#.##..###.
;;; .#...#.##..#.#.#.#.#...#.#...#..#...#.##..#..#..
;;; .#####.#.....#.#.#.#####.#...#..#####.#......#..
;;; .#.....#.....#.#.#.#.....#...#..#.....#......#..
;;; ..###..#......#.#...###...#...#..###..#.......#.
;;; ................................................
;;; ................................................
tberwe	defb 48,10	; Größe in Pixeln
	defb %FF,%FF,%FF,%FF,%FF,%FF
	defb %FF,%FF,%FF,%BB,%FF,%FB
	defb %FF,%FF,%FF,%FB,%FF,%FB
	defb %C6,%9A,%B1,%B1,%8D,%31
	defb %BA,%6A,%AE,%BB,%74,%DB
	defb %82,%FA,%A0,%BB,%05,%FB
	defb %BE,%FA,%AF,%BB,%7D,%FB
	defb %C6,%FD,%71,%DD,%8D,%FD
	defb %FF,%FF,%FF,%FF,%FF,%FF
	defb %FF,%FF,%FF,%FF,%FF,%FF

;;; _-_-_-_|_-_-_-_|_-_-_-_|_-_-_-_|_-_-_-_|_-_-_-_|_-
;;; ..................................................
;;; .........................................#........
;;; .........................................#........
;;; ..###..#.##..#.#.#.....#.....###...###..###..###..
;;; .#...#.##..#.#.#.#.....#....#.....#...#..#..#...#.
;;; .#####.#.....#.#.#...#####...###..#####..#..#.....
;;; .#.....#.....#.#.#.....#........#.#......#..#...#.
;;; ..###..#......#.#..#...#....####...###....#..###..
;;; ..................................................
;;; ..................................................
tberws	defb 50,10	; Größe in Pixeln
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF
	defb %FF,%FF,%FF,%FF,%FF,%BF,%FF
	defb %FF,%FF,%FF,%FF,%FF,%BF,%FF
	defb %C6,%9A,%BE,%F8,%E3,%18,%FF
	defb %BA,%6A,%BE,%F7,%DD,%B7,%7F
	defb %82,%FA,%B8,%38,%C1,%B7,%FF
	defb %BE,%FA,%BE,%FF,%5F,%B7,%7F
	defb %C6,%FD,%6E,%F0,%E3,%D8,%FF
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF

;;; _-_-_-_|_-_-_-_|_-_-_-_|_-_-_-_|
;;; #...#..#.#..#.....#.............
;;; #..#........#.....#.............
;;; #.#....###..#.##..#...###..#.##.
;;; ##....#...#.##..#.#..#...#.##..#
;;; #.#...#...#.#...#.#..#####.#....
;;; #..#..#...#.#...#.#..#.....#....
;;; #...#..###..#...#..#..###..#....
tbkoeh	defb 32,7	; Größe in Pixeln
	defb %76,%B7,%DF,%FF
	defb %6F,%F7,%DF,%FF
	defb %5E,%34,%DC,%69
	defb %3D,%D3,%5B,%A6
	defb %5D,%D7,%58,%2F
	defb %6D,%D7,%5B,%EF
	defb %76,%37,%6C,%EF

;;; _-_-_-_|_-_-_-_|_-_-_-_|_-_-_-_|_-_-_-_|_-_-_-_|_-_-_-
;;; ####................................#.................
;;; .#..#...............................#.................
;;; .#..#.#.##...###...###...###..#.##..#.##...##.#..##.#.
;;; .###..##..#.#...#.#.....#...#.##..#.##..#.#..##..#.#.#
;;; .#..#.#.....#####..###..#####.#...#.#...#.#...#..#.#.#
;;; .#..#.#.....#.........#.#.....#...#.#...#.#...#..#.#.#
;;; ####..#......###..####...###..#...#.#...#..###.#.#.#.#
tbbres	defb 54,7	; Größe in Pixeln
	defb %0F,%FF,%FF,%FF,%F7,%FF,%FF
	defb %B7,%FF,%FF,%FF,%F7,%FF,%FF
	defb %B5,%38,%E3,%8D,%34,%E5,%97
	defb %8C,%D7,%5F,%74,%D3,%59,%AB
	defb %B5,%F0,%63,%05,%D7,%5D,%AB
	defb %B5,%F7,%FD,%7D,%D7,%5D,%AB
	defb %0D,%F8,%C3,%8D,%D7,%62,%AB
	
tbregn	defb 104,24	; Größe in Pixeln
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%BF
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%BF
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%BF
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%BF
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%BF
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%A7
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%1F
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF,%FA,%7F,%FF,%FF,%FC,%BF
	defb %E3,%FF,%7F,%FF,%FF,%FE,%5F,%F7,%DE,%F3,%F8,%FF,%BF
	defb %BD,%FD,%FF,%FF,%EF,%7D,%EF,%EF,%DE,%ED,%EF,%7F,%DF
	defb %BE,%FB,%FF,%FF,%EE,%FB,%F7,%DF,%9E,%DD,%EF,%BF,%DF
	defb %7C,%F7,%FF,%FF,%ED,%F7,%E7,%BF,%BE,%DD,%DF,%BF,%DF
	defb %43,%F7,%FF,%FF,%EB,%F4,%3F,%BF,%BE,%BD,%D0,%FF,%DF
	defb %3F,%F9,%FF,%FF,%E7,%F7,%FF,%7F,%3E,%BD,%DF,%FF,%DF
	defb %7F,%FE,%7F,%FF,%E7,%F7,%FF,%7F,%3F,%7D,%DF,%FF,%DF
	defb %7F,%7F,%BF,%FF,%E7,%F7,%FB,%BE,%DF,%7D,%DF,%EF,%DF
	defb %7F,%7F,%BF,%FF,%E7,%FB,%F7,%A9,%BF,%7D,%EF,%BF,%DF
	defb %9D,%FF,%BF,%FF,%F7,%FD,%EF,%EF,%DF,%7D,%F7,%BF,%DF
	defb %E3,%FB,%BF,%FF,%FF,%FE,%3F,%FF,%DF,%FF,%F8,%FF,%FF
	defb %FF,%FC,%FF,%FF,%FF,%FF,%FF,%EF,%BF,%FF,%FF,%FF,%FF
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF,%EF,%BF,%FF,%FF,%FF,%FF
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF,%E7,%BF,%FF,%FF,%FF,%FF
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF,%F9,%BF,%FF,%FF,%FF,%FF
	defb %FF,%FF,%FF,%FF,%FF,%FF,%FF,%FF,%7F,%FF,%FF,%FF,%FF


;;; ------------------------------------------------------------

;;; Bitmap mit vorangestellter Größe anzeigen
;;; Parameter:
;;;   HL = Startadresse, dort:
;;;     - Breite in Pixeln
;;;     - Höhe in Pixeln
;;;     - Bitmap, zeilenweise auf volle Bytes aufgefüllt
;;;   DE = (y,x) linke obere Koordinate
zgbmp:
	push ix
	push bc
	push de		; Anfang der aktuellen Zeile

	;; Vorbereitung
	ld   a,(hl)
	ld   b,a	; schneller Zugriff für Restb.-Berech.
	inc  hl
	ld   a,(hl)
	ld   (zbbh),a
	inc  hl
	push hl		; Bildadr. zwischenztl. sichern

	call yx2ad	; DE (Koord) -> HL (BWS-Adr) + A (Px-Sp)
	push hl		; Sicherung BWS-Adresse
	;; Pixelspalte verwenden
	ld   ix,zbmu
	ld   c,a	; a sichern, Bildversatz
	ld   (zbms2+2),a
	;; zwischendurch Jump-Weite für Rotiersequenz
	dec  a
	jpp  zbms1
	ld   a,7
zbms1:	ld   (zbrot+1),a; Jump-Weite für Rotiersequenz
zbms2:	ld   a,(ix+0)	; AND-Maske holen
	ld   (zbrec+1),a
	cpl
	ld   (zblin+1),a
	
	;; Analyse der Bildgröße und -position
	ld   a,b	; Bildbreite
	and  7		; Bildverschnitt
	jrnz zb01
	ld   a,8	; letztes Byte vollständig
zb01:	add  c		; Bildversatz
	cmp  8
	jrz  zbenbg	; Ende auf Bytegrenze
	jrc  zbrbin	; Restbits innerhalb des Bytes
	;; sonst: Restbits ins nächste BWS-Byte
	ld   a,0	; akt. Pixelbyte in BWS
	jr   zbsetj
zbenbg:	ld   a,zbj2-zbj0	; akt. Pixelbyte in BWS
	jr   zbsetj
zbrbin:	ld   a,zbj1-zbj0	; Pixelbyte zurückhalten
zbsetj:	ld   (zbj+1),a

	ld   a,b	; Bildbreite
	add  c		; Restbits
	and  7		; Bildverschnitt
	;; setze AND-Masken
	ld   (zbms3+2),a
zbms3:	ld   a,(ix+0)
	ld   (zbere+1),a
	cpl
	ld   (zbeli+1),a
	;;
	ld   a,b	; Restbytes berechnen
	dec  a		; Sonderfall ganzes Byte wg. Aufru. m. INC
	and  %F8
	rrca
	rrca
	rrca		; bit -> Byte
	inc  a		; aufrunden
	exx
	ld   h,a	; Restbytes, Anfangswert für jede Zeile
	exx
	pop  hl		; BWS-Adresse wiederherstellen
	pop  bc		; aktuelle Bildadr. holen
	jr   zbanf

	;; Anfang
zbzeil:	call yx2ad
zbanf:	exx
	ld   l,h	; Restbytes auf Anfangswert
	exx
	ld   a,(hl)	; Byte aus BWS holen
	
	;; Mitte
zbnxb:	; nächstes Byte
zblin:	and  0-0	; schneide linken Teil aus (*)
	ld   e,a	; Pixelbyte sichern
	ld   a,(bc)	; Bildbyte holen
	inc  bc		; vorsorglich...
zbrot:	jr   zbrot-0	; rotiere ausreichend (*)
	rlca
	rlca
	rlca
	rlca
	rlca
	rlca
	rlca		; rotiere ausreichend
	ld   d,a	; rotiertes Bildbyte sichern
zbrec:	and  0-0	; schneide rechten Teil aus (*)
	or   e		; Pixel zusammensetzen
	exx
	dec  l		; Restbytes--
	exx
zbj:	jrz  zbj0-0	; vor- oder letztes Byte? (*)
zbj0:	ld   (hl),a	; schreibe Pixelbyte in BWS
	inc  hl		; INC BWS-Adresse - ändert keine Flags
	ld   a,d	; rotiertes Bild-Byte holen
zbj1:	jrnz zbnxb	; DO ... WHILE Restbytes > 0
	
	;; Ende
zbeli:	and  0-0	; schneide linken Teil aus (*) 
	ld   d,a
	ld   a,(hl)	; Byte aus BWS holen
zbere:	and  0-0	; schneide rechten Teil aus (*)
	or   d		; Pixelbyte und BWS kombinieren
zbj2:	ld   (hl),a	; schreibe Ergebnis in BWS

	;; für alle Bildzeilen wiederholen
zbnxz:	ld   hl,zbbh
	dec  (hl)	; restliche Zeilen im Bild
	jrz  zb99
	pop  de
	inc  d		; nächste Bildzeile im BWS
	push de
	jp   zbzeil

zb99:	pop  de
	pop  bc
	pop  ix
	ret
	
zbbh	defb 0		; Bildhöhe, für einfachere Erreichbarkeit

zbmu	defb %FF,%7F,%3F,%1F,%0F,%07,%03,%01
			; Bitmuster für das Ausschneiden mit and
;;; Registersatz 1: HL = BWS-Adresse
;;;                 D  = rotiertes Bildbyte
;;;                  E = Pixelbyte
;;;                 BC = Bildadresse
;;; Registersatz 2: H = Restbytes pro Zeile
;;;                 L = Restbytes aktuell in der Zeile


;;; ============================================================

;;; Tabelle mit Linienkoordinaten für den Stern
lina	equ  16
stlin	defb 16
; Aufbau:    x1  y1  x2  y2
lint	defb 129,127,253,127
	defb 132,129,243,175
	defb 129,129,216,216
	defb 129,132,175,243
	defb 127,129,127,253
	defb 125,132,079,243
	defb 125,129,038,216
	defb 122,129,011,175
	defb 125,127,001,127
	defb 122,125,011,079
	defb 125,125,038,038
	defb 125,122,079,011
	defb 127,125,127,001
	defb 129,122,175,011
	defb 129,125,216,038
	defb 132,125,243,079

;;; Stern zeigen - löschen - zeigen
stern:
	call setcf
	ld   hl,stlin
	call nlin
	call setcb
	ld   hl,stlin
	call nlin
	call setcf
	ld   hl,stlin
	call nlin
	ret
	
; ---------------------------------------------
	
;;; ============================================================

;;; mehrere Linien zeichnen
;;; Parameter: hl: Adresse des Linienfeldes
;;; Aufbau Linienfeld:
;;;   1. Byte: Anzahl der Linien (N)
;;;   folgende 4*N Bytes: Linienkoord. (x1,y1,x2,y2)
;;; Demo: 2 Rechtecke mit versetzten Punkten füllen
;;; erst mit originalem Algorithmus, dann mit erweitertem
nlin:	push de
	push bc

	ld   c,(hl)
	ld   b,0	; bc: Anzahl Linien
	sla  c
	rl   b
	sla  c
	rl   b		; bc: Anzahl Koord.
	inc  hl
nlin1:	ld   de,px1	; de: Parameterfeld für Routine linie
	ldi		; Parameter setzen
	ldi
	ldi
	ldi
	push af		; PV-Flag sichern
	call linie
	pop  af
	jppe nlin1

	pop  bc
	pop  de
	ret

;;; ------------------------------------------------------------

;;; Rahmen, damit es leichter fällt, die Zeit zu stoppen :-)
ptfrah	defb 6
; Aufbau:    x1  y1  x2  y2
	defb   1,  1,  1,254
	defb  85,254, 85,  1
	defb 169,  1,169,254
	defb 253,254,253,  1
	defb 253,  0,  1,  0
	defb   1,255,253,255


ptfdko	defb   3,  2, 83,253
	defb  87,  2,167,253
	defb 171,  2,251,253
ptfeld:
	call gein
	call cls

	ld   hl,ptfrah
	call nlin
	
	ld   hl,tborig
	ld   d,230
	ld   e,23
	call zgbmp
	ld   hl,tberwe
	ld   d,230
	ld   e,103
	call zgbmp
	ld   hl,tberws
	ld   d,230
	ld   e,186
	call zgbmp
	
	ld   hl,ptfdko
	ld   de,px1
	ldi
	ldi
	ldi
	ldi
	push hl
	call ptfeor

	ld   hl,tborig
	ld   d,230
	ld   e,23
	call zgbmp
	
	ld   a,100
	ld   (wzeit),a
	call warte

	pop  hl
	ld   de,px1
	ldi
	ldi
	ldi
	ldi
	push hl
	call ptfeer
	
	ld   hl,tberwe
	ld   d,230
	ld   e,103
	call zgbmp
	
	ld   a,100
	ld   (wzeit),a
	call warte

	pop  hl
	ld   de,px1
	ldi
	ldi
	ldi
	ldi
	call ptfees
	
	ld   hl,tberws
	ld   d,230
	ld   e,186
	call zgbmp
	
	ret

;;; ------------------------------------------------------------

;;; Punkte im Rechteck setzen
;;; Parameter px1 - py2 (siehe Linienalgo.): beschreibendes Rechteck

;;; Variante 1 für originalen Punktalgorithmus
ptfeor:
	push de

	ld   a,(py1)
	ld   d,a
	ld   a,(px1)
	ld   e,a
	
ptfor1:	ld   a,e
	ld   (pixsp),a	; Parameterübergabe X
	ld   a,d
	ld   (pixze),a
	call point
	call pfnxko	; 1 Punkt überspringen
	jrc  ptfor9
	call pfnxko	; d.h. übernächster Punkt
	jrnc ptfor1
ptfor9:	
	pop  de
	ret

;;; Variante 2 für erweiterten Punktalgorithmus
ptfeer:
	push de

	call setcf	; Punkte in Vordergrundfarbe
	ld   a,(py1)
	ld   d,a
	ld   a,(px1)
	ld   e,a
ptfer1:	call spixl
	call pfnxko	; 1 Punkt überspringen
	jrc  ptfer9
	call pfnxko	; d.h. übernächster Punkt
	jrnc ptfer1
ptfer9:	
	pop  de
	ret

;;; Variante 3 für erw. P-alg. und Farbe setzen
ptfees:
	push de

	ld   a,(py1)
	ld   d,a
	ld   a,(px1)
	ld   e,a
ptfes1:	call setcf	; Punkte in Vordergrundfarbe
	call spixl
	call pfnxko	; 1 Punkt überspringen
	jrc  ptfes9
	call pfnxko	; d.h. übernächster Punkt
	jrnc ptfes1
ptfes9:	
	pop  de
	ret


;;; ------------------------------------------------------------
	
;;; nächste Punktkoordinate ermitteln
;;; dabei zeilenweises Ablaufen von (px1,py2) bis (px2,py2)
;;; Beim Überschreiten von (px2,py2) wird Carry gesetzt
;;; Annahmen:
;;;   1. aktuelle Koordinate in DE
;;;   2. px1 <= px2 < 255
;;;   3. py1 <= py2 < 255
pfnxko:
	inc  e
	ld   a,(px2)
	cmp  e
	rnc
	ld   a,(px1)
	ld   e,a
	inc  d
	ld   a,(py2)
	cmp  d
	ret


;;; ============================================================

;;; Feldkoordinaten, innerhalb derer die Linien gezeichnet werden
;;; Aufbau:  x1  y1  x2  y2
lifdko	defb   0, 15,128,135
	defb   0,135,128,255
	defb 127, 15,255,135
	defb 128,135,255,255
;;; Zeiger auf die aktuelle Koordinate
lifdak  defw 0

lifeld:
	push ix
	push hl
	push de
	push bc

	call gein
	call cls
	call setcf

	ld   hl,tbkoeh
	ld   d,1
	ld   e,48
	call zgbmp
	ld   hl,tbbres
	ld   d,1
	ld   e,165
	call zgbmp
	
	ld   ix,lifaz
	ld   hl,lifdko
	ld   (lifdak),hl
	
	call lifini
	ld   (ix+lfdx),4
	ld   (ix+lfdy),4
lif45k:	call liflik
	call lifnxk
	jrnc lif45k

	call lifini
	ld   (ix+lfdx),8
	ld   (ix+lfdy),4
lif22k:	call liflik
	call lifnxk
	jrnc lif22k

	call lifini
	ld   (ix+lfdx),4
	ld   (ix+lfdy),4
lif45b:	call liflib
	call lifnxk
	jrnc lif45b

	call lifini
	ld   (ix+lfdx),8
	ld   (ix+lfdy),4
lif22b:	call liflib
	call lifnxk
	jrnc lif22b
	
	pop  bc
	pop  de
	pop  hl
	pop  ix
	ret

lifini:
	ld   de,lifaz
	ld   hl,(lifdak)
	ldi
	ldi
	ldi
	ldi
	ld   (lifdak),hl
	ld   a,(ix+lffx2)
	ld   (ix+lflx1),a
	ld   (ix+lflx2),a
	ld   a,(ix+lffy1)
	ld   (ix+lfly1),a
	ld   (ix+lfly2),a
	ret


;;; Linie zeichnen (Besenham)
liflib:	
	push ix
	pop  hl
	ld   de,lflx1
	add  hl,de
	ld   de,px1
	ldi
	ldi
	ldi
	ldi
	call linie
	ret

;;; Linie zeichnen (Köhler)
liflik:	ld   a,(ix+lflx1)
	ld   (pixlx),a
	ld   a,(ix+lfly1)
	ld   (pixly),a
	ld   a,(ix+lflx2)
	ld   (pixex),a
	ld   a,(ix+lfly2)
	ld   (pixey),a
	call draw
	ret

;;; nächste Koordinaten errechnen
;;; Carry = 1 wenn Ende
;;; Parameter: IX zeigt auf:
;;; 	        - 4 Byte Feldbegrenzung (Koord.)
;;; 	        - 2 Byte DeltaY, DeltaX
;;; 	        - 4 Byte aktuelle Linie (Koord.)
lffx1	equ 0
lffy1	equ 1
lffx2	equ 2
lffy2	equ 3
lfdx	equ 4
lfdy	equ 5
lflx1	equ 6
lfly1	equ 7
lflx2	equ 8
lfly2	equ 9
;;; Arbeitszellen
lifaz	defs 10

lifnxk:
	ld   a,(ix+lflx1)
	cmp  (ix+lffx1)
	jrz  lifnx2		; nicht mehr im oberen Dreieck
	sub  (ix+lfdx)
;	jrnc lifnx1		; li. Rand überschritten?
;	ld   a,(ix+lffx1)	; ja -> Korrektur, ist meist falsch
	jrc  lifnx0		; li. Bildschirmrand überschritten?
	cmp  (ix+lffx1)		; li. Rand überschritten?
	jrnc lifnx1		; keine Korrektur nötig
lifnx0:	ld   a,(ix+lffx1)	; ja -> Korrektur, ist meist falsch
lifnx1:	ld   (ix+lflx1),a
	jr   lifnx4
lifnx2:	ld   a,(ix+lfly1)
	add  (ix+lfdy)
	rc			; Überlauf, sofortiger Abbruch
	cmp  (ix+lffy2)		; unteres Ende überschritten?
	jrc  lifnx3		; nein, weiter
	scf			; return: Carry
	ret
lifnx3:	ld   (ix+lfly1),a
lifnx4:	
	ld   a,(ix+lfly2)
	cmp  (ix+lffy2)
	jrz  lifnx7		; im unteren Dreieck
	add  (ix+lfdy)
	jrc  lifnx5		; Überlauf -> Korrektur
	cmp  (ix+lffy2)		; unterer Rand erreicht?
	jrz  lifnx6
	jrc  lifnx6
lifnx5:	ld   a,(ix+lffy2)	; Korrektur, ist meist falsch
lifnx6:	ld   (ix+lfly2),a
	or   a			; Carry = 0
	ret
lifnx7:	ld   a,(ix+lflx2)
	sub  (ix+lfdx)
	ld   (ix+lflx2),a
	ret

;;; ============================================================

lbanz	equ  10		; Anzahl Linien
lbakt	defw 0		; Zeiger auf aktuelle Linie in lblin
lblli	defs 4*lbanz	; Feld mit Linienkordinaten
lbllie:			; Endemarke für das Feld
lblin	defb 0		; aktuelle Linie und...
lbdx1	defb 0		; Deltawerte für Endpunkte der Linien
lblny1	defb 0
lbdy1	defb 0		; ...ineinander verschränkt
lblnx2	defb 0
lbdx2	defb 0
lblny2	defb 0
lbdy2	defb 0
lbzae	defb 0		; Zähler für Wechsel zw. Algorithmen
lbvare:			; Endemarke für die Variablen
lbzv    defb 0		; Zufallszahl von
lbzb    defb 0		; Zufallszahl bis
	

;;; Bildschirmschoner "Linien"
linbss:
	push hl
	push de
	push bc

	ld   hl,lblli	; Initialisierung
	ld   (lbakt),hl
	xor  a
	ld   (lblli),a
	ld   d,h
	ld   e,l
	inc  de
	ld   bc,lbvare-lblli-1
	ldir		; alle Linien und die Deltas auf 0

	ld   d,30
	ld   e,10
	call rndvb
	ld   (lblin),a	; x1 = 30 .. 39
	ld   d,205
	ld   e,10
	call rndvb
	ld   (lblnx2),a ; x2 = 205 .. 214

	ld   d,9
	ld   e,2
	call rndvb
	ld   (lbdy1),a	; dy1 = 9 .. 10
	ld   d,9
	ld   e,2
	call rndvb
	ld   (lbdy2),a	; dy2 = 9 .. 10

	call drei
	ld   a,0
	ld   (lbzae),a
	call linb1	; Variante 1
;	ld   a,(lbzae)
;	or   a		; vorzeitiger Abbruch?
;	jrnz linbs9
	
	call drei
	ld   a,0
	ld   (lbzae),a
	call linb2	; Variante 2
;	ld   a,(lbzae)
;	or   a		; vorzeitiger Abbruch?
;	jrnz linbs9
	
	call drei
	ld   a,160
	ld   (lbzae),a
	call linb3	; Variante 3
;	ld   a,(lbzae)
;	or   a		; vorzeitiger Abbruch?
;	jrnz linbs9
	
	call drei
	ld   a,0
	ld   (lbzae),a
	call linb2	; Variante 2
;	ld   a,(lbzae)
;	or   a		; vorzeitiger Abbruch?
;	jrnz linbs9
	
	call drei
	ld   a,160
	ld   (lbzae),a
	call linb1	; Variante 1
	
linbs9:	pop  bc
	pop  de
	pop  hl
	ret

	;; Variante 1, Linien, deren Enden sich nicht berühren
linb1:
	ld   hl,lbzv
	ld   (hl),2
	inc  hl
	ld   (hl),8
linb11:	ld   hl,lblin
	call lb2nx	; nächstes x1
	call lb2nx	; nächstes y1
	call lb2nx	; nächstes x2
	call lb2nx	; nächstes y2
	ld   hl,lblin
	ld   de,lblny1
	call lbnxz
	ld   hl,lblny1
	ld   de,lblin
	call lbnxz
	ld   hl,lblnx2
	ld   de,lblny2
	call lbnxz
	ld   hl,lblny2
	ld   de,lblnx2
	call lbnxz
	call linbli

;	rst  %20
;	defb %04	; INKEY
;	or   a
;	rnz

	ld   hl,lbzae
	inc  (hl)
	jrnz linb11
	ret

	;; Variante 2, Linien im Zickzack
linb2:
	ld   hl,lbzv
	ld   (hl),8
	inc  hl
	ld   (hl),16
linb21:	ld   hl,lblin
	call lb2nx	; nächstes x1
	call lb2nx	; nächstes y1
	ld   hl,lblin
	ld   de,lblny1
	call lbnxz
	ld   hl,lblny1
	ld   de,lblin
	call lbnxz
	call linbli
	ld   hl,lblnx2
	call lb2nx	; nächstes x2
	call lb2nx	; nächstes y2
	ld   hl,lblnx2
	ld   de,lblny2
	call lbnxz
	ld   hl,lblny2
	ld   de,lblnx2
	call lbnxz
	call linbli

;	rst  %20
;	defb %04	; INKEY
;	or   a
;	rnz

	ld   hl,lbzae
	inc  (hl)
	jrnz linb21
	ret

	;; nächster Koordinatenwert, basierend auf Delta
lb2nx:	
	;; (hl) = akt. Wert, (hl+1) = Delta
	ld   d,(hl)
	inc  hl
	ld   a,(hl)	; a = Deltawert
	cmp  0		; negativer Deltawert?
	jpm  lb2nxm
	ld   e,a	; e = Deltawert
	add  d
	jrnc lb2nx9	; kein Überlauf
	ld   a,e
	neg		; Richtungsumkehr
	ld   (hl),a
	ld   a,255	; maxx oder maxy
	jr   lb2nx9
lb2nxm:	neg		; a = |Deltawert| = -Delta
	cmp  d
	jrnc lb2nx2	; kurz vor Überlauf --> Korr.
	sub  d
	neg		; a = -( -Delta - akt_Wert)
	jr   lb2nx9
lb2nx2:	ld   (hl),a	; Delta = -Delta
	xor  a		; minx oder miny
lb2nx9:	dec  hl
	ld   (hl),a	; akt. Wert neu setzen
	inc  hl
	inc  hl
	ret

	;; neues Delta bei Anschlag am Bildschirmrand
lbnxz:	
	;; hl = Adr. vom akt. Wert, de = Adr. andere Richtung
	ld   a,(hl)
	inc  a
	jrz  lbnxz1	; a war 255 -> am Delta wackeln
	dec  a
	jrz  lbnxz1	; a war 0 -> am Delta wackeln
	ld   a,(de)
	inc  a
	jrz  lbnxz1	; and. Ri. war 255 -> am Delta wackeln
	dec  a
	rnz
lbnxz1:	inc  hl
	ld   a,(lbzv)
	ld   d,a
	ld   a,(lbzb)
	ld   e,a
	ld   a,(hl)
	cmp  0
	jpp  lbnxz3
	xor  a
	sub  d
	sub  e
	ld   d,a
lbnxz3:	call rndvb
	ld   (hl),a
	ret

	;; alte Linie löschen, neue anzeigen
linbli:
	call setcb	; Hintergrundfarbe
	ld   hl,(lbakt)
	call linhl	; "alte" Linie löschen
	ex   de,hl
	ld   hl,lblin
	ldi		; neue Linie archivieren
	inc  hl
	ldi
	inc  hl
	ldi
	inc  hl
	ldi		; (lbakt) = aktuelle Linie, de = nä.L.
	call setcf	; Vordergrundfarbe
	ld   hl,(lbakt)
	call linhl	; neue Linie zeichnen

	ld   de,4
	add  hl,de
	ex   de,hl
	ld   hl,lbllie	; hl = Ende des Linienfeldes
	or   a		; Carry = 0
	sbc  hl,de	; Vergleich durch Subtraktion
	jrnz linbl2	; Feldende noch nicht erreicht
	ld   de,lblli	; Zeiger zurück auf Feldanfang
linbl2:	ld   (lbakt),de
	ret
	
	;; Variante 3, "wilde" Linien
linb3:
	ld   hl,lbzv
	ld   (hl),0
	inc  hl
	ld   (hl),5
linb31:	ld   hl,lblin
	call lb3nx	; nächstes x1
	call lb2nx	; nächstes y1
	call lb3t2	; Teil 2
	ld   hl,lblin
	call lb2nx	; nächstes x1
	call lb3nx	; nächstes y1
	call lb3t2	; Teil 2

	ld   a,10
	ld   (wzeit),a
	call warte

;	rst  %20
;	defb %04	; INKEY
;	or   a
;	rnz

	ld   hl,lbzae
	inc  (hl)
	jrnz linb31
	ret

lb3t2:	
	ld   hl,lblin
	ld   de,lblny1
	call lbnxz
	call linbli
	ld   a,(lblin)
	ld   (lblnx2),a
	ld   a,(lblny1)
	ld   (lblny2),a
	ret

lb3nx:	
	;; (hl) = akt. Wert; Delta (hl+1) spielt keine Rolle
	ld   d,0
	ld   e,120
	call rndvb
	cmp  60
	jrnc lb3nxm

	add  10		; 10..70 addieren
	ld   e,a	; e = neues Delta
	ld   a,(hl)	; a = akt. Wert
	cmp  255	; Stehen wir bereits am Rand?
	jrnz lb3nx2	; nein
	sub  e		; ja, dann weg vom Rand
	sub  10		; und noch e bissl weiter
	jp   lb3nx9
lb3nx2:	add  e
	jpnc lb3nx9	; Rand nicht überschritten -> ok
	ld   a,255	; nur bis zum Rand gehen
	jr   lb3nx9
lb3nxm:
	sub  50		; 10..70 subtrahieren
	ld   e,a	; e = neues Delta
	ld   a,(hl)	; a = akt. Wert
	cmp  0		; Stehen wir bereits am Rand?
	jrnz lb3nx4	; nein
	add  e		; ja, dann weg vom Rand
	add  10		; und noch e bissl weiter
	jp   lb3nx9
lb3nx4:	sub  e
	jpnc lb3nx9	; Rand nicht überschritten -> ok
	xor  a		; ohh, nur bis zum Rand gehen
lb3nx9:
	ld   (hl),a	; akt. Wert neu setzen
	inc  hl
	inc  hl
	ret


;;; ============================================================

;;; Stern mit FA-Algorithmus
stfa:	
	ld   b,lina
	ld   hl,lint
dlin:	ld   a,(hl)
	ld   (pixlx),a
	inc  hl
	ld   a,(hl)
	ld   (pixly),a
	inc  hl
	ld   a,(hl)
	ld   (pixex),a
	inc  hl
	ld   a,(hl)
	ld   (pixey),a
	inc  hl
	call draw
	djnz dlin
	ret

;;; Stern mit Bresenham-Algorithmus
;;; Parameter: a: 0 = komplett anzeigen
;;;               1 = nur zweite Hälfte, hl muss gesetzt sein!
stbr:
	ld   h,0
	ld   l,lina
	add  hl,hl
	add  hl,hl
	ld   b,h
	ld   c,l
	ld   hl,lint
nxtl:	ld   de,px1
	ldi
	ldi
	ldi
	ldi
	push af		; Ende per P/V-Flag erkennen
	call linie
	pop  af
	jppe nxtl
	ret

; ---------------------------------------------

	
kreise:	
	call cls
	
	ld   a,8
	ld   (whalb),a
	ld   a,50
	ld   (wzeit),a

	call setcf
	call stbr

	call drei
	ld   a,3
	call tastto
	call leer

	;; wachsende Kreise mit Bresenham-Algo.
	ld   a,42
	ld   (px1),a
	ld   a,140
	ld   (py1),a
	ld   hl,px2
	ld   (hl),0	; Radius
nxtk:
	call setci
	call kreis
	call warte
	call setcb
	call kreis
	inc  (hl)
	jrnz nxtk	; wdh. bis Überlauf
	ret


;;; ============================================================
	
kbx	equ  0		; Mittelpunkt x
kby	equ  1		; Mittelpunkt y
kbdr	equ  2		; Delta für Radius
kbz1	equ  3		; Zähler für Anzahl Kreise
kbr1	equ  4		; Radius 1
kbr2	equ  5		; Radius 2
kbr3	equ  6		; Radius 3
kbwz	equ  7		; Wartezeit-Zähler
kbmo	equ  8		; Modus (0=aus, 1=an, -1=Ende)
kbgr	equ  9		; Größe der Struktur
kbzae	defb 0		; Zähler für äußere Schleife
kbwta	defb 0		; Wartezeit äußere Schleife
kbvarg  equ  4*kbgr	; Größe des Variablenfeldes
kbvar	defs kbvarg	; Variablen, Feld von Strukturen
kbvare:			; Endeadresse zu kbvar

;;; Parameter
kbpa	equ  0		; Anzahl aktiver Kreisobjekte
kbpd	equ  1		; Anzahl Durchläufe
kbpw	equ  2		; Wartezyklen zwischen 2 Durchl.
kbpgr	equ  3		; Größe der Struktur

kbpar	defb 1,  4,16
	defb 1,  8, 5
	defb 2, 20, 4
	defb 4,100, 0
	defb 2, 20, 4
	defb 1,  8, 5
	defb 1,  4,16
	defb 0


;;; Bildschirmschoner "Kreise"
krbss:
	push hl
	push de
	push bc
	push ix

	ld   hl,tbregn
	ld   d,110
	ld   e,70
	call zgbmp
	call drei
	
	ld   hl,kbvar
	ld   (hl),0	; erste Initialisierung
	ld   de,kbvar+1
	ld   bc,kbvarg-1
	ldir

	ld   hl,kbpar
	push hl
krbs01:
	;; Parameter auslesen
	pop  hl
	ld   a,(hl)
	or   a
	jpz  krbss9	; Parameter = 0? --> Ende

	ld   c,a	; c: Anzahl aktiver Kreisobjekte
	inc  hl
	ld   a,(hl)	; Anzahl Durchläufe
	ld   (kbzae),a
	inc  hl
	ld   a,(hl)	; Wartezyklen zw. 2 Durchl.
	ld   (kbwta),a
	inc  hl
	push hl

	;; Kreisobjekte anhand der Parameter vorbelegen
	ld   b,4	; b: Anzahl Kreisobjekte
	ld   ix,kbvar
krbs02:	xor  a
	or   c		; keine weiteren aktiven Kreisobj?
	jrz  krbs04	; ja, nur noch Endemodus setzen
	ld   a,1	; Modus 1 (=an)
	ld   (ix+kbmo),a
	ld   hl,kbwta
	push bc
	ld   b,c
	ld   a,2	; min. Wartezeit
krbs03:	add  b
	djnz krbs03
	ld   (ix+kbwz),a	; init. Wartezeit
	pop  bc
	dec  c
	jr   krbs05
krbs04:	
	ld   a,(ix+kbmo)
	or   a		; Modus 0 (=aus)?
	jrz  krbs05	; ja, nichts weiter tun
	ld   a,-1	; Modus -1 (=Endemodus)
	ld   (ix+kbmo),a	; Endemodus setzen
krbs05:
	ld   de,kbgr
	add  ix,de
	djnz krbs02

	;; aktuellen Parametersatz abarbeiten
	ld   ix,kbvar
krbs11:
	rst  %20
	defb %04	; INKEY
	or   a
	jpnz krbss8	; Tastendruck bricht ab

	ld   a,(ix+kbmo)
	or   a		; Modus = aus?
	jrz  krbs13	; ja, nächstes Objekt prüfen
	ld   a,(ix+kbwz)
	or   a		; noch Wartezeit?
	jrz  krbs12	; nein, weiter mit nä. Aktion
	dec  (ix+kbwz)	; Wartezeitzähler verringern
	jr   krbs14
krbs12:	call krbakt
	ld   a,(ix+kbdr)
	or   a		; Kreisablauf zu Ende?
	jrnz krbs14	; nein, weiter
	ld   a,(kbwta)	; setze Wartezeitzähler
	ld   (ix+kbwz),a
krbs13:	;ld   a,2
	;ld   (wzeit),a
	;call warte
krbs14:	
	ld   de,kbgr
	add  ix,de	; nächstes Kreisobjekt
	;; Überlauf?
	push ix
	pop  hl
	ld   de,kbvare	; erste Adr. nach den Kreisobjekten
	or   a		; Carry := 0
	sbc  hl,de	; Ergebnis in HL ist egal
	jrnz krbs15	; nein, kein Überlauf
	ld   ix,kbvar	; Überlauf -> zurück auf 1. Obj.
krbs15:	

	ld   a,(kbzae)
	or   a
	jpnz krbs11

	;; loop
	jp   krbs01

	;; ................................
	
krbss8:	
	pop  hl		; Stack-Korrektur
krbss9:	
	pop  ix
	pop  bc
	pop  de
	pop  hl
	ret


;;; nächste Aktion: Init oder nächster Kreis
krbakt:
	ld   a,(ix+kbdr)	; wenn 0 dann Init
	or   a
	jpnz krbkr	; JP erspart ein RET
	ld   a,(ix+kbmo)
	inc  a		; Modus = -1?
	jrnz krbak1	; nein, weiter mit Init
	ld   (ix+kbmo),a	; Modus := 0
	ret
krbak1:	ld   a,(kbzae)
	or   a
	rz
	dec  a		; Zähler wird nur bei Init erniedrigt
	ld   (kbzae),a
	;; statt CALL und RET: weiter mit krbini

;;; Initialisierung
;;; Parameter: IX: Adr. Speicherbereich
krbini:
	call randb
	ld   (ix+kbx),a
	ld   (px1),a	; Der 1. Kreis wird gleich gemalt.
	call randb
	ld   (ix+kby),a
	ld   (py1),a
	ld   d,7
	ld   e,4
	call rndvb
	ld   (ix+kbdr),a	; Radius: 7..10
	ld   d,7
	ld   e,4
	call rndvb
	ld   (ix+kbz1),a	; 7..10 Kreise
	xor  a
	ld   (ix+kbr1),a
	ld   (ix+kbr2),a
	inc  a
	ld   (ix+kbr3),a
	ld   (px2),a	; Radius
	call setci
	call kreis
	ret

;;; nächster Schritt: inneren Kreis löschen, außen neuen malen
;;; Parameter: IX: Adr. Speicherbereich
;;; Return: Z = 1 -> nichts mehr zu tun
;;;         (ix+kbdr) = 0 --> Ende erreicht
krbkr:
	ld   a,(ix+kbr1)
	or   a
	jrz  krbkr1	; Radius 0 -> leere Stelle -> kein Kreis
	call setci	; Kreis löschen
	ld   a,(ix+kbr1)
	call krbkrk
krbkr1:	ld   a,(ix+kbr2)	; Nachrücken
	ld   (ix+kbr1),a
	ld   a,(ix+kbr3)
	ld   (ix+kbr2),a	; Nachrücken
	or   a		; r3 == 0? --> Kennz. f. Endephase
	jrnz krbkr2	; Endephase noch nicht erreicht
	or   (ix+kbr1)
	rnz		; Endephase aber noch kein Ende
	ld   (ix+kbdr),a	; Endekennzeichen: dr:=0
	ret
krbkr2:	dec  (ix+kbz1)
	jrnz krbkr3
	xor  a		; Kennzeichen für Endephase
	ld   (ix+kbr3),a
	ret
krbkr3:	add  (ix+kbdr)	; nä. Radius
	ld   (ix+kbr3),a
	call setci	; neuen Kreis zeichnen
	ld   a,(ix+kbr3)
	call krbkrk
	ret

krbkrk:	
	ld   (px2),a	; px2 = Radius
	ld   a,(ix+kbx)
	ld   (px1),a
	ld   a,(ix+kby)
	ld   (py1),a
	call kreis
	ret

	
; ---------------------------------------------

nkb:	defb 255
	defb 255
bild1:	binclude bilder/nashornkaefer.pbmb
bild2:	binclude bilder/klaviertasten.pbmb
	; erwartet wird Bitmap 256x256

; Bitmap anzeigen
; Parameter: hl = Anfangsadresse
zbild:
	push de
	push bc

	ld   de,%EC00	; BWS Anfang
	ld   bc,%20	; 32 Bytes pro Zeile
	xor  a		; Block 0
zbi1:	out  deco	; Block aktivieren
	ldir
	ld   bc,%20	; 32 Bytes pro Zeile
	inc  a
	and  7		; Blocknr < 8
	jrnz zbi2	; gleiche Zieladr., nä. Block
	ld   a,d
	cmp  %F0	; BWS Ende erreicht?
	jrz  zbi3
	xor  a		; Blocknr. wieder auf 0
	jr   zbi1
zbi2:	ex   de,hl
	sbc  hl,bc	; CY wurde von and auf 0 gesetzt
	ex   de,hl	; de = Zeilenanfang nä. Block
	jr   zbi1
	
zbi3:	pop  bc
	pop  de
	ret
	

; ---------------------------------------------

;;; Bild von unten einschieben
bivuba	defw 0		; Bildadresse
;;; Parameter: hl = Bildadresse
	;; von y0=255 bis 0
	;;   ba=bildadresse
	;;   von y=y0 bis 255
	;;     schreibe 32 Byte ab ba in BWS(0,y)
	;;     ba += 32 (passiert "automatisch")
bivus:	
	push de
	push bc
	ld   (bivuba),hl	; Bildadr. sichern

	ld   b,0	; djnz soll bis einschl. b=0 gehen
bivus1:
	dec  b		; djnz soll bis einschl. b=0 gehen
	push bc		; Zähler der äußeren Schleife sichern

	ld   a,b
	and  %F8	; y zu Adr, 1.: letzte 3 bit löschen
	ld   l,a
	ld   h,0
	add  hl,hl
	add  hl,hl	; y zu Adr, 2.: 2 Pos. links schieben
	ld   a,h
	add  %EC	; y zu Adr, 3.: BWS Anfang addieren
	ld   h,a	; --> hl += %EC00
	ld   a,b
	and  7		; passenden Block ermitteln
	out  deco	; und einschalten
	ex   de,hl
	ld   hl,(bivuba)
bivus2:
	push bc		; Zähler der inneren Schleife sichern
	ld   bc,%20
	ldir		; 1 Zeile in BWS schreiben

	inc  a		; nächster BWS-Block?
	cmp  8
	jpc  bivus3	; ja
	xor  a		; nein: Block := 0, Adr. ist schon hochgesetzt
	jp   bivus4
bivus3:	ld   bc,%1F	; Cy ist gesetzt!
	ex   de,hl
	sbc  hl,bc	; BWS-Adr. zurück, nur Blockwechsel
	ex   de,hl
bivus4:	out  deco

	pop  bc		; Zähler der inneren Schleife holen
	inc  b		; letzte Schiebezeile erreicht?
	jrnz bivus2	; nein, im Schiebezyklus bleiben
	
	pop  bc		; Zähler der äußeren Schleife holen
	inc  b		; djnz soll bis einschl. b=0 gehen
	djnz bivus1

	pop  bc
	pop  de
	ret

; ---------------------------------------------

;;; Ameisenalgorithmus von Langton
;;; Die Ameise bewegt sich hier im Feld (1,1) bis (255,255).
;;; Wenn sie das Feld verlässt, endet die Prozedur.
;;; Die Ameise selbs ist unsichtbar.

;;; Feld mit Schrittbefehlen. 1 Befehl wird nach avorw kopiert.
ariop:	inc  d		; Süd
	dec  e		; West
	dec  d		; Nord
	inc  e		; Ost

	;; whalb und wzeit müssen vorher gesetzt werden
ameise:
	push hl
	push de
	push bc

;	call cls	; definierter Zustand im Feld
	ld   d,128
	ld   e,d	; Ameise auf Bildschirmmitte
	ld   b,0
	ld   c,b	; Startrichtung Süd
	call setci	; Pixelfarbe invertieren
amei1:
	call tpixl
	jrnz amei2	; Z=0 = weißes Feld -> 90° nach rechts
	inc  c		; Z=1 = schwarzes Feld -> 90° nach links
	inc  c		; 3x rechts = links
amei2:	inc  c
	ld   a,3	; Bitmaske für 4 Richtungen
	and  c
	ld   c,a
	call spixl	; aktuelle Position invertieren
	ld   hl,ariop	; Startadr. für Befehlsmanipulation
	add  hl,bc
	ld   a,(hl)
	ld   (avorw),a
avorw:	nop		; Schritt vorwärts

	call warte	; künstlich warten
	
	xor  a
	cmp  d		; Hat sie das Feld verlassen?
	jrz  amei9
	cmp  e		; Hat sie das Feld verlassen?
	jrnz amei1
amei9:	
	pop  bc
	pop  de
	pop  hl
	ret

; ---------------------------------------------

;;; Der Ameise einen Stein in den Weg legen
amstn:
	push ix
	ld   ix,berstn
	call amsrnd
	inc  ix
	call amsrnd
	pop  ix
	
	ld   a,10
	ld   (whalb),a
	ld   a,20
	ld   (wzeit),a
	call setcf
	call amslin
	call setcb
	call amslin
	call setcf
	ld   hl,stlko+2
	ld   e,(hl)
	inc  hl
	ld   d,(hl)
	call spixl
	call warte
	ret

amsrnd:	
	ld   e,(ix+2)
	call randb
	ld   h,a
	call mul8b
	ld   a,h
	add  (ix+0)
	ld   (ix+6),a
	ret

amslin:	
	ld   hl,stlko
	ld   de,px1
	ldi
	ldi
	ldi
	ldi
	call linie
	call warte
	ret
	
;;; Bereich, in dem der Stein liegen kann
berstn	defb 167,107,5,5	; Werte: x0, y0, Breite, Höhe
stlko	defb 0,0,0,0		; Linienkoordinaten


;;; ============================================================
	
;;; Einzelbilder, immer 16x16 Pixel
mnl_l1	include bilder/mnl_l1.def
mnl_l2	include bilder/mnl_l2.def
mnl_l3	include bilder/mnl_l3.def
mnl_l4	include bilder/mnl_l4.def
mnl_l5	include bilder/mnl_l5.def
mnl_l6	include bilder/mnl_l6.def
mnl_r1	include bilder/mnl_r1.def
mnl_r2	include bilder/mnl_r2.def
mnl_r3	include bilder/mnl_r3.def
mnl_r4	include bilder/mnl_r4.def
mnl_r5	include bilder/mnl_r5.def
mnl_r6	include bilder/mnl_r6.def
mnl_s	include bilder/mnl_s.def
mnl_g	include bilder/mnl_g.def
mnl_g1	include bilder/mnl_g1.def

;;; Bildfolgen
mnlauf	defb 12		; Folgenlänge
	defw mnl_r1,mnl_r2,mnl_r3,mnl_r4,mnl_r5,mnl_r6
	defw mnl_l1,mnl_l2,mnl_l3,mnl_l4,mnl_l5,mnl_l6

;;; guck her
mnguhe	defb 2
	defw mnl_g1,mnl_g

;;; guck weg
mnguwe	defb 2
	defw mnl_g1,mnl_s
	
;;; steh rum
mnsteh	defb 1
	defw mnl_s
	
;;; Anz. Bilder, Folge,
;;; Delta x, Delta y, Pause dazwischen, Pause danach
mnseq	defb 1
	defw mnsteh
	defb 0,0,50,50
	;;
	defb 50
	defw mnlauf
	defb 2,0,10,10	; Bilder der Folge immer mit 2 Pixel Versatz anzeigen
	;;
	defb 1
	defw mnsteh
	defb 0,0,50,50
	;;
	defb 2
	defw mnguhe
	defb 0,0,20,80
	;;
	defb 2
	defw mnguwe
	defb 0,0,20,50
	;;
	defb 77
	defw mnlauf
	defb 2,-1,4,10
	;;
	defb 0		; Ende

;;; ------------------------------------------------------------

maennl:
	push hl
	push de
	push bc
	push ix
	push iy

	call hgini	; Hintergrundsicherung initialisieren
	ld   iy,mnseq
	ld   d,200	; y=200
	ld   e,0	; x=0

maen1:	ld   a,(iy+0)
	or   a		; 0 Bilder = Endekennung
	jpz  maen9
	
	ld   c,a	; Anzahl Bilder gesamt
maen2:	ld   l,(iy+1)
	ld   h,(iy+2)
	ld   a,(hl)	; Anzahl Bilder in der Folge
	ld   b,a
	inc  hl
	push hl
	pop  ix
maen3:	
	call yx2ad	; Parameter für sihg
	call sihg	; Hintergrund sichern
	ld   l,(ix+0)
	ld   h,(ix+1)	; HL = Adr. Bild
	call zgmb
	ld   a,(iy+5)
	ld   (wzeit),a
	call warte
	dec  c		; Gesamtanzahl
	jpz  maen8
	
	call zghg	; Hg. an der alten Stelle anzeigen
	ld   a,e
	add  (iy+3)
	ld   e,a	; e += Delta x
	ld   a,d
	add  (iy+4)
	ld   d,a	; d += Delta y

	dec  b		; Anz. Bilder in der Folge
	jpz  maen2	; Folge von vorn beginnen
	inc  ix
	inc  ix		; nä. Bild in der Folge
	jr   maen3
maen8:
	ld   a,(iy+6)	; Wartezeit am Ende
	ld   (wzeit),a
	call warte
	
	;; iy weiterschieben (+7)
	push iy
	pop  hl
	ld   bc,7
	add  hl,bc
	push hl
	pop  iy
	jp   maen1
maen9:	
	pop  iy
	pop  ix
	pop  bc
	pop  de
	pop  hl
	ret


;;; ------------------------------------------------------------

hgpxb	defb 0	  ; Bit im BWS-Byte (= Erg. von yx2ad)
shbb	defb 0	  ; aktuelle BWS-Blocknr.
hgbb	defb 0	  ; erste Blocknr. des HG-Bereichs
hgadr	defw 0	  ; BWS-Adr. des HG-Bereichs
hinten	defs 48	  ; Sicherung für Hintergrund

hgini:
	xor  a
	ld   (hgadr),a
	ld   (hgadr+1),a
	ret
	
;;; Hintergrund 3 Byte (24 Pixel) x 16 Pixel sichern
;;; Parameter: HL = BWS-Adr., D = Y (Pixelzeile)
;;;            d.h. Ergebnis von yx2ad
sihg:
	ld   (hgpxb),a
	ld   a,(hgadr)
	cmp  l
	jrnz sihg1
	ld   a,(hgadr+1)
	cmp  h
	rz		; Hintergrund wurde bereits gesichert
	
sihg1:	push hl
	push de
	push bc

	ld   a,d	; a:= Y-Koord.
	and  %07	; y[2..0] -> G-BWS-Block/-Seite
	ex   de,hl
	ld   hl,hgbb
	ld   (hl),a
	inc  hl		; hl = hgadr
	ld   (hl),e
	inc  hl		; hl = hgadr + 1
	ld   (hl),d
	inc  hl		; hl = hinten
	ex   de,hl
	ld   bc,48
sihg2:	
	out  deco
	ld   (shbb),a
	ldi
	ldi
	ldi
	jppo sihg9
	dec  hl
	dec  hl
	dec  hl
	ld   a,(shbb)
	cmp  7		; letzter Block im BWS?
	jrz  sihgna
	inc  a
	jr   sihg2
sihgna:
	push bc
	ld   bc,32	; Zeilenlänge in Bytes
	add  hl,bc
	pop  bc
	xor  a
	jr   sihg2
sihg9:	
	pop  bc
	pop  de
	pop  hl
	ret
	

;;; ------------------------------------------------------------

;;; Zeige den Hintergrund wieder an
zghg:	
	push hl
	push de
	push bc

	ld   hl,hgadr
	ld   e,(hl)
	inc  hl		; hl = hgadr + 1
	ld   d,(hl)
	ld   a,d
	or   e		; (hgadr) = 0 ?
	jrz  zghg9	; ja -> nichts gespeichert
	inc  hl		; hl = hinten
	ld   bc,48
	ld   a,(hgbb)
zghg1:
	out  deco
	ld   (shbb),a
	ldi
	ldi
	ldi
	jppo zghg9
	dec  de
	dec  de
	dec  de
	ld   a,(shbb)
	inc  a
	and  7		; letzter Block im BWS?
	jrnz zghg1
zghgna:
	push hl
	ld   hl,32	; Zeilenlänge in Bytes
	add  hl,de
	ex   de,hl
	pop  hl
;	xor  a
	jr   zghg1
zghg9:	
	pop  bc
	pop  de
	pop  hl
	ret


;;; ------------------------------------------------------------

zgmbza	defb 0	     ; Zähler

;;; Bild 16x16 Pixel mit Maske anzeigen
;;; Parameter:
;;;   HL = Adresse der Maske, HL+32 = Bildadresse
;;; Voraussetzung: call sihg
zgmb:	
	push hl
	push de
	push bc
	push ix
	push iy

	push hl
	pop  ix		; IX = Adr. Maske, IX+32 = Adr. Bild
	ld   iy,hinten
	ld   a,(hgpxb)	; Pixelbit im BWS-Byte
	add  a		; 2 Byte pro "Schleifenkörper"
	ld   (s16j+1),a	; JR anpassen
	ld   bc,(hgadr)
	push bc		; BWS-Adr. beiseite legen
	ld   a,16	; 16 Zeilen
	ld   (zgmbza),a	; Zähler initialisieren
	
	;; rechten Rand beachten
	ld   a,e
	ld   bc,%0202	; 2x "ld (bc),a"
	cp   -16
	jrc  nnur2b	; -> nicht nur 2 Byte anzeigen
	ld   bc,%0200	; "ld (bc),a" und "nop"
nnur2b:	cp   -8
	jrc  nnur1b	; -> nicht nur 1 Byte anzeigen
	ld   bc,%0000	; 2x "nop"
nnur1b:	ld   a,b
	ld   (zgmb1b),a	; Opcode verändern
	ld   a,c
	ld   (zgmb2b),a	; Opcode verändern
	pop  bc		; BWS-Adr. wiederherstellen
	push bc		; ...und beiseite legen

	ld   a,(hgbb)	; BWS-Block

	jr   zgmbs3
zgmbs1:			; Einsprung bei gleicher Adr. u.
	dec  bc		; nächstem BWS-Block
	dec  bc
	push bc
zgmbs2:	inc  ix		; Einsprung bei nä. BWS-Zeilenadr.
	inc  ix		; Maske u. Bild: 2 Byte/Zeile
	inc  iy
	inc  iy
	inc  iy		; Hintergund: 3 Byte/Zeile
	ld   hl,zgmbza	; Zähleradresse
	dec  (hl)
	jpz  zgmb9
zgmbs3:			; Einsprung bei 1. Schleifendurchlauf
	out  deco
	ld   (shbb),a
	;; Maske rotieren
	ld   h,(ix+0)		; ### richtig oder falsch??? ###
	ld   l,(ix+1)
	call srl16
	;; Maske auf Hintergrund legen
	or   (iy+0)	; BWS: neg. Logik
	ld   c,a
	ld   a,h
	or   (iy+1)
	ld   d,a
	ld   a,l
	or   (iy+2)
	ld   e,a	; C,D,E = maskierter Hintergrund

	;; Bild rotieren
	ld   h,(ix+32)
	ld   l,(ix+33)
	call srl16
	;; Bild auf den maskierten Hintergrund legen
	;; und in BWS schreiben
	cpl
	and  c		; BWS: neg. Logik
	pop  bc		; BWS-Adr. holen
	ld   (bc),a	; 1. Byte in BWS schreiben
	inc  bc
	ld   a,h
	cpl
	and  d
zgmb1b:	ld   (bc),a	; 2. Byte in BWS schreiben - oder auch nicht
	inc  bc
	ld   a,l
	cpl
	and  e
zgmb2b:	ld   (bc),a	; 3. Byte in BWS schreiben - oder auch nicht
	ld   a,(shbb)	; BWS-Block
	inc  a
	and  7		; letzter Block im BWS?
	jpnz zgmbs1

	ld   hl,30	; 30 = Zeilenlänge in Bytes - 2* INC BC
	add  hl,bc
	push hl		; BWS-Adr. für den nächsten Durchlauf
	jp   zgmbs2

zgmb9:
	pop  bc		; Wert wird nicht gebraucht, nur Stack bereinigen
	pop  iy
	pop  ix
	pop  bc
	pop  de
	pop  hl
	ret

;;; 16-bit-Wort um 0-7 Bit nach rechts schieben
;;; Parameter:
;;;   HL = 16-bit-Wort
;;;   (s16j+1) = Schiebedistanz * 2 (!)
;;; Return:
;;;   A,HL = 3 Byte, an den Rändern 0-Bits
srl16:	
	xor  a
s16j:	jr   s16j	; wird extern gesetzt
	jr   s160b	; Sonderbehandlung 0 Bit Verschiebung
	add  hl,hl
	rla
	add  hl,hl
	rla
	add  hl,hl
	rla
	add  hl,hl
	rla
	add  hl,hl
	rla
	add  hl,hl
	rla
	add  hl,hl
	rla
	ret
s160b:	ld   a,h
	ld   h,l
	ld   l,0
	ret
	
;;; ============================================================

;;; Drachenkurve
;;; Startpunkt: x,y = 43,171
;;; 10. Iteration
;;; Linienlänge 5 Pixel

d_plu	equ  %86 ; add (hl)
d_mnu	equ  %96 ; sub (hl)
;;; Delta in Ri. y,     x
;;; bei geradzahliger Iteration
drrig	defb d_mnu,     0  	; N
	defb     0, d_mnu	; W
	defb d_plu,     0	; S
	defb     0, d_plu	; O
;;; bei ungeradzahliger Iteration
drriu	defb d_mnu, d_mnu  	; NW
	defb d_plu, d_mnu	; SW
	defb d_plu, d_plu	; SO
	defb d_mnu, d_plu	; NO

drzlae	defw 0		; Länge der Zeichenkette
drrlae	defb 0		; Länge des Rasters
drsri	defb 0		; Startrichtung pro Iteration
drit	defb 0		; Iteration
drstxy  equ  %964B	; Startkoord: y = 150, x = 75
drstla	equ  128	; Startlänge
drstri	equ  0		; Startrichtung global, Nord
drsttb	equ  drrig
drmaxi  equ  12		; maximale Iteration
;drz	defs 4095 --> steht am Dateiende

drku:
	push hl
	
	call setcf
	ld   hl,drhgli
	call nlin	; ä bissl hübsch machn

	ld   a,100	; ~1s
	ld   (wzeit),a
	ld   (whalb),a	; Wartezeit soll gleich bleiben
	call warte
	
	ld   hl,drzlae
	ld   (hl),0	; Länge(low) der Zeichenkette := 0
	inc  hl
	ld   (hl),0	; Länge(high) der Zeichenkette := 0
	inc  hl		; HL = drrlae
	ld   (hl),drstla	; Rasterlänge := Startlänge
	inc  hl		; HL = drsri
	ld   (hl),drstri   ; Startrichtung global...
			; ...für Iteration 1(!) übernehmen
	inc  hl		; HL = drit
	ld   (hl),0	; Iteration 0 beginnt
	ld   hl,px1
	ld   a,low(drstxy)
	ld   (px1),a
	ld   (drlx1),a
	add  drstla	; waagerechte Linie zeichnen
	ld   (px2),a
	ld   a,high(drstxy)
	ld   (py1),a
	ld   (drly1),a
	ld   (py2),a

	call setcf
	call linie	; Iteration 0 endet
drku1:
	call warte
	ld   hl,drit
	inc  (hl)
	ld   a,(hl)
	and  1		; geradzahlige Iteration?
	jrz  drkuig	; ja
drkuiu:
	ld   hl,drriu	; Richtungstabelle "ungerade"
	ld   (drstab+1),hl	; Code-Manipulation
	;		; (Richtungsindex bleibt)
	ld   hl,drrlae
	srl  (hl)	; Rasterlänge halbieren
	jp   drku2
drkuig:
	ld   hl,drrig	; Richtungstabelle "gerade"
	ld   (drstab+1),hl	; Code-Manipulation
	ld   a,(drsri)	; Richtungsindex weiterdrehen
	inc  a
	and  3		; modulo 4
	ld   (drsri),a
	;		; (Rasterlänge bleibt)
drku2:
	ld   a,low(drstxy)
	ld   (drlx1),a
	ld   a,high(drstxy)
	ld   (drly1),a
	call drbk	; "berechne" Kurve in aktueller Iteration
	call drzk	; zeige die Kurve
	ld   a,(drit)
	cp   drmaxi
	jpnz drku1

	pop  hl
	ret

;;; - - - - - - - - - - - - - - - -
	
d_re    equ  1
d_li    equ  0
	
;;; "berechne" Kurve in aktueller Iteration
drbk:
	push hl
	push de
	push bc

	ld   hl,drz
	ld   d,h
	ld   e,l
	ld   bc,(drzlae)
	add  hl,bc
	ex   de,hl	; HL = drz-Anfang, DE = drz-Ende
	ld   a,d_re
	ld   (de),a	; Rechtsdrehung anfügen
	inc  de
	ld   a,b
	or   c
	jrz  drbk2
	srl  b
	rr   c		; BC /= 2
	ld   a,b
	or   c		; d.h. BC war vorher 1
	jrz  drbk1
	push bc
	ldir		; 1. Hälfte der alten Iteration
	ld   a,d_li
	ld   (de),a	; Linksdrehung
	inc  de
	inc  hl
	pop  bc
	ldir		; 2. Hälfte der alten Iteration
	jp   drbk2
drbk1:
	ld   a,d_li
	ld   (de),a	; Linksdrehung	
drbk2:
	ld   hl,(drzlae)
	add  hl,hl
	inc  hl		; Rechtsdrehung mitzählen
	ld   (drzlae),hl

	pop  bc
	pop  de
	pop  hl
	ret

;;; - - - - - - - - - - - - - - - -

;;; Zeige die Kurve
drzk:
	push hl
	push de
	push bc

	;; Zähler wo?
	ld   hl,(drzlae)
	srl  h
	rr   l		; HL /= 2
	push hl		; Zähler auf TOS
	ld   hl,drz-1	; Start mit unsichtbarer Rechtsdrehung
	ld   de,drstxy	; DE: Startkoord.
	ld   a,(drsri)
	ld   c,a
	ld   b,0	; BC: akt. Richtung
drzk1:	
	call drzws
	ex   (sp),hl
	ld   a,h
	or   l		; Zähler HL == 0?
	dec  hl
	ex   (sp),hl
	jrnz drzk1

	pop  hl		; Zähler entfernen
	pop  bc
	pop  de
	pop  hl
	ret


;;; - - - - - - - - - - - - - - - -
	
;;; Zwei Schritte
;;; Parameter:
;;;   HL = Adr. in drz
;;;   B = 0
;;;   C = Richtung aus vorherigem Schritt
;;;   DE = aktuelle Koordinaten
drzws:	
	ld   a,(hl)
	or   a
	call drschr
	ld   (drlx2),de
	inc  hl
	ld   a,(hl)
	or   a
	call drschr
	ld   (drlx3),de
	call drlll
	inc  hl
	ret


;;; Drehung und Schritt
;;; Parameter:
;;;   Z-Flag = 1 -> links, 0 -> rechts
;;;   B = 0
;;;   C = Richtung aus vorherigem Schritt
;;;   DE = aktuelle Koordinaten
drschr:
	push hl
	
	jpz  drs1
	inc  c
	inc  c
drs1:	inc  c
	ld   a,3	; für modulo 4
	and  c
	ld   c,a
drstab:	ld   hl,drrig	; wird manipuliert!
	add  hl,bc
	add  hl,bc	; 2 Byte pro Tabellenfeld
	ld   a,(hl)	; Befehl
	ld   (drsdy),a
	inc  hl
	ld   a,(hl)
	ld   (drsdx),a

	ld   hl,drrlae
	ld   a,d
drsdy:	add  (hl)	; Befehl wird manipuliert!
	ld   d,a
	ld   a,e
drsdx:	add  (hl)	; Befehl wird manipuliert!
	ld   e,a

	pop  hl
	ret

;;; - - - - - - - - - - - - - - - -
	
drlx1	defb 0
drly1	defb 0
drlx2	defb 0
drly2	defb 0
drlx3	defb 0
drly3	defb 0
;;; Linie 1-3 löschen, Linien 1-2 und 2-3 zeichnen
drlll:	
	push hl
	push de
	push bc

	ld   hl,drlx1
	ld   de,px1
	ldi
	ldi
	inc  hl
	inc  hl
	ldi
	ldi
	call setcb	; lösche...
	call linie	; ...die Linie aus der vorigen Iteration
	ld   hl,drlx1
	ld   de,px1
	ldi
	ldi
	ldi
	ldi
	call setcf	; zeichne...
	call linie	; ...Linie 1-2
	ld   hl,drlx2
	ld   de,px1
	ldi
	ldi
	ldi
	ldi
	call linie	; ...Linie 2-3
	ld   hl,drlx3	; jetziger Endpunkt...
	ld   de,drlx1	; ...wird neuer Anfang
	ldi
	ldi

	pop  bc
	pop  de
	pop  hl
	ret

; ---------------------------------------------

;;; Linien zur Hintergrundgestaltung der Drachenkurve
drhgli	defb 16
	defb 0,255,255,255
	defb 0,  0,255,  0
	defb 0,245,255,245
	defb 0, 10,255, 10
	defb 0,236,255,236
	defb 0, 19,255, 19
	defb 0,228,255,228
	defb 0, 27,255, 27
	defb 0,221,255,221
	defb 0, 35,255, 35
	defb 0,215,255,215
	defb 0, 41,255, 41
	defb 0,210,255,210
	defb 0, 46,255, 46
	defb 0,206,255,206
	defb 0, 50,255, 50
	defb 0,203,255,203
	defb 0, 53,255, 53
	defb 0,201,255,201
	defb 0, 57,255, 57
	
; ---------------------------------------------


tstrng:
	push hl
	push de
	push bc

	call gein
	call cls
	call setci
	call rndini
	ld   bc,0
;tstrg1:	call random
;	ld   de,(rna)
tstrg1:	call randb
	ld   d,a
	call randb
	ld   e,a
	call spixl
	djnz tstrg1
	dec  c
	jrnz tstrg1

	pop  bc
	pop  de
	pop  hl
	ret
	
; ---------------------------------------------

;;; Zeichenpuffer für die Drachenkurve.
;;; muss am Ende stehen, damit es nicht
;;; das Programm vergrößert
drzs	defb d_re	; Start mit unsichtbarer Rechtsdrehung
drz  ; defs 4095
