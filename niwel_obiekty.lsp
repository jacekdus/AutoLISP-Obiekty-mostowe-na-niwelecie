(defun getData (fileName / file filePath warunek pos line_string lista lst len data kolumny)
	
	;UWAGA
	;Plik z danymi musi byc w tej samej lokalizacji co plik dwg korzystajacy z funkcji
	
	(setq filePath (strcat (getvar "DWGPREFIX") fileName))
	(setq file (open filePath "r"))
	(princ (strcat fileName " has been opened.\n"))
	
	(while (setq line_string (read-line file))
		(setq kolumny 0)
		(setq warunek T)
		(while warunek
			(setq pos (vl-string-search ";" line_string))
			(if (/= pos nil)
				(progn
					(setq lst (substr line_string 1 pos))
					(setq line_string (substr line_string (+ pos 2)))
					(setq len (strlen line_string))
					(setq warunek T)
				  	(setq lst (vl-string-subst "." "," lst))
					(setq lista (cons lst lista))
					(setq kolumny (+ kolumny 1))
				);progn
				(progn
					(setq warunek nil)
					(setq lst line_string)
				  	(setq lst (vl-string-subst "." "," lst))
					(setq lista (cons lst lista))
					(setq kolumny (+ kolumny 1))
				);progn
			);if
		);while
	);while
	(close file)
	
	(setq lista (reverse lista))
	(setq data (list lista kolumny))
);defun
(defun c:niwM (/ obiekt rodzaj kilometraz poczatek_obiektu_km koniec_obiektu_km	wysokosc_konstrukcyjna szerokosc obiektu kat_skrzyzowania skrajnia_pionowa 
				data lista kolumny len niweleta teren zerowy i iter scale oldSettings)
	
	;OPIS FUNKCJI
	;- Zadaniem funkcji jest rysowanie mostow i wiaduktow na niwelecie drogowej w postaci 
	;  zakreskowanych prostokatow reprezentujacych rozpietosc obiektu w osiach
	;  (lub dlugosc pomostu), dodatkowo tworzony jest opis.
	;- Aby narysowac obiekty nalezy wskazac polilinie reprezentujaca niwelete oraz polilinie 
	;  wskazujaca poczatek kilometraza (ta linia powinna byc nie nizsza niz najwyzszy punkt 
	;  niwelety i nie nizsza niz rzedna odniesienia niwelety.
	;- Funkcja wyronia rysowanie obiektow w ciagu trasy, ktorej przedmiotem jest niweleta oraz nad.
	;- Jako dane wejsciowe funkcja przyjmuje plik excel domyslnie nazwany  "obiekty.csv", ktory powinien byc przechowywany w tym samym folderze co rysunek niwelety
	;- Plik danych wejsciowych jest baza danych z kolumnami 
	;  (Obiekt|Rodzaj|Kilometraz do opisu [m]|Poczatek obiektu [m]|Wysokosc konstrukcyjna|Koniec obiektu [m]|Szerokosc obiektu [m]|Kat skrzyzowania [st]|Rozpietosc [m])
	; gdzie pierwszy wiersz jest ignorowany podczas generowania, kazdy kolejny reprezentuje
	; obiekt WS badz WD np.:
	;Obiekt					Rodzaj	Kilometraz do opisu [m]	Poczatek obiektu [m]	Koniec obiektu [m]	Wysoko�� konstrukcyjna [m]	Szerokosc obiektu [m]	Kat skrzyzowania [st]	Rozpietosc [m]
	;MS-PZM/A-6,90/S11/1	WS		6900					6900					6926,9				1,75						28,9					90						26,9
	;WD-PZDs/A-9,08/S11/1	WD		9080					9080					9105,1				1,63						29,6					90						12,55+12,55
		
	;Pobranie listy danych z pliku .csv
	;(setq fileName (getstring "Enter database filename: "))
	(setq data (getData "obiekty.csv"))
	(setq lista (car data))
	(setq kolumny (cadr data))
	(setq len (length lista))
	
	;Parametry funkcji
	(setq scale 10.0)
	
	;Wkazanianie polilinii niwelety i polilini poczatku kilometraza
	(setq niweleta (car (entsel "\nWskaz polilinie niwelety: ")))
	(setq teren (car (entsel "\nWskaz polinie terenu: ")))
	(setq zerowy (car (entsel "\nWskaz polilinie pionowa poczatku kilometraza: ")))
	(setq poczatek_kilometraza (getreal "\nPodaj poczatek kilometraza w metrach: "))
	(command "_move" zerowy ""
		 (list 0.0 0.0)
		 (list (- 0.0 poczatek_kilometraza) 0.0)
	)
	
	;Wylaczenie przyciagania kursora
	(setq oldSettings (przyciaganieOFF))
	
	;Glowna petla programu: Rysowanie obiektow.
	(setq i 2) ; zmienna iteracyjna
	(repeat (- (/ len kolumny) 1)

		;Pobranie informacji na temat 1 obiektu:
		(setq iter (- 0 1)) ; zmienna iteracyjna
		(setq obiekt				(nth (- (* i kolumny) 		(- kolumny (setq iter (+ iter 1)))) lista))
		(setq rodzaj				(nth (- (* i kolumny) 		(- kolumny (setq iter (+ iter 1)))) lista))
		(setq kilometraz			(atof (nth (- (* i kolumny) (- kolumny (setq iter (+ iter 1)))) lista)))
		(setq poczatek_obiektu_km	(atof (nth (- (* i kolumny) (- kolumny (setq iter (+ iter 1)))) lista)))
		(setq koniec_obiektu_km 	(atof (nth (- (* i kolumny) (- kolumny (setq iter (+ iter 1)))) lista)))
		(setq wysokosc_konstrukcyjna(atof (nth (- (* i kolumny) (- kolumny (setq iter (+ iter 1)))) lista)))
		(setq szerokosc_obiektu		(atof (nth (- (* i kolumny) (- kolumny (setq iter (+ iter 1)))) lista)))
		(setq kat_skrzyzowania		(atof (nth (- (* i kolumny) (- kolumny (setq iter (+ iter 1)))) lista)))
		(setq skrajnia_pionowa		5.2)
		(setq rozpietosc			(nth (- (* i kolumny) (- kolumny (setq iter (+ iter 1)))) lista))
		(princ rozpietosc)
		;Przeskalowanie wysokosci
		(setq wysokosc_konstrukcyjna (* wysokosc_konstrukcyjna scale))
		(setq skrajnia_pionowa (* skrajnia_pionowa scale))
		
		(if (= rodzaj "WS")
			(WS niweleta teren zerowy
				obiekt 
				kilometraz 
				poczatek_obiektu_km
				koniec_obiektu_km
				wysokosc_konstrukcyjna
				scale
				rozpietosc)
		)
		(if (= rodzaj "WD")
			(WD niweleta zerowy
				obiekt 
				kilometraz 
				wysokosc_konstrukcyjna 
				szerokosc_obiektu
				kat_skrzyzowania
				skrajnia_pionowa)
		)
		(setq i (+ i 1))
	);repeat
	
	;Powrot poczatku kilometraza na jego domyslne miejsce
	(command "_move" zerowy ""
		 (list 0.0 0.0)
		 (list poczatek_kilometraza 0.0)
	)
	;Wlaczenie domyslnych ustawien przyciagania kursora
	(przyciaganieON oldSettings)
	
	(princ)
);defun
(defun WS ( niweleta teren zerowy obiekt kilometraz poczatek_obiektu_km koniec_obiektu_km wysokosc_konstrukcyjna scale rozpietosc
			/ niweleta_punkt_0 temp kopia_niweleta koniec_obiektu niweleta_length niweleta_middle_point spod_konstr_length spod_konstr_middle_point)

	;Utworzenie tymczasowej kopii niwelety (kopia_niweleta)
	(command "_copy" niweleta "" (list 0 0 0) (list 0 0 0))
	(setq kopia_niweleta (entlast))
	
	;Wyznaczenie kierunku tworzenia tymczasowych kopii polilinii poczatku kilometraza (temp)
	(setq niweleta_punkt_0 (vlax-curve-getStartPoint kopia_niweleta))
	(setq temp (+ (car niweleta_punkt_0) 1000))

	;Wrysowanie prostej pionowej (poczatek_obiektu)
	(command "_offset" poczatek_obiektu_km zerowy (list temp 0 0) "")
	(setq poczatek_obiektu (entlast))
	
	;Wrysowanie podpor
	(podpory niweleta teren poczatek_obiektu wysokosc_konstrukcyjna rozpietosc scale)
	
	;Wrysowanie prostej pionowej (koniec_obiektu)
	(command "_offset" koniec_obiektu_km zerowy (list temp 0 0) "")
	(setq koniec_obiektu (entlast))
	
	;Przesuniecie niwelety w dol o wysokosc konstrukcyjna (spod_konstr)
	(command "_copy" kopia_niweleta "" (list 0 0 0) (list 0 (- 0 wysokosc_konstrukcyjna) 0))
	(setq spod_konstr (entlast))

	;Znalezienie punktu wewnatrz obwiedni (pnt0)
	(setq spod_konstr_punkt_0 (vlax-curve-getStartPoint spod_konstr))
	(setq niweleta_punkt_koncowy (vlax-curve-getEndPoint kopia_niweleta))
	(setq spod_konstr_punkt_koncowy (vlax-curve-getEndPoint spod_konstr))
	
	(command "_trim" poczatek_obiektu ""(list kopia_niweleta niweleta_punkt_0) "")
	(command "_trim" koniec_obiektu "" 	(list kopia_niweleta niweleta_punkt_koncowy) "")
	(command "_trim" poczatek_obiektu ""(list spod_konstr spod_konstr_punkt_0) "")
	(command "_trim" koniec_obiektu ""	(list spod_konstr spod_konstr_punkt_koncowy) "")

	(setq niweleta_length (vlax-curve-getDistAtPoint kopia_niweleta (vlax-curve-getEndPoint kopia_niweleta)))
	(setq niweleta_middle_point (vlax-curve-getPointAtDist kopia_niweleta (/ niweleta_length 2))) 
	
	(setq spod_konstr_length (vlax-curve-getDistAtPoint spod_konstr (vlax-curve-getEndPoint spod_konstr)))
	(setq spod_konstr_middle_point (vlax-curve-getPointAtDist spod_konstr (/ spod_konstr_length 2)))
	
	(setq pnt0x (/ (+ (car niweleta_middle_point) (car spod_konstr_middle_point)) 2))
	(setq pnt0y (/ (+ (cadr niweleta_middle_point) (cadr spod_konstr_middle_point)) 2))  
	(setq pnt0z (caddr niweleta_middle_point))
	(setq pnt0 (list pnt0x pnt0y pnt0z))

	;Wykonanie obwiedni pomiedzy (niweleta), (poczatek_obiektu), (koniec_obiektu), (spod_konstr)
	(command "_zoom" "_c" pnt0 10 "_-boundary" "_a" "_b" "_n" kopia_niweleta spod_konstr poczatek_obiektu koniec_obiektu "" "" pnt0 "" "_zoom" "_p" )
	
	;Zakreskowanie obwiedni
	(command "_-hatch" "_s" (entlast) "" "")

	;Usuniecie fragmentow polilinii:
	(entdel kopia_niweleta)
	(entdel spod_konstr)
	(entdel poczatek_obiektu)
	(entdel koniec_obiektu)
	
	;Opis
	(setq text1 (desc obiekt 				(list (car niweleta_middle_point) (+ (cadr niweleta_middle_point) 9.0))))
	(setq text2 (desc (strcat "KM: " (getKM kilometraz)) 	(list (car niweleta_middle_point) (+ (cadr niweleta_middle_point) 6.0))))
)
(defun podpory (niweleta teren poczatek_obiektu wysokosc_konstrukcyjna rozpietosc scale / B h g gap zas pos lst len odcinki i temp fundament slup p0 pt)
	
	;; Rysuje podpory posrednie obiektu na niwelecie na podstawie rozpietosci oddzielonych znakiem "+", np:
	;; rozpietosc = 60+60+60 -> Rysowanie podpor podsrednich na dlugosci obiektu 60, 120m
	
	;(setq rozpietosc "60+180+150+120+80")
	(setq B 5.0) ;szerokosc fundamentu
	(setq h 1.0) ;wysokoc fundamentu
	(setq g 1.3) ;szerokosc slupa
	(setq gap 0.3) ;odleglosc miedzy podstawa slupa (lawy podlozyskowej) a dolna dolna plaszczyzna konstrukcji pomostu
	(setq zas 0.2) ;zasypka fundamentow
	
	;skalowanie wysokosci
	(setq h (* h scale))
	(setq zas (* zas scale))
	
	;wydobycie listy rozpietosci obiektu miedzy podporami 
	(setq odcinki (list))
	(if (= (vl-string-search "+" rozpietosc) nil)
		(setq odcinki (list rozpietosc))
		(progn
		(while (setq pos (vl-string-search "+" rozpietosc))
		
			(setq lst (substr rozpietosc 1 pos))
			(setq rozpietosc (substr rozpietosc (+ pos 2)))
			(setq len (strlen rozpietosc))
			(setq lst (vl-string-subst "." "," lst))
			(setq odcinki (cons lst odcinki))
			(if (= (vl-string-search "+" rozpietosc) nil)
				(setq odcinki (cons rozpietosc odcinki))
			)
		);while
		);progn
	);if
	(setq odcinki (reverse odcinki))
	(setq odcinki (cons "0.0" odcinki))
	
  	(setq rozpietosc 0.0)
	(setq i 0)
	(repeat (length odcinki)
		
		(setq rozpietosc (+ rozpietosc (atof (nth i odcinki))))

		(command "_copy" poczatek_obiektu "" (list 0.0 0.0) (list rozpietosc 0.0))
		(setq temp (entlast))
		
		;okreslenie punktu powyzej niwelety i ponizej
		(setq p0 (vlax-curve-getEndPoint temp))
		(setq pt (vlax-curve-getStartPoint temp))

		;dociecie osi
		(command "_trim" niweleta "" (list temp p0) "")
		(command "_trim" teren "" (list temp pt) "")
		
		;uzyskanie punktow przeciecia osi obiektu z niweleta p0 oraz z terenem pt
		(setq p0 (vlax-curve-getEndPoint temp))
		(setq pt (vlax-curve-getStartPoint temp))
		
		;narysowanie jednej podpory
		(if (and (> i 0) (< i (- (length odcinki) 1)));DOPISAC WARUNEK Z PODPORA SKRAJNA
		(progn
			;Fundament
			(command "_pline"
				(list 0.0 				0.0)
				(list (- 0.0 (/ B 2)) 	0.0)
				(list (- 0.0 (/ B 2)) 	h)
				(list (+ 0.0 (/ B 2)) 	h)
				(list (+ 0.0 (/ B 2)) 	0.0)
				"z"
			);_pline
			(setq fundament (entlast))
			;Slup
			(command "_pline"
				(list (- 0.0 (/ g 2)) 	h)
				(list (- 0.0 (/ g 2)) 	(+ h zas (- (cadr p0) (cadr pt) wysokosc_konstrukcyjna (* gap scale))))
				(list (+ 0.0 (/ g 2)) 	(+ h zas (- (cadr p0) (cadr pt) wysokosc_konstrukcyjna (* gap scale))))
				(list (+ 0.0 (/ g 2)) 	h)
				"z"
			);_pline
			(setq slup (entlast))
			;Przesuniecie podory w poziomie terenu do poczatku ukladu wspolrzednych
			(command "_move" fundament slup ""
				(list 0.0 0.0)
				(list 0.0 (- 0.0 h zas))
			)
			;Przesuniecie podpory na niwelete
			(command "_move" fundament slup ""
				(list 0.0 0.0)
				pt
			)
		);progn
		);if
		(setq i (+ i 1))
	);repeat
)
(defun WD ( niweleta zerowy obiekt kilometraz wysokosc_konstrukcyjna szerokosc_obiektu kat_skrzyzowania skrajnia_pionowa
			/ niweleta_punkt_0 temp pomocnicza pnt)

	;Zamiana stopni na radiany
	(setq kat_skrzyzowania (/ (* kat_skrzyzowania PI) 180.0))
	
	;Zmiana szeroko�ci obiektu wzgl�dem jego osi na szeroko�� wzgl�dem niwelety
	(setq szerokosc_obiektu (/ szerokosc_obiektu (sin kat_skrzyzowania)))
	
	;Wyznaczenie kierunku tworzenia tymczasowych kopii polilinii pocz�tku kilometra�a (temp)
	(setq niweleta_punkt_0 (vlax-curve-getStartPoint niweleta))
	(setq temp (+ (car niweleta_punkt_0) 1000))
	
	;Utworzenie tymczasowej kopii roboczej niwelety (kopia_niweleta)
	(command "_copy" niweleta "" (list 0 0 0) (list 0 0 0))
	(setq kopia_niweleta (entlast))

	;Konstrukcja
	;Znalezienie na niwelecie punktu przeciecia z osia obiektu WD (pnt)
	(command "_offset" kilometraz zerowy (list temp 0 0) "")
	(setq pomocnicza (entlast))
	(setq pnt (vlax-curve-getEndPoint kopia_niweleta))
	(command "_trim" pomocnicza "" (list kopia_niweleta pnt) "")
	(setq pnt (vlax-curve-getEndPoint kopia_niweleta ))
	
	;Wykasowanie linii pomocniczej i roboczej niwelety
	(entdel pomocnicza)
	(entdel kopia_niweleta)
	
	;Przesuniecie punktu na niwelecie o wysokosc skrajni pionowej
	(setq pnt (list (car pnt) (+ (cadr pnt) skrajnia_pionowa)))

	;Rysowanie obiektu
	(command "_pline"
		(list (- (car pnt) (/ szerokosc_obiektu 2.0)) (cadr pnt))
		(list (- (car pnt) (/ szerokosc_obiektu 2.0)) (+ (cadr pnt) wysokosc_konstrukcyjna))
		(list (+ (car pnt) (/ szerokosc_obiektu 2.0)) (+ (cadr pnt) wysokosc_konstrukcyjna))
		(list (+ (car pnt) (/ szerokosc_obiektu 2.0)) (cadr pnt))
	"z")
	
	;Zakreskowanie obwiedni
	(command "_-hatch" "_s" (entlast) "" "")
	
	;Opis
	(setq text1 (desc obiekt (list (car pnt) (+ (cadr pnt) wysokosc_konstrukcyjna 9.0))))
	(setq text2 (desc (strcat "KM: " (getKM kilometraz)) (list (car pnt) (+ (cadr pnt) wysokosc_konstrukcyjna 6.0))))
)
(defun c:niwP ( / scale fileName lista punkt0 oldSettings i iter OBIEKT RODZAJ KM B H D RZ)

	;OPIS FUNKCJI
	;- Zadaniem funkcji jest rysowanie przekrojow przepustow na niwelecie drogowej
	;- Aby narysowac przepusty nalezy wskazac punkt poczatku niwelety oraz jego rzedna wysokosciowa.
	;- Jako dane wejsciowe funkcja przyjmuje plik excel domyslnie nazwany  "przepusty.csv", ktory powinien byc przechowywany w tym samym folderze co rysunek niwelety
	;- Plik danych wejsciowych jest baza danych przepusty.csv z 7 kolumnami (OBIEKT|RODZAJ|KM|B|H|D|RZ)
	; gdzie pierwszy wiersz jest ignorowany podczas generowania, kazdy kolejny reprezentuje
	; przepust rurowy badz skrzyniowy np.:
	;OBIEKT		RODZAJ		KM		B	H		D		RZ
	;P-1		skrzynia	1000	2	1,5				45,4652
	;R-1		rura		1250,12				1,2		250,645
	
	;Parametry funkcji
	(setq scale 10.0)

	;Pobranie listy danych z pliku .csv
	;(setq fileName (getstring "Enter database filename: "))
	(setq data (getData "przepusty.csv"))
	(setq lista (car data))
	(setq kolumny (cadr data))
	(setq len (length lista))
	
	;Pobranie punktu odniesienia i rzednej odniesienia niwelety:
	(setq punkt0 (getpoint "\nWskaz punkt przeciecia linii odniesienia z osia niwelety: "))
	(setq punkt0 (list (- (car punkt0) (getreal "\nWpisz poczatek kilometraza w metrach: "))  (cadr punkt0) (getreal "\nWpisz wspolrzedna odniesienia w metrach: ")))

	;Wylaczenie przyciagania kursora
	(setq oldSettings(przyciaganieOFF))

	;Glowna petla programu: Rysowanie przepustow.
	(setq i 2) ; zmienna iteracyjna
	(repeat (- (/ len kolumny) 1)
	  	;Pobranie informacji na temat 1 przepustu:
		(setq iter (- 0 1)) ; zmienna iteracyjna
		(setq OBIEKT(nth (- (* i kolumny) (- kolumny (setq iter (+ iter 1)))) lista))
		(setq RODZAJ(nth (- (* i kolumny) (- kolumny (setq iter (+ iter 1)))) lista))
		(setq KM 	(atof (nth (- (* i kolumny) (- kolumny (setq iter (+ iter 1)))) lista)))
		(setq B 	(atof (nth (- (* i kolumny) (- kolumny (setq iter (+ iter 1)))) lista)))
		(setq H 	(atof (nth (- (* i kolumny) (- kolumny (setq iter (+ iter 1)))) lista)))
		(setq D 	(atof (nth (- (* i kolumny) (- kolumny (setq iter (+ iter 1)))) lista)))
		(setq RZ	(atof (nth (- (* i kolumny) (- kolumny (setq iter (+ iter 1)))) lista)))
		;Rysowanie
		(if (= RODZAJ "skrzynia")
			(skrzynia punkt0 OBIEKT KM RZ B H scale)
		)

		(if (= RODZAJ "rura")
			(rura punkt0 OBIEKT KM RZ D scale)
		)

		(setq i (+ i 1))
  	)

	;Wlaczenie domyslnych ustawien przyciagania kursora
	(przyciaganieON oldSettings)
	
	(princ)	
)
(defun skrzynia ( punkt0 OBIEKT KM RZ B H scale / sc pld plg zas inside outside dotted hatch text1 text2 text3)

	;(setq punkt0 (list 0.0 0.0 0.0))
	;(setq OBIEKT "SKRZYNIA")
	;(setq KM 10980.45)
	;(setq RZ 297.78)
	;(setq B 2.0)
	;(setq H 2.0)
	
	;Zmienne domyslne
	(setq sc 0.4)	;gr sciany
	(setq pld sc)	;gr plyty dolnej
  	(setq plg 0.6)	;gr plyty gornej
	(setq zas 1.0)	;gr zasypki

	;Rysowanie przekroju w punkcie 0,0,0
	;Opis
	(setq text1 (desc (strcat OBIEKT " Typ: " (rtos B 2 1) "x" (rtos H 2 1) "m")	(list 0.0 (- 0.0 (+ (* pld scale) 3.0 ) 0.0))))
	(setq text2 (desc (strcat "KM: " (getKM KM)) 				(list 0.0 (- 0.0 (+ (* pld scale) 6.0) 0.0))))
	(setq text3 (desc (strcat "RZ: " (rtos RZ 2 2)) 			(list 0.0 (- 0.0 (+ (* pld scale) 9.0) 0.0))))
	
  	;Konstrukcja
	;Przeskalowanie wysokosci
	(setq H (* H scale))
	(setq pld (* pld scale))
	(setq plg (* plg scale))
	(setq zas (* zas scale))
	
	(command "_pline" 	(list 0.0 		0.0)
				(list (- 0.0 (/ B 2)) 	0.0)
				(list (- 0.0 (/ B 2)) 	(+ 0.0 H))
				(list (+ 0.0 (/ B 2)) 	(+ 0.0 H))
		 		(list (+ 0.0 (/ B 2)) 	0.0)
	"z")
	(setq inside (entlast))

	(command "_pline" 	(list 0.0 			(- 0.0 pld))
				(list (- 0.0 (/ B 2) sc)	(- 0.0 pld))
				(list (- 0.0 (/ B 2) sc)	(+ 0.0 H plg))
				(list (+ 0.0 (/ B 2) sc)	(+ 0.0 H plg))
		 		(list (+ 0.0 (/ B 2) sc)	(- 0.0 pld))
	"z")
	(setq outside (entlast))

	;Zasypka
	(command "_-LINETYPE" "U" "DASHED" "")
	(command "_pline" 	(list (- 0.0 (/ B 2) sc)	(+ 0.0 H plg zas))
				(list (+ 0.0 (/ B 2) sc)	(+ 0.0 H plg zas))
	"")
	(setq dotted (entlast))
	(command "_-LINETYPE" "U" "ByLayer" "")

  	;Kreskowanie
	(command "_-hatch" "_s" outside inside "" "")
	(setq hatch (entlast))

	;Przesuniecie przekroju do wlasciwego kilometraza i rzednej
	(command "_move" inside outside	dotted hatch text1 text2 text3 ""
		 (list 0.0 0.0)
		 (list (+ KM (car punkt0)) (+ (cadr punkt0) (* scale (- RZ (caddr punkt0)))))
	)
	
	(princ)
);defun
(defun rura (punkt0 OBIEKT KM RZ D scale / rx ry zas center pipe dotted text1 text2 text3)

	;(setq punkt0 (list 0.0 0.0))
	;(setq OBIEKT "RURA")
	;(setq KM 10980.45)
	;(setq RZ 297.78)
	;(setq D 2.0)
	
	;Zmienne domyslne
	(setq rx (/ D 2.0))
	(setq ry (/ D 2.0))
  	(setq zas 1.0)

	;Rysowanie przekroju w punkcie 0,0,0
	;Opis
	(setq text1 (desc (strcat OBIEKT " fi" (rtos D 2 1)  "m")	(list 0.0 (- 0.0 3.0) 0.0)))
	(setq text2 (desc (strcat "KM: " (getKM KM)) 			(list 0.0 (- 0.0 6.0) 0.0)))
	(setq text3 (desc (strcat "RZ: " (rtos RZ 2 2)) 		(list 0.0 (- 0.0 9.0) 0.0)))

	;Konstrukcja
	;Przeskalowanie wysokosci
	(setq ry (* ry scale))
	(setq zas (* zas scale))
	
  	(setq center (list 0.0 ry))
	(command "_ellipse" "_c" center (polar center 0.0 rx) (polar center 90.0 ry))
	(setq pipe (entlast))

	;Zasypka
	(command "_-LINETYPE" "U" "DASHED" "")
	(command "_pline" 	(list (- 0.0 rx)	(+ (* ry 2) zas))
				(list (+ 0.0 rx)	(+ (* ry 2) zas))
	"")
	(setq dotted (entlast))
	(command "_-LINETYPE" "U" "ByLayer" "")
	
	;Przesuniecie przekroju do wlasciwego kilometraza i rzednej
	(command "_move" pipe dotted text1 text2 text3 ""
		 (list 0.0 0.0)
		 (list (+ KM (car punkt0)) (+ (cadr punkt0) (* scale (- RZ (caddr punkt0)))))
	)
	
	(princ)
)
(defun desc (opis pkt / text)

	;Utworzenie opisu
	(command "_text" "n" "cs" pkt 2.5 0.0 opis)
	(setq text (entlast))
)
(defun getKM (km / remainder result)

	;Konwersja formatu kilometraza z (np. 19879.90 na 19+880)
	(setq remainder (rem km 1000.0))
	(setq result remainder) ;tymczasowo zapisuje do result reszte z dzielenia
	(if (< remainder 10.0)
		(setq remainder (strcat "00" (rtos remainder 2 0)))
		(progn
			(if (< remainder 100.0)
				(setq remainder (strcat "0" (rtos remainder 2 0)))
				(setq remainder (rtos remainder 2 0))
			);if
		);progn
	);if
	(setq result (strcat (rtos (/ (- km result) 1000) 2 0) "+" remainder))
)
(defun przyciaganieOFF (/ oldSettings)

  	(setq oldSettings (list (getvar "osmode") (getvar "blipmode")))
	(setvar "osmode" 0)
	;Switch OFF snap
	(setvar "blipmode" 0)
	;Switch OFF Blipmode
	(princ oldSettings)
)
(defun przyciaganieON (oldSettings)

	(setvar "osmode" (car oldSettings))
	;Reset snap
	(setvar "blipmode" (cadr oldSettings))
	;Reset blipmode
)
(defun SplitStr ( s d / p )
	(if (setq p (vl-string-search d s))
		(cons (substr s 1 p) (SplitStr (substr s (+ p 1 (strlen d))) d)) (list s))
)

(defun c:km (/ ent1 pt1 pt2 lgt1 lgt11 rst userinput)

    (if (= n nil)    		(setq n 1.0))
    (if (= start_point nil) (setq start_point 0.0))
    (if (= typeoutput nil)  (setq typeoutput "T"))
	
    (setq userinput (getreal (strcat "\nPodaj zaokraglenie m <" (rtos n) ">: "))) ;zaokraglenie
    (if (= userinput "")
         (progn)
         (setq n userinput)
    )

    (setq userinput (getreal (strcat "\nKilometraz poczatkowy w metrach <" (rtos start_point) ">: "))) ;km pocz�tkowy
    (if (= userinput "")
         (progn)
         (setq start_point userinput)
    )

    (setq userinput (getstring (strcat "\nKilometraz w metrach [T/N]? <" typeoutput ">: "))) ;typ kilometraza
    (if (= userinput "")
         (progn)
         (setq typeoutput (strcase userinput))
    )
	
	;(command "_ncopy" (getpoint "\nWskaz niwelete: ") "" (list 0 0 0) (list 0 0 0))
    (setq ent1 (car (entsel "\nWskaz polilinie niwelety: ")))
    (while (setq pt1 (getpoint "\nWskaz punkt: "))
		(setq pt2 (vlax-curve-getClosestPointTo ent1 pt1))
		(setq lgt1 (+ start_point (vlax-curve-getDistAtPoint ent1 pt2)))
		;(setq lgt11 (* n (atoi (rtos (/ lgt1 (float n)) 2 0))))
		(setq lgt11 (atof (rtos lgt1 2 n)))
		(setq rst (rem lgt11 1000))
		;(command "_line" pt1 pt2 "")
		
		(if (= typeoutput "T")
			(princ lgt11)
			(princ (strcat (rtos (/ (- lgt11 rst) 1000))
				(if (< rst 10)
					"+00"
					(if (< rst 100)
						"+0"
						"+"
					)
				);if
				(rtos rst)
				);strcat
			);princ
		);if
	);while
	(entdel ent1)
	(princ)
);defun


