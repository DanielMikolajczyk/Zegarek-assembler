
.include "m328PBdef.inc"

.org 0x00
	rjmp prog_start ;skok do programu g³ównego

.org $0008 
	rjmp keypad_ISR ;Keypad External Interrupt Request

.org 0x32 ;adres pocz¹tku listy danych dyrektywy DB
	;stworzenie listy dziesiêciu liczb w przestrzeni pamiêci 
	prime: .DB 0x7E, 0x30, 0x6D, 0x79, 0x33, 0x5B, 0x5F, 0x70, 0x7F, 0x7B
.DSEG
.org 0x100
	liczniksek: .BYTE 1
	licznikmin: .BYTE 1
	licznikgod: .BYTE 1
	budzikmin: .BYTE 1
	budzikgod: .BYTE 1
.CSEG
.org 0x200 ;adres pocz¹tku programu
	prog_start:
		;inicjalizacja stosu
		ldi r16, high(ramend)	;load sph
		out sph, r16
		ldi r16, low(ramend)	;load spl
		out spl, r16
		;inicjalizacja wyjœæ
		ldi r16, $ff
		out ddrb, r16	; ustawienie na wyjœcie
		out ddrd, r16	; ustawienie na wyjœcie
		out ddre, r16	; ustawienie na wyjœcie
		ldi r27, $0f	;wyzeruj flagi
		out pcifr, r27


;---------------------------------------------------------
;				START
;---------------------------------------------------------
	restart:
		;inicjalizacja z
		ldi zl, low(2*prime)	;mno¿enie przez dwa, celem uzyskania adresu
		ldi zh, high(2*prime)	;w przestrzeni bajtowej

		;SET UP KEYPAD, 2 rows x 4 cols
		;Set rows as inputs and columns as outputs
		ldi r16, 0x0f
		out ddrc, r16
		;Set rows to high (pull ups) and columns to low
		ldi r16, 0x30
		out portc, r16
		;Select rows as interrupt triggers
		ldi r16, (1<<pcint12)|(1<<pcint13)
		sts pcmsk1, r16
		;Enable pcint1
		ldi r16, (1<<pcie1)
		sts pcicr, r16
		;Reset register for output
		ldi r20, 0x00
		;Global Enable Interrupt
		sei

		ldi r16, $00		;0 sekund
		ldi r17, $00		;0 minut
		ldi r18, $00		;0 godzin
		ldi r20, $00		;¿aden przycisk
		ldi r21, $01		;1 minut budzik
		ldi r22, $00		;0 godzin budzik

		sts liczniksek, r16		;zapisz zainicjowane wartoœci
		sts licznikmin, r17
		sts licznikgod, r18
		sts budzikmin, r21
		sts budzikgod, r22


;---------------------------------------------------------
;				MAIN LOOP
;---------------------------------------------------------
	repeat:
		ldi r27, $22					;sprawdŸ czy zosta³ wciœniêty 2x2 przycisk
		cp r20, r27
		breq jump_wyswietl_budzik		;jak tak to wyswietl budzik
		ldi r27, $24					;sprawdŸ czy zosta³ wciœniêty 2x3 przycisk
		cp r20, r27
		breq jump_wyswietl_budzik		;jak tak to wyswietl budzik
		ldi r27, $28					;sprawdŸ czy zosta³ wciœniêty 2x4 przycisk
		cp r20, r27
		breq jump_call_budzik				;jak tak to wyswietl budzik
		ldi r27, $18					;sprawdŸ czy zosta³ wciœniêty 1x4 przycisk
		cp r20, r27		
		brne wyswietl_mm_ss_jump		;jak nie to pokazuj minuty i sekundy
		rjmp wyswietl_gg_mm_jump		;jak tak to pokazuj godziny i minuty

	increment:
		cli
		inc r16 ;zwieksz sekunde
		
		;SEKUNDY
		call check_seconds_unity	;sprawdz 1 cyfre	
		cp r27, r29					;sprawdŸ czy jednoœci wynosz¹ 10
		brne zapisz					;jeœli nie to skocz z powrotem

		call check_seconds_tens		;sprawdz 2 cyfre
		cp r27, r29					;sprawdŸ czy dziesi¹tki wynosz¹ 6
		brne zapisz					;jak nie to skocz z powrotem
		ldi r16, $00				;jak tak to wyzeruj licznik sekund (nale¿y dodaæ minute)

		;MINUTY
		call check_minutes_unity	;sprawdz 1 cyfre
		cp r27, r29					;sprawdŸ czy jednoœci wynosz¹ 10
		brne zapisz					;jeœli nie to skocz z powrotem

		call check_minutes_tens
		cp r27, r29					;sprawdz 2 cyfre	
		brne zapisz					;jak nie to skocz z powrotem
		ldi r17, $00				;jak tak to wyzeruj licznik minut (nale¿y dodaæ godzinê)

		;GODZINY
		call check_hours_tens		;sprawdz 2 cyfre
		cp r27, r29
		brne godz_10				;jak nie s¹ równe 2 to sprawdŸ jednoœci dla liczby 10
		rjmp godz_4 				;jak s¹ równe 2 to sprawdŸ jednoœci dla liczby 4

	pogodzinach: nop
	zapisz:
		sts liczniksek, r16		;zapisz zainicjowane wartoœci
		sts licznikmin, r17
		sts licznikgod, r18
		sts budzikmin, r21
		sts budzikgod, r22

		rjmp compare
	compare_out:
		ldi r29, $11
		cp r20, r29
		breq jump_budzik_off
	sei_up:
		sei

		rjmp repeat
	rjmp restart
;----------------------------
	jump_increment:
		rjmp increment
	jump_wyswietl_budzik:
		rjmp wyswietl_budzik_jump
	jump_call_budzik:
		rjmp call_budzik
	jump_zapisz:
		rjmp zapisz
	jump_repeat:
		rjmp repeat
	jump_compare_out:
		rjmp compare_out
	jump_budzik_off:
		rjmp budzik_off

	;relatywne skoki
		wyswietl_mm_ss_jump:				;wyswietlanie formatu mm-ss
			rjmp jump_wyswietl_mm_ss

		wyswietl_budzik_jump:
			ldi r27, $24					;sprawdŸ czy zosta³ wciœniêty 2x3 przycisk
			cp r20, r27
			breq inc_budzik_min				;jak tak to zwiêksz minuty
			ldi r27, $22					;sprawdŸ czy zosta³ wciœniêty 2x2 przycisk
			cp r20, r27						;jak tak to zwiêksz godziny
			breq inc_budzik_god				
		call_budzik:	
			rjmp jump_call_wyswietl_budzik
			
		godz_10:
			call check_godz_10
			cp r27, r29			;sprawdŸ czy jednoœci wynosz¹ 10
			brne jump_zapisz	;jeœli nie to skocz z powrotem
			call check_godz_10_2
			rjmp pogodzinach	;powrót

		inc_budzik_min:
			call check_budzik_minutes_unity
			cp r27, r29					;sprawdŸ czy jednoœci wynosz¹ 10
			brne call_budzik			;jeœli nie to skocz do wyswietlenia budzika
			call check_budzik_minutes_tens
			cp r27, r29					;sprawdz 2 cyfre	
			brne call_budzik			;jak nie to skocz z powrotem
			ldi r21, $00				;jak tak to wyzeruj licznik minut
			rjmp call_budzik
			
		inc_budzik_god:
			call get_budzik_hours_tens
			cp r27, r29
			brne godz_10_budzik				;jak nie s¹ równe 2 to sprawdŸ jednoœci dla liczby 10
			rjmp godz_4_budzik 				;jak s¹ równe 2 to sprawdŸ jednoœci dla liczby 4


	;pozosta³e skoki 
		godz_4:
			call check_godz_4
			cp r27, r29			;sprawdŸ czy jednoœci wynosz¹ 4
			brne jump_zapisz	;jeœli nie to skocz z powrotem
			ldi r18, $00		;jeœli tak to wyzeruj ca³y licznik
			rjmp pogodzinach	;powrót

		godz_4_budzik:
			call check_godz_4_budzik
			cp r27, r29						;sprawdŸ czy jednoœci wynosz¹ 4
			brne jump_call_wyswietl_budzik	;jeœli nie to skocz z powrotem
			ldi r22, $00					;jeœli tak to wyzeruj godziny
			rjmp jump_call_wyswietl_budzik	;powrót

		godz_10_budzik:
			call check_godz_10_budzik
			cp r27, r29						;sprawdŸ czy jednoœci wynosz¹ 10
			brne jump_call_wyswietl_budzik	;jeœli nie to skocz z powrotem
			call check_godz_10_2_budzik
			rjmp jump_call_wyswietl_budzik	;powrót

		wyswietl_gg_mm_jump:
			rjmp jump_wyswietl_gg_mm
		
		compare:
			cp r17, r21
			brne jump_compare_out
			cp r18, r22
			brne jump_compare_out
			rjmp ring_budzik

stop: rjmp stop

jump_wyswietl_mm_ss:
	call wyswietl_mm_ss
	call wyswietl_mm_ss
	call wyswietl_mm_ss
	call wyswietl_mm_ss
	call wyswietl_mm_ss
	call wyswietl_mm_ss
	call wyswietl_mm_ss
	rjmp increment

jump_call_wyswietl_budzik:
	call wyswietl_budzik
	call wyswietl_budzik
	call wyswietl_budzik
	call wyswietl_budzik
	call wyswietl_budzik
	call wyswietl_budzik
	call wyswietl_budzik
	rjmp increment

jump_wyswietl_gg_mm:
	call wyswietl_gg_mm
	call wyswietl_gg_mm
	call wyswietl_gg_mm
	call wyswietl_gg_mm
	call wyswietl_gg_mm
	call wyswietl_gg_mm
	call wyswietl_gg_mm
	rjmp increment

ring_budzik:
	sbi porte, 0
	rjmp jump_compare_out

budzik_off:
	cbi porte, 0
	rjmp sei_up
;---------------------------------------------------
;			END OF LOOOP
;---------------------------------------------------

wyswietl_mm_ss:
	push r16
	push r17
	;------------------------------
	;wybranie pierwszego wyœwietlacza
	;------------------------------
	ldi r16, 0b11111110
	out portb, r16
	;przypisanie mu wartoœci
	lds r16, liczniksek
	andi r16,$0f ; ustawia bity m³odszej po³owy rejestru R16 na 0
	add zl, r16  ; dodaj ostatni¹ cyfrê do rejestru z
	lpm r16, z   ; pobierz wartoœæ rejestru z
	com r16      ; zaneguj
	out portd, r16 ;wyœwietl na wyœwietlaczu
	;odnowienie z
	ldi zl, low(2*prime)	;mno¿enie przez dwa, celem uzyskania adresu
	ldi zh, high(2*prime)	;w przestrzeni bajtowej
	call wait_50ms

	;------------------------------
	;wybranie drugiego wyœwietlacza
	;------------------------------
	ldi r16, 0b11111101
	out portb, r16
	;przypisanie mu wartoœci
	lds r16, liczniksek
	swap r16
	andi r16,$0f ; ustawia bity starszej po³owy rejestru R16 na 0
	add zl, r16  ; dodaj ostatni¹ cyfrê do rejestru z
	lpm r16, z   ; pobierz wartoœæ rejestru z
	com r16      ; zaneguj
	out portd, r16 ;wyœwietl na wyœwietlaczu
	;odnowienie z
	ldi zl, low(2*prime)	;mno¿enie przez dwa, celem uzyskania adresu
	ldi zh, high(2*prime)	;w przestrzeni bajtowej
	call wait_50ms

	;------------------------------
	;wybranie trzeciego wyœwietlacza
	;------------------------------
	ldi r16, 0b11111011
	out portb, r16
	;przypisanie mu wartoœci
	lds r16, licznikmin
	andi r16,$0f ; ustawia bity starszej po³owy rejestru R16 na 0
	add zl, r16  ; dodaj ostatni¹ cyfrê do rejestru z
	lpm r16, z   ; pobierz wartoœæ rejestru z
	com r16      ; zaneguj
	out portd, r16 ;wyœwietl na wyœwietlaczu
	;odnowienie z
	ldi zl, low(2*prime)	;mno¿enie przez dwa, celem uzyskania adresu
	ldi zh, high(2*prime)	;w przestrzeni bajtowej
	call wait_50ms

	;------------------------------
	;wybranie czwartego wyœwietlacza
	;------------------------------
	ldi r16, 0b11110111
	out portb, r16
	;przypisanie mu wartoœci
	lds r16, licznikmin
	swap r16
	andi r16,$0f ; ustawia bity starszej po³owy rejestru R16 na 0
	add zl, r16  ; dodaj ostatni¹ cyfrê do rejestru z
	lpm r16, z   ; pobierz wartoœæ rejestru z
	com r16      ; zaneguj
	out portd, r16 ;wyœwietl na wyœwietlaczu
	;odnowienie z
	ldi zl, low(2*prime)	;mno¿enie przez dwa, celem uzyskania adresu
	ldi zh, high(2*prime)	;w przestrzeni bajtowej
	call wait_50ms

	pop r17
	pop r16
	ret

;-------------------------


wyswietl_gg_mm:
	push r16
	push r17
	;------------------------------
	;wybranie pierwszego wyœwietlacza
	;------------------------------
	ldi r16, 0b11111110
	out portb, r16
	;przypisanie mu wartoœci
	lds r16, licznikmin
	andi r16,$0f ; ustawia bity m³odszej po³owy rejestru R16 na 0
	add zl, r16  ; dodaj ostatni¹ cyfrê do rejestru z
	lpm r16, z   ; pobierz wartoœæ rejestru z
	com r16      ; zaneguj
	out portd, r16 ;wyœwietl na wyœwietlaczu
	;odnowienie z
	ldi zl, low(2*prime)	;mno¿enie przez dwa, celem uzyskania adresu
	ldi zh, high(2*prime)	;w przestrzeni bajtowej
	call wait_50ms

	;------------------------------
	;wybranie drugiego wyœwietlacza
	;------------------------------
	ldi r16, 0b11111101
	out portb, r16
	;przypisanie mu wartoœci
	lds r16, licznikmin
	swap r16
	andi r16,$0f ; ustawia bity starszej po³owy rejestru R16 na 0
	add zl, r16  ; dodaj ostatni¹ cyfrê do rejestru z
	lpm r16, z   ; pobierz wartoœæ rejestru z
	com r16      ; zaneguj
	out portd, r16 ;wyœwietl na wyœwietlaczu
	;odnowienie z
	ldi zl, low(2*prime)	;mno¿enie przez dwa, celem uzyskania adresu
	ldi zh, high(2*prime)	;w przestrzeni bajtowej
	call wait_50ms

	;------------------------------
	;wybranie trzeciego wyœwietlacza
	;------------------------------
	ldi r16, 0b11111011
	out portb, r16
	;przypisanie mu wartoœci
	lds r16, licznikgod
	andi r16,$0f ; ustawia bity starszej po³owy rejestru R16 na 0
	add zl, r16  ; dodaj ostatni¹ cyfrê do rejestru z
	lpm r16, z   ; pobierz wartoœæ rejestru z
	com r16      ; zaneguj
	out portd, r16 ;wyœwietl na wyœwietlaczu
	;odnowienie z
	ldi zl, low(2*prime)	;mno¿enie przez dwa, celem uzyskania adresu
	ldi zh, high(2*prime)	;w przestrzeni bajtowej
	call wait_50ms

	;------------------------------
	;wybranie czwartego wyœwietlacza
	;------------------------------
	ldi r16, 0b11110111
	out portb, r16
	;przypisanie mu wartoœci
	lds r16, licznikgod
	swap r16
	andi r16,$0f ; ustawia bity starszej po³owy rejestru R16 na 0
	add zl, r16  ; dodaj ostatni¹ cyfrê do rejestru z
	lpm r16, z   ; pobierz wartoœæ rejestru z
	com r16      ; zaneguj
	out portd, r16 ;wyœwietl na wyœwietlaczu
	;odnowienie z
	ldi zl, low(2*prime)	;mno¿enie przez dwa, celem uzyskania adresu
	ldi zh, high(2*prime)	;w przestrzeni bajtowej
	call wait_50ms

	pop r17
	pop r16
	ret

;-------------------------

wyswietl_budzik:
	push r16
	push r17
	;------------------------------
	;wybranie pierwszego wyœwietlacza
	;------------------------------
	ldi r16, 0b11111110
	out portb, r16
	;przypisanie mu wartoœci
	lds r16, budzikmin
	andi r16,$0f ; ustawia bity m³odszej po³owy rejestru R16 na 0
	add zl, r16  ; dodaj ostatni¹ cyfrê do rejestru z
	lpm r16, z   ; pobierz wartoœæ rejestru z
	com r16      ; zaneguj
	out portd, r16 ;wyœwietl na wyœwietlaczu
	;odnowienie z
	ldi zl, low(2*prime)	;mno¿enie przez dwa, celem uzyskania adresu
	ldi zh, high(2*prime)	;w przestrzeni bajtowej
	call wait_50ms

	;------------------------------
	;wybranie drugiego wyœwietlacza
	;------------------------------
	ldi r16, 0b11111101
	out portb, r16
	;przypisanie mu wartoœci
	lds r16, budzikmin
	swap r16
	andi r16,$0f ; ustawia bity starszej po³owy rejestru R16 na 0
	add zl, r16  ; dodaj ostatni¹ cyfrê do rejestru z
	lpm r16, z   ; pobierz wartoœæ rejestru z
	com r16      ; zaneguj
	out portd, r16 ;wyœwietl na wyœwietlaczu
	;odnowienie z
	ldi zl, low(2*prime)	;mno¿enie przez dwa, celem uzyskania adresu
	ldi zh, high(2*prime)	;w przestrzeni bajtowej
	call wait_50ms

	;------------------------------
	;wybranie trzeciego wyœwietlacza
	;------------------------------
	ldi r16, 0b11111011
	out portb, r16
	;przypisanie mu wartoœci
	lds r16, budzikgod
	andi r16,$0f ; ustawia bity starszej po³owy rejestru R16 na 0
	add zl, r16  ; dodaj ostatni¹ cyfrê do rejestru z
	lpm r16, z   ; pobierz wartoœæ rejestru z
	com r16      ; zaneguj
	out portd, r16 ;wyœwietl na wyœwietlaczu
	;odnowienie z
	ldi zl, low(2*prime)	;mno¿enie przez dwa, celem uzyskania adresu
	ldi zh, high(2*prime)	;w przestrzeni bajtowej
	call wait_50ms

	;------------------------------
	;wybranie czwartego wyœwietlacza
	;------------------------------
	ldi r16, 0b11110111
	out portb, r16
	;przypisanie mu wartoœci
	lds r16, budzikgod
	swap r16
	andi r16,$0f ; ustawia bity starszej po³owy rejestru R16 na 0
	add zl, r16  ; dodaj ostatni¹ cyfrê do rejestru z
	lpm r16, z   ; pobierz wartoœæ rejestru z
	com r16      ; zaneguj
	out portd, r16 ;wyœwietl na wyœwietlaczu
	;odnowienie z
	ldi zl, low(2*prime)	;mno¿enie przez dwa, celem uzyskania adresu
	ldi zh, high(2*prime)	;w przestrzeni bajtowej
	call wait_50ms

	pop r17
	pop r16
	ret

;-------------------------

;podprogram -> opoŸnienie
wait_50ms:
	push r16
	push r17
	push r18
	ldi r16, $01
main_loop:
	ldi r17, $80
outer_loop:
	ldi r18, $ff
inner_loop:
	dec r18
	brne inner_loop
;koniec inner_loop
	dec r17
	brne outer_loop
;koniec outer_loop
	dec r16
	brne main_loop
;koniec main_loop
	pop r18
	pop r17
	pop r16
	ret
;-------------------------
;podprogram -> opoŸnienie
wait_500ms:
	push r16
	push r17
	push r18
	ldi r16, $a0
main_loop1:
	ldi r17, $ff
outer_loop1:
	ldi r18, $ff
inner_loop1:
	dec r18
	brne inner_loop1
;koniec inner_loop
	dec r17
	brne outer_loop1
;koniec outer_loop
	dec r16
	brne main_loop1
;koniec main_loop
	pop r18
	pop r17
	pop r16
	ret

;-------------------------
check_seconds_unity:
	mov r27, r16 
	andi r27, $0f	;weŸ tylko koñcow¹ czêœæ rejestru (odpowiedzialna za jednoœci sekund)
	ldi r29, $0a
	ret

;-------------------------
check_seconds_tens:
	andi r16, $f0	;wyzeruj jednoœci
	ldi r27, $10	;skoro doszliœmy tutaj to dodajemy 1 do liczby dziesi¹tek
	add r16, r27
	mov r27, r16
	andi r27, $f0	;weŸ tylko pocz¹tkow¹ czêœæ rejestru (odpowiedzialna za dziesi¹tki sekund)
	ldi r29, $60	;sprawdŸ czy dziesi¹tki wynosz¹ 6
	ret

;-------------------------
check_minutes_unity:
	inc r17			;jeœli tu jesteœmy to jest 60 sekund (nale¿y dodaæ 1 do jednoœci)
	mov r27, r17 
	andi r27, $0f	;weŸ tylko koñcow¹ czêœæ rejestru (odpowiedzialna za jednoœci minut)
	ldi r29, $0a
	ret

;-------------------------
check_minutes_tens:
	andi r17, $f0	;wyzeruj jednoœci
	ldi r27, $10	;skoro doszliœmy tutaj to dodajemy 1 do liczby dziesi¹tek
	add r17, r27
	mov r27, r17
	andi r27, $f0	;weŸ tylko pocz¹tkow¹ czêœæ rejestru (odpowiedzialna za dziesi¹tki minut)
	ldi r29, $60	;sprawdŸ czy dziesi¹tki wynosz¹ 6
	ret

;-------------------------
check_hours_tens:
	inc r18			;jeœli tu jesteœmy to jest 60 sekund (nale¿y dodaæ 1 do jednoœci)
	mov r27, r18 
	andi r27, $f0	;sprawdŸ liczê dziesi¹tek (wynosi 0,1, czy 2)
	ldi r29, $20
	ret

;-------------------------
check_godz_10:
	mov r27, r18
	andi r27, $0f		;weŸ tylko koñcow¹ czêœæ rejestru (odpowiedzialna za jednoœci godzin)
	ldi r29, $0a
	ret

;-------------------------
check_godz_10_2:
	andi r18, $f0		;jeœli tak to wyzeruj jednoœci
	ldi r27, $10	
	add r18, r27		;i dodaj jeden do dziesi¹tek
	ret

;-------------------------
check_godz_4:
	mov r27, r18
	andi r27, $0f		;weŸ tylko koñcow¹ czêœæ rejestru (odpowiedzialna za jednoœci godzin)
	ldi r29, $04
	ret

;-------------------------
check_budzik_minutes_unity:
	inc r21			;jeœli tu jesteœmy to jest 60 sekund (nale¿y dodaæ 1 do jednoœci)
	mov r27, r21 
	andi r27, $0f	;weŸ tylko koñcow¹ czêœæ rejestru (odpowiedzialna za jednoœci minut)
	ldi r29, $0a
	ret

;-------------------------
check_budzik_minutes_tens:
	andi r21, $f0	;wyzeruj jednoœci
	ldi r27, $10	;skoro doszliœmy tutaj to dodajemy 1 do liczby dziesi¹tek
	add r21, r27
	mov r27, r21
	andi r27, $f0	;weŸ tylko pocz¹tkow¹ czêœæ rejestru (odpowiedzialna za dziesi¹tki minut)
	ldi r29, $60	;sprawdŸ czy dziesi¹tki wynosz¹ 6
	ret

;------------------------
get_budzik_hours_tens:
	inc r22			;jeœli tu jesteœmy to jest 60 sekund (nale¿y dodaæ 1 do jednoœci)
	mov r27, r22 
	andi r27, $f0	;sprawdŸ liczê dziesi¹tek (wynosi 0,1, czy 2)
	ldi r29, $20
	ret

;-------------------------
check_godz_4_budzik:
	mov r27, r22
	andi r27, $0f		;weŸ tylko koñcow¹ czêœæ rejestru (odpowiedzialna za jednoœci godzin)
	ldi r29, $04
	ret

;-------------------------
check_godz_10_budzik:
	mov r27, r22
	andi r27, $0f		;weŸ tylko koñcow¹ czêœæ rejestru (odpowiedzialna za jednoœci godzin)
	ldi r29, $0a
	ret

;-------------------------
check_godz_10_2_budzik:
	andi r22, $f0		;jeœli tak to wyzeruj jednoœci
	ldi r27, $10	
	add r22, r27		;i dodaj jeden do dziesi¹tek
	ret


;--------------------------------------------------------
;			INTERRUPTIONS
;--------------------------------------------------------
;Keypad Interrupt Service Routine
keypad_ISR:
	cli					;wy³¹cz przerwania
	push r16
	ldi r20, $00
	
	;Set rows as outputs and columns as inputs
	ldi r16, 0x30
	out ddrc, r16
	;Set columns to high (pull ups) and rows to low
	ldi r16, 0x0f
	out portc, r16
	;Read Port C. Columns code in low nibble
	in r16, pinc
	;Store columns code to r18 on low nibble
	mov r20, r16
	andi r20, 0x0f
	;Set rows as inputs and columns as outputs
	ldi r16, 0x0f
	out ddrc, r16
	;Set rows to high (pull ups) and columns to low
	ldi r16, 0x30
	out portc, r16
	;Read Port C. Rows code in high nibble
	in r16, pinc
	;Merge with previous read
	andi r16, 0x30
	add r20, r16

	call wait_500ms		;poczekaj aby zapobiec drganiom styków
	ldi r27, $0f		;wyzeruj flagi
	out pcifr, r27
	pop r16
	sei					;w³¹cz przerwania
	reti
