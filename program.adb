with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Float_Text_IO;
use Ada.Float_Text_IO;

with Ada.Calendar;
use Ada.Calendar;

with Ada.Strings;
use Ada.Strings;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Ada.Exceptions;
use Ada.Exceptions;

with Zaluzja;
use Zaluzja;

procedure Program is
  
  Koniec : Boolean := False with Atomic;
	
	Godzina : Natural := 0 with Atomic;
		
    Nastepny     : Ada.Calendar.Time;
    Okres        : constant Duration := 1.0;
    Przesuniecie : constant Duration := 1.0;

		ZaslonietePolnoc : Natural := 6;
		ZaslonietePoludnie : Natural := 6;
		ZaslonieteWschod : Natural := 6;
		ZaslonieteZachod : Natural := 6;
  
  type StanyPogody is (Deszcz, Slonce);
  Pogoda : StanyPogody := Slonce with Atomic;
	
  type TypySterowania is (Automatyczne, Manualne);
  SterowaniePolnoc : TypySterowania := Automatyczne with Atomic;
  SterowaniePoludnie : TypySterowania := Automatyczne with Atomic;
  SterowanieWschod : TypySterowania := Automatyczne with Atomic;
  SterowanieZachod : TypySterowania := Automatyczne with Atomic;
	
  type TypyZaluzji is (Polnoc, Poludnie, Wschod, Zachod);
  Zaluzja : TypyZaluzji := Polnoc with Atomic;
	
  type TypySterowaniaManualnego is (Zaslon, Odslon, Stop);
  SterowanieManualne : TypySterowaniaManualnego := Stop with Atomic;
  
  type Atrybuty is (Czysty, Jasny, Podkreslony, Negatyw, Migajacy, Szary);

  protected Ekran is
    procedure Pisz_XY(X,Y: Positive; S: String; Atryb : Atrybuty := Czysty);
		procedure RysujZaluzje(X, Y : Positive; Zamkniete : Integer);
		procedure RysujPolnoc(Godzina : Natural);
		procedure RysujPoludnie(Godzina : Natural);
		procedure RysujWschod(Godzina : Natural);
		procedure RysujZachod(Godzina : Natural);
--		procedure RysujWszystkie(Godzina : Natural);
		procedure SterujManualnie (X, Y : Positive; IleZaslonietych : in out Natural);
		
		procedure AktualizujZaluzje(X, Y : Positive;
																Godzina : Natural;
																GodzinaZaslaniania : Natural;
																GodzinaOdslaniania : Natural;
																GodzinaZaslonietych : Natural;
																GodzinaOdslonietych : Natural);		
    procedure Pisz_Float_XY(X, Y: Positive; 
                            Num: Float; 
                            Pre: Natural := 3; 
                            Aft: Natural := 2; 
                            Exp: Natural := 0; 
                            Atryb : Atrybuty := Czysty);
    procedure Czysc;
    procedure Tlo;
		procedure Rysowanie(Godzina : Natural);
	private
  end Ekran;
  
  protected body Ekran is
		
    function Atryb_Fun(Atryb : Atrybuty) return String is 
      (case Atryb is 
       when Jasny => "1m", when Podkreslony => "4m", when Negatyw => "7m",
       when Migajacy => "5m", when Szary => "2m", when Czysty => "0m"); 
       
    function Esc_XY(X,Y : Positive) return String is 
      ( (ASCII.ESC & "[" & Trim(Y'Img,Both) & ";" & Trim(X'Img,Both) & "H") );   
       
    procedure Pisz_XY(X,Y: Positive; S: String; Atryb : Atrybuty := Czysty) is
      Przed : String := ASCII.ESC & "[" & Atryb_Fun(Atryb);              
    begin
      Put( Przed);
      Put( Esc_XY(X,Y) & S);
      Put( ASCII.ESC & "[0m");
    end Pisz_XY;  
		
		--
		--
		procedure RysujZaluzje(X, Y : Positive; Zamkniete : Integer) is
			tmpY : Positive := Y;
		begin
			for I in Integer range 1 .. Zamkniete loop
			  Pisz_XY(X,tmpY,RysujZamknieta, Atryb=>Jasny);
				tmpY := tmpY + 1;
			end loop;
			for I in Integer range Zamkniete+1 .. 6 loop
			  Pisz_XY(X,tmpY,RysujOtwarta, Atryb=>Jasny);
				tmpY := tmpY + 1;
			end loop;
		end RysujZaluzje;
		
		procedure RysujPolnoc(Godzina : Natural) is
		begin
			if (Pogoda = Slonce) then
				Ekran.AktualizujZaluzje(38,3,Godzina,16,5,21, 9);
			elsif (Pogoda = Deszcz) then
				Ekran.AktualizujZaluzje(38,3,0,16,5,21, 9);
			end if;
		end RysujPolnoc;
		
		procedure RysujPoludnie(Godzina : Natural) is
		begin
			if (Pogoda = Slonce) then
			  Ekran.AktualizujZaluzje(38,11,Godzina,14,6,18, 12);
			elsif (Pogoda = Deszcz) then
			  Ekran.AktualizujZaluzje(38,11,0,14,6,18, 12);
			end if;
		end RysujPoludnie;
		
		procedure RysujWschod(Godzina : Natural) is
		begin
			if (Pogoda = Slonce) then
			  Ekran.AktualizujZaluzje(63,7,Godzina,14,5,17, 12);
			elsif (Pogoda = Deszcz) then
			  Ekran.AktualizujZaluzje(63,7,0,14,5,17, 12);
			end if;
		end RysujWschod;
		
		procedure RysujZachod(Godzina : Natural) is
		begin
			if (Pogoda = Slonce) then
			  Ekran.AktualizujZaluzje(13,7,Godzina,16,10,21, 13);
			elsif (Pogoda = Deszcz) then
			  Ekran.AktualizujZaluzje(13,7,0,16,10,21, 13);
			end if;
		end RysujZachod;
		
		procedure SterujManualnie (X, Y : Positive; IleZaslonietych : in out Natural) is
		begin
			if (SterowanieManualne = Stop) then
				RysujZaluzje(X,Y,IleZaslonietych);
			elsif (SterowanieManualne = Zaslon) then
				if (IleZaslonietych < 6) then	
					IleZaslonietych := IleZaslonietych + 1;
					RysujZaluzje(X,Y,IleZaslonietych);
				else
					RysujZaluzje(X,Y,IleZaslonietych);
				end if;
			else
				if (IleZaslonietych > 0) then	
					IleZaslonietych := IleZaslonietych - 1;
					RysujZaluzje(X,Y,IleZaslonietych);
				else
					RysujZaluzje(X,Y,IleZaslonietych);
				end if;
			end if;
		end SterujManualnie;
		
--		procedure RysujWszystkie(Godzina : Natural) is
--		begin
--			if (SterowaniePolnoc = Automatyczne) then
--				RysujPolnoc(Godzina);
--			elsif (Zaluzja = Polnoc) then
--				SterujManualnie(38,3,ZaslonietePolnoc);
--			else
--				RysujZaluzje(38,3,ZaslonietePolnoc);
--			end if;
--			if (SterowaniePoludnie = Automatyczne) then
--				RysujPoludnie(Godzina);
--			elsif (Zaluzja = Poludnie) then
--				SterujManualnie(38,11,ZaslonietePoludnie);
--			else
--				RysujZaluzje(38,11,ZaslonietePoludnie);
--			end if;
--			if (SterowanieWschod = Automatyczne) then
--				RysujWschod(Godzina);
--			elsif (Zaluzja = Wschod) then
--				SterujManualnie(63,7,ZaslonieteWschod);
--			else
--				RysujZaluzje(63,7,ZaslonieteWschod);
--			end if;
--			if (SterowanieZachod = Automatyczne) then
--				RysujZachod(Godzina);
--			elsif (Zaluzja = Zachod) then
--				SterujManualnie(13,7,ZaslonieteZachod);
--			else
--				RysujZaluzje(13,7,ZaslonieteZachod);
--			end if;
--		end RysujWszystkie;
--		
		procedure AktualizujZaluzje(X, Y : Positive;
																Godzina : Natural;
																GodzinaZaslaniania : Natural;
																GodzinaOdslaniania : Natural;
																GodzinaZaslonietych : Natural;
																GodzinaOdslonietych : Natural) is
			Zasloniete : Natural := 6;
			CzasZaslaniania : Natural := GodzinaZaslonietych - GodzinaZaslaniania;
			CzasOdslaniania : Natural := GodzinaOdslonietych - GodzinaOdslaniania;
		begin
			if (Godzina < GodzinaOdslaniania or Godzina >= GodzinaZaslonietych) then
				Zasloniete := 6;
				Ekran.RysujZaluzje(X,Y,Zasloniete);
			elsif (Godzina < GodzinaOdslonietych and Godzina >= GodzinaOdslaniania) then
				Zasloniete := Zasloniete - (Godzina - GodzinaOdslaniania) * 6 / CzasOdslaniania;
				if Zasloniete = 6 then
	    	 Ekran.RysujZaluzje(X,Y,Zasloniete - 1);
			 	else
	    	 Ekran.RysujZaluzje(X,Y,Zasloniete);
			 	end if;
			elsif (Godzina < GodzinaZaslaniania and Godzina >= GodzinaOdslonietych) then
				Zasloniete := 0;
	    	Ekran.RysujZaluzje(X,Y,Zasloniete);
			elsif (Godzina >= GodzinaZaslaniania and Godzina < GodzinaZaslonietych) then
				Zasloniete := (Godzina - GodzinaZaslaniania) * 6 / CzasZaslaniania;
	    	Ekran.RysujZaluzje(X,Y,Zasloniete);
			end if;
		end AktualizujZaluzje;
		--
		
    procedure Pisz_Float_XY(X, Y: Positive; 
                            Num: Float; 
                            Pre: Natural := 3; 
                            Aft: Natural := 2; 
                            Exp: Natural := 0; 
                            Atryb : Atrybuty := Czysty) is
                              
      Przed_Str : String := ASCII.ESC & "[" & Atryb_Fun(Atryb);              
    begin
      Put( Przed_Str);
      Put( Esc_XY(X, Y) );
      Put( Num, Pre, Aft, Exp);
      Put( ASCII.ESC & "[0m");
    end Pisz_Float_XY;
		
    
    procedure Czysc is
    begin
      Put(ASCII.ESC & "[2J");
    end Czysc;   
		
    -- RYSOWANIE TŁA NA POCZĄTKU
    procedure Tlo is
    begin
      Ekran.Czysc;
      Ekran.Pisz_XY(20,1,"+=========== Sterowanie żaluzjami ===========+");
      Ekran.Pisz_XY(40,2,"Północ", Atryb=>Podkreslony);
      Ekran.Pisz_XY(15,6,"Zachód", Atryb=>Podkreslony);
      Ekran.Pisz_XY(65,6,"Wschód", Atryb=>Podkreslony);
      Ekran.Pisz_XY(40,10,"Południe", Atryb=>Podkreslony);
      Ekran.Pisz_XY(20 ,17, "Aktualnie wybrana: ", Atryb=>Czysty);
      Ekran.Pisz_XY(1,18," Q-koniec, A-automatyczne, M-manualne =+");
      Ekran.Pisz_XY(1,19," [-odsłoń, ]-zasłoń, S-stop");
      Ekran.Pisz_XY(1,20," D-Deszcz, S-Słońce =+");
      Ekran.Pisz_XY(1,21," 1-Północ");
      Ekran.Pisz_XY(1,22," 2-Południe");
      Ekran.Pisz_XY(1,23," 3-Wschód");
      Ekran.Pisz_XY(1,24," 4-Zachód");
    end Tlo; 
		
		procedure Rysowanie(Godzina : Natural) is
		begin
			Ekran.Czysc;
			Ekran.Tlo;
			--Ekran.RysujWszystkie(Godzina);
			Ekran.Pisz_XY(2,17, Godzina'Img & ":00", Atryb=>Czysty);
      Ekran.Pisz_XY(12 ,17, Pogoda'Img, Atryb=>Podkreslony);
      Ekran.Pisz_XY(40 ,17, Zaluzja'Img, Atryb=>Podkreslony);
      Ekran.Pisz_XY(18 ,21, SterowaniePolnoc'Img, Atryb=>Podkreslony);
      Ekran.Pisz_XY(18 ,22, SterowaniePoludnie'Img, Atryb=>Podkreslony);
      Ekran.Pisz_XY(18 ,23, SterowanieWschod'Img, Atryb=>Podkreslony);
      Ekran.Pisz_XY(18 ,24, SterowanieZachod'Img, Atryb=>Podkreslony);
		end Rysowanie;
  end Ekran;

	
	-- KONTROLERY ŻALUZJI, ZADANIA
	
	
	task KontrolerPolnocy;
	task body KontrolerPolnocy is
	begin
    Nastepny := Clock + Przesuniecie;
		loop
      delay until Nastepny;
			-- kontroler
			if (SterowaniePolnoc = Automatyczne) then
				Ekran.RysujPolnoc(Godzina);
			elsif (Zaluzja = Polnoc) then
				Ekran.SterujManualnie(38,3,ZaslonietePolnoc);
			else
				Ekran.RysujZaluzje(38,3,ZaslonietePolnoc);
			end if;
			--
      exit when Koniec;
		end loop;
    exception
      when E:others =>
        Put_Line("Error: Zadanie Przebieg");
        Put_Line(Exception_Name (E) & ": " & Exception_Message (E)); 
	end KontrolerPolnocy;
	
	task KontrolerPoludnia;
	task body KontrolerPoludnia is
	begin
    Nastepny := Clock + Przesuniecie;
		loop
      delay until Nastepny;
			-- kontroler
			if (SterowaniePoludnie = Automatyczne) then
				Ekran.RysujPoludnie(Godzina);
			elsif (Zaluzja = Poludnie) then
				Ekran.SterujManualnie(38,11,ZaslonietePoludnie);
			else
				Ekran.RysujZaluzje(38,11,ZaslonietePoludnie);
			end if;
			--
      exit when Koniec;
		end loop;
    exception
      when E:others =>
        Put_Line("Error: Zadanie Przebieg");
        Put_Line(Exception_Name (E) & ": " & Exception_Message (E)); 
	end KontrolerPoludnia;
	
	task KontrolerWschodu;
	task body KontrolerWschodu is
	begin
    Nastepny := Clock + Przesuniecie;
		loop
      delay until Nastepny;
			-- kontroler
			if (SterowanieWschod = Automatyczne) then
				Ekran.RysujWschod(Godzina);
			elsif (Zaluzja = Wschod) then
				Ekran.SterujManualnie(63,7,ZaslonieteWschod);
			else
				Ekran.RysujZaluzje(63,7,ZaslonieteWschod);
			end if;
			--
      exit when Koniec;
		end loop;
    exception
      when E:others =>
        Put_Line("Error: Zadanie Przebieg");
        Put_Line(Exception_Name (E) & ": " & Exception_Message (E)); 
	end KontrolerWschodu;
	
	task KontrolerZachodu;
	task body KontrolerZachodu is
	begin
    Nastepny := Clock + Przesuniecie;
		loop
      delay until Nastepny;
			-- kontroler
			if (SterowanieZachod = Automatyczne) then
				Ekran.RysujZachod(Godzina);
			elsif (Zaluzja = Zachod) then
				Ekran.SterujManualnie(13,7,ZaslonieteZachod);
			else
				Ekran.RysujZaluzje(13,7,ZaslonieteZachod);
			end if;
			--
      exit when Koniec;
		end loop;
    exception
      when E:others =>
        Put_Line("Error: Zadanie Przebieg");
        Put_Line(Exception_Name (E) & ": " & Exception_Message (E)); 
	end KontrolerZachodu;
	
	-- KONIEC KONTROLERÓW
	
	
	-- Obliczanie Godziny, rysowanie tła
  task Przebieg;
  task body Przebieg is
  begin
    Nastepny := Clock + Przesuniecie;
    loop
      delay until Nastepny;
			
			Ekran.Rysowanie(Godzina);
			Godzina := (Godzina + 1) mod 24;
			
      exit when Koniec;
      Nastepny := Nastepny + Okres;
    end loop; 
    Ekran.Pisz_XY(1,11,"");
    exception
      when E:others =>
        Put_Line("Error: Zadanie Przebieg");
        Put_Line(Exception_Name (E) & ": " & Exception_Message (E)); 
  end Przebieg;

	-- ZADANIE ODPOWIEDZIALNE ZA STEROWANIE
	
	task KontrolerSterowania;
	task body KontrolerSterowania is
	  Zn : Character;
	begin
	  loop
   	 	Get_Immediate(Zn);
	    exit when Zn in 'q'|'Q';
			
	    Pogoda := (if Zn in 'D'|'d' then Deszcz elsif Zn in 'S'|'s' then Slonce else Pogoda);
			if (Zaluzja = Polnoc) then
	    	SterowaniePolnoc := (if Zn in 'A'|'a' then Automatyczne
					elsif Zn in 'M'|'m' then Manualne else SterowaniePolnoc);
			elsif (Zaluzja = Poludnie) then
	    	SterowaniePoludnie := (if Zn in 'A'|'a' then Automatyczne
					elsif Zn in 'M'|'m' then Manualne else SterowaniePoludnie);
			elsif (Zaluzja = Wschod) then
	    	SterowanieWschod := (if Zn in 'A'|'a' then Automatyczne
					elsif Zn in 'M'|'m' then Manualne else SterowanieWschod);
			elsif (Zaluzja = Zachod) then
	    	SterowanieZachod := (if Zn in 'A'|'a' then Automatyczne
					elsif Zn in 'M'|'m' then Manualne else SterowanieZachod);
			end if;
			-- wybór żaluzji
	    Zaluzja := (if Zn in '1' then Polnoc
				elsif Zn in '2' then Poludnie
				elsif Zn in '3' then Wschod 
			  elsif Zn in '4' then Zachod else Zaluzja);
			
			SterowanieManualne := (if Zn in '[' then Odslon
				elsif Zn in ']' then Zaslon
				elsif Zn in 'S'|'s' then Stop else SterowanieManualne);
		end loop;
		Koniec := True;
    exception
      when E:others =>
        Put_Line("Error: Zadanie Przebieg");
        Put_Line(Exception_Name (E) & ": " & Exception_Message (E)); 
	end KontrolerSterowania;

begin
  loop
		exit when koniec;
  end loop;
	delay 0.5;
	Ekran.Czysc;
end Program;    