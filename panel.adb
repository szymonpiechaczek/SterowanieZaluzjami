-- panel.adb
--
-- materiały dydaktyczne
-- 2016
-- (c) Jacek Piwowarczyk
--

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Float_Text_IO;
use Ada.Float_Text_IO;

with Ada.Calendar;
use Ada.Calendar;
with Ada.Numerics.Float_Random;

with Ada.Strings;
use Ada.Strings;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Ada.Exceptions;
use Ada.Exceptions;

with Zaluzja;
use Zaluzja;

procedure Panel is
  
  Koniec : Boolean := False with Atomic;
  
  type StanyPogody is (Deszcz, Slonce);
  Pogoda : StanyPogody := Deszcz with Atomic;
	
  type TypySterowania is (Automatyczne, Manualne);
  SterowaniePolnoc : TypySterowania := Automatyczne with Atomic;
  SterowaniePoludnie : TypySterowania := Automatyczne with Atomic;
  SterowanieWschod : TypySterowania := Automatyczne with Atomic;
  SterowanieZachod : TypySterowania := Automatyczne with Atomic;
	
  type TypyZaluzji is (Polnoc, Poludnie, Wschod, Zachod);
  Zaluzja : TypyZaluzji := Polnoc with Atomic;
	
  type TypySterowaniaZaluzja is (Zaslon, Odslon, Stop);
  SterowanieZaluzja : TypySterowaniaZaluzja := Stop with Atomic;
  
  type Atrybuty is (Czysty, Jasny, Podkreslony, Negatyw, Migajacy, Szary);

  protected Ekran  is
    procedure Pisz_XY(X,Y: Positive; S: String; Atryb : Atrybuty := Czysty);
		procedure RysujZaluzje(X, Y : Positive; Zamkniete : Integer);
		procedure RysujPolnoc(Godzina : Natural);
		procedure RysujPoludnie(Godzina : Natural);
		procedure RysujWschod(Godzina : Natural);
		procedure RysujZachod(Godzina : Natural);
		procedure RysujWszystkie(Godzina : Natural);
		
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
  end Ekran;
  
  protected body Ekran is
    -- implementacja dla Linuxa i macOSX
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
			AktualizujZaluzje(38,3,Godzina,14,3,21, 12);
		end RysujPolnoc;
		
		procedure RysujPoludnie(Godzina : Natural) is
		begin
			AktualizujZaluzje(38,11,Godzina,14,6,18, 12);
		end RysujPoludnie;
		
		procedure RysujWschod(Godzina : Natural) is
		begin
			AktualizujZaluzje(63,7,Godzina,14,5,17, 12);
		end RysujWschod;
		
		procedure RysujZachod(Godzina : Natural) is
		begin
			AktualizujZaluzje(13,7,Godzina,16,10,21, 13);
		end RysujZachod;
		
		procedure RysujWszystkie(Godzina : Natural) is
		begin
			RysujPolnoc(Godzina);
			RysujPoludnie(Godzina);
			RysujWschod(Godzina);
			RysujZachod(Godzina);
		end RysujWszystkie;
		
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
      Ekran.Pisz_XY(40,2,"Północ", Atryb=>Czysty);
			Ekran.RysujWszystkie(0);
      Ekran.Pisz_XY(40,10,"Południe", Atryb=>Podkreslony);
      Ekran.Pisz_XY(65,6,"Wschód", Atryb=>Podkreslony);
      Ekran.Pisz_XY(15,6,"Zachód", Atryb=>Podkreslony);
      Ekran.Pisz_XY(1,18,"+= Q-koniec, A-automatyczne, M-manualne =+");
      Ekran.Pisz_XY(1,19,"+= [-odsłoń, ]-zasłoń, S-stop");
      Ekran.Pisz_XY(1,20,"+= D-Deszcz, S-Słońce =+");
      Ekran.Pisz_XY(1,21,"+= 1-Północ");
      Ekran.Pisz_XY(1,22,"+= 2-Południe");
      Ekran.Pisz_XY(1,23,"+= 3-Wschód");
      Ekran.Pisz_XY(1,24,"+= 4-Zachód");
    end Tlo; 
        
  end Ekran;
  
	-- PĘTLA PROGRAMU
  task Przebieg;
  task body Przebieg is
    use Ada.Numerics.Float_Random;
    
    Nastepny     : Ada.Calendar.Time;
		Godzina			 : Natural;
    Okres        : constant Duration := 1.0; -- sekundy
    Przesuniecie : constant Duration := 1.0;
    
    Gen : Generator;
    -- function Los_Fun return Float is 
    --    (Random(Gen) * (if Stan=Deszcz then 80.0 else 20.0) - 20.0);
    --Wartosc : Float := Los_Fun;
  begin
    --Reset(Gen);
    Nastepny := Clock + Przesuniecie;
		Godzina := 0;
    loop
      delay until Nastepny;
			
			Ekran.Czysc;
			Ekran.Tlo;
      Ekran.RysujWszystkie(Godzina);
			Ekran.Pisz_XY(2,17, Godzina'Img & ":00", Atryb=>Czysty);
      Ekran.Pisz_XY(10 ,17, Pogoda'Img, Atryb=>Podkreslony);
      Ekran.Pisz_XY(20 ,17, "Aktualnie wybrana: " & Zaluzja'Img, Atryb=>Podkreslony);
      Ekran.Pisz_XY(18 ,21, SterowaniePolnoc'Img, Atryb=>Podkreslony);
      Ekran.Pisz_XY(18 ,22, SterowaniePoludnie'Img, Atryb=>Podkreslony);
      Ekran.Pisz_XY(18 ,23, SterowanieWschod'Img, Atryb=>Podkreslony);
      Ekran.Pisz_XY(18 ,24, SterowanieZachod'Img, Atryb=>Podkreslony);
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

  Zn : Character;
  
begin
  -- inicjowanie
  Ekran.Tlo; 
  loop
    Get_Immediate(Zn);
    exit when Zn in 'q'|'Q';
    Pogoda := (if Zn in 'D'|'d' then Deszcz elsif Zn in 'S'|'s' then Slonce else Pogoda);
		-- zmiana sterowania żaluzji
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
			
			SterowanieZaluzja := (if Zn in '[' then Odslon
				elsif Zn in ']' then Zaslon
				elsif Zn in 'S'|'s' then Stop else SterowanieZaluzja);
  end loop;
  Koniec := True;
	delay 0.5;
	Ekran.Czysc;
end Panel;    