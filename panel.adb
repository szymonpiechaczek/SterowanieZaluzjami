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
  
  type Stany is (Deszcz, Slonce);
  Stan : Stany := Deszcz with Atomic;
  
  type Atrybuty is (Czysty, Jasny, Podkreslony, Negatyw, Migajacy, Szary);

  protected Ekran  is
    procedure Pisz_XY(X,Y: Positive; S: String; Atryb : Atrybuty := Czysty);
		procedure RysujZaluzje(X, Y : Positive; Zamkniete : Integer);
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
    
    procedure Tlo is
    begin
      Ekran.Czysc;
      Ekran.Pisz_XY(20,1,"+=========== Sterowanie żaluzjami ===========+");
      Ekran.Pisz_XY(40,3,"Północ", Atryb=>Czysty);
      Ekran.RysujZaluzje(38,4,6);
      Ekran.Pisz_XY(40,13,"Południe", Atryb=>Podkreslony);
      Ekran.Pisz_XY(65,8,"Wschód", Atryb=>Podkreslony);
      Ekran.Pisz_XY(15,8,"Zachód", Atryb=>Podkreslony);
      Ekran.Pisz_XY(1,19,"+= Q-koniec, A-automatyczne, M-manualne =+");
      Ekran.Pisz_XY(1,20,"+= D-Deszcz, S-Słońce =+");
      Ekran.Pisz_XY(1,21,"+= 1-Północ, 2-Południe, 3-Wschód, 4-Zachód =+");
    end Tlo; 
        
  end Ekran;
  
  task Przebieg;

  task body Przebieg is
    use Ada.Numerics.Float_Random;
    
    Nastepny     : Ada.Calendar.Time;
		Godzina			 : Natural;
    Okres        : constant Duration := 1.0; -- sekundy
    Przesuniecie : constant Duration := 1.0;
    
    Gen : Generator;
    function Los_Fun return Float is 
        (Random(Gen) * (if Stan=Deszcz then 80.0 else 20.0) - 20.0);
    Wartosc : Float := Los_Fun;
  begin
    Reset(Gen);
    Nastepny := Clock + Przesuniecie;
		Godzina := 0;
    loop
      delay until Nastepny;
			
      Ekran.Pisz_XY(10 ,18, Stan'Img, Atryb=>Podkreslony);
			if (Godzina < 12 or else Godzina >= 18) then
				Ekran.RysujZaluzje(38,4,6);
			end if;
			if (Godzina < 12 and Godzina >= 6) then
      	Ekran.RysujZaluzje(38,4,6 - Godzina mod 6);
			end if;
			if (Godzina >= 12 and Godzina < 18) then
      	Ekran.RysujZaluzje(38,4,Godzina mod 6);
			end if;
			
			Ekran.Pisz_XY(2,18, Godzina'Img, Atryb=>Negatyw);
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
    Stan := (if Zn in 'D'|'d' then Deszcz elsif Zn in 'S'|'s' then Slonce else Stan);
  end loop; 
  Koniec := True;
end Panel;    