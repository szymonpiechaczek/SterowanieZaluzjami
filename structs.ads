package Structs is

	type Vector_Float is array (Positive range <>) of Float;
	type Vector_Integer is array (Positive range <>) of Integer;
	subtype Vector2 is Vector_Integer(1..2);

	type Weather is (Deszcz, Slonce);
	
	type Field_State is (Otwarta, Zamknieta);
	type Field_Matrix is array (Positive range <>, Positive range <>) of Field_State;
	
	function Make_Weather(Weather_Coefficient : in Integer) return Weather;

	function Str(W : Weather) return String;
	function Str(F_S : Field_State) return String;
	function Str(F_M : Field_Matrix) return String;
	

end Structs;
