with Ada.Numerics.Discrete_Random, Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package body Structs is

	function Make_Weather(Weather_Coefficient : in Integer) return Weather is
	begin
		if Weather_Coefficient < 16 then
			return Sunny;
		elsif Weather_Coefficient < 70 then
			return Cloudy;
		else
			return Rainy;
		end if;
	end Make_Weather;

	function Str(W : Weather) return String is
	begin
		case W is
			when Deszcz => return "DESZCZ";
			when Slonce => return "SLONCE";
		end case;
	end Str;

	function Str(F_S : Field_State) return String is
	begin
		case F_S is
			when Otwarta => return "|";
			when Zamknieta => return "-";
		end case;
	end Str;

	function Str(F_M : Field_Matrix) return String is
		Tmp : Unbounded_String;
	begin
		for R in F_M'Range(1) loop
			for C in F_M'Range(2) loop
				Tmp := Tmp & Str(F_M(R, C)) & " ";
			end loop;
			Tmp := Tmp & Ascii.LF;
		end loop;
		return To_String(Tmp);
	end Str;

end Structs;
