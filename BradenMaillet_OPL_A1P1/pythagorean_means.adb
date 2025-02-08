with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Numerics.Elementary_Functions;

procedure Pythagorean_Means is
   package Math renames Ada.Numerics.Elementary_Functions;
   type Float_Array is array (Positive range <>) of Float;
   Count : Natural;
   Numbers : Float_Array(1..Ada.Command_Line.Argument_Count);
   Sum : Float := 0.0;
   Sum_Logs : Float := 0.0;
   Reciprocal_Sum : Float := 0.0;
   Arithmetic_Mean, Geometric_Mean, Harmonic_Mean : Float;
begin
   Count := Ada.Command_Line.Argument_Count;
   if Count = 0 then
      Ada.Text_IO.Put_Line("Error: Please provide numbers as command line arguments.");
      return;
   end if;

   -- Read numbers from command line
   for I in 1..Count loop
      Numbers(I) := Float'Value(Ada.Command_Line.Argument(I));
   end loop;

   -- Compute arithmetic mean
   for Num of Numbers loop
      Sum := Sum + Num;
   end loop;
   Arithmetic_Mean := Sum / Float(Count);

   -- Compute geometric mean using logarithms to prevent overflow
   for Num of Numbers loop
      Sum_Logs := Sum_Logs + Math.Log(Num);
   end loop;
   Geometric_Mean := Math.Exp(Sum_Logs / Float(Count));

   -- Compute harmonic mean
   for Num of Numbers loop
      Reciprocal_Sum := Reciprocal_Sum + (1.0 / Num);
   end loop;
   Harmonic_Mean := Float(Count) / Reciprocal_Sum;

   -- Output results
   Ada.Text_IO.Put_Line("Arithmetic Mean: " & Float'Image(Arithmetic_Mean));
   Ada.Text_IO.Put_Line("Geometric Mean:  " & Float'Image(Geometric_Mean));
   Ada.Text_IO.Put_Line("Harmonic Mean:   " & Float'Image(Harmonic_Mean));

exception
   when others =>
      Ada.Text_IO.Put_Line("Error: Invalid input. Please enter positive numbers.");
end Pythagorean_Means;