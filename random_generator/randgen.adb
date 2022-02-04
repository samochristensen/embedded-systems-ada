-- file: randgen.adb
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Numerics;
use Ada.Numerics;
with Ada.Numerics.Discrete_Random;
--use Ada.Numerics.Discrete_Random;

procedure randgen is 
	subtype Num_Gen is Integer range 0 .. 10;
	package Random_Gen is new Ada.Numerics.Discrete_Random(Num_Gen);
	use Random_Gen;
	G: Random_Gen.Generator;
	i: Num_Gen;
	procedure printVal(a: Num_Gen) is
	begin
		put_line(Integer'Image(a));
	end printVal;
begin
	Reset(G);
	loop
		i := random(G);
		printVal(a => i); -- explicit mapping of inputs is not necessary
		exit when i = 10;
	end loop;
end randgen;
