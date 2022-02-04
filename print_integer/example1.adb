with Ada.Text_IO;
use Ada.Text_IO;

procedure example1 is 
	i: Integer:= 0;

	procedure printVal(a: Integer) is
	begin
		if a>=0 then
			put_line(Integer'Image(a));
		end if;
	end printVal;
begin
	loop
		printVal(a => i); -- explicit mapping of inputs is not necessary
		i:= i + 2;
		exit when i > 10;
	end loop;
end example1;
