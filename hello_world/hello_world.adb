with Text_Io; -- similar to include statement
procedure Hello_World is  -- main compilation unit
	Message: constant String := "Hello World!";
begin
	Text_Io.Put_Line(Message); -- similar to printf in c
end Hello_World;
