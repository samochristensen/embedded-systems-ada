with Ada.Text_IO; use Ada.Text_IO;
with Text_Io; use Text_Io;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Calendar; use Ada.Calendar;

procedure lab1_p4 is

    package DIO is new Text_Io.Fixed_Io(Duration);

    F3_start: constant Time_Span := Milliseconds(500);
    hyperPeriod : constant Time_Span := Milliseconds(1000);
    Poll_Time: Ada.Real_Time.Time:= Clock + hyperPeriod;

    Start_Time: Duration:= Ada.Calendar.Seconds(Clock);

    task F1 is entry F1_start; end F1;
    task F2 is entry F2_start; end F2;
    task F3 is entry F3_start; end F3;

    task body F1 is
        F1_delay: Duration := 0.3;
    begin
        loop
            accept F1_start;
            put("F1 Started: ");
            DIO.Put(Ada.Calendar.Seconds(Clock) - Start_Time); put_line("");
            delay F1_delay;
            put("F1 Finished:");
            DIO.Put(Ada.Calendar.Seconds(Clock) - Start_Time); put_line("");
            F2.F2_start;
        end loop;
    end F1;

    task body F2 is
        F2_delay: Duration := 0.15;
    begin
        loop 
            accept F2_start;
            put("F2 started: ");
            DIO.Put(Ada.Calendar.Seconds(Clock) - Start_Time); put_line("");
            delay F2_delay;
            put("F2 finished:");
            DIO.Put(Ada.Calendar.Seconds(Clock) - Start_Time); put_line("");
        end loop;
    end F2;

    task body F3 is
        F3_delay: Duration := 0.2;
    begin
        loop
            accept F3_start;
            put("F3 started: ");
            DIO.Put(Ada.Calendar.Seconds(Clock) - Start_Time); put_line("");
            delay F3_delay;
            put("F3 finished:");
            DIO.Put(Ada.Calendar.Seconds(Clock) - Start_Time); put_line("");
        end loop;
    end F3;

begin
    loop 
        delay until Poll_Time;
        F1.F1_start;
        delay until Poll_Time + F3_start;
        F3.F3_start;
        Poll_Time := Poll_Time + hyperPeriod;
    end loop;
end lab1_p4;