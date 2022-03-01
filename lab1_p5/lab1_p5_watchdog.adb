with Ada.Text_IO; use Ada.Text_IO;
with Text_Io; use Text_Io;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Calendar; use Ada.Calendar;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Discrete_Random;

procedure lab1_p5_watchdog is

    package DIO is new Text_Io.Fixed_Io(Duration);

    F3_start: constant Time_Span := Milliseconds(500);
    hyperPeriod : constant Time_Span := Milliseconds(1000);
    Poll_Time: Ada.Real_Time.Time:= Clock + hyperPeriod;

    Start_Time: Duration:= Ada.Calendar.Seconds(Clock);

    task F1 is entry F1_start; end F1;
    task F2 is entry F2_start; end F2;
    task F3 is entry F3_start; end F3;
    task watchdog is
        entry start;
        entry finish;
    end watchdog;

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
            watchdog.start;
            delay F3_delay;
            put("F3 finished:");
            DIO.Put(Ada.Calendar.Seconds(Clock) - Start_Time); put_line("");
            watchdog.finish;
        end loop;
    end F3;

    function randgen return Float is 
        subtype Num_Gen is Integer range 0 .. 10;
        package Random_Gen is new Ada.Numerics.Discrete_Random(Num_Gen);
        use Random_Gen;
        G: Random_Gen.Generator;
    begin
        return Float(random(G))/10.0; -- return scaled value
    end randgen;

    task body watchdog is
        start_time, end_time: Duration;
        curr_time: Duration;
    begin
        loop
            select
                accept start do
                    start_time := Ada.Calendar.Seconds(Ada.Calendar.Clock);
                    -- put_line("watchdog started");
                end start;
            or  
                accept finish do        
                    -- put_line("watchdog finished");
                    end_time := Ada.Calendar.Seconds(Ada.Calendar.Clock);
                    if ((end_time - start_time) > 0.5) then
                        put_line("[WARNING] F3 Deadline Missed" );
                        loop 
                            curr_time := Ada.Calendar.Seconds(Ada.Calendar.Clock);
                            exit when (curr_time - start_time) >= 2.0;
                        end loop;
                    end if;
                end finish;
            or
                terminate;
            end select;
        end loop;
    end watchdog;

begin
    loop 
        delay until Poll_Time;
        F1.F1_start;
        delay until Poll_Time + F3_start;
        F3.F3_start;
        Poll_Time := Poll_Time + hyperPeriod;
    end loop;
end lab1_p5_watchdog;