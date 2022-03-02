with Ada.Text_IO; use Ada.Text_IO;
with Text_Io; use Text_Io;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Calendar; use Ada.Calendar;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

procedure lab1_p5_watchdog is

    package DIO is new Text_Io.Fixed_Io(Duration);

    F3_START: constant Time_Span := Milliseconds(500);
    HYPER_PERIOD : constant Time_Span := Milliseconds(1000);
    Start_Time: constant Duration:= Ada.Calendar.Seconds(Clock);
    PollTime: Ada.Real_Time.Time := Clock + HYPER_PERIOD;

    function randgen return Float is 
        G: Generator;
        X: Uniformly_Distributed;
    begin
        reset(G);
        X:=Random(G);
        return X;
    end randgen;

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
            accept F1_start do
                put("F1 Started: ");
                DIO.Put(Ada.Calendar.Seconds(Clock) - Start_Time); put_line("");
                delay F1_delay;
                put("F1 Finished:");
                DIO.Put(Ada.Calendar.Seconds(Clock) - Start_Time); put_line("");
            end;
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
        F3_DELAY: constant Duration := 0.2;
        JITTER_SCALE: constant Float := 0.5;
        randomJitter: Duration;
        F3_actual_time: Duration;
    begin
        loop
            accept F3_start do
                randomJitter := Duration(randgen*JITTER_SCALE); -- get jitter from random variable
                F3_actual_time := F3_DELAY + randomJitter;
                put("F3 started: "); DIO.Put(Ada.Calendar.Seconds(Clock) - Start_Time); put_line("");
                watchdog.start;
                delay F3_actual_time;
                put("F3 finished:"); DIO.Put(Ada.Calendar.Seconds(Clock) - Start_Time); put_line("");
                watchdog.finish;
            end;
        end loop;
    end F3;

    task body watchdog is
        start_time, end_time: Duration;
        F3_time: Duration;
    begin
        loop
            select
                accept start do
                    start_time := Ada.Calendar.Seconds(Ada.Calendar.Clock);
                end start;
            or  
                accept finish do        
                    end_time := Ada.Calendar.Seconds(Ada.Calendar.Clock);
                    F3_time := end_time - start_time;
                    if ((F3_time) > 0.5) then
                        put("Warning, F3 Deadline Exceeded by:"); put_line(Duration'Image(F3_time - 0.5));
                        PollTime := PollTime + HYPER_PERIOD;
                    end if;
                end finish;
            or
                terminate;
            end select;
        end loop;
    end watchdog;

begin
    loop 
        delay until PollTime;
        F1.F1_start;
        F2.F2_start;
        delay until PollTime + F3_start;
        F3.F3_start;
        PollTime := PollTime + HYPER_PERIOD;
    end loop;
end lab1_p5_watchdog;