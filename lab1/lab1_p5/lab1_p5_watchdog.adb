with Ada.Text_IO; use Ada.Text_IO;
with Text_Io; use Text_Io;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Calendar; use Ada.Calendar;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

procedure lab1_p5_watchdog is
    -- packages (1 total)
        -- 1. DIO, allows printing Duration to terminal
    -- globals (4 total)
        -- 1. F3_START, designates start time for F3 after F1 has started
        -- 2. HYPER_PERIOD, designates length of time for all tasks to repeat
        -- 3. Start_Time, stores the start time of the program, to display relative time
        -- 4. PollTime, stores the time to start the next 'hyper period', time to start F1 again
    -- functions (1 total)
        -- 1. randgen, random float generator with range [0 .. 1]
    -- tasks (4 total)
        -- 1. F1, runs every second on the second, unless deadlines aren't met. runs for 0.3s
        -- 2. F2, runs immediately after F1. runs for 0.15s
        -- 3. F3, runs 0.5s after F1 starts, runtime varies
        -- 4. watchdog, runs while F3 is running. detects if F3 exceeds deadline and resynchronizes F1

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
                F3_actual_time := F3_DELAY + randomJitter; -- add jitter to default runtime
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
                        PollTime := PollTime + HYPER_PERIOD;  -- resyncronize PollTime, pushes F1 to the next whole second
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