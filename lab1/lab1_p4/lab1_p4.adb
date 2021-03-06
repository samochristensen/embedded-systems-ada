with Ada.Text_IO; use Ada.Text_IO;
with Text_Io; use Text_Io;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Calendar; use Ada.Calendar;

procedure lab1_p4 is
    -- packages (1 total)
        -- 1. DIO, allows printing Duration to terminal
    -- globals (4 total)
        -- 1. F3_START, designates start time for F3 after F1 has started
        -- 2. HYPER_PERIOD, designates length of time for all tasks to repeat
        -- 3. PollTime, stores the time to start the next 'hyper period', time to start F1 again
        -- 4. Start_Time, stores the start time of the program, to display relative time
    -- tasks (4 total)
        -- 1. F1, runs every second on the second, unless deadlines aren't met. runs for 0.3s
        -- 2. F2, runs immediately after F1. runs for 0.15s
        -- 3. F3, runs 0.5s after F1 starts, runtime varies
        -- 4. watchdog, runs while F3 is running. detects if F3 exceeds deadline and resynchronizes F1
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