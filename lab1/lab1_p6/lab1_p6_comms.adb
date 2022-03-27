with Ada.text_io; use Ada.text_io;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

procedure lab1_p6_comms is
    -- globals (1 total)
        -- 1. isDone -- TODO: Replace with non-global method
    -- functions (1 total)
        -- 1. randgen
    -- tasks (3 total)
        -- 1. buffer
        -- 2. producer
        -- 3. consumer

    isDone: Boolean:= False; 
    
    function randgen return Float is 
    -- creates a random float from [0 .. 1]
        G: Generator;
        X: Uniformly_Distributed;
    begin -- randgen
        reset(G);
        X:=Random(G);
        return X;
    end randgen;

    task buffer is
        entry push(i : Integer);
        entry pop(i : out Integer);
    end buffer;
    task producer is entry producer_start; end producer;
    task consumer is entry consumer_start; end consumer;

    task body buffer is
    -- integers can be added to or removed from the locally stored array using the respective entry points
        buffer_array: array(1 .. 10) of Integer;
        length : Integer := 0;
    begin -- buffer
        loop
            select
                when length < 10 =>  -- block producer if full
                    accept push(i: Integer) do
                        buffer_array(length + 1) := i; -- insert value
                        length := length + 1; -- increment length
                    end push;
            or
                when length > 0 => -- block consumer if empty
                    accept pop(i: out Integer) do
                        i:= buffer_array(length); -- remove value
                        length := length - 1; -- decrement length
                    end pop;
            end select;
        end loop;

    end buffer;

    task body producer is
    -- implements buffer.push() to add integer values in range [1 .. 25] with random delays in range [0 .. 1.5]
        DELAY_SCALE: constant Float:= 1.5;
        PRODUCT_SCALE: constant Float:= 25.0;
        producer_delay: Float;
        product: Integer;
    begin -- producer
        accept producer_start;
        while isDone = False loop
            producer_delay := DELAY_SCALE * randgen;
            delay Duration(producer_delay);
            product:= Integer(PRODUCT_SCALE * randgen);
            buffer.push(product);
            put("Produced:"); put_line(Integer'Image(product));
        end loop;
    end producer;

    task body consumer is
    -- implements buffer.pop() to remove integers from buffer and increment locally stored sum. task ends when sum exceeds 100
        DELAY_SCALE: constant Float:= 2.0;
        consumed: Integer;
        sum: Integer:= 0;
        consumer_delay: Float;
    begin -- consumer
        accept consumer_start;
        while sum < 100 loop
            consumer_delay := DELAY_SCALE * randgen;
            delay Duration(consumer_delay); -- get random delay
            buffer.pop(consumed);
            put("Consumed: "); put_line(Integer'Image(consumed));
            sum := sum + consumed;
        end loop;
        put_line("Complete..");
        isDone:= True;
    end consumer;

begin -- lab1_p6_comms
    consumer.consumer_start;
    producer.producer_start;

end lab1_p6_comms;