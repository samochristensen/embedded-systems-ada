with Ada.text_io; use Ada.text_io;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

procedure lab1_p6_comms is
    -- globals
    -- functions
    function randgen return Float is 
        G: Generator;
        X: Uniformly_Distributed;
    begin
        reset(G);
        X:=Random(G);
        return X;
    end randgen;

    -- types
    -- tasks
    task buffer is
        entry push(i : Integer);
        entry pop(i : out Integer);
    end buffer;
    task producer is entry producer_start; end producer;
    task consumer is entry consumer_start; end consumer;

    task body buffer is
        -- buffer_array: BufferArray := (0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        buffer_array: array(1 .. 10) of Integer;
        length : Integer := 0;
    begin
        loop
            select
                when length < 10 =>  -- block producer if full
                    accept push(i: Integer) do
                        buffer_array(length + 1) := i; -- insert value
                        length := length + 1; -- increment length
                        -- put("length now: "); put_line(Integer'Image(length));
                    end push;
            or
                when length > 0 => -- block consumer if empty
                    accept pop(i: out Integer) do
                        i:= buffer_array(length); -- remove value
                        length := length - 1;
                        -- put("length now: "); put_line(Integer'Image(length));
                    end pop;
            end select;
        end loop;

    end buffer;

    task body producer is
        DELAY_SCALE: constant Float:= 1.5;
        PRODUCT_SCALE: constant Float:= 25.0;
        producer_delay: Float;
        product: Integer;
    begin -- producer
        accept producer_start;
        loop
            producer_delay := DELAY_SCALE * randgen;
            delay Duration(producer_delay);
            product:= Integer(PRODUCT_SCALE * randgen);
            buffer.push(product);
            put("Produced:"); put_line(Integer'Image(product));
        end loop;
    end producer;

    task body consumer is
        DELAY_SCALE: constant Float:= 1.5;
        consumed: Integer;
        sum: Integer:= 0;
        consumer_delay: Float;
    begin -- consumer
        accept consumer_start;
        while sum < 100 loop
            consumer_delay := DELAY_SCALE * randgen;
            delay Duration(consumer_delay); -- get random delay
            buffer.pop(consumed);
            -- put("Delay:    "); put_line(Float'Image(consumer_delay));
            put("Consumed: "); put_line(Integer'Image(consumed));
            sum := sum + consumed;
        end loop;
        put_line("Should be done now!");
    end consumer;

begin -- lab1_p6_comms
    put_line("hello world!");
    consumer.consumer_start;
    producer.producer_start;

end lab1_p6_comms;