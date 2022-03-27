/*
    // LAB 2 PROBLEM 3
    Using C or C++, write a simulator that generates schedules for Rate Monotonic (RM) and Earliest Deadline First (EDF) 
    scheduling algorithms. The grader will use Cygwin and gcc / g++ to compile your code. Your program should read from 
    an input file, which contains the following information:
         Number of tasks
         Total simulation time length (msec)
         For each task, the following attributes are specified:
            o ID (needs to be unique)
            o Execution time (msec)
            o Period (with implicit deadline msec)

    To an output file, your program will write the corresponding RM and EDF schedules, clearly indicating when a job is 
    being preempted and when a job misses its deadline. The format of the output file is up to you but should be easy to
    understand. At the end of the simulation, your program will also include a summary, indicating the number of 
    preemptions and deadline misses per task and in total.

    The names of the input and output files will be specified by the user and passed in from the command line like these examples:
        $./scheduler.exe input.txt output.txt
        $./scheduler.exe f1.txt f2.txt

    Un-openable input or output files and mal-formed command lines should be handled.
*/
/*
    // Pseudocode

    1. Read in user input   
        a. number of tasks
        b. total simulation time (msec)
        c. tasks info:
            I. ID
            II. Execution Time
            III. Period
    2. Schedule Tasks...
    3. Output
    constants:
    variables:

*/