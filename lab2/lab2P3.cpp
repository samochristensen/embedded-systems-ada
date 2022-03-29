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
    0. Parse CLI arguments (as CSV)
    1. Read in user input
        o. make vector of task objects
        a. number of tasks
        b. total simulation time (msec)
        c. tasks info:
            I. ID
            II. Execution Time
            III. Period
    2. Schedule Tasks
        a. Rate Monotonic (RM) - shortest period first
        b. Earliest Deadline First (EDF) - earliest deadline first
    3. Output Schedule
        a. indicate preemption
        b. indicate missed deadlines
    4. Output Report
        a. number of preemptions
        b. number of deadlines missed


    constants:
    variables:
        1. number of preemptions
        2. number of missed deadlines
        3. number of tasks
        4. max simulation time
        5. simulation time
*/

#include <vector>
#include <iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <algorithm>

bool PRINT_DEBUG_2_SCREEN = false;

struct Task {
    std::string task_id= "x";
    int exec_time= -1;
    int release_time= -1;
    int period= -1;
};

void dispTasks(std::vector<Task> Tasks, std::string title=""){
    std::cout<< title << std::endl;
    std::cout<<"numTasks: " << Tasks.size() << std::endl;
    for(int i=0; i<Tasks.size(); i++){
        std::cout<<"  task_id:" << Tasks.at(i).task_id << std::endl;
        std::cout<<"  execution_time:" << Tasks.at(i).exec_time << std::endl;
        std::cout<<"  release_time:" << Tasks.at(i).release_time << std::endl;
        std::cout<<"  period:" << Tasks.at(i).period << std::endl;
        std::cout<<"--------------------\n";
    }
}

struct RM_sort{
    inline bool operator() (const Task& lhs, const Task& rhs){
        if(lhs.period < 0 || rhs.period < 0){
            std::cout<< "Warning, Comparing uninitialized period in 'RM_sort'"<< std::endl;
        }
        // std::printf("Comparing '%i' < '%i'.\n", lhs.period, rhs.period);
        return lhs.period < rhs.period;
    }
};

struct EDF_sort{
    inline bool operator() (const Task& lhs, const Task& rhs){
        if(lhs.exec_time < 0 || rhs.exec_time < 0){
            std::cout<< "Warning, Comparing uninitialized execution time in 'EDF_sort'"<< std::endl;
        }
        // std::printf("Comparing '%i + %i' < '%i + %i'.\n",lhs.release_time, lhs.exec_time, rhs.release_time, rhs.exec_time);
        return lhs.release_time + lhs.exec_time < rhs.release_time + rhs.exec_time;
    }
};

int getEmpty(char* sim, int lenSim, int min=0){
    int i = min;
    while (i < lenSim){
        if(sim[i] == '-'){
            return i;
        }
        i++;
    }
    return -1;
}

int insertPeriodicTask(Task task, char* sim, int lenSim, int min=0){
    int nextEmpty = getEmpty(sim, lenSim, min);
    int startPeriod = nextEmpty;
    if(nextEmpty < 0){return -1;}
    if(task.period < 0){return -2;}
    printf("nextEmpty (#0): %i\n", nextEmpty);
    for(int i = 0; i < task.exec_time; i++){
        if(sim[nextEmpty + i] != '-'){
            printf("nextEmpty (#1): %i\n", nextEmpty);
            nextEmpty = getEmpty(sim, lenSim, nextEmpty+i);
            if(nextEmpty < 0){return -3;}
        }
        sim[nextEmpty + i] = task.task_id[0];
    }
    printf("____Task %s_______\n", task.task_id.c_str());
    printf("startperiod: %i\n", startPeriod);
    printf("task.period: %i\n", task.period);
    insertPeriodicTask(task, sim, lenSim, startPeriod+task.period);
    return 1;
}

int main(int argc, char** argv){
    // std::vector<Task> myTasks;
    std::string inputFilename;
    std::string outputFilename;
    int numTasks = -1;
    int simTime = -1;

    // 1. Read in user input
    if(argc != 3){
        std::cout<< "Error: Invalid input. Requires two arguments: input filename, output filename" << std::endl;
        std::cout<< "\t\"./a.out input.txt output.txt\"" << std::endl;
        std::cout<< "Exiting..." << std::endl;
        exit(3);
    }else{
        inputFilename = argv[1];
        outputFilename = argv[2];
    }

    // 1.1 Store Data from Input File
    std::fstream inputFile(inputFilename, std::ios::in);    
    std::string line, word;
    std::vector<Task> periodicTasks;
    std::vector<Task> aperiodicTasks;
    if(inputFile.is_open()){
        getline(inputFile, line); std::istringstream (line) >> numTasks;
        getline(inputFile, line); std::istringstream (line) >> simTime;  

        // read in each __periodic task__ 
        for(int i=0; i<numTasks; i++){
            Task newTask;
            getline(inputFile, line);

            std::stringstream sstream(line);
            std::string tempStr;

            getline(sstream, tempStr, ','); std::istringstream (tempStr) >> newTask.task_id;
            getline(sstream, tempStr, ','); std::istringstream (tempStr) >> newTask.exec_time;
            getline(sstream, tempStr, ','); std::istringstream (tempStr) >> newTask.period;

            periodicTasks.push_back(newTask);
        }

        if (PRINT_DEBUG_2_SCREEN){dispTasks(periodicTasks, "Period Tasks-----");}

        // check for a aperiodic tasks
        getline(inputFile, line); std::istringstream (line) >> numTasks;
        // read in each __aperiodic task__ 
        for(int i=0; i<numTasks; i++){
            Task newTask;
            getline(inputFile, line);
            std::stringstream sstream(line);
            std::string tempStr;

            getline(sstream, tempStr, ','); std::istringstream (tempStr) >> newTask.task_id;
            getline(sstream, tempStr, ','); std::istringstream (tempStr) >> newTask.exec_time;
            getline(sstream, tempStr, ','); std::istringstream (tempStr) >> newTask.release_time;

            aperiodicTasks.push_back(newTask);
        }

        if (PRINT_DEBUG_2_SCREEN){dispTasks(aperiodicTasks, "Aperiodic Tasks-----");}
        // close file
    }else{
        std::printf("Failed to open file, user-provided filename: \"%s\"\n", &inputFilename[0]);
        std::cout<<"Exiting...\n";
        exit(3);
    }

    // 2. Schedule Tasks
    //     a. Rate Monotonic (RM) - shortest period first
    //     b. Earliest Deadline First (EDF) - earliest deadline first
    std::sort(periodicTasks.begin(), periodicTasks.end(), RM_sort());
    // std::sort(aperiodicTasks.begin(), aperiodicTasks.end(), RM_sort());
    // std::sort(periodicTasks.begin(), periodicTasks.end(), EDF_sort());
    // std::sort(aperiodicTasks.begin(), aperiodicTasks.end(), EDF_sort());

    // 2.1 Run Simulation, RM
    char* sim = new char[simTime];
    for(int i=0; i<simTime; i++){
        sim[i] = '-';
    }
    
    // for(int i=0; i < periodicTasks.size(); i++){
    for(int i = 0; i < periodicTasks.size(); i++){
        Task nextTask = periodicTasks.at(i);
        std::cout<<"Inserting: " << periodicTasks.at(i).task_id << std::endl;
        int result = insertPeriodicTask(nextTask, sim, simTime);
        if (result < 0){
            std::cout<<"Failed to insert task: " << periodicTasks.at(i).task_id << std::endl;
            printf("Failed (%i)\n", result);
        } else {std::cout<<"Success\n";}
    }
    // 3. Output Schedule
    //     a. indicate preemption
    //     b. indicate missed deadlines
    for (int i=0; i<simTime; i++){
        printf("(time, task): (%i, %c)\n", i, sim[i]);
    }

    // 4. Output Report
    //     a. number of preemptions
    //     b. number of deadlines missed

    delete sim;
}


