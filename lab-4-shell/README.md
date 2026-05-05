# SNU CS M1522.00800 : System Programming
## Lab 4 : A Unix Shell

#### Please note that late submission for assignment 4 is NOT allowed.
#### Submission deadline : Friday, May 29, 23:59

## Purpose
A Unix shell is a program that makes the facilities of the operating system available to interactive users. There are several popular Unix shells: `sh` (the Bourne shell), `csh` (the C shell), and `bash` (the Bourne Again shell) are a few. The purpose of this assignment is to help you learn about Unix processes, low-level input/output, and signal handling.


## Your Task

Your task in this assignment is to create a program named `snush`. **If your program name isn't `snush`, you cannot get any score**. Your program should be a minimal but realistic interactive Unix shell. `sample_snush` will be provided so that you can compare your implementation of snush with the results.


## Building a Program
If your code cannot be compiled  with `gcc800`, **we cannot give you any points.** Please double check before you submit.
Makefile for building a program is already prepared. 
What you need to do is just build a program with `make` command. 
The submission you submit should of course also be able to be compiled using the make command. 
You cannot receive points if it is not compiled with 'make'. 

Note. The `tools/` directory is provided only for convenience and should not be included in your submission. The Makefile is already prepared so that even if the `tools/` directory is removed, `make` will still work without any problem.


## Initialization and Termination
Your program should print a **percent sign and a space (% )** before each such line. To facilitate your debugging and our testing, set `export DEBUG=1` before you run your snush. For clean running of your shell do `unset DEBUG`. Make sure the DEBUG environment variable is set by checking with `echo $DEBUG`.

#### exec snush example
% echo "snush shell here"
snush shell here
% 

Your program should terminate when the user types Ctrl-d or issues the `exit` command. (See also the section below entitled "Signal Handling.")


## Interactive Operation
After start-up processing, your program repeatedly should perform these actions:

*   Print a prompt, which consists of a percent sign(`%`) followed by a space, to the standard output stream.
*   Read a line from the standard input stream.
*   Lexically analyze the line to form an array of tokens.
*   Syntactically analyze the token array.
*   Form a command line for execution.
*   Add a job to a job manager.
*   Execute the command.

## Lexical Analysis
Informally, a _token_ should be a word. More formally, a token should consist of a sequence of non-white-space characters that are separated from other tokens by white-space characters. There should be two exceptions:

*   The special characters '>', and '<' should form separate tokens.
*   Strings enclosed in double quotes (") or single quotes(') should form part or all of a single token. Special characters inside of strings should not form separate tokens.

Your program should assume that no line of the standard input stream contains more than 1023 characters; the terminating newline character is included in that count. In other words, your program should assume that a string composed from a line of input can fit in an array of characters of length 1024. If a line of the standard input stream is longer than 1023 characters, then your program need not handle it properly; but it should not corrupt memory.

## Syntactic Analysis
A _command_ should be a sequence of tokens, the first of which specifies the command name.

## Built-In commands
Your program should interpret two shell built-in commands:

`cd [_dir_]`: Your program should change its working directory to `_dir_`, or to the HOME directory if `_dir_` is omitted.

`exit`: Your program should exit with exit status 0.

If the command is not a built-in, your shell must fork a child and execvp the target program with its arguments. If execvp fails, print an error message explaining the reason. 

If the command is a built-in (`cd`, `exit`) and is not part of a pipeline, it must be executed in the parent process (no fork/exec) so that its effects apply to the shell itself (e.g., `cd` changes the shell’s working directory; `exit` terminates the shell). 

If a built-in appears inside a pipeline, your shell must fork for that pipeline stage but must not call execvp. Instead, the child process executes the built-in directly and then calls exit. Consequently, the built-in’s effects do not propagate to the parent shell. 

*   `cd /tmp`: executed in parent; the shell’s working directory becomes /tmp.
*   `cd /tmp | ls`: cd runs in a child (no execvp), parent’s working directory is unchanged. And print out the result of `ls`.
*   `exit`: executed in parent, shell terminates.
*   `exit | cd`: exit runs only in that pipeline child, the shell does not exit. Does not change the parent's working directory.

### Job control with built-ins
In case of standalone built-ins (`cd`, `exit`), it is executed in the parent process without fork and exec. They are not added to the job manager. 
However, in case of built-in inside a pipeline (`exit | cd `, `cd | ls`), that pipeline stage runs in a child (not exec for the built-in) and the entire pipeline is added as one job with a process group. The job remains until all stages finish. 

`kill`: Modern shells (like bash) provide `kill` as a built-in command. However, in `snush` it is executed via the external `/bin/kill` or `/usr/bin/kill`. Because of this, there is a difference in syntax when killing a process group:

*   kill -9 pid: works (kill the specific process)
*   kill -9 -pgid: does not work in `snush`
*   kill -9 -- -pgid: works (kill the entire process group)

So when testing in snush, use `kill -9 -- -pgid` to send signals to a process group.

## Process Handling
*   A job must be added by the parent process (shell) before the child process begins executing the new binary program. (Hint: By default, `read()` performs blocking I/O. After a `fork()`, if a process performs a `read()` on one end of a pipe, the process will be blocked until the other writes to the pipe, effectively pausing until the data is available.)

*   In the current sample_snush, the job id is using unique identifiers starting from 1. but it is free to use the job id as a process group id or stick with the current unique identifier approach used in sample_snush. Remember that the job id should be unique integer number. (See also the section below entitled 'Job ID allocation')

*   All child processes created by your program should run in the foreground, meaning the shell should wait for them to complete before accepting new commands. You can wait for these processes using `wait_fg()` in `execute.c` if you have registered them properly with the job manager.

*   There is a global variable for a job manager. When a job is created, it is registered with the job manager which handles the processes at the granularity of the job. A job is a user's command line input. For example, if the user's command line input is "`ps -ef | grep job`" then one job, two processes.

*   To run processes in the background, use print_job() instead of wait_fg(). This function simply announces the start of a background job. Background processes will be also handled by `sigchld_handler()` and `handle_sigchld()` when they exit. After all processes belonging to a background job have terminated, handle the job accordingly. A message should be printed to indicate that the background job has completed.

## Signal Handling
**\[NOTE\] Ctrl-d represents EOF, not a signal. Do NOT make a signal handler for Ctrl-d.**

When the user types Ctrl+C, Linux sends a `SIGINT` signal to the parent process and its children. Upon receiving a `SIGINT` signal:

*   When the user types Ctrl+C, parent process receives `SIGINT`. You should implement your own `handle_sigint()`. It should interrupt the foreground job and sends signals to all processes.
*   Whenever the child process exits, it sends a signal `SIGCHLD`. You should implement your own `handle_sigchld()` for background process handling. It determines which job the process belongs to and remove the process information from that job. If all processes in the job have terminated, The job is removed from the job manager.
*   The printf function is not async-signal-safe, and using it in a signal handler can lead to unpredictable behavior across the program. So, please do not modify signal handlers(`sigint_handler`, `sigchld_handler`), as mentioned in the code comments. For more details, please refer to the signal-safety(7) official documentation: https://man7.org/linux/man-pages/man7/signal-safety.7.html
*   (Hint: Temporarily block SIGCHLD and SIGINT when updating job data to avoid race conditions.)
*   Instead of using printf functions to indicate that all processes belonging to a background job have terminated, print the message inside the `check_bg_status()` function.

## Redirection
You are going to implement redirection of standard input and standard output.
The input redirection handler is already implemented in the skeleton code as `redin_handler()`.
You need to implement the output redirection handler, `redout_handler()`.

*   The special character '<' and '>' should form separate token in lexical analysis.
*   The '<' token should indicate that the following token is a name of a file(`< filename`). Your program should redirect the command's standard input to that file. It should be an error to redirect a command's standard input stream more than once. (`cat < in1 < in2` : error)
* The '>' token should indicate that the following token is a name of a file(`> filename`). Your program should redirect the command's standard output to that file. It should be an error to redirect a command's standard output stream more than once. (`cmd > out.txt > out2.txt` : error)
*   If the standard input stream is redirected to a file that does not exist, then your program should print an appropriate error message.
*   If the standard output stream is redirected to a file that does not exist, then your program should create it. If the standard output stream is redirected to a file that already exists, then your program should destroy the file's contents and rewrite the file from scratch.
*   In this lab, you don't have to redirect standard error.
*   Note: While a regular shell allows per-segment input redirection (e.g., `A < a | B < b | C < c`, `A > x | B > y | C > z`), in snush these are treated as multiple redirections and is not allowed.
The implementation is designed to reject such cases, so please keep this in mind.

## Pipe
You are going to implement pipe between commands to redirect output from one command to the input of another.
A pipeline is a single job composed of N processes connected with `|`.
You need to implement `iter_pipe_for_exec()` which should return `PID` of the first child.

*   The special character `|` should be treated as a separate token during lexical analysis.
*   The `|` token should indicate that the standard output of the command before `|` should be redirected to the standard input of the command after `|`.
*   Multiple pipes should be supported, allowing chaining of commands (e.g., `cmd1 | cmd2 | cmd3`).
*   All processes in a pipeline must share the same process group ID(PGID).

## Error Handling
Return value/error checking is mandatory when doing the assignment to ensure proper error handling. 

## Submission
Your submission should be one gzipped tar file whose name is YourStudentID\_assign4.tar.gz. Your submission need to include the following files:

*   Your source code files. (If you used `dynarray` ADT, then submit the `dynarray.h` and `dynarray.c` files as well.)
*   `Makefile`. The first dependency rule should build your entire program. The `Makefile` should maintain object (.o) files to allow for partial builds, and encode the dependencies among the files that comprise your program. As always, use the `gcc800` command to build.
*   A `readme` file.

We also provide the [interface](dynarray.h) and [implementation](dynarray.c) of the `dynarray` ADT. You are welcome to use that ADT in your program.

Your `readme` file should contain:
*   Your name and the name and the student ID.
*   (Optionally) An indication of how much time you spent doing the assignment.
*   (Optionally) Any information that will help us to grade your work in the most favorable light. In particular you should describe all known bugs.

Your submission file should look like this:

202612345\_assign4.tar.gz 

    202612345\_assign4

        your\_source\_code.c (can be multiple files)

        your\_header.h (can be multiple files)

        Makefile

        readme

## Grading
Note: The total score for all tests before the Background process test is 100 points. 

### Extra Credit (extra 30 points)
To earn extra credit, implement background process support by enabling processes to run in the background. To earn the 30 extra credit points, your background process implementation must pass all background tests. **No partial extra credit will be awarded for background process support.**

**Since the testing server is not supported, please use `sample_snush` to complete the assignment.** 
**There are some test cases in `snush_sample_test_cases.md`.**

## Miscellenious

### Job ID allocation
Each job must have a unique integer job id(JID). In `sample_snush`, job ids are assigned as monotonically increasing unique identifiers starting from 1 by canning the current job list and picking max(JID) + 1. 
You are free to either keep this independent, seuqnectial JID scheme or using the processis group id as a JID. But remember that the JID must be a unique integer within the shell's job instance. If you choose JID, enuser the value is positive and unique. Job is managed with a JID in the job manager.

## Appendix
### functions for checking the presence of pipe or background
`count_pipe()` function counts the number of pipes in the command
`check_bg()` function checks if background process control operator '&' exists
do not modify these functions!