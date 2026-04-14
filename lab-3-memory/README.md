# Lab 3: Dynamic Memory Manager Module

## Purpose

The purpose of this assignment is to help you understand how dynamic memory management works in C. It also will give you more opportunity to use the GNU/Unix programming tools.

## Important Dates

| Date | Description |
|:---  |:--- |
| Friday, May 1, 23:59 | Submission deadline |

## Overview

Please read this file (README.md) carefully.

The assignment starts by extracting the provided `lab-3-memory.tar.gz` file.
```bash
$ tar -xvfz lab-3-memory.tar.gz
```

## Your task

You are given the interface of a `heapmgr` (heap manager) module in a file named `heapmgr.h`. It declares two functions:
```
void *heapmgr_malloc(size_t ui_bytes);
void  heapmgr_free(void *pv_bytes);
```
You also are given three implementations of the heapmgr module:

* `heapmgrgnu.c` is an implementation that simply calls the GNU `malloc()` and `free()` functions.
* `heapmgrkr.c` is the Kernighan and Ritchie (K&R) implementation, with small modifications for the sake of simplicity.
* `heapmgrbase.c` is an implementation that you will find useful as a baseline for your implementations.

Your task is to create a implementation (or two implementations) of the `heapmgr` module.  
Your implementation, **`heapmgr1.c`**, should enhance `heapmgrbase.c` so it is reasonably efficient.  
To do that it should use a single doubly-linked list and chunks that contains headers and footers (as described in lectures).  
Unlike the K&R implementation, the baseline code uses a **non-circular** list, and your code should implement a **doubly-linked** list. (You can implement either **circular** or **non-circular**)

If designed properly, `heapmgr1.c` will be reasonably efficient in most cases.  
However, `heapmgr1.c` is subject to poor worst-case behavior. An additional implementation, **`heapmgr2.c`**, should enhance `heapmgr1.c` so the worst-case behavior is not poor.  
To do that it should use **multiple doubly-linked lists**, alias **bins**.  
Note that implementing `heapmgr2.c` is **not mandatory**, and you can get full marks without the implementation of `heapmgr2.c`; Instead, you will get **extra credit** if you implement `heapmgr2.c`.  

Your `heapmgr` implementations should not call the standard `malloc()`, `free()`, `calloc()`, or `realloc()` functions.

Your `heapmgr` implementations should thoroughly validate function parameters by calling the standard `assert()` macro.

Your `heapmgr` implementations should check invariants by:
* Defining a thorough `check_heap_validity()` function, and
* Calling `assert(check_heap_validity())` at the leading and trailing edges of the `heapmgr_malloc()` and `heapmgr_free()` functions.

Your `heapmgr` implements the strategies for good memory utilization:
* heapmgr_malloc() should check blocks from the free list before allocate new memory
* If free block is bigger then requested, heapmgr_malloc() should divide the free block and keep leftover block in the free list
* heapmgr_free() should check lower/upper neighbor and coalesce if possible

If you ignore memory utilization, you won't get points no matter how fast your implementation is.


## Logistics

### Given codes

#### /reference
* `heapmgr.h`, `heapmgrgnu.c`, `heapmgrkr.c`, and `heapmgrbase.c`: as described above.
* `chunkbase.h` and `chunkbase.c`: a Chunk module used by `heapmgrbase.c`.

#### /src
* `chunk.h` and `chunk.c`: a Chunk module that you may use in both implementations of your `heapmgr` module.
* You may modify `chunk.h` and `chunk.c` and use them, or leave them as is and not use them.

#### /test
* `testheapmgr.c`: a client program that tests the `heapmgr` module, and reports timing and memory usage statistics.
* `testheap1`, `testheap2` and `testheapimp`: bash shell scripts that automate testing.

See the **Build and Test** section below for available `make` targets.

### Perform a test

The `testheapmgr` program requires three command-line arguments. The first should be any one of seven strings, as shown in the following table, indicating which of seven tests the program should run:

| Argument | Test Performed |
|:---          |:---  |
| `LIFO_fixed` | LIFO with fixed size chunks |
| `FIFO_fixed` | FIFO with fixed size chunks |
| `LIFO_random` | LIFO with random size chunks |
| `FIFO_random` | FIFO with random size chunks |
| `random_fixed` | Random order with fixed size chunks |
| `random_random` | Random order with random size chunks |
| `worst` | Worst case order for a heap manager implemented using a single linked list |

The second command-line argument is the number of calls of `heapmgr_malloc()` and `heapmgr_free()` that the program should execute. The third command-line argument is the (maximum) size, in bytes, of each memory chunk that the program should allocate and free.

Immediately before termination testheapmgr prints to stdout an indication of how much CPU time and heap memory it consumed. See the `testheapmgr.c` file for more details.

When testing, set the product of the number of calls (second command line argument) and size in bytes (third command line argument) to less than or equal to $3\times10^8$. In all tests, this product is guaranteed to be less than or equal to $3\times10^8$.

### Build and Test

The provided `Makefile` supports building and testing your implementations. Performance test builds use `-O3 -D NDEBUG` flags. The `-O3` argument commands gcc to optimize the machine language code that it produces. When given the `-O3` argument, `gcc` spends more time compiling your code so, subsequently, the computer spends less time executing your code. The `-D NDEBUG` argument commands gcc to define the `NDEBUG` macro, just as if the preprocessor directive `#define NDEBUG` appeared in the specified .c file(s). Defining the `NDEBUG` macro disables the calls of the `assert` macro within the `heapmgr` implementations. Doing so also disables code within `testheapmgr.c` that performs (very time consuming) checks of memory contents.

Available `make` targets:

* `make check1` — Build `heapmgr1` in debug mode (assertions enabled) and run a quick correctness test with small inputs. Use this during development to catch crashes and assertion failures.
* `make check2` — Same as `check1`, but for `heapmgr2`.
* `make test1-local` — Build all implementations (`heapmgrgnu`, `heapmgrkr`, `heapmgrbase`, `heapmgr1`) with `-O3 -D NDEBUG` and run the full performance test suite. Use this to compare your implementation against reference implementations locally. Note that this may take a long time due to slow reference implementations.
* `make test2-local` — Same as `test1-local`, but also builds and tests `heapmgr2`.
* `make test1-remote` — Submit your code to the grading server and run `heapmgr1` in the server environment. The reference implementation results (`heapmgrgnu`, `heapmgrkr`, `heapmgrbase`) are pre-computed on the server and displayed alongside your results for comparison.
* `make test2-remote` — Submit your code to the grading server and run `heapmgr2` in the server environment. Pre-computed reference results are displayed alongside your results for comparison.
* `make clean` — Remove all built binaries.

**Note:** The test server is a shared resource. Please do not wait until the submission deadline to run `make test1-remote` or `make test2-remote`  — start early to avoid long wait times when many students submit at the same time.

You can create additional test programs as you deem necessary. You need not submit your additional test programs.

### Make readme

Create a `readme` text file that contains (You can write in either English or Korean):

* Your name and student ID.
* The **CPU times and heap memory consumed** by testheapmgr using `heapmgrgnu.c`, `heapmgrkr.c`, `heapmgrbase.c`, `heapmgr1.c`, and `heapmgr2.c`(optional), with all arguments(`LIFO_fixed`, `FIFO_fixed`, `LIFO_random`, `FIFO_random`, `random_fixed`, `random_random`, `worst`), with call count 50000 and chunk size 1000, and call count 30000 and chunk size 10000.  
Note that if the CPU time consumed is more than **5 minutes**, `testheapmgr` will **abort execution**.  
To report the time and memory consumption, it is sufficient to paste the output of `make test1-local` (or `make test2-local`) into your `readme` file.
* A description of your implementation's design and intent (e.g., data structures used, allocation/free strategy, how coalescing works). There is no length requirement.
* If you received help from LLMs or Coding Agents (e.g., ChatGPT, claude code, antigravity ...), describe what kind of assistance you received.

### Submission

As in lab 1 and lab 2, create a directory named after your student number, zip it up, and submit it. Be sure to follow the following directory structure:

```
202600000_assign3
  |-heapmgr1.c
  |-heapmgr2.c (optional)
  |-chunk.c
  |-chunk.h 
  `-readme
```

Compress the submission files into tar format using the following command (assuming your ID is 202600000):

If you want to submit only `heapmgr1.c`:
```
mkdir 202600000_assign3
mv heapmgr1.c chunk.c chunk.h readme 202600000_assign3
tar zcf 202600000_assign3.tar.gz 202600000_assign3
```
If you want to submit both `heapmgr1.c` and `heapmgr2.c`:
```
mkdir 202600000_assign3
mv heapmgr1.c heapmgr2.c chunk.c chunk.h readme 202600000_assign3
tar zcf 202600000_assign3.tar.gz 202600000_assign3
```
Upload your submission file (202600000_assign3.tar.gz) to eTL.

## Grading

We will grade your work on submit format and function correctness.
You will get full point if your module is well-designed(which means that your code executes all `malloc()` and `free()` without any memory errors), and faster then given codes.  

`heapmgr1` should `free()` faster then `heapmgrkr` and `heapmgrbase` in 3 cases (`LIFO_fixed`, `LIFO_random`, `FIFO_random`), and should faster then `heapmgrkr` and `heapmgrbase` in 2 cases (`random_fixed`, `random_random`).  
`heapmgr2` should faster then `heapmgrkr`, `heapmgrbase`, and `heapmgr1`, in all cases.  

If you submit `heapmgr2.c`, you can get up to 30% of the score you received for implementing `heapmgr1.c` as extra credit.

You will not get the 30% extra credit just by submitting heapmgr2; If you do not get a perfect score on heapmgr2, extra credit you receive will be reduced.

Since extra credit is proportional to the score you received for implementing `heapmgr1.c`, we recommend that you first fully implement `heapmgr1.c` and then challenge the implementation of `heapmgr2.c`.

## Frequently Asked Questions (FAQ)

### About the implementation of the code

**Question 1.**  
`chunk.c`, `chunk.h` files are almost empty; Is there an error in the provided files?

**Answer.**  
These files provided as almost empty files so that you can use `Makefile`.  
If you use `heapmgrbase.c` as a baseline, you can modify `chunkbase.c` and `chunkbase.h` and use them as chunk.c and chunk.h. In this case, you can remove given empty files.  
If not, you can leave `chunk.c` and `chunk.h` as empty files and use them just for the `Makefile`.  
Regardless of whether you modified `chunk.c` and `chunk.h`, you **must** submit `chunk.c` and `chunk.h`.  

**Question 2.**  
Is using `heapmgrbase.c`, `chunkbase.c` or `chunkbase.h` considered plagiarism?

**Answer.**  
No, using `heapmgrbase.c`, `chunkbase.c` and `chunkbase.h` as baselines of your implementation is **not** considered plagiarism.  
You can use the functions, structures, etc. from given `heapmgrbase.c`, `chunkbase.c` and `chunkbase.h`.

**Question 3.**  
When implementing bins, are there any limits on the number of bins, or the size of the chunks?

**Answer.**  
No, there are no limits outside of the specified input range ($3\times10^8$).

### About the execution of the code

**Question 4.**  
What is the environment for grading?

**Answer.**  
Your code will be graded on the test server.  
The code you submit will be placed in the `/src` folder, and the structure is completely identical to the structure of the assignment3 folder provided to you.  
In other words, the code you submit should **not** use any header files other than the code you wrote (e.g. do not use heapmgr.h in your implementation).  
You can check how your code performs on the server by running `make test1-remote` or `make test2-remote`.

**Question 5.**  
When performing tests in a local environment, the execution time often exceeds 5 minutes and stops; Can I adjust the time limit?

**Answer.**  
You can adjust the time limit for testing in a local environment.  
You can adjust the time limit by changing `s_rlimit.rlim_cur` and `s_rlimit.rlim_max` in `/test/testheapmgr.c`. (Default is 300)  

**Question 6.**  
When `Count` or `Size` is large, my code is fast enough, but when `Count` or `Size` is small, my code is slower than the existing code or has a similar speed. Is my implementation still not enough?

**Answer.**  
When `Count` or `Size` is not large enough, it is difficult to make difference in speed due to difference in implementation, and errors depending on the environment can easily affect the speed.  
When grading your code, we will grade it with large enough `Count` and `Size`.  
If your code is fast enough when tested with `Count = 30000` and `Size = 10000`, you don't need to worry about the results with small `Count` or `Size`.  

### About grading criteria

**Question 7.**  
How is well-designed evaluated?

**Answer.**  
We build your code in debug mode (`make check1`) and verify that it executes normally without errors for tests with `malloc()` and `free()`.

**Question 8.**  
When should I perform validity checks?

**Answer.**  
As mentioned in the `Your task` section above, `assert(check_heap_validity())` must be called on the leading and trailing edges of `heapmgr_malloc()` and `heapmgr_free()` functions.  
Creating additional validity checks and functions for them in other areas is permitted, but is not included in the grading criteria.

**Question 9.**  
What is the grading criteria for memory usage?

**Answer.**  
All the strategies for good memory utilization mentioned in the `Your task` section above must be satisfied, and the memory usage of your implementation must be 'small enough'.  
If `memory usage at heapmgr1 (or heapmgr2) / memory usage at heapmgrbase` is 3 or less, it is small enough, and if it is 10 or more, it is not small enough.  
The exact threshold for `memory usage at heapmgr1 (or heapmgr2) / memory usage at heapmgrbase` is not disclosed, but as mentioned before, it is more than 3 and less than 10.

### Other questions

**Question 10.**  
I cannot run the test properly on MacOS.

**Answer.**  
Please try using Linux(VM/wsl, etc.).