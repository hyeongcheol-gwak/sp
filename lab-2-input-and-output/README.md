# Lab 2: I/O

In this lab, you will implement a tool that lists all files in a directory and its subdirectories.

You will learn:
  * 📂 Explore directories — iterate through all files
  * 📑 Inspect files — retrieve metadata (ownership, size, type)
  * 📝 Present data — produce clean, formatted output
  * ⚠️ Handle problems — practice robust error handling
  * 🔤 Tame strings — discover C’s weak spot in string handling
  * 🔎 Think in patterns — grasp the basics of regular expressions
  * ⚙️ Build your own — implement a mini regex engine
  * ✨ Plus: handy C tricks & library functions

## Deadline

| Date | Description |
|:---  |:--- |
| Friday, April 10, 23:59 | Submission deadline |

## Logistics

### Hand-out

Please read this file (README.md) carefully. Additionally, the PPT slide presented during the lab session is included in the distribution archive for students. However, please remember that the PPT slide is based on the README.md file, and it is **important to understand the contents of the README.md file**.

The assignment starts by extracting the provided `assignment2.tar.gz` file.
```bash
$ tar -xvzf assignment2.tar.gz
```

### Submission

As in lab-1-decomment, create a directory named after your student number, compress it up, and submit it to Classum submission page. Be sure to follow the following directory structure:

```bash
202600000_assign2
  |-dirtree.c (source file)
  `-readme

$ tar zcf 202600000_assign2.tar.gz 202600000_assign2
```

### Implementing the Lab on an Mac or custom Unix/Linux

While it is possible to implement the lab on an ARM-based Mac, MacOS is not 100% compatible with Linux and thus requires a number of changes to the code (extra header files, etc.) We recommend installing an ARM-based Linux virtual machine such as [Ubuntu Server for ARM](https://ubuntu.com/download/server/arm). On custom Intel-compatible Linux systems, you should be able to implement the lab without any modifications.

Note that we evaluate your submissions in the provided Bacchus VMs. If you do not use the Bacchus VMs to develop your code, **we strongly recommend testing your submission in the Bacchus VMs** to make sure it runs correctly.

---

## Dirtree Overview

Our tool is called _dirtree_. Dirtree recursively traverses a directory tree, prints out a sorted list of all files, and shows details about each file.

```bash
$ dirtree demo
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
demo
  subdir1                                                     ta:ta              4096         8    d
    simplefile                                                ta:ta               256         8     
    sparsefile                                                ta:ta              8192         8     
    thisisanextremelyveryverylongfilenameforsuchasi...        ta:ta              1000         8     
  subdir2                                                     ta:ta              4096         8    d
    brokenlink                                                ta:ta                 8         0    l
    symboliclink                                              ta:ta                 6         0    l
    textfile1.txt                                             ta:ta              1024         8     
    textfile2.txt                                             ta:ta              2048         8     
  subdir3                                                     ta:ta              4096         8    d
    code1.c                                                   ta:ta               200         8     
    code2.c                                                   ta:ta               300         8     
    pipe                                                      ta:ta                 0         0    f
    socket                                                    ta:ta                 0         0    s
  one                                                         ta:ta                 1         8     
  two                                                         ta:ta                 2         8     
----------------------------------------------------------------------------------------------------
9 files, 3 directories, 2 links, 1 pipe, and 1 socket                           25325        96

```

Last but not least, dirtree can generate aggregate statistics for several directories:
```
$ dirtree demo/subdir1 demo/subdir2
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
demo/subdir1
  simplefile                                                  ta:ta               256         8     
  sparsefile                                                  ta:ta              8192         8     
  thisisanextremelyveryverylongfilenameforsuchasimp...        ta:ta              1000         8     
----------------------------------------------------------------------------------------------------
3 files, 0 directories, 0 links, 0 pipes, and 0 sockets                          9448        24

Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
demo/subdir2
  brokenlink                                                  ta:ta                 8         0    l
  symboliclink                                                ta:ta                 6         0    l
  textfile1.txt                                               ta:ta              1024         8     
  textfile2.txt                                               ta:ta              2048         8     
----------------------------------------------------------------------------------------------------
2 files, 0 directories, 2 links, 0 pipes, and 0 sockets                          3086        16

Analyzed 2 directories:
  total # of files:                       5
  total # of directories:                 0
  total # of links:                       2
  total # of pipes:                       0
  total # of sockets:                     0
  total # of entries:                     7
  total file size:                    12534
  total # of blocks:                     40
```

## Dirtree Specification 

### Command line arguments

Dirtree accepts the following command line arguments.
```bash
$ dirtree [Options] [Directories]
```

| Option          | Description |
|:----            |:----        |
| -d \<depth>     | Traverse directories up to the specified *depth* |
| -f \<pattern>   | Apply a pattern matching filter to file names (supports `?`, `*`, `()`) |
| -h              | Display help screen |

`Directories` is a list of directories to traverse (up to 64).
If none is specified, the current directory (`.`) is traversed by default.

### Operation

1. Dirtree recursively traverses each directory in the `Directories` list. 
2. Within each directory, it enumerates all entries and prints them in **alphabetical order**. Directories are listed before files, and the special entries `.` and `..` are ignored.
3. After processing a directory, a **summary line** is printed. If more than one directory is traversed, **aggregate statistics** are printed at the end.

### Output

#### Path/Name
As dirtree traverses the directory tree, it prints the sorted names of the entities in each directory.
Indentation increases by **two spaces** for each level of depth, making the directory hierarchy easy to recognize.

<table>
  <tbody>
    <tr>
      <td>
        <pre>dir             <br>  subdir1<br>    subdir2<br>      file1<br>      file2<br>      file3<br>  file4</pre>
      </td>
    </tr>
  </tbody>
</table>

#### Detailed information
To the right of the path/name, dirtree prints the following details for each entry:
* User and group: Each file in Unix belongs to a user and a group. The names of the user and the group are printed, separated by a colon (`:`).
* Size: The size of the file in bytes.
* Disk blocks: The number of disk blocks occupied by the file. 
* File type: A single-character code indicating the type of file:
  | Type              | Character |
  |:---               |:---------:|
  | Regular file      | _(empty)_ |
  | Directory         |     d     |
  | Symbolic Link     |     l     |
  | FIFO (named pipe) |     f     |
  | Socket            |     s     |
  <!-- | Character device  |     c     | -->
  <!-- | Block device      |     b     | -->

> **Note**: In this lab, `Character device` and `Block device` are just treated as `Regular file` for simplicity. 

#### Per-directory summary & Aggregate statistics
Dirtree prints a **header** and **footer** around the listing of each directory, followed by a **one-line summary of statistics** for that directory.

If multiple directories are specified on the command line, **aggregate statistics** of all traversed directories are printed at the end.
Note that the lines printed below `Analyzed N directories:` represent the cumulative totals across all processed directories.
```
$ dirtree demo/subdir2 demo/subdir3
Name                                                        User:Group           Size    Blocks Type  # header
----------------------------------------------------------------------------------------------------
demo/subdir2
  brokenlink                                                  ta:ta                 8         0    l
  symboliclink                                                ta:ta                 6         0    l
  textfile1.txt                                               ta:ta              1024         8     
  textfile2.txt                                               ta:ta              2048         8     
----------------------------------------------------------------------------------------------------
2 files, 0 directories, 2 links, 0 pipes, and 0 sockets                          3086        16       # footer(summary line)

Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
demo/subdir3
  code1.c                                                     ta:ta               200         8     
  code2.c                                                     ta:ta               300         8     
  pipe                                                        ta:ta                 0         0    f
  socket                                                      ta:ta                 0         0    s
----------------------------------------------------------------------------------------------------
2 files, 0 directories, 0 links, 1 pipe, and 1 socket                             500        16

Analyzed 2 directories:                      # aggregate statistics
  total # of files:                       4
  total # of directories:                 0
  total # of links:                       2
  total # of pipes:                       1
  total # of sockets:                     1
  total # of entries:                     8  # aggregate only, not included in per-directory summary line
  total file size:                     3586
  total # of blocks:                     32
```

#### Output format
* All entries must be printed with the correct indentation (**two spaces** per subdirectory depth).
* The output should be aligned in columns: path/name, user:group, size, blocks, and type.
* If a path/name or a summary line is too long to fit in its column, it must be truncated and end with three dots (`...`).

```bash
$ dirtree demo/subdir1
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
demo/subdir1
  simplefile                                                  ta:ta               256         8     
  sparsefile                                                  ta:ta              8192         8     
  thisisanextremelyveryverylongfilenameforsuchasimp...        ta:ta              1000         8     
----------------------------------------------------------------------------------------------------
3 files, 0 directories, 0 links, 0 pipes, and 0 sockets                          9448        24

```

* The output line consists of the path/name on the left, followed by detailed information fields on the right.
* The total line length is fixed at **100 characters**. Each field has a reserved width and is aligned for readability:
* We only consider file names encoded in UTF-8. 

| Output element | Width | Alignment | Action on overflow |
|:---            |:-----:|:---------:|:---      |
| Path/name      |  54   |   Left    | Cut and end with three dots (`...`) |
| User name      |   8   |   Right   | Truncate to first 8 characters |
| Group name     |   8   |   Left    | Truncate to first 8 characters |
| File size      |  10   |   Right   | Ignore |
| Disk blocks    |   8   |   Right   | Ignore |
| Type           |   1   |           |        |
| Summary line   |  68   |   Left    | Ignore |
| Total size     |  14   |   Right   | Ignore |
| Total blocks   |   9   |   Right   | Ignore |

The following rendering shows the output formatting in detail for each of the different elements. The two rows on top indicate the character position on a line. 
```bash
         1         2         3         4         5         6         7         8         9        10
1........0.........0.........0.........0.........0.........0.........0.........0.........0.........0

Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
<path/name                                           >  <  user>:<group >  <    size>  <blocks>    t
<path/name                                           >  <  user>:<group >  <    size>  <blocks>    t
...
----------------------------------------------------------------------------------------------------
<summary line                                                      >   <  total size> <totblks>

```
> **Note 1**: When the field length is exactly equal to its maximum width (e.g., a path/name of 54 characters), no cutting or truncation is applied.

> **Note 2**: In this assignment, your userid and group are set to "sp" + 9-digit student IDs. Use a format specifier such as `printf("%.8s");` to enforce the 8-byte limit.

> **Note 3**: You do not need to handle cases where the file size, blocks, total size, or total blocks exceed their maximum field width. However, since the total size and total blocks may exceed the range of an `(unsigned) int`, it is safer to use an `(unsigned) long long` type.

#### Summary line
Dirtree ensures grammatically correct English in its summary lines. 
* If the count is **0** or **>= 2**, the *plural form* is used;
* Else if the count is exactly **1**, the *singular form* is used.

Compare the two summary lines:
```
0 files, 2 directories, 1 link, 1 pipe, and 1 socket

1 file, 1 directory, 2 links, 0 pipes, and 5 sockets
```

### Option 1: Depth limit `-d \<depth>`

#### Description
The `-d` option limits the depth of the directory traversal.
* The root directory is considered depth **0**.
* Its immediate child entries are depth **1**, their child entries are depth **2**, and so on.
* **Only entries up to the given depth are displayed and traversed**.

#### Examples
Without `-d` option, all subdirectories and files are shown until the maximum depth is reached:
* The skeleton code defines `MAX_DEPTH`=20 as default; you do not need to handle cases deeper than this.
```
$ dirtree test1/a
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
test1/a      # depth 0
  b          # depth 1                                        ta:ta              4096         8    d
    c        # depth 2                                        ta:ta              4096         8    d
      d      # depth 3                                        ta:ta              4096         8    d
        e    # depth 4                                        ta:ta              4096         8    d
          f  # depth 5                                        ta:ta                 0         0     
    f        # depth 2                                        ta:ta                 0         0     
----------------------------------------------------------------------------------------------------
2 files, 4 directories, 0 links, 0 pipes, and 0 sockets                         16384        32

```

With `-d 1`, only the root (depth 0) and its immediate children (depth 1) are shown:
```
$ dirtree -d 1 test1/a
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
test1/a      
  b          # depth 1                                        ta:ta              4096         8    d
----------------------------------------------------------------------------------------------------
0 files, 1 directory, 0 links, 0 pipes, and 0 sockets                            4096         8

```

With `-d 2`, traversal goes one level deeper, showing entries at depth 2 as well.
```
$ dirtree -d 2 test1/a
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
test1/a
  b                                                           ta:ta              4096         8    d
    c        # depth 2                                        ta:ta              4096         8    d
    f        # depth 2                                        ta:ta                 0         0     
----------------------------------------------------------------------------------------------------
1 file, 2 directories, 0 links, 0 pipes, and 0 sockets                           8192        16

```

With `-d 3`, entries at depth 3 are now included in the output:
```
$ dirtree -d 3 test1/a
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
test1/a
  b                                                           ta:ta              4096         8    d
    c                                                         ta:ta              4096         8    d
      d      # depth 3                                        ta:ta              4096         8    d
    f                                                         ta:ta                 0         0     
----------------------------------------------------------------------------------------------------
1 file, 3 directories, 0 links, 0 pipes, and 0 sockets                          12288        24

```
... You do not need to print `# depth N`; this is only shown in the examples to help your understanding.

#### Notes
* Valid depth values are `1 ~ 20`.
* If the `-d` option is not specified, the default maximum depth is **20**.
  * You do not need to handle cases where the maximum reachable depth is greater than 20. 
  * See `MAX_DEPTH` macro in the skeleton code. 
* Inherently, statistics (both per-directory summary and aggregate statistics) should **not** count skipped entries when their depth is greater than the given `depth`.

### Option 2: File name filtering `-f \<pattern>` 

#### Description
The `-f` option applies a regex‑like pattern filter to file and directory names.
It determines **which entries are printed**, but directories are **always traversed** regardless of whether their names match the given pattern.

```
$ dirtree demo/subdir2/
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
demo/subdir2/
  brokenlink                                                  ta:ta                 8         0    l
  symboliclink                                                ta:ta                 6         0    l
  textfile1.txt                                               ta:ta              1024         8     
  textfile2.txt                                               ta:ta              2048         8     
----------------------------------------------------------------------------------------------------
2 files, 0 directories, 2 links, 0 pipes, and 0 sockets                          3086        16

$ dirtree -f '.txt' demo/subdir2/
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
demo/subdir2/
  textfile1.txt                                               ta:ta              1024         8     
  textfile2.txt                                               ta:ta              2048         8     
----------------------------------------------------------------------------------------------------
2 files, 0 directories, 0 links, 0 pipes, and 0 sockets                          3072        16

```

#### Matching rules
* **Partial matching**: the pattern may match any contiguous substring of the file or directory name.
* Matching is **case-sensitive** (e.g., 'a' vs 'A') and applies to the **basename** (not the full path).
  * e.g., a path ‘/home/ta/abc/def’ does not match a pattern ‘abc’: basename ‘def’ vs pattern ‘abc’
* Handling entries
  * Matched → Printed with full details & counted in stats
  * Not matched → Not printed & not counted in stats
    * However, directories are still traversed; if at least one descendant matches → Print only ‘path/name’ for that non-matching parent directories (with proper indentation)
```
$ dirtree -f 'k' abc
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
abc
  def
    ghi
      jkl                                                     ta:ta                 0         0     
----------------------------------------------------------------------------------------------------
1 file, 0 directories, 0 links, 0 pipes, and 0 sockets                              0         0

```

#### Supported Wildcards
| wildcard | Description |
|:---  |:--- |
| `?` | Matches exactly one arbitrary character |
| `*` | Kleene star; repeats the immediately preceding character or a `group` zero or more times |
| `()` | Grouping; encloses a subpattern so it can be treated as a `group` |

Here, a `group` is a subpattern wrapped in `()` and treated as a single unit. For example:
* `a(bc)*d`: the group `bc` is repeated by `*`; so it may appear **zero or more times**.

> **Note 1**: Test cases requiring `?`, `*`, `()` as **literal characters** are not considered. You do not need to handle them.

> **Note 2**: Maximum pattern length is limited to **64** (including the last `\0` character).

#### Pattern matching examples
| Pattern | Matches | Explanation |
| :- | :- | :- |
| `a?c` | `abc`, `axc`, ... | `?` matches exactly one arbitrary character |
| `ab*`           | `a`, `ab`, `abb`, `abbbb...`, ... | `*` repeats the preceding `b` zero or more times |
| `(ab)*c` | `c`, `abc`, `ababc`, `abababc`, ... | `()` groups `ab` as a subpattern, then `*` repeats it |
| `ab?(de)*f` | `abcf`, `abXf`, `abcdef`, `abXdef`, `abXdedef`, ... | Combined usage: `?` = any char, `de` = groups `de` as a subpattern, `(de)*` = the group `de` may appear zero or more times |

Example 1: `?`
```
$ dirtree pat1
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
pat1
  subdir1                                                     ta:ta              4096         8    d
    aXc                                                       ta:ta                 1         8     
  abc                                                         ta:ta                 1         8     
  axc                                                         ta:ta                 1         8     
  zxc                                                         ta:ta                 1         8     
  zzz                                                         ta:ta                 1         8     
----------------------------------------------------------------------------------------------------
5 files, 1 directory, 0 links, 0 pipes, and 0 sockets                            4101        48

$ dirtree -f 'a?c' pat1
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
pat1
  subdir1
    aXc                                                       ta:ta                 1         8     
  abc                                                         ta:ta                 1         8     
  axc                                                         ta:ta                 1         8     
----------------------------------------------------------------------------------------------------
3 files, 0 directories, 0 links, 0 pipes, and 0 sockets                             3        24

```
> **Note 1**: When specifying the pattern as a program argument, it must be enclosed in quotes (`' '` or `" "`) to ensure the shell does not interpret wildcards before passing them to the program.

> **Note 2**: The root directory does not need to be filtered (e.g., `pat1/` in Example 1).

Example 2: `*` + `()`
```
$ dirtree pat2
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
pat2
  outer                                                       ta:ta              4096         8    d
    inner                                                     ta:ta              4096         8    d
      ababc                                                   ta:ta                 1         8     
  abababc                                                     ta:ta                 1         8     
  ababc                                                       ta:ta                 1         8     
  abc                                                         ta:ta                 1         8     
  abxy                                                        ta:ta                 1         8     
  c                                                           ta:ta                 1         8     
  xababcx                                                     ta:ta                 1         8     
----------------------------------------------------------------------------------------------------
7 files, 2 directories, 0 links, 0 pipes, and 0 sockets                          8199        72

$ dirtree -f '(ab)*c' pat2
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
pat2
  outer
    inner
      ababc                                                   ta:ta                 1         8     
  abababc                                                     ta:ta                 1         8     
  ababc                                                       ta:ta                 1         8     
  abc                                                         ta:ta                 1         8     
  c                                                           ta:ta                 1         8     
  xababcx                                                     ta:ta                 1         8     
----------------------------------------------------------------------------------------------------
6 files, 0 directories, 0 links, 0 pipes, and 0 sockets                             6        48

$ dirtree -f 'b(ab)*c' pat2   # 'pat2/c' will be filtered
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
pat2
  outer
    inner
      ababc                                                   ta:ta                 1         8     
  abababc                                                     ta:ta                 1         8     
  ababc                                                       ta:ta                 1         8     
  abc                                                         ta:ta                 1         8     
  xababcx                                                     ta:ta                 1         8     
----------------------------------------------------------------------------------------------------
5 files, 0 directories, 0 links, 0 pipes, and 0 sockets                             5        40

```

Example 3: `?` + `*` + `()`
```
$ dirtree pat3
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
pat3
  A                                                           ta:ta              4096         8    d
    hold                                                      ta:ta              4096         8    d
      aef                                                     ta:ta                 1         8     
      ef                                                      ta:ta                 1         8     
  B                                                           ta:ta              4096         8    d
    none                                                      ta:ta              4096         8    d
      here                                                    ta:ta                 1         8     
  abxdbydef                                                   ta:ta                 1         8     
  abxdef                                                      ta:ta                 1         8     
  aef                                                         ta:ta                 1         8     
  alpha                                                       ta:ta                 1         8     
  ef                                                          ta:ta                 1         8     
  g                                                           ta:ta                 1         8     
----------------------------------------------------------------------------------------------------
9 files, 4 directories, 0 links, 0 pipes, and 0 sockets                         16393       104

$ dirtree -f 'a(b?d)*ef' pat3
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
pat3
  A
    hold
      aef                                                     ta:ta                 1         8     
  abxdbydef                                                   ta:ta                 1         8     
  abxdef                                                      ta:ta                 1         8     
  aef                                                         ta:ta                 1         8     
----------------------------------------------------------------------------------------------------
4 files, 0 directories, 0 links, 0 pipes, and 0 sockets                             4        32

```

#### Handling invalid patterns
You must handle invalid pattern inputs. 
1. Empty pattern or group is not allowed: A pattern or group must contain at least one element.
  * Entire pattern must not be empty; e.g., `$ dirtree -f '' demo`
  * `a()b` → invalid (empty `group`)

2. Improper use of `*`: `*` must always follow a valid character or `group`. Using `*` at the beginning is invalid. Using consecutive `*`s is also invalid.
  * `*abc` → invalid (`*` at the beginning)
  * `a**b` → invalid (multiple `*` with no character in between)

3. Unmatched or misused `()`: `()` must always form a valid, balanced group. Missing, extra, or misplaced `()` make the pattern invalid.
  * `a)bc(` → invalid (closing before opening, or stray `(`)
  * `(abc` → invalid (missing closing `)`)

> **Note**: You do not need to handle cases such as:
> 
> (1) `*` applied inside of `()`, or
> 
> (2) nested `()`s like `(())`.
>  
> These cases will not be included in the grading. 

#### Error handling 
When an invalid pattern is detected, **only** the following error message must be printed:
```
$ dirtree -f 'a**b' demo
Invalid pattern syntax
```

When there doesn't exist any file or directory with given name : 
```

$ dirtree b
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
b
  ERROR: No such file or directory
----------------------------------------------------------------------------------------------------
0 files, 0 directories, 0 links, 0 pipes, and 0 sockets                             0         0

```

For Permission denied : 

```
$ dirtree pat4
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
pat4
  subdir1                                                student:student         4096        16    d
    root directory                                          root:root            4096        16    d
      ERROR: Permission denied
  five                                                   student:student            5         8     
  four                                                   student:student            4         8     
  one                                                    student:student            1         8     
----------------------------------------------------------------------------------------------------
3 files, 2 directories, 0 links, 0 pipes, and 0 sockets                          8202        56

```
* This error message must be printed to `stderr`, not to `stdout`. Simply use the panic() function provided in the skeleton code.
* **Any other errors do not need to be handled** and will not be included in the grading.

---

## Your Task

Implement dirtree according to the specification above.

### Design

**Recommendation**: Do not look at the provided `src/dirtree.c` yet! First outline the logic yourself. 
The design phase is the most difficult and important part of any project;
and it is also the phase that requires the most practice and distinguishes hobby programmers from experts.

### Implementation

Once you have designed the outline, you can start implementing it. We provide a skeleton code to help you get started.

You have to implement the following two parts:
1. `main()`:
   * Iterate over `directories`. 
   * For each directory, 
     * Print the header.
     * Call `process_dir()` with the proper options and initial depth.
     * Print the footer and per-directory summary line.
     * If multiple directories were processed, print the aggregate statistics at the end.
2. `process_dir()`:
   * Open the directory
   * Enumerate directory entries; skip `.` and `..`.
   * Sort directory entries (directories before non-directory files, then alphabetical order).
   * For each entry:
     * Print path/name and detailed information using the output format (apply indentation, width, alignment, and truncation rules).
     * Update the per-directory statistics (and the aggregate statistics, if applicable).
     * If current entry is a directory, call `process_dir()` recursively.
   * Close the directory before returning.
   * Respect the `-d <depth>` option by stopping recursion when the maximum depth is reached.  
   * Respect the `-f <pattern>` option by suppressing printing of non-matching entries, while still traversing directories.
  
---

## Handout Overview

The handout contains the following files and directories:

| File/Directory | Description |
|:---  |:--- |
| README.md | This file (assignment description) | 
| Makefile | Build driver for the project |
| src/dirtree.c | Skeleton source file. Implement your solution by editing this file or from scratch. |
| tools/ | Utilities to generate directory trees for testing |


### How to use Makefile
*  You should keep the directory structure as specified in the `Makefile`:

| File | Path |
|:---  |:--- |
| dirtree.c | ~/assignment2/src/ |
| dirtree (executable file) | ~/assignment2/bin/ |

* You don’t need to modify the content of `Makefile`. Just remember two commands:
  * `$ make`: compiles dirtree.c.
  * `$ make clean`: removes compilation results (useful to force a full rebuild).
* Most of the time, it is enough to just run `$ make` command after editing your source code.

### Tools

The `tools` directory contains utilities for generating test directory trees to validate your solution.

| File/Directory | Description |
|:---  |:--- |
| gentree.sh | Utility to generate a test directory tree. |
| mksock     | Helper program to generate a Unix socket used in `gentree.sh`. |
| *.tree     | Script files describing directory tree layout. |

To generate a test tree, invoke `gentree.sh` with one of the provided script files.

Assuming you are located in the assignment2 directory, use the following command to generate the `demo` directory tree:
```bash
$ ls
Makefile  README.md   src  tools
$ cd tools/
$ ./gentree.sh demo.tree 
Generating tree from 'tools/demo.tree'...
Done. Generated 4 files, 2 links, 1 fifos, and 1 sockets. 0 errors reported.
```



---

### Hints

#### Pseudo code for file name filtering (`*`-only version)
This pseudo code implements file name filtering in a recursive manner, supporting only the `*` wildcard.
The recursive branching at `*` keeps the logic simple and elegant.
By extending the same recursive manner, you can easily add `?` and `()` support.

```c
bool match(str, pattern):
  while *str != ‘\0’:
    if submatch(str, pattern): // try partial matching
      return TRUE
    str++
  
  return FALSE

bool submatch(s, p):
  while *s and *p is not ‘\0’:
    if *p != ‘*’:
      if *(p + 1) == ‘*’:
        // save the preceding character for repetition
      else if *s != *p:
        return FALSE
      else s++
      p++
      
    else: // *p == ‘*’
      if submatch(s, p + 1): // zero repetition
        return TRUE
      else:  
        // one or more repetition
      return FALSE

  if *p == ‘\0’: // pattern matched
    return TRUE
    
  return FALSE	
```

#### Skeleton code
The skeleton code is provided to help you get started. You may modify it in any way you see fit - or implement this lab completely from scratch.

The skeleton code provides:
* `const char* print_formats[]`: Predefined format strings used in the output (e.g., table headers, error messages, per-entry line format)
* `struct summary`: Data structures for per-directory and aggregate statistics 
* `get_next()` : A helper function to read the next entry (skipping `.` and `..`)
* `dirent_compare()`: A `qsort` comparator to sort entries
* `syntax()` and `main()`: Full argument parsing and syntax helpers. 

#### Useful C library calls

When learning any programming language, it is often difficult at the beginning to know which library functions exist and how to use them. To help you get started, we provide a list of C library and system calls, grouped by topic, that may be useful for solving this lab. Read the corresponding man pages carefully to understand exactly how each function operates.

<table>
  <thead>
    <tr>
      <th align="left">Topic</th>
      <th align="left">C library call</th>
      <th align="left">Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td rowspan="4">
        String operations
      </td>
      <td>
        <code>strcmp()</code>
      </td>
      <td>
        compare two strings
      </td>
    </tr>
    <tr>
      <td>
        <code>strncpy()</code>
      </td>
      <td>
        copy up to n characters of one string into another
      </td>
    </tr>
    <tr>
      <td>
        <code>strdup()</code>
      </td>
      <td>
        create a copy of a string. Use <code>free()</code> to free it after use
      </td>
    </tr>
    <tr>
      <td>
        <code>asprintf()</code>
      </td>
      <td>
        asprintf() is extremely helpful to print into a string and allocate memory for it at the same time.
        We will show some examples during the lab session.
      </td>
    </tr>
    <tr>
      <td rowspan="3">
        Directory management
      </td>
      <td>
        <code>opendir()</code>
      </td>
      <td>
        open a directory to enumerate its entries
      </td>
    </tr>
    <tr>
      <td>
        <code>closedir()</code>
      </td>
      <td>
        close an open directory
      </td>
    </tr>
    <tr>
      <td>
        <code>readdir()</code>
      </td>
      <td>
        read next entry from directory
      </td>
    </tr>
    <tr>
      <td rowspan="2">
        File metadata
      </td>
      <td>
        <code>stat()</code>
      </td>
      <td>
        retrieve metadata of a file, follow links
      </td>
    </tr>
    <tr>
      <td>
        <code>lstat()</code>
      </td>
      <td>
        retrieve metadata of a file, do not follow links
      </td>
    </tr>
    <tr>
      <td rowspan="2">
        User/group information
      </td>
      <td>
        <code>getpwuid()</code>
      </td>
      <td>
        retrieve user information (including their name) for a given user ID
      </td>
    </tr>
    <tr>
      <td>
        <code>getgrgid()</code>
      </td>
      <td>
        retrieve group information (including its name) for a given group ID
      </td>
    </tr>
    <tr>
      <td>
        Sorting
      </td>
      <td>
        <code>qsort()</code>
      </td>
      <td>
        quick-sort an array
      </td>
    </tr>
  </tbody>
</table>

> **Important**: You are **not allowed to use external libraries (e.g., `regex.h`)** for pattern filtering, and `scandir()` from `dirent.h` when you implement `process_dir()`. These features must be implemented entirely by yourself.
However, you may consult those libraries’ interfaces and behavior to get implementation ideas.

## Frequently Asked Questions (FAQ)

**Question 1.** '$ ./gentree.sh demo.tree' command does not work!
```bash
Generating tree from 'tools/demo.tree'...
Failed to create named pipe './demo/subdir3/pipe'.
Failed to create unix socket './demo/subdir3/socket'.
Done. Generated 4 files, 2 links, 0 fifos, and 0 sockets. 2 errors reported.
```

**Answer.** Please **delete** the previously created directory and run ./gentree.sh demo.tree again.
```bash
$ rm -rf demo/
$ ./gentree.sh demo.tree
```

**Question 2.** Does the 54-byte limit apply to the indentation in the left side of the file/dir name? 

**Answer.** Yes. The 54-byte limit includes the indentation space in the left side of the name.

**Question 3.** What is the **maximum path length limit** for the target directory?

**Answer.** You don't need to handle path length overflow explicitly. Simply use the `MAX_PATH_LEN` macro in the skeleton code. You can also modify the value of that macro if necessary.

**Question 4.** How to execute the dirtree command like the examples?

**Answer.** Use `alias` command for your easy execution!

But `alias` command is not permanent; if you want to keep using `alias` after logging out and back in on servers, 
you need to add it to `~/.bashrc`.
```bash
$ alias dirtree=/home/sp[your student id]/assignment2/bin/dirtree
$ cd ~/assignment2/tools
$ dirtree pat1
Name                                                        User:Group           Size    Blocks Type
----------------------------------------------------------------------------------------------------
pat1
  subdir1                                                     ta:ta              4096         8    d
    aXc                                                       ta:ta                 1         8     
  abc                                                         ta:ta                 1         8     
  axc                                                         ta:ta                 1         8     
  zxc                                                         ta:ta                 1         8     
  zzz                                                         ta:ta                 1         8     
----------------------------------------------------------------------------------------------------
5 files, 1 directory, 0 links, 0 pipes, and 0 sockets                            4101        48

```


<div align="center" style="font-size: 1.75em;">

**Happy coding!**
</div>
