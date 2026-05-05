## Sample Test Cases

The following examples are provided to clarify the expected behavior of `snush`.
Passing these examples does not guarantee full credit, but they illustrate the kinds of behavior your shell should support.

In the examples below, the `% ` prompt may appear in the actual output and can be ignored when comparing the result.

---

### 1. Basic Shell Function

#### Example 1: External command

**Input**

```sh
echo hello
exit
```

**Expected output**

```text
hello
```

#### Example 2: `cd` built-in

**Input**

```sh
cd /tmp
pwd
exit
```

**Expected output**

```text
/tmp
```

---

### 2. I/O Redirection

#### Example 1: Output redirection

**Input**

```sh
echo hello > out.txt
cat out.txt
exit
```

**Expected output**

```text
hello
```

**Expected file content**

```text
out.txt:
hello
```

#### Example 2: Input redirection

Assume `input.txt` contains:

```text
alpha
beta
```

**Input**

```sh
cat < input.txt
exit
```

**Expected output**

```text
alpha
beta
```

---

### 3. Pipelines

#### Example 1: Simple pipe

**Input**

```sh
echo hello | wc -c
exit
```

**Expected output**

```text
6
```

#### Example 2: Multiple pipes

**Input**

```sh
/usr/bin/printf 'a\nb\na\n' | grep a | wc -l
exit
```

**Expected output**

```text
2
```

---

### 4. Signals & Interrupt

#### Example 1: Interrupt a foreground command

**Input / interaction**

```sh
sleep 10
# press Ctrl-C
echo alive
exit
```

**Expected output**

```text
alive
```

**Expected behavior**

`sleep 10` should be interrupted by Ctrl-C, but `snush` itself should continue running and accept the next command.

#### Example 2: Interrupt a foreground pipeline

**Input / interaction**

```sh
cat | cat | cat
# press Ctrl-C
echo survived
exit
```

**Expected output**

```text
survived
```

**Expected behavior**

The foreground pipeline should be interrupted by Ctrl-C, and `snush` should continue accepting commands.

---

### 5. Mixed Scenarios

#### Example 1: Redirection + pipeline

Assume `input.txt` contains:

```text
apple
banana
apple
carrot
```

**Input**

```sh
cat < input.txt | grep apple | wc -l > count.txt
cat count.txt
exit
```

**Expected output**

```text
2
```

**Expected file content**

```text
count.txt:
2
```

#### Example 2: Built-in inside a pipeline

**Input**

```sh
exit | echo alive
echo still_alive
exit
```

**Expected output**

```text
alive
still_alive
```

**Expected behavior**

`exit` inside a pipeline should run only in the pipeline child process. It should not terminate the parent `snush` process.

---

### 6. Background Process: Extra Credit

Background process support is an extra credit feature.

#### Example 1: Background command should not block the shell

**Input**

```sh
sleep 2 &
echo immediate
exit
```

**Expected output**

```text
immediate
```

**Expected behavior**

`echo immediate` should run without waiting for `sleep 2` to finish.

#### Example 2: Background pipeline with output redirection

**Input**

```sh
echo hello | wc -c > count.txt &
sleep 1
cat count.txt
exit
```

**Expected output**

```text
6
```

**Expected file content**

```text
count.txt:
6
```

**Expected behavior**

The background pipeline should complete, and its output should be written to `count.txt`.
