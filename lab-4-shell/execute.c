#include "dynarray.h"
#include "token.h"
#include "util.h"
#include "lexsyn.h"
#include "snush.h"
#include "execute.h"
#include "job.h"

extern struct job_manager *manager;
extern volatile sig_atomic_t sigchld_flag;
extern volatile sig_atomic_t sigint_flag;

/*--------------------------------------------------------------------*/
void block_signal(int sig, int block) {
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, sig);

    if (sigprocmask(block ? SIG_BLOCK : SIG_UNBLOCK, &set, NULL) < 0) {
    	fprintf(stderr, 
			"[Error] block_signal: sigprocmask(%s, sig=%d) failed: %s\n",
            block ? "SIG_BLOCK" : "SIG_UNBLOCK", sig, strerror(errno));
        exit(EXIT_FAILURE);
    }
}
/*--------------------------------------------------------------------*/
void handle_sigchld(void) {
	pid_t pid;
	int status;
	int i;

	if (!sigchld_flag) return;

	block_signal(SIGCHLD, TRUE);
	sigchld_flag = 0;

	while ((pid = waitpid(-1, &status, WNOHANG)) > 0) {
		/* Find the job this pid belongs to */
		for (i = 0; i < manager->n_jobs; i++) {
			struct job *j = &manager->jobs[i];
			if (remove_pid_from_job(j, pid)) {
				if (j->remaining_processes == 0) {
					if (j->state == BACKGROUND) {
						j->bg_done = 1;
					}
				}
				break;
			}
		}
	}

	block_signal(SIGCHLD, FALSE);
}
/*--------------------------------------------------------------------*/
void handle_sigint(void) {
	struct job *fg;

	if (!sigint_flag) return;

	block_signal(SIGINT, TRUE);
	sigint_flag = 0;

	fg = find_fg_job();
	if (fg != NULL) {
		/* Send SIGINT to the entire process group */
		killpg(fg->pgid, SIGINT);
	}

	block_signal(SIGINT, FALSE);
}
/*--------------------------------------------------------------------*/
void dup2_e(int oldfd, int newfd, const char *func, const int line) {
	int ret;

	ret = dup2(oldfd, newfd);
	if (ret < 0) {
		fprintf(stderr, 
			"Error dup2(%d, %d): %s(%s) at (%s:%d)\n", 
			oldfd, newfd, strerror(errno), errno_name(errno), func, line);
		_exit(127);
	}
}
/*--------------------------------------------------------------------*/
/* Do not modify this function. It is used to check the signals and 
 * handle them accordingly. It is called in the main loop of snush.c.
 */
void check_signals(void) {
    handle_sigchld();
    handle_sigint();
}
/*--------------------------------------------------------------------*/
void redout_handler(char *fname) {
	int fd;

	fd = open(fname, O_WRONLY | O_CREAT | O_TRUNC, 
	           S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	if (fd < 0) {
		error_print(NULL, PERROR);
		_exit(127);
	}

	dup2_e(fd, STDOUT_FILENO, __func__, __LINE__);
	close(fd);
}
/*--------------------------------------------------------------------*/
void redin_handler(char *fname) {
	int fd;

	fd = open(fname, O_RDONLY);
	if (fd < 0) {
		error_print(NULL, PERROR);
		_exit(127);
	}

	dup2_e(fd, STDIN_FILENO, __func__, __LINE__);
	close(fd);
}
/*--------------------------------------------------------------------*/
void build_command_partial(DynArray_T oTokens, int start, 
						int end, char *args[]) {
	int i, redin = FALSE, redout = FALSE, cnt = 0;
	struct Token *t;

	/* Build command */
	for (i = start; i < end; i++) {
		t = dynarray_get(oTokens, i);

		if (t->token_type == TOKEN_WORD) {
			if (redin == TRUE) {
				redin_handler(t->token_value);
				redin = FALSE;
			}
			else if (redout == TRUE) {
				redout_handler(t->token_value);
				redout = FALSE;
			}
			else {
				args[cnt++] = t->token_value;
			}
		}
		else if (t->token_type == TOKEN_REDIN)
			redin = TRUE;
		else if (t->token_type == TOKEN_REDOUT)
			redout = TRUE;
	}

	if (cnt >= MAX_ARGS_CNT) 
		fprintf(stderr, "[BUG] args overflow! cnt=%d\n", cnt);

	args[cnt] = NULL;

#ifdef DEBUG
	for (i = 0; i < cnt; i++) {
		if (args[i] == NULL)
			printf("CMD: NULL\n");
		else
			printf("CMD: %s\n", args[i]);
	}
	printf("END\n");
#endif
}
/*--------------------------------------------------------------------*/
void build_command(DynArray_T oTokens, char *args[]) {
	build_command_partial(oTokens, 0, 
						dynarray_get_length(oTokens), 
						args);
}
/*--------------------------------------------------------------------*/
int execute_builtin_partial(DynArray_T toks, int start, int end,
                            enum BuiltinType btype, int in_child) {
    
	int argc = end - start;
	struct Token *t1;
	int ret;
    char *dir;

    switch (btype) {
    case B_EXIT:
        if (in_child) return 0;
        
		if (argc == 1) {
			dynarray_map(toks, free_token, NULL);
			dynarray_free(toks);
			exit(EXIT_SUCCESS);
		}
		else {
			error_print("exit does not take any parameters", FPRINTF);
			return -1;
		}

    case B_CD: {
        if (argc == 1) {
            dir = getenv("HOME");
            if (!dir) {
                error_print("cd: HOME variable not set", FPRINTF);
                return -1;
            }
        } 
		else if (argc == 2) {
            t1 = dynarray_get(toks, start + 1);
            if (t1 && t1->token_type == TOKEN_WORD) 
				dir = t1->token_value;
        } 
		else {
            error_print("cd: Too many parameters", FPRINTF);
            return -1;
        }

        ret = chdir(dir);
        if (ret < 0) {
            error_print(NULL, PERROR);
            return -1;
        }
        return 0;
    }

    default:
        error_print("Bug found in execute_builtin_partial", FPRINTF);
        return -1;
    }
}
/*--------------------------------------------------------------------*/
int execute_builtin(DynArray_T oTokens, enum BuiltinType btype) {
	return execute_builtin_partial(oTokens, 0, 
								dynarray_get_length(oTokens), btype, FALSE);
}
/*--------------------------------------------------------------------*/
/* 
 * You need to finish implementing job related APIs. (find_job_by_jid(),
 * remove_pid_from_job(), delete_job()) in job.c to handle the job.
 * Feel free to modify the format of the job API according to your design.
 */
void wait_fg(int jobid) {
	pid_t pid;
	int status;

	 // Find the job structure by job ID
    struct job *job = find_job_by_jid(jobid);
    if (!job) {
        fprintf(stderr, "Job: %d not found\n", jobid);
        return;
    }

    while (job->remaining_processes > 0) {
        pid = waitpid(-job->pgid, &status, WUNTRACED);

        if (pid > 0) {
			// Remove the finished process from the job's pid list
			if (!remove_pid_from_job(job, pid)) {
				fprintf(stderr, "Pid %d not found in the job: %d list\n", 
					pid, job->job_id);
			}
			continue;
        }

		if (pid < 0) {
			if (errno == EINTR) {
				/* Forward SIGINT to child process group */
				if (sigint_flag) {
					sigint_flag = 0;
					killpg(job->pgid, SIGINT);
					fprintf(stdout, "\n");
					fflush(stdout);
				}
				/* Reap any already-terminated children */
				while ((pid = waitpid(-job->pgid, &status, WNOHANG)) > 0) {
					remove_pid_from_job(job, pid);
				}
				continue;
			}
			if (errno == ECHILD) break;
			error_print("Unknown error waitpid() in wait_fg()", PERROR);
			break;
		}
    }

	// Clean up job table entry if all processes are done
    if (job->remaining_processes == 0)
        delete_job(job->job_id);
}
/*--------------------------------------------------------------------*/
void print_job(int jobid, pid_t pgid) {
    fprintf(stdout, 
		"[%d] Process group: %d running in the background\n", jobid, pgid);
}
/*--------------------------------------------------------------------*/
static void setup_child_signals(void) {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = SIG_DFL;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGINT, &sa, NULL);
    sigaction(SIGCHLD, &sa, NULL);
    sigaction(SIGQUIT, &sa, NULL);
    sigaction(SIGTSTP, &sa, NULL);
    sigaction(SIGTTOU, &sa, NULL);
    sigaction(SIGTTIN, &sa, NULL);

    /* Unblock all signals in child */
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGINT);
    sigaddset(&set, SIGCHLD);
    sigprocmask(SIG_UNBLOCK, &set, NULL);
}
/*--------------------------------------------------------------------*/
int fork_exec(DynArray_T oTokens, int is_background) {
	pid_t pid;
	char *args[MAX_ARGS_CNT];
	int jobid;
	pid_t pids[1];

	/* Block SIGCHLD to avoid race condition */
	block_signal(SIGCHLD, TRUE);

	pid = fork();
	if (pid < 0) {
		error_print(NULL, PERROR);
		block_signal(SIGCHLD, FALSE);
		return -1;
	}

	if (pid == 0) {
		/* Child process */
		/* Set own process group */
		setpgid(0, 0);

		setup_child_signals();

		/* Build and exec the command */
		build_command(oTokens, args);
		execvp(args[0], args);
		error_print(args[0], PERROR);
		_exit(127);
	}

	/* Parent process */
	setpgid(pid, pid); /* Also set in parent to avoid race */
	
	pids[0] = pid;
	jobid = add_job(pid, pids, 1, 
	                is_background ? BACKGROUND : FOREGROUND);
	if (jobid < 0) {
		fprintf(stderr, "[Error] Failed to add job\n");
		block_signal(SIGCHLD, FALSE);
		return -1;
	}

	block_signal(SIGCHLD, FALSE);

	if (is_background) {
		print_job(jobid, pid);
	}
	else {
		wait_fg(jobid);
	}

	return jobid;
}
/*--------------------------------------------------------------------*/
int iter_pipe_fork_exec(int n_pipe, DynArray_T oTokens, int is_background) {
	int n_cmds = n_pipe + 1;
	int pipefds[n_pipe][2];
	pid_t pids[MAX_PROCS_PER_JOB];
	pid_t pgid = 0;
	int cmd_start = 0;
	int cmd_idx, i;
	int jobid;
	int len = dynarray_get_length(oTokens);

	/* Block SIGCHLD to avoid race */
	block_signal(SIGCHLD, TRUE);

	/* Create all pipes */
	for (i = 0; i < n_pipe; i++) {
		if (pipe(pipefds[i]) < 0) {
			error_print(NULL, PERROR);
			block_signal(SIGCHLD, FALSE);
			return -1;
		}
	}

	cmd_idx = 0;
	cmd_start = 0;

	for (cmd_idx = 0; cmd_idx < n_cmds; cmd_idx++) {
		int cmd_end = cmd_start;
		struct Token *t;
		char *args[MAX_ARGS_CNT];
		enum BuiltinType btype;
		pid_t pid;

		/* Find end of current command segment (pipe or end of tokens) */
		while (cmd_end < len) {
			t = dynarray_get(oTokens, cmd_end);
			if (t->token_type == TOKEN_PIPE || t->token_type == TOKEN_BG) 
				break;
			cmd_end++;
		}

		pid = fork();
		if (pid < 0) {
			error_print(NULL, PERROR);
			block_signal(SIGCHLD, FALSE);
			return -1;
		}

		if (pid == 0) {
			/* Child process */
			if (cmd_idx == 0) {
				setpgid(0, 0);
			} else {
				setpgid(0, pgid);
			}

			setup_child_signals();

			/* Set up pipe redirections */
			/* If not the first command, read from previous pipe */
			if (cmd_idx > 0) {
				dup2_e(pipefds[cmd_idx - 1][0], STDIN_FILENO, 
				       __func__, __LINE__);
			}
			/* If not the last command, write to next pipe */
			if (cmd_idx < n_pipe) {
				dup2_e(pipefds[cmd_idx][1], STDOUT_FILENO, 
				       __func__, __LINE__);
			}

			/* Close all pipe fds in child */
			for (i = 0; i < n_pipe; i++) {
				close(pipefds[i][0]);
				close(pipefds[i][1]);
			}

			/* Check if command is a builtin */
			t = dynarray_get(oTokens, cmd_start);
			btype = check_builtin(t);
			if (btype != NORMAL) {
				/* Execute builtin in child (in_child = TRUE) */
				build_command_partial(oTokens, cmd_start, cmd_end, args);
				execute_builtin_partial(oTokens, cmd_start, cmd_end, 
				                       btype, TRUE);
				_exit(0);
			}

			/* Build and exec */
			build_command_partial(oTokens, cmd_start, cmd_end, args);
			execvp(args[0], args);
			error_print(args[0], PERROR);
			_exit(127);
		}

		/* Parent */
		if (cmd_idx == 0) {
			pgid = pid;
		}
		setpgid(pid, pgid);
		pids[cmd_idx] = pid;

		/* Move to next command segment (skip past pipe token) */
		cmd_start = cmd_end + 1;
	}

	/* Close all pipe fds in parent */
	for (i = 0; i < n_pipe; i++) {
		close(pipefds[i][0]);
		close(pipefds[i][1]);
	}

	/* Add job */
	jobid = add_job(pgid, pids, n_cmds, 
	                is_background ? BACKGROUND : FOREGROUND);
	if (jobid < 0) {
		fprintf(stderr, "[Error] Failed to add job\n");
		block_signal(SIGCHLD, FALSE);
		return -1;
	}

	block_signal(SIGCHLD, FALSE);

	if (is_background) {
		print_job(jobid, pgid);
	}
	else {
		wait_fg(jobid);
	}

	return jobid;
}
/*--------------------------------------------------------------------*/