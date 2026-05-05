#include "util.h"
#include "token.h"
#include "dynarray.h"
#include "execute.h"
#include "lexsyn.h"
#include "snush.h"
#include "job.h"

struct job_manager *manager;
volatile sig_atomic_t sigchld_flag = 0;
volatile sig_atomic_t sigint_flag = 0;

/*--------------------------------------------------------------------*/
void check_bg_status() {
    /*
     * TODO: Implement check_bg_status()
     */
}
/*--------------------------------------------------------------------*/
void terminate_jobs() {

    /*
     * TODO: Implement terminate_jobs()
    */
}
/*--------------------------------------------------------------------*/
void cleanup() {
    terminate_jobs();
    /*
     * TODO: Implement cleanup(), if necessary
     */
    free(manager);
}
/*--------------------------------------------------------------------*/
/* Do not modify this function */
static void sigint_handler(int signo) {
    sigint_flag = 1;
}
/*--------------------------------------------------------------------*/
/* Do not modify this function */
static void sigchld_handler(int signo) {
    sigchld_flag = 1;
}
/*--------------------------------------------------------------------*/
static void shell_helper(const char *in_line) {
    DynArray_T oTokens;
    enum LexResult lexcheck;
    enum SyntaxResult syncheck;
    enum BuiltinType btype;
    int n_pipe, is_background;
    int ret;

    oTokens = dynarray_new(0);
    if (oTokens == NULL) {
        error_print("Cannot allocate memory", FPRINTF);
        exit(EXIT_FAILURE);
    }

    lexcheck = lex_line(in_line, oTokens);
    switch (lexcheck) {
    case LEX_SUCCESS:
        if (dynarray_get_length(oTokens) == 0)
            return;

        /* dump lex result when DEBUG is set */
        dump_lex(oTokens);

        syncheck = syntax_check(oTokens);
        if (syncheck == SYN_SUCCESS) {
            if (manager->n_jobs + 1 > MAX_JOBS) {
                fprintf(stderr, 
                    "[Error] Total number of jobs execeed the limit(%d)\n",
                    MAX_JOBS);
                
                dynarray_map(oTokens, free_token, NULL);
                dynarray_free(oTokens);
                return;
            }

            is_background = check_bg(oTokens);
            n_pipe = count_pipe(oTokens);
            
            if (n_pipe > 0) {
                ret = iter_pipe_fork_exec(n_pipe, oTokens, 
                                            is_background);
                if (ret < 0) {
                    error_print("Invalid return value "\
                        "of iter_pipe_fork_exec()", FPRINTF);
                }
            }
            else {
                btype = check_builtin(dynarray_get(oTokens, 0));
                if (btype == NORMAL) {
                    ret = fork_exec(oTokens, is_background);
                    if (ret < 0) {
                        error_print("Invalid return value "\
                            "of fork_exec()", FPRINTF);
                    }
                }
                else {
                    ret = execute_builtin(oTokens, btype);
                    if (ret < 0) {
                        error_print("Invalid return value "\
                            " of execute_builtin()", FPRINTF);
                    }
                }
            }
        }
        /* syntax error cases */
        else if (syncheck == SYN_FAIL_NOCMD)
            error_print("Missing command name", FPRINTF);
        else if (syncheck == SYN_FAIL_MULTREDOUT)
            error_print("Multiple redirection of standard out", 
                FPRINTF);
        else if (syncheck == SYN_FAIL_NODESTOUT)
            error_print("Standard output redirection without file name", 
                        FPRINTF);
        else if (syncheck == SYN_FAIL_MULTREDIN)
            error_print("Multiple redirection of standard input", 
                FPRINTF);
        else if (syncheck == SYN_FAIL_NODESTIN)
            error_print("Standard input redirection without file name", 
                        FPRINTF);
        else if (syncheck == SYN_FAIL_INVALIDBG)
            error_print("Invalid use of background", FPRINTF);
        break;

    case LEX_QERROR:
        error_print("Unmatched quote", FPRINTF);
        break;

    case LEX_NOMEM:
        error_print("Cannot allocate memory", FPRINTF);
        break;

    case LEX_LONG:
        error_print("Command is too large", FPRINTF);
        break;

    default:
        error_print("lex_line needs to be fixed", FPRINTF);
        exit(EXIT_FAILURE);
    }

    /* Free memories allocated to tokens */
    dynarray_map(oTokens, free_token, NULL);
    dynarray_free(oTokens);
}
/*--------------------------------------------------------------------*/
void init_signal(int sig, void (*handler)(int), int flag) {
    struct sigaction action;

    if (!handler) {
        fprintf(stderr, "NULL handler sig: %d\n", sig);
        exit(EXIT_FAILURE);
    }

    block_signal(sig, TRUE);

    memset(&action, 0, sizeof(action));
    action.sa_handler = handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags   = flag;
    
    if (sigaction(sig, &action, NULL) < 0) {
        fprintf(stderr, 
            "Cannot install signal handler sig: %d %s(%s))\n", 
            sig, strerror(errno), errno_name(errno));
        exit(EXIT_FAILURE);
    }

    block_signal(sig, FALSE);
}
/*--------------------------------------------------------------------*/
int main(int argc, char *argv[]) {
    char c_line[MAX_LINE_SIZE + 2];

    atexit(cleanup);
    init_job_manager();
    error_print(argv[0], SETUP);

    /* Register signal handler */
    init_signal(SIGINT, sigint_handler, 0);
    init_signal(SIGCHLD, sigchld_handler, SA_RESTART);
    init_signal(SIGQUIT, SIG_IGN, 0);
    init_signal(SIGTSTP, SIG_IGN, 0);
    init_signal(SIGTTOU, SIG_IGN, 0);
    init_signal(SIGTTIN, SIG_IGN, 0);

    while (1) {
        check_signals();
        check_bg_status();

        fprintf(stdout, "%% ");
        fflush(stdout);

        if (fgets(c_line, MAX_LINE_SIZE, stdin) == NULL) {
		if (errno == EINTR){
		clearerr(stdin);
		fprintf(stdout, "\n");
		fflush(stdout);
		continue;
		}
            printf("\n");
            exit(EXIT_SUCCESS);
        }
        shell_helper(c_line);
    }

    return 0;
}
/*--------------------------------------------------------------------*/
