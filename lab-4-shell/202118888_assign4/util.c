#include "util.h"

/*--------------------------------------------------------------------*/
const char *errno_name(int err) {
    switch (err) {
        case EPERM:   return "EPERM";   // Operation not permitted
        case ENOENT:  return "ENOENT";  // No such file or directory
        case ESRCH:   return "ESRCH";   // No such process
        case EINTR:   return "EINTR";   // Interrupted system call
        case EIO:     return "EIO";     // I/O error
        case ENXIO:   return "ENXIO";   // No such device or address
        case E2BIG:   return "E2BIG";   // Argument list too long
        case ENOEXEC: return "ENOEXEC"; // Exec format error
        case EBADF:   return "EBADF";   // Bad file number
        case ECHILD:  return "ECHILD";  // No child processes
        case EAGAIN:  return "EAGAIN";  // Try again
        case ENOMEM:  return "ENOMEM";  // Out of memory
        case EACCES:  return "EACCES";  // Permission denied
        case EFAULT:  return "EFAULT";  // Bad address
        case ENOTBLK: return "ENOTBLK"; // Block device required
        case EBUSY:   return "EBUSY";   // Device or resource busy
        case EEXIST:  return "EEXIST";  // File exists
        case EXDEV:   return "EXDEV";   // Cross-device link
        case ENODEV:  return "ENODEV";  // No such device
        case ENOTDIR: return "ENOTDIR"; // Not a directory
        case EISDIR:  return "EISDIR";  // Is a directory
        case EINVAL:  return "EINVAL";  // Invalid argument
        case ENFILE:  return "ENFILE";  // File table overflow
        case EMFILE:  return "EMFILE";  // Too many open files
        case ENOTTY:  return "ENOTTY";  // Not a typewriter
        case ETXTBSY: return "ETXTBSY"; // Text file busy
        case EFBIG:   return "EFBIG";   // File too large
        case ENOSPC:  return "ENOSPC";  // No space left on device
        case ESPIPE:  return "ESPIPE";  // Illegal seek
        case EROFS:   return "EROFS";   // Read-only file system
        case EMLINK:  return "EMLINK";  // Too many links
        case EPIPE:   return "EPIPE";   // Broken pipe
        case EDOM:    return "EDOM";    // Math argument out of domain
        case ERANGE:  return "ERANGE";  // Math result not representable
        default:      return "UNKNOWN_ERRNO";
    }
}
/*--------------------------------------------------------------------*/
void error_print(char *input, enum PrintMode mode) {
    static char *sh_name = NULL;

    if (mode == SETUP)
        sh_name = input;
    else {
        if (!sh_name) {
            fprintf(stderr, "Initialization failed: Shell name not set\n");
            fflush(stderr);
            exit(EXIT_FAILURE);
        }
        
        if (mode == PERROR) {
            if (input == NULL)
                fprintf(stderr, "%s: %s\n", sh_name, strerror(errno));
            else
                fprintf(stderr, "%s: %s\n", input, strerror(errno));
        }
        else if (mode == FPRINTF) {
            fprintf(stderr, "%s: %s\n", sh_name, input);
        }
        else {
            fprintf(stderr, "mode %d not supported "\
                    "in error_print\n", mode);
        }
        fflush(stderr); 
    }
}
/*--------------------------------------------------------------------*/
enum BuiltinType check_builtin(struct Token *t) {
    /* Check null input before using string functions  */
    assert(t);
    assert(t->token_value);

    if (strncmp(t->token_value, "cd", 2) == 0 && strlen(t->token_value) == 2)
        return B_CD;
    if (strncmp(t->token_value, "exit", 4) == 0 && strlen(t->token_value) == 4)
        return B_EXIT;
    else
        return NORMAL;
}
/*--------------------------------------------------------------------*/
int count_pipe(DynArray_T oTokens) {
    int cnt = 0, i;
    struct Token *t;

    for (i = 0; i < dynarray_get_length(oTokens); i++) {
        t = dynarray_get(oTokens, i);

        if (t->token_type == TOKEN_PIPE)
            cnt++;
    }

    return cnt;
}
/*--------------------------------------------------------------------*/
/* Check if the user demands it to run as background processes */
int check_bg(DynArray_T oTokens) {
    int i;
    struct Token *t;

    for (i = 0; i < dynarray_get_length(oTokens); i++) {
        t = dynarray_get(oTokens, i);

        if (t->token_type == TOKEN_BG)
            return 1;
    }
    return 0;
}
/*--------------------------------------------------------------------*/
const char *special_token_to_str(struct Token *sp_token) {
    switch (sp_token->token_type) {
    case TOKEN_PIPE:
        return "TOKEN_PIPE(|)";
        break;
    case TOKEN_REDIN:
        return "TOKEN_REDIRECTION_IN(<)";
        break;
    case TOKEN_REDOUT:
        return "TOKEN_REDIRECTION_OUT(>)";
        break;
    case TOKEN_BG:
        return "TOKEN_BACKGROUND(&)";
        break;
    case TOKEN_WORD:
        /* This should not be called with TOKEN_WORD */
    default:
        assert(0 && "Unreachable");
        return NULL;
    }
}
/*--------------------------------------------------------------------*/
void dump_lex(DynArray_T oTokens) {
    if (getenv("DEBUG") != NULL) {
        int i;
        struct Token *t;

        for (i = 0; i < dynarray_get_length(oTokens); i++) {
            t = dynarray_get(oTokens, i);

            if (t->token_value == NULL)
                fprintf(stderr, "[%d] %s\n", i, special_token_to_str(t));
            else
                fprintf(stderr, 
                            "[%d] TOKEN_WORD(\"%s\")\n", i, t->token_value);
        }
    }
}
/*--------------------------------------------------------------------*/