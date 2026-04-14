//--------------------------------------------------------------------------------------------------
// System Programming                         I/O Lab                                     Spring 2026
//
/// @file
/// @brief recursively traverse directory tree and list all entries
/// @author <곽형철>
/// @studid <2021-18888>
//--------------------------------------------------------------------------------------------------

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <unistd.h>
#include <stdarg.h>
#include <assert.h>
#include <grp.h>
#include <pwd.h>

/// output control flags
#define F_DEPTH 0x1  ///< print directory tree
#define F_Filter 0x2 ///< pattern matching

/// maximum numbers
#define MAX_DIR 64         ///< maximum number of supported directories
#define MAX_PATH_LEN 1024  ///< maximum length of a path
#define MAX_DEPTH 20       ///< maximum depth of directory tree (for -d option)
int max_depth = MAX_DEPTH; ///< maximum depth of directory tree (for -d option)

/// struct holding the summary
struct summary
{
  unsigned int dirs;  ///< number of directories encountered
  unsigned int files; ///< number of files
  unsigned int links; ///< number of links
  unsigned int fifos; ///< number of pipes
  unsigned int socks; ///< number of sockets

  unsigned long long size;   ///< total size (in bytes)
  unsigned long long blocks; ///< total number of blocks (512 byte blocks)
};

// Output format strings for different entry types and errors.
const char *print_formats[8] = {
    "Name                                                        User:Group           Size    Blocks Type\n",
    "----------------------------------------------------------------------------------------------------\n",
    "%-54s  ERROR: %s\n",
    "%-54s  No such file or directory\n",
    "%-54s  %8.8s:%-8.8s  %10llu  %8llu    %c\n",
    "Invalid pattern syntax\n",
    "Out of memory\n",
};

const char *pattern = NULL;

// Aborts the program with an error message.
void panic(const char *msg, const char *format)
{
  if (msg)
  {
    if (format)
      fprintf(stderr, format, msg);
    else
      fprintf(stderr, "%s\n", msg);
  }
  exit(EXIT_FAILURE);
}

// Regex AST node types.
typedef enum
{
  N_CHAR,
  N_ANY,
  N_CONCAT,
  N_ALT,
  N_STAR
} NodeType;

// Regex AST node structure.
typedef struct RegexNode
{
  NodeType type;
  char ch;
  struct RegexNode *left;
  struct RegexNode *right;
  int id;
} RegexNode;

int node_id_counter = 0;
const char *p_curr;

// Validates regular expression syntax.
int validate_pattern(const char *p)
{
  if (!p || !*p)
    return 0;
  int len = strlen(p);
  int parens = 0;
  for (int i = 0; i < len; i++)
  {
    if (p[i] == '(')
    {
      parens++;
      if (i + 1 < len && (p[i + 1] == ')' || p[i + 1] == '*' || p[i + 1] == '|'))
        return 0;
    }
    else if (p[i] == ')')
    {
      parens--;
      if (parens < 0 || (i > 0 && p[i - 1] == '|'))
        return 0;
    }
    else if (p[i] == '*')
    {
      if (i == 0 || p[i - 1] == '(' || p[i - 1] == '|' || p[i - 1] == '*')
        return 0;
    }
    else if (p[i] == '|')
    {
      if (i == 0 || i == len - 1 || p[i - 1] == '(' || p[i - 1] == '|' || p[i + 1] == ')')
        return 0;
    }
  }
  return parens == 0;
}

RegexNode *parse_expr(void);

// Allocates a new regex AST node.
RegexNode *make_node(NodeType t, char c, RegexNode *l, RegexNode *r)
{
  RegexNode *n = calloc(1, sizeof(RegexNode));
  if (!n)
    panic("Out of memory", print_formats[6]);
  n->type = t;
  n->ch = c;
  n->left = l;
  n->right = r;
  n->id = node_id_counter++;
  return n;
}

// Parses a character, wildcard, or group.
RegexNode *parse_atom(void)
{
  if (*p_curr == '(')
  {
    p_curr++;
    RegexNode *n = parse_expr();
    if (*p_curr == ')')
      p_curr++;
    return n;
  }
  else if (*p_curr == '?')
  {
    p_curr++;
    return make_node(N_ANY, 0, NULL, NULL);
  }
  else
  {
    char c = *p_curr++;
    return make_node(N_CHAR, c, NULL, NULL);
  }
}

// Parses a repetition operator.
RegexNode *parse_factor(void)
{
  RegexNode *n = parse_atom();
  while (*p_curr == '*')
  {
    p_curr++;
    n = make_node(N_STAR, 0, n, NULL);
  }
  return n;
}

// Parses a sequence of regex factors.
RegexNode *parse_term(void)
{
  RegexNode *n = parse_factor();
  while (*p_curr && *p_curr != '|' && *p_curr != ')')
  {
    RegexNode *right = parse_factor();
    n = make_node(N_CONCAT, 0, n, right);
  }
  return n;
}

// Parses a regex choice (alternation).
RegexNode *parse_expr(void)
{
  RegexNode *n = parse_term();
  while (*p_curr == '|')
  {
    p_curr++;
    RegexNode *right = parse_term();
    n = make_node(N_ALT, 0, n, right);
  }
  return n;
}

// Compiles a pattern string into an AST.
RegexNode *compile_regex(const char *pat)
{
  if (!validate_pattern(pat))
    panic("Invalid pattern syntax", print_formats[5]);
  p_curr = pat;
  RegexNode *root = parse_expr();
  if (*p_curr != '\0')
    panic("Invalid pattern syntax", print_formats[5]);
  return root;
}

// Recursively cleans up regex AST memory.
void free_regex(RegexNode *n)
{
  if (!n)
    return;
  free_regex(n->left);
  free_regex(n->right);
  free(n);
}

// Matches a string against a regex AST using DP.
int memo_match(RegexNode *n, const char *s, int i, int j, int8_t *memo, int num_nodes, int len)
{
  int idx = n->id * ((len + 1) * (len + 1)) + i * (len + 1) + j;
  if (memo[idx] != -1)
    return memo[idx];

  int res = 0;
  if (n->type == N_CHAR)
    res = (j - i == 1) && (s[i] == n->ch);
  else if (n->type == N_ANY)
    res = (j - i == 1);
  else if (n->type == N_CONCAT)
  {
    for (int k = i; k <= j; k++)
    {
      if (memo_match(n->left, s, i, k, memo, num_nodes, len) &&
          memo_match(n->right, s, k, j, memo, num_nodes, len))
      {
        res = 1;
        break;
      }
    }
  }
  else if (n->type == N_ALT)
  {
    res = memo_match(n->left, s, i, j, memo, num_nodes, len) ||
          memo_match(n->right, s, i, j, memo, num_nodes, len);
  }
  else if (n->type == N_STAR)
  {
    if (i == j)
      res = 1;
    else
    {
      for (int k = i + 1; k <= j; k++)
      {
        if (memo_match(n->left, s, i, k, memo, num_nodes, len) &&
            memo_match(n, s, k, j, memo, num_nodes, len))
        {
          res = 1;
          break;
        }
      }
    }
  }
  return memo[idx] = res;
}

// Checks if a string contains a regex match.
int check_match(RegexNode *root, const char *s)
{
  if (!root)
    return 1;
  int len = strlen(s);
  int8_t *memo = malloc(node_id_counter * (len + 1) * (len + 1) * sizeof(int8_t));
  memset(memo, -1, node_id_counter * (len + 1) * (len + 1) * sizeof(int8_t));

  int matched = 0;
  for (int i = 0; i <= len && !matched; i++)
  {
    for (int j = i; j <= len && !matched; j++)
    {
      if (memo_match(root, s, i, j, memo, node_id_counter, len))
      {
        matched = 1;
      }
    }
  }
  free(memo);
  return matched;
}

// Directory tree node structure.
RegexNode *regex_root = NULL;

typedef struct TreeNode
{
  char *name;
  struct stat st;
  int is_dir;
  int is_symlink;
  int is_fifo;
  int is_socket;
  int lstat_error;
  int opendir_error;
  char *error_msg;

  struct TreeNode **children;
  int num_children;

  int matches;
  int subtree_matches;
  int is_placeholder;
} TreeNode;

// Returns the next valid directory entry.
struct dirent *get_next(DIR *dir)
{
  struct dirent *next;
  int ignore;
  do
  {
    errno = 0;
    next = readdir(dir);
    if (errno != 0)
      perror(NULL);
    ignore = next && ((strcmp(next->d_name, ".") == 0) || (strcmp(next->d_name, "..") == 0));
  } while (next && ignore);
  return next;
}

// Sorts entries by type and name.
static int dirent_compare(const void *a, const void *b)
{
  TreeNode *n1 = *(TreeNode **)a;
  TreeNode *n2 = *(TreeNode **)b;
  if (n1->is_dir != n2->is_dir)
    return n1->is_dir ? -1 : 1;
  return strcmp(n1->name, n2->name);
}

// Returns the final component of a path.
const char *get_basename(const char *path)
{
  const char *base = strrchr(path, '/');
  return base ? base + 1 : path;
}

// Recursively cleans up directory tree memory.
void free_tree(TreeNode *n)
{
  if (!n)
    return;
  for (int i = 0; i < n->num_children; i++)
    free_tree(n->children[i]);
  free(n->children);
  free(n->name);
  if (n->error_msg)
    free(n->error_msg);
  free(n);
}

// Constructs a filtered directory tree.
TreeNode *build_tree(const char *path, const char *name, int depth)
{
  TreeNode *node = calloc(1, sizeof(TreeNode));
  node->name = strdup(name);

  if (lstat(path, &node->st) < 0)
  {
    node->lstat_error = 1;
    node->error_msg = strdup(strerror(errno));
    node->matches = (regex_root == NULL) || check_match(regex_root, name);
    node->subtree_matches = node->matches;
    return node;
  }

  node->is_dir = S_ISDIR(node->st.st_mode);
  node->is_symlink = S_ISLNK(node->st.st_mode);
  node->is_fifo = S_ISFIFO(node->st.st_mode);
  node->is_socket = S_ISSOCK(node->st.st_mode);

  node->matches = (regex_root == NULL) || check_match(regex_root, name);
  node->subtree_matches = node->matches;

  if (node->is_dir && depth < max_depth)
  {
    DIR *dir = opendir(path);
    if (!dir)
    {
      node->opendir_error = 1;
      node->error_msg = strdup(strerror(errno));
    }
    else
    {
      struct dirent *ent;
      int cap = 16;
      node->children = malloc(cap * sizeof(TreeNode *));
      while ((ent = get_next(dir)) != NULL)
      {
        char *child_path;
        if (asprintf(&child_path, "%s/%s", strcmp(path, "/") == 0 ? "" : path, ent->d_name) < 0)
          panic("Out of memory", print_formats[6]);

        TreeNode *child = build_tree(child_path, ent->d_name, depth + 1);
        free(child_path);

        if (child)
        {
          if (node->num_children >= cap)
          {
            cap *= 2;
            node->children = realloc(node->children, cap * sizeof(TreeNode *));
          }
          node->children[node->num_children++] = child;
        }
      }
      closedir(dir);
      qsort(node->children, node->num_children, sizeof(TreeNode *), dirent_compare);

      for (int i = 0; i < node->num_children; i++)
      {
        if (node->children[i]->subtree_matches)
        {
          node->subtree_matches = 1;
          break;
        }
      }
    }
  }

  // Apply exact filter rules
  if (!node->is_dir && !node->matches)
  {
    free_tree(node);
    return NULL;
  }
  if (node->is_dir && !node->subtree_matches)
  {
    free_tree(node);
    return NULL;
  }
  if (node->is_dir && !node->matches && node->subtree_matches)
  {
    node->is_placeholder = 1;
  }

  return node;
}

// Displays the tree and calculates statistics.
void print_tree(TreeNode *node, int depth, struct summary *stats)
{
  if (!node)
    return;

  char name_buf[256];
  int indent = depth * 2;
  int max_name_len = 54 - indent;
  if ((int)strlen(node->name) > max_name_len)
  {
    snprintf(name_buf, sizeof(name_buf), "%*s%.*s...", indent, "", max_name_len - 3, node->name);
  }
  else
  {
    snprintf(name_buf, sizeof(name_buf), "%*s%s", indent, "", node->name);
  }

  if (node->is_placeholder)
  {
    printf("%s\n", name_buf);
  }
  else if (node->lstat_error)
  {
    printf("%s\n", name_buf);
    if (node->error_msg)
    {
      fflush(stdout);
      fprintf(stderr, "%*sERROR: %s\n", indent + 2, "", node->error_msg);
    }
  }
  else if (depth == 0)
  {
    printf("%s\n", name_buf);
    if (node->opendir_error && node->error_msg)
    {
      fflush(stdout);
      fprintf(stderr, "%*sERROR: %s\n", indent + 2, "", node->error_msg);
    }
  }
  else
  {
    char user[32] = {0};
    char group[32] = {0};
    struct passwd *pw = getpwuid(node->st.st_uid);
    if (pw)
      strncpy(user, pw->pw_name, 8);
    else
      snprintf(user, sizeof(user), "%d", node->st.st_uid);

    struct group *gr = getgrgid(node->st.st_gid);
    if (gr)
      strncpy(group, gr->gr_name, 8);
    else
      snprintf(group, sizeof(group), "%d", node->st.st_gid);

    char type_c = ' ';
    if (node->is_dir)
      type_c = 'd';
    else if (node->is_symlink)
      type_c = 'l';
    else if (node->is_fifo)
      type_c = 'f';
    else if (node->is_socket)
      type_c = 's';

    printf(print_formats[4], name_buf, user, group,
           (unsigned long long)node->st.st_size, (unsigned long long)node->st.st_blocks, type_c);

    // Accumulate statistics ONLY for non-placeholder
    if (depth > 0)
    {
      if (node->is_dir)
        stats->dirs++;
      else if (node->is_symlink)
        stats->links++;
      else if (node->is_fifo)
        stats->fifos++;
      else if (node->is_socket)
        stats->socks++;
      else
        stats->files++;

      stats->size += node->st.st_size;
      stats->blocks += node->st.st_blocks;
    }

    if (node->opendir_error && node->error_msg)
    {
      fflush(stdout);
      fprintf(stderr, "%*sERROR: %s\n", indent + 2, "", node->error_msg);
    }
  }

  for (int i = 0; i < node->num_children; i++)
  {
    print_tree(node->children[i], depth + 1, stats);
  }
}

// Displays a single directory summary line.
void print_summary(struct summary *stats)
{
  char buf[256];
  snprintf(buf, sizeof(buf), "%u file%s, %u director%s, %u link%s, %u pipe%s, and %u socket%s",
           stats->files, stats->files == 1 ? "" : "s",
           stats->dirs, stats->dirs == 1 ? "y" : "ies",
           stats->links, stats->links == 1 ? "" : "s",
           stats->fifos, stats->fifos == 1 ? "" : "s",
           stats->socks, stats->socks == 1 ? "" : "s");

  if (strlen(buf) > 68)
    strcpy(buf + 65, "...");
  printf("%-68s   %14llu %9llu\n", buf, stats->size, stats->blocks);
}

// Prints usage help and exits.
void syntax(const char *argv0, const char *error, ...)
{
  if (error)
  {
    va_list ap;
    va_start(ap, error);
    vfprintf(stderr, error, ap);
    va_end(ap);
    printf("\n\n");
  }

  assert(argv0 != NULL);
  fprintf(stderr, "Usage %s [-d depth] [-f pattern] [-h] [path...]\n"
                  "Recursively traverse directory tree and list all entries. If no path is given, the current directory\n"
                  "is analyzed.\n"
                  "\n"
                  "Options:\n"
                  " -d depth   | set maximum depth of directory traversal (1-%d)\n"
                  " -f pattern | filter entries using pattern (supports \'?\', \'*\' and \'()\')\n"
                  " -h         | print this help\n"
                  " path...    | list of space-separated paths (max %d). Default is the current directory.\n",
          basename(argv0), MAX_DEPTH, MAX_DIR);
  exit(EXIT_FAILURE);
}

// Processes a single directory path.
void process_dir(const char *dn, const char *pstr, struct summary *stats, unsigned int flags)
{
  TreeNode *root = build_tree(dn, dn, 0);
  if (root)
  {
    print_tree(root, 0, stats);
    free_tree(root);
  }
}

// Application entry point and argument parser.
int main(int argc, char *argv[])
{
  const char CURDIR[] = ".";
  const char *directories[MAX_DIR];
  int ndir = 0;

  struct summary tstat = {0};
  unsigned int flags = 0;

  for (int i = 1; i < argc; i++)
  {
    if (argv[i][0] == '-')
    {
      if (!strcmp(argv[i], "-d"))
      {
        flags |= F_DEPTH;
        if (++i < argc && argv[i][0] != '-')
        {
          max_depth = atoi(argv[i]);
          if (max_depth < 1 || max_depth > MAX_DEPTH)
          {
            syntax(argv[0], "Invalid depth value '%s'. Must be between 1 and %d.", argv[i], MAX_DEPTH);
          }
        }
        else
          syntax(argv[0], "Missing depth value argument.");
      }
      else if (!strcmp(argv[i], "-f"))
      {
        if (++i < argc && argv[i][0] != '-')
        {
          flags |= F_Filter;
          pattern = argv[i];
          regex_root = compile_regex(pattern);
        }
        else
          syntax(argv[0], "Missing filtering pattern argument.");
      }
      else if (!strcmp(argv[i], "-h"))
        syntax(argv[0], NULL);
      else
        syntax(argv[0], "Unrecognized option '%s'.", argv[i]);
    }
    else
    {
      if (ndir < MAX_DIR)
        directories[ndir++] = argv[i];
      else
        fprintf(stderr, "Warning: maximum number of directories exceeded, ignoring '%s'.\n", argv[i]);
    }
  }

  if (ndir == 0)
    directories[ndir++] = CURDIR;

  for (int i = 0; i < ndir; i++)
  {

    struct summary dstat = {0};

    printf("%s", print_formats[0]);
    printf("%s", print_formats[1]);

    process_dir(directories[i], "", &dstat, flags);

    printf("%s", print_formats[1]);
    print_summary(&dstat);
    printf("\n");

    tstat.dirs += dstat.dirs;
    tstat.files += dstat.files;
    tstat.links += dstat.links;
    tstat.fifos += dstat.fifos;
    tstat.socks += dstat.socks;
    tstat.size += dstat.size;
    tstat.blocks += dstat.blocks;
  }

  if (ndir > 1)
  {
    printf("Analyzed %d directories:\n"
           "  total # of files:        %16d\n"
           "  total # of directories:  %16d\n"
           "  total # of links:        %16d\n"
           "  total # of pipes:        %16d\n"
           "  total # of sockets:      %16d\n"
           "  total # of entries:      %16d\n"
           "  total file size:         %16llu\n"
           "  total # of blocks:       %16llu\n",
           ndir, tstat.files, tstat.dirs, tstat.links, tstat.fifos, tstat.socks,
           tstat.files + tstat.dirs + tstat.links + tstat.fifos + tstat.socks,
           tstat.size, tstat.blocks);
  }

  if (regex_root)
    free_regex(regex_root);

  return EXIT_SUCCESS;
}