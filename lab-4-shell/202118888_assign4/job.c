#include "job.h"

extern struct job_manager *manager;
/*--------------------------------------------------------------------*/
void init_job_manager() {
	manager = (struct job_manager *)calloc(1, sizeof(struct job_manager));
	if (manager == NULL) {
		fprintf(stderr, "[Error] job manager allocation failed\n");
		exit(EXIT_FAILURE);
	}

    manager->jobs = (struct job *)calloc(MAX_JOBS, sizeof(struct job));
    if (manager->jobs == NULL) {
        fprintf(stderr, "[Error] job array allocation failed\n");
        exit(EXIT_FAILURE);
    }
    manager->n_jobs = 0;
    manager->next_jid = 1;
}
/*--------------------------------------------------------------------*/
struct job *find_job_by_jid(int job_id) {
    int i;
    for (i = 0; i < manager->n_jobs; i++) {
        if (manager->jobs[i].job_id == job_id)
            return &manager->jobs[i];
    }
    return NULL;
}
/*--------------------------------------------------------------------*/
struct job *find_fg_job() {
    int i;
    for (i = 0; i < manager->n_jobs; i++) {
        if (manager->jobs[i].state == FOREGROUND)
            return &manager->jobs[i];
    }
    return NULL;
}
/*--------------------------------------------------------------------*/
int remove_pid_from_job(struct job *job, pid_t pid) {
    int i;
    for (i = 0; i < job->total_processes; i++) {
        if (job->pids[i] == pid) {
            job->pids[i] = 0;
            job->remaining_processes--;
            return 1;
        }
    }
    return 0;
}
/*--------------------------------------------------------------------*/
int delete_job(int jobid) {
    int i, found = -1;
    for (i = 0; i < manager->n_jobs; i++) {
        if (manager->jobs[i].job_id == jobid) {
            found = i;
            break;
        }
    }
    if (found < 0) return 0;

    /* Shift remaining jobs down */
    for (i = found; i < manager->n_jobs - 1; i++) {
        manager->jobs[i] = manager->jobs[i + 1];
    }
    memset(&manager->jobs[manager->n_jobs - 1], 0, sizeof(struct job));
    manager->n_jobs--;
    return 1;
}
/*--------------------------------------------------------------------*/
static int get_next_jid() {
    int max_jid = 0;
    int i;
    for (i = 0; i < manager->n_jobs; i++) {
        if (manager->jobs[i].job_id > max_jid)
            max_jid = manager->jobs[i].job_id;
    }
    return max_jid + 1;
}
/*--------------------------------------------------------------------*/
int add_job(pid_t pgid, pid_t *pids, int n_procs, job_state state) {
    int jid;
    struct job *j;

    if (manager->n_jobs >= MAX_JOBS)
        return -1;

    jid = get_next_jid();
    j = &manager->jobs[manager->n_jobs];
    j->job_id = jid;
    j->pgid = pgid;
    j->remaining_processes = n_procs;
    j->total_processes = n_procs;
    j->state = state;
    j->bg_done = 0;
    memcpy(j->pids, pids, sizeof(pid_t) * n_procs);
    manager->n_jobs++;

    return jid;
}
/*--------------------------------------------------------------------*/
