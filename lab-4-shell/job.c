#include "job.h"

extern struct job_manager *manager;
/*--------------------------------------------------------------------*/
void init_job_manager() {
	manager = (struct job_manager *)calloc(1, sizeof(struct job_manager));
	if (manager == NULL) {
		fprintf(stderr, "[Error] job manager allocation failed\n");
		exit(EXIT_FAILURE);
	}

    /*
     * TODO: Init job manager
     */
}
/*--------------------------------------------------------------------*/
/* This is just a placeholder for compilation. You can modify it if you want. */
struct job *find_job_by_jid(int job_id) {
    /*
     * TODO: Implement find_job_by_jid()
     */
    return NULL;
}
/*--------------------------------------------------------------------*/
int remove_pid_from_job(struct job *job, pid_t pid) {

    /*
     * TODO: Implement remove_pid_from_job()
    */
    return 0;
}
/*--------------------------------------------------------------------*/
int delete_job(int jobid) {
	
    /*
     * TODO: Implement delete_job()
     */
    return 0;
}
/*--------------------------------------------------------------------*/
/*
 * TODO: Implement any necessary job-control code in job.c 
 */
