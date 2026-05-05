#ifndef _JOB_H_
#define _JOB_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <assert.h>

#define MAX_JOBS 16

typedef enum State {
    UNKNOWN = 0,
    FOREGROUND,
    BACKGROUND,
    STOPPED,
} job_state;

/* 
 * Job = The user's command line input
 * ex) if the user's command line input is "ps -ef | grep job" then 
 * One job, Two processes. 
 */
struct job {
    int job_id;
    pid_t pgid;
    int remaining_processes;
    /* TODO: Add any necessary fields to the job */
};

/* 
 * One global variable for a job manager. 
 * When a job is created, register it with the job manager, 
 * regardless of whether it is a foreground or background job.
 */
struct job_manager {
    int n_jobs;
    struct job *jobs;
    /* TODO: Add any necessary fields to the job manager */
};

void init_job_manager();
struct job *find_job_by_jid(int job_id);
int remove_pid_from_job(struct job *job, pid_t pid);
int delete_job(int job_id);

/*
 * TODO: Implement any necessary job-control code in job.h 
 */

#endif /* _JOB_H_ */
