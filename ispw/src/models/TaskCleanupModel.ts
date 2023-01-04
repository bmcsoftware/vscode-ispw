export interface TaskCleanupModel {
    assignmentId: string;
    setId: string;
    taskId: string;
}

export interface TaskCleanupRespose {
    assignmentId: string;
    setId: string;
    taskId: string;
    taskCleanupStatus: boolean;
    setCleanupStatus: boolean;
    status: number;
    message: string;
}