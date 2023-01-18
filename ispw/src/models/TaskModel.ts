export interface TaskModel {
    moduleName: string;
    application: string;
    moduleType: string;
    checkoutToLevel: string;
}

export interface TaskResponse {
    taskId: string;
    message: string;
    status: number;
    url: string;
    setId: string;
    assignmentId: string;
    moduleName: string;
    moduleType: string;
}