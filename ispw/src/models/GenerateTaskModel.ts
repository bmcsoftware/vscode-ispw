export interface GenerateTaskModel {
    taskId: string;
    containerId: string;
    containerType: string;
    updateDetails: UpdateDetails[];
    setId: string;
}

export interface UpdateDetails {
    category: string;
	name: string;
	value: string;
}

export interface GenerateDialogModel {
    taskId: string;
    containerId: string;
    containerType: string;
}