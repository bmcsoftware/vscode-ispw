export interface XmlResponse {
    datagroupInfo: DataGroupsEntry;
    setId: string;
    xml: string;
    message: string;
    status: number;
    htmlFormData: HtmlFormData;
}

export interface DataGroupsEntry {
    entry: KeyVal[];
}

export interface KeyVal {
    key: string;
    value: dgInfo[];
}

export interface dgInfo {
    field: string;
    type: string;
    defaultVal: string;
}

export interface HtmlFormData {
    html: string;
    title: string;
    componentName?: string;
    componentType?: string;
    setId: string;
}

export interface GenerateResponse {
    setId: string;
    message?: string;
    url: string;
    status?: number;
}