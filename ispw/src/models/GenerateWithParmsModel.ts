export interface XmlResponse {
    dataGroups: DataGroupsEntry;
    setId: string;
    xml: string;
    message: string;
    defaults: DataGroupsEntry;
    status: number;
    htmlFormData: HtmlFormData;
}

export interface DataGroupsEntry {
    entry: KeyVal[];
}

export interface KeyVal {
    key: string;
    value: string;
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