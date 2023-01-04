export interface DialogNode {
  readonly id?: string;
  readonly label?: string;
  readonly type?: string;
  nodeType: "DialogNode";
}

export interface AreaNode {
  readonly id: string;
  readonly label?: string;
  readonly type?: string;
  readonly columnCount?: number;
  nodeType: "AreaNode";
}

export enum Option {
  readonly = 1,
  required,
  focus,
  multiple,
  single,
}

export enum FieldType {
  text = "TEXT",
  combo = "COMBO",
  radio = "RADIO",
  check = "CHECK",
  list = "LIST",
  unSupported = "UNSUPPORTED",
}

export interface ItemNode {
  readonly label?: string;
  value?: string;
  readonly return?: string;
  checked?: boolean;
  nodeType: "ItemNode";
}

export interface FieldNode {
  readonly id: string;
  readonly label?: string;
  readonly type: FieldType;

  readonly options?: Option[];
  readonly size?: number;
  readonly target?: string;
  value?: string;

  nodeType: "FieldNode";
}

export type ISPWNode = CommonElement | ItemNode;

export class CommonElement {
  node?: DialogNode | AreaNode | FieldNode;
  children: ISPWNode[] = [];

  get id(): string {
    if (this.node) {
      return this.node.id ? this.node.id : "";
    }
    return "error";
  }

  get label(): string {
    if (this.node) {
      return this.node.label ? this.node.label : "";
    }
    return "no label";
  }
  
  public constructor(theNode: DialogNode | AreaNode | FieldNode) {
    this.node = theNode;
  }

  addChild(node: ISPWNode): void {
    this.children.push(node);
  }

  getChildren(): ISPWNode[] {
    return this.children;
  }

  isFieldNode(): boolean {
    if (this.node) {
      switch (this.node.nodeType) {
        case "FieldNode":
          return true;
      }
    }
    return false;
  }

  getFieldNodeType(): FieldType {
    let typeData: FieldType = FieldType.unSupported;
    if (this.node) {
      switch (this.node.nodeType) {
        case "FieldNode":
          typeData = this.node.type;
      }
    }

    return typeData;
  }

  getFieldNodeValue(): string | undefined {
    let valueData: string | undefined;
    if (this.node) {
      switch (this.node.nodeType) {
        case "FieldNode":
          valueData = this.node.value;
      }
    }

    return valueData;
  }
}
