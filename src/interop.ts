import * as TaskPort from "elm-taskport/dist/taskport.min.js";
import { ClickOutside } from "./web-components/clickOutside.js";
import * as Y from "yjs";
import { IndexeddbPersistence } from "y-indexeddb";
// @ts-ignore TS2305
import { HocuspocusProvider } from "@hocuspocus/provider";

type ElmApp = {
  ports: {
    incoming: {
      send: (message: unknown) => Promise<void>;
    };
    syncState: {
      send: (message: SyncState) => Promise<void>;
    };
    outgoing: {
      subscribe: (callback: (data: unknown) => Promise<void>) => void;
    };
  };
};

type SyncState = "none" | "offline" | "paused" | "syncing" | "synced" | { error: string };

type AppSettings = {
  theme: "auto" | "light" | "dark";
  sync: {
    config: { room: string; url: string } | null;
    state: SyncState;
  };
  version: number;
};

type ItemState = "stuffed" | "required";

type Item = {
  id: string;
  name: string;
  quantity: {
    count: number;
    unit: string;
  };
  comment?: string;
  slug: string;
  symbol: string;
  state: ItemState;
  created: number;
  updated: number;
};

type CollapsedState = "open" | "collapsed";

type Category = {
  id: string;
  name: string;
  items: number[];
  state: CollapsedState;
  created: number;
  updated: number;
};

type DataDump = {
  version: number;
  items: Record<string, Item>;
  categories: Category[];
};

type GlobalState = {
  doc?: Y.Doc;
  settings?: AppSettings;
  items?: Y.Map<Item>;
  categories?: Y.Array<Category>;
  provider?: HocuspocusProvider;
  db?: IndexeddbPersistence;
  app?: ElmApp;
};

window.customElements.define("on-click-outside", ClickOutside);

TaskPort.install({ logCallErrors: true, logInteropErrors: true });

TaskPort.register("storeDump", storeDump);
TaskPort.register("queryAllCatsAndItems", queryAllCatsAndItems);
TaskPort.register("getUuid", getUuid);

TaskPort.register("storeAllItems", storeAllItems);
TaskPort.register("storeItem", storeItem);
TaskPort.register("deleteItem", deleteItem);

TaskPort.register("storeCategory", storeCategory);
TaskPort.register("deleteCategory", deleteCategory);

TaskPort.register("selectInput", selectInput);
TaskPort.register("setTheme", setTheme);

TaskPort.register("initSync", initSync);
TaskPort.register("pauseSync", pauseSync);
TaskPort.register("resumeSync", resumeSync);

const globalState: GlobalState = {};

export const flags = async () => {
  await init(globalState);
  return globalState.settings;
};

export const onReady = ({ app }: { app: ElmApp }) => {
  globalState.app = app;
  if (globalState.settings?.sync?.config?.url && globalState.settings?.sync?.config?.room) {
    globalState.provider = startSync({
      url: globalState.settings.sync.config.url,
      room: globalState.settings.sync.config.room,
      doc: globalState.doc,
      app: globalState.app,
    });
  }

  globalState.doc.on("update", (_, origin) => {
    if (origin == null) return;
    app.ports.incoming.send({
      categories: globalState.categories.toJSON(),
      items: globalState.items.toJSON(),
    });
  });
};

async function init(globalState: GlobalState) {
  globalState.doc = new Y.Doc();
  globalState.categories = globalState.doc.getArray("categories");
  globalState.items = globalState.doc.getMap("items");

  try {
    globalState.db = await initDb(globalState.doc);

    globalState.settings = await getSettings(globalState.db);
    return globalState;
  } catch (error) {
    return error;
  }
}

function initDb(doc: Y.Doc): Promise<IndexeddbPersistence> {
  return new Promise((resolve, reject) => {
    try {
      const db = new IndexeddbPersistence("grocery", doc);
      db.set("version", 1);
      db.once("synced", () => resolve(db));
    } catch (error) {
      reject(error);
    }
  });
}

function startSync({ url, room, doc, app }) {
  return new HocuspocusProvider({
    url: url,
    name: room,
    document: doc,
    onStatus(data) {
      console.log("STATUS", data.status);
      if (data.status === "connecting") {
        app.ports.syncState.send("syncing");
      }
    },
    onConnect() {
      globalState.settings.sync.state = "syncing";
      globalState.app.ports.syncState.send("syncing");
    },
    onSynced() {
      globalState.settings.sync.state = "synced";
      globalState.app.ports.syncState.send("synced");
    },
    onDisconnect(data) {
      if (data.event.code === 1005) {
        globalState.settings.sync.state = "paused";
        app.ports.syncState.send("paused");
      } else {
        globalState.settings.sync.state = {
          error: `Connection failed.
									 Code: ${data.event.code}, reason: ${data.event.reason}`,
        };
        globalState.app.ports.syncState.send({
          error: `Connection failed.
									 Code: ${data.event.code}, reason: ${data.event.reason}`,
        });
      }
    },
  });
}

function pauseSync() {
  if (globalState.provider) {
    globalState.provider.disconnect();
  }
}

async function resumeSync() {
  if (globalState.provider) {
    const result = await globalState.provider.connect();
    console.log("RESUME RESULT", result);
  }
}

async function initSync({ room, url }: { room: string; url: string }) {
  if (globalState.db) {
    const onConnected = async () => {
      await globalState.db.set("room", room);
      await globalState.db.set("url", url);
      globalState.provider?.off("connect", onConnected);
    };

    globalState.provider = startSync({
      url,
      room,
      doc: globalState.doc,
      app: globalState.app,
    });

    globalState.provider.on("connect", onConnected);
  }
  return {
    room,
    url,
  };
}

async function getSettings(dbHandler: IndexeddbPersistence): Promise<AppSettings> {
  const theme = await dbHandler.get("theme");
  const room = await dbHandler.get("room");
  const url = await dbHandler.get("url");
  const version = await dbHandler.get("version");

  return {
    theme: theme ?? "auto",
    sync: {
      config: room && url ? { room, url } : null,
      state: room && url ? "offline" : "none",
    },
    version: version ?? 1,
  };
}

function queryAllCatsAndItems() {
  const result = {
    categories: [],
    items: {},
  };
  if (globalState.doc) {
    result.categories = globalState.categories.toJSON();
    result.items = globalState.items.toJSON();
  }
  return result;
}

function storeItem(item: Item) {
  if (globalState.items) {
    globalState.items.set(item.id, item);
    return true;
  }
  return false;
}

function deleteItem(itemId: string) {
  if (globalState.items) {
    globalState.items.delete(itemId);
    return true;
  }
  return false;
}

function storeAllItems(items: Record<string, Item>) {
  if (globalState.items) {
    Object.entries(items).forEach(([id, item]) => {
      console.log(item.id);
      globalState.items.set(id, item);
    });
    return true;
  }
  return false;
}

function deleteCategory(categoryId: string) {
  if (globalState.categories) {
    const index = globalState.categories.toArray().findIndex((cat) => cat.id === categoryId);
    if (index !== -1) {
      globalState.categories.delete(index, 1);
    }

    return true;
  }
  return false;
}

function storeCategory(category: Category) {
  if (globalState.categories) {
    const index = globalState.categories.toArray().findIndex((cat) => cat.id === category.id);

    if (index !== -1) {
      globalState.categories.insert(index, [category]);
      globalState.categories.delete(index + 1, 1);
    } else {
      globalState.categories.insert(0, [category]);
    }

    return true;
  }
  return false;
}

function storeDump(dump: DataDump) {
  if (globalState.doc) {
    // data.categories.delete
    // await db.categories.clear();
    // await db.categories.bulkAdd(
    // 	dump.categories.map((cat) => {
    // 		cat.lastUpdatedBy = clientId;
    // 		return cat;
    // 	}),
    // );
    // await db.items.clear();
    // await db.items.bulkAdd(
    // 	Object.values(dump.items).map((item) => {
    // 		item.lastUpdatedBy = clientId;
    // 		return item;
    // 	}),
    // );
    // await db.lastUpdatedBy.put({ id: 1, clientId: clientId });
    return true;
  }
  return false;
}

function getUuid() {
  return window.crypto.randomUUID();
}

function selectInput(id: string) {
  window.requestAnimationFrame(() => {
    const el = document.getElementById(id);
    if (el && ["INPUT", "TEXTAREA"].includes(el.tagName)) {
      (el as HTMLInputElement).select();
    }
  });
}

function setTheme(theme: string) {
  console.log("THEME!", theme);
  document.getElementsByTagName("html")[0].dataset.theme = theme;
}
