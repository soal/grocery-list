import * as TaskPort from "elm-taskport/dist/taskport.min.js";
import { ClickOutside } from "./web-components/clickOutside.js";
import * as Y from "yjs";
import { IndexeddbPersistence } from "y-indexeddb";
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

type SyncState = "none" | "offline" | "syncing" | "synced" | { error: string };

type AppSettings = {
	theme: "auto" | "light" | "dark";
	sync: { room: string; url: string } | null;
	syncState: SyncState;
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
	room?: string;
	doc?: Y.Doc;
	items?: Y.Map<Item>;
	categories?: Y.Array<Category>;
	provider?: HocuspocusProvider;
	db?: IndexeddbPersistence;
	app?: ElmApp;
};

window.customElements.define("on-click-outside", ClickOutside);

TaskPort.install({ logCallErrors: true, logInteropErrors: true });

const globalState: GlobalState = {};

async function initDb({
	name,
	version,
}: {
	name: string;
	version: number;
}): Promise<AppSettings | Error> {
	return new Promise((resolve, reject) => {
		try {
			globalState.doc = new Y.Doc();

			globalState.categories = globalState.doc.getArray("categories");
			globalState.items = globalState.doc.getMap("items");

			globalState.db = new IndexeddbPersistence(name, globalState.doc);
			globalState.db.set("version", version);

			globalState.db.once("synced", async () => {
				const appSettings = await getSettings(globalState.db);
				if (appSettings.sync?.room && appSettings.sync?.url) {
					startSync(
						appSettings.sync.room,
						appSettings.sync.url,
						globalState.doc,
						() => {
							if (globalState.app) {
								globalState.app.ports.syncState.send("syncing");
							}
						},
					);
					appSettings.syncState = "syncing";
				}

				resolve(appSettings);
			});
		} catch (error) {
			reject(error);
		}
	});
}

async function startSync(
	name: string,
	url: string,
	ydoc: Y.Doc,
	onConnect: () => void,
) {
	globalState.provider = new HocuspocusProvider({
		url: url,
		name: name,
		document: ydoc,
		onConnect,
		onSynced() {
			if (globalState.app) {
				globalState.app.ports.syncState.send("synced");
			}
		},
		onStatus(data) {
			if (data.status === "connecting") {
				if (globalState.app) {
					globalState.app.ports.syncState.send("syncing");
				}
			}
		},
		onDisconnect(data) {
			if (data.event.code === 1000) {
				if (globalState.app) {
					globalState.app.ports.syncState.send("offline");
				}
			} else {
				globalState.app.ports.syncState.send({
					error: `Connection failed.
									 Code: ${data.event.code}, reason: ${data.event.reason}`,
				});
				// resolve("Connection failed");
			}
		},
	});
}

async function storeSyncSettings(
	room: string,
	url: string,
	dbHandler: IndexeddbPersistence,
) {
	globalState.room = room;
	await dbHandler.set("room", room);
	await dbHandler.set("url", url);
}

async function initSync({ room, url }: { room: string; url: string }) {
	if (globalState.db) {
		try {
			startSync(room, url, globalState.doc, () => {
				storeSyncSettings(room, url, globalState.db);
				if (globalState.app) {
					globalState.app.ports.syncState.send("syncing");
				}
			});
		} catch (err) {
			throw new Error(err);
		}
		return {
			room,
			url,
		};
	}
	return {
		room: "",
		url: "",
	};
}

async function getSettings(
	dbHandler: IndexeddbPersistence,
): Promise<AppSettings> {
	try {
		const theme = await dbHandler.get("theme");
		const room = await dbHandler.get("room");
		const url = await dbHandler.get("url");
		const version = await dbHandler.get("version");

		return {
			theme: theme ?? "auto",
			sync: room && url ? { room, url } : null,
			syncState: room && url ? "syncing" : "none",
			version: version ?? 1,
		};
	} catch (_) {
		return {
			theme: "auto",
			syncState: "none",
			sync: null,
			version: 1,
		};
	}
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
		const index = globalState.categories
			.toArray()
			.findIndex((cat) => cat.id === categoryId);
		if (index !== -1) {
			globalState.categories.delete(index, 1);
		}

		return true;
	}
	return false;
}

function storeCategory(category: Category) {
	if (globalState.categories) {
		const index = globalState.categories
			.toArray()
			.findIndex((cat) => cat.id === category.id);

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
		if (["INPUT", "TEXTAREA"].includes(el.tagName)) {
			(el as HTMLInputElement).select();
		}
	});
}

TaskPort.register("initDb", initDb);
TaskPort.register("storeDump", storeDump);
TaskPort.register("queryAllCatsAndItems", queryAllCatsAndItems);
TaskPort.register("getUuid", getUuid);

TaskPort.register("storeAllItems", storeAllItems);
TaskPort.register("storeItem", storeItem);
TaskPort.register("deleteItem", deleteItem);

TaskPort.register("storeCategory", storeCategory);
TaskPort.register("deleteCategory", deleteCategory);
TaskPort.register("selectInput", selectInput);
TaskPort.register("initSync", initSync);

export const flags = async () => {
	const settings = await initDb({
		name: "grocery",
		version: 1,
	});

	return settings;
};

export const onReady = ({ app }: { app: ElmApp }) => {
	globalState.app = app;
	// if (globalState.provider) {
	// 	globalState.provider.on("connect", () => {
	// 		app.ports.syncState.send("syncing");
	// 	});
	// 	globalState.provider.on("synced", () => {
	// 		app.ports.syncState.send("synced");
	// 	});
	// 	globalState.provider.on("disconnect", () => {
	// 		app.ports.syncState.send("offline");
	// 	});
	// }

	globalState.doc.on("update", (_, origin) => {
		if (origin == null) return;
		app.ports.incoming.send({
			categories: globalState.categories.toJSON(),
			items: globalState.items.toJSON(),
		});
	});
};
