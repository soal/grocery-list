import * as TaskPort from "elm-taskport/dist/taskport.min.js";
import { ClickOutside } from "./web-components/clickOutside.js";
import * as Y from "yjs";
import { IndexeddbPersistence } from "y-indexeddb";
import { HocuspocusProvider } from "@hocuspocus/provider";

window.customElements.define("on-click-outside", ClickOutside);

TaskPort.install({ logCallErrors: true, logInteropErrors: true });

type SyncState = "none" | "syncReady" | "syncing" | "synced" | "error";

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

type Data = {
	room?: string;
	doc?: Y.Doc;
	items?: Y.Map<Item>;
	categories?: Y.Array<Category>;
	provider?: HocuspocusProvider;
	db?: IndexeddbPersistence;
};

const data: Data = {};

async function initDb({
	name,
	version,
}: {
	name: string;
	version: number;
}): Promise<AppSettings | Error> {
	return new Promise((resolve, reject) => {
		try {
			data.doc = new Y.Doc();

			data.categories = data.doc.getArray("categories");
			data.items = data.doc.getMap("items");

			data.db = new IndexeddbPersistence(name, data.doc);
			data.db.set("version", version);
			// const data.db.get("room")

			data.db.once("synced", async () => {
				console.log(data.db);
				const appSettings = await getSettings(data.db);
				if (appSettings.sync?.room && appSettings.sync?.url) {
					startSync(appSettings.sync.room, appSettings.sync.url, data.doc);
					appSettings.syncState = "syncReady";
				}

				resolve(appSettings);
			});

			// data.provider = new HocuspocusProvider({
			// 	url: "ws://127.0.0.1:4444",
			// 	name: name,
			// 	document: data.doc,
			// });
		} catch (error) {
			reject(error);
		}
	});
}

function startSync(name: string, url: string, ydoc: Y.Doc) {
	data.provider = new HocuspocusProvider({
		url: url,
		name: name,
		document: ydoc,
	});
}

async function storeSyncSettings(
	room: string,
	url: string,
	dbHandler: IndexeddbPersistence,
) {
	data.room = room;
	await dbHandler.set("room", room);
	await dbHandler.set("url", url);
}

async function initSync({ room, url }: { room: string; url: string }) {
	if (data.db) {
		storeSyncSettings(room, url, data.db);
		startSync(room, url, data.doc);
		console.log("HEY!", room, url, data);
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
			syncState: room && url ? "syncReady" : "none",
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
	if (data.doc) {
		result.categories = data.categories.toJSON();
		result.items = data.items.toJSON();
	}
	return result;
}

function storeItem(item: Item) {
	if (data.items) {
		data.items.set(item.id, item);
		return true;
	}
	return false;
}

function deleteItem(itemId: string) {
	if (data.items) {
		data.items.delete(itemId);
		return true;
	}
	return false;
}

function storeAllItems(items: Record<string, Item>) {
	if (data.items) {
		Object.entries(items).forEach(([id, item]) => {
			console.log(item.id);
			data.items.set(id, item);
		});
		return true;
	}
	return false;
}

function deleteCategory(categoryId: string) {
	if (data.categories) {
		const index = data.categories
			.toArray()
			.findIndex((cat) => cat.id === categoryId);
		if (index !== -1) {
			data.categories.delete(index, 1);
		}

		return true;
	}
	return false;
}

function storeCategory(category: Category) {
	if (data.categories) {
		const index = data.categories
			.toArray()
			.findIndex((cat) => cat.id === category.id);

		if (index !== -1) {
			data.categories.insert(index, [category]);
			data.categories.delete(index + 1, 1);
		} else {
			data.categories.insert(0, [category]);
		}

		return true;
	}
	return false;
}

function storeDump(dump: DataDump) {
	if (data.doc) {
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

export const onReady = ({ app }) => {
	data.doc.on("update", (_, origin) => {
		if (origin == null) return;
		app.ports.incoming.send({
			categories: data.categories.toJSON(),
			items: data.items.toJSON(),
		});
	});
};
