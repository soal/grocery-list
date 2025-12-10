import Dexie, { type EntityTable, liveQuery } from "dexie";
import * as TaskPort from "elm-taskport/dist/taskport.min.js";
import { ClickOutside } from "./web-components/clickOutside.js";
import * as Y from "yjs";
import { IndexeddbPersistence } from "y-indexeddb";
import { HocuspocusProvider } from "@hocuspocus/provider";

// import { WebrtcProvider } from "y-webrtc";

window.customElements.define("on-click-outside", ClickOutside);

TaskPort.install({ logCallErrors: true, logInteropErrors: true });

type AppSettings = {
	id: number;
	theme: "auto" | "light" | "dark";
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
	lastUpdatedBy?: typeof clientId;
};

type CollapsedState = "open" | "collapsed";

type Category = {
	id: string;
	name: string;
	items: number[];
	state: CollapsedState;
	created: number;
	updated: number;
	lastUpdatedBy?: typeof clientId;
};

type DataDump = {
	version: number;
	items: Record<string, Item>;
	categories: Category[];
};

let clientId: number | null = null;

type Data = {
	room?: string;
	doc?: Y.Doc;
	items?: Y.Map<Item>;
	categories?: Y.Array<Category>;
	provider?: HocuspocusProvider;
};

const data: Data = {};

async function initDb({
	name,
	version,
}: {
	name: string;
	version: number;
}): Promise<boolean | Error> {
	return new Promise((resolve, reject) => {
		try {
			data.room = name;
			data.doc = new Y.Doc();
			clientId = data.doc.clientID;

			data.categories = data.doc.getArray("categories");
			data.items = data.doc.getMap("items");

			const persistence = new IndexeddbPersistence(name, data.doc);
			persistence.once("synced", () => {
				console.log("INITIAL CONTENT LOADED");
				resolve(true);
			});

			data.provider = new HocuspocusProvider({
				url: "ws://127.0.0.1:4444",
				name: name,
				document: data.doc,
			});
		} catch (error) {
			reject(error);
		}
	});
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

export const flags = async () => {
	await initDb({
		name: "grocery-list-y",
		version: 1,
	});

	return {
		settings: {
			theme: "auto",
		},
	};
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
