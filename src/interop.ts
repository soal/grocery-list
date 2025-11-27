import Dexie, { type EntityTable } from "dexie";
import * as TaskPort from "elm-taskport";
import { ClickOutside } from "./web-components/clickOutside.js";

window.customElements.define("on-click-outside", ClickOutside);

TaskPort.install({ logCallErrors: true, logInteropErrors: true });

type AppSettings = {
	id: number;
	theme: "auto" | "light" | "dark";
};
type ItemState = "stuffed" | "required";

type Item = {
	id: number;
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
	id: number;
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

type DB = Dexie & {
	settings: EntityTable<AppSettings, "id">;
	items: EntityTable<Item, "id">;
	categories: EntityTable<Category, "id">;
};

let db: DB | null = null;

async function initDb({
	name,
	version,
}: {
	name: string;
	version: number;
}): Promise<boolean | Error> {
	try {
		console.log("INIT DB IN jS", name, version);
		db = new Dexie(name) as DB;

		db.version(version).stores({
			settings: "id, theme",
			items:
				"++id, name, quantity, comment, slug, symbol, state, created, updated",
			categories: "++id, name, items, state, created, updated",
		});
	} catch (error) {
		return error;
	}

	return true;
}

async function queryAllCatsAndItems() {
	if (db) {
		const items = await db.items.toArray().then((itemArray) =>
			itemArray.reduce((acc, item) => {
				acc[item.id] = item;
				return acc;
			}, {}),
		);
		console.log("!!!", items);
		return {
			categories: await db.categories.toArray(),
			items,
		};
	}
	return {
		categories: [],
		items: {},
	};
}

async function storeItem(item: Item) {
	if (db) {
		console.log("STORE ITEM", item);
		await db.items.put(item);
		return true;
	}
	false;
}

async function storeAllItems(items: Item[]) {
	if (db) {
		await db.items.bulkPut(Object.values(items));
		return true;
	}
	false;
}

async function storeDump(dump: DataDump) {
	if (db) {
		console.log("DUMP", dump);
		await db.categories.clear();
		await db.categories.bulkAdd(dump.categories);
		await db.items.clear();
		await db.items.bulkAdd(Object.values(dump.items));
		return true;
	}
	return false;
}

function getUuid() {
	return window.crypto.randomUUID();
}

TaskPort.register("initDb", initDb);
TaskPort.register("queryAllCatsAndItems", queryAllCatsAndItems);
TaskPort.register("storeItem", storeItem);
TaskPort.register("storeAllItems", storeAllItems);
TaskPort.register("storeDump", storeDump);
TaskPort.register("getUuid", getUuid);

export const flags = ({ env }) => ({
	settings: {
		theme: "auto",
	},
});

export const onReady = ({ app, env }) => {
	console.log("APP READY", app);
};
