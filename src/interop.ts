import Dexie, { type EntityTable, liveQuery } from "dexie";
import * as TaskPort from "elm-taskport/dist/taskport.min.js";
import { ClickOutside } from "./web-components/clickOutside.js";

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
	id: number;
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

type DB = Dexie & {
	settings: EntityTable<AppSettings, "id">;
	items: EntityTable<Item, "id">;
	categories: EntityTable<Category, "id">;
};

let db: DB | null = null;
const clientId = window.crypto.randomUUID();
let lastUpdate = Date.now();

async function initDb({
	name,
	version,
}: {
	name: string;
	version: number;
}): Promise<boolean | Error> {
	try {
		db = new Dexie(name) as DB;

		db.version(version).stores({
			settings: "id, theme",
			items:
				"id, name, quantity, comment, slug, symbol, state, created, updated, lastUpdatedBy",
			categories: "++id, name, items, state, created, updated, lastUpdatedBy ",
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
		item.lastUpdatedBy = clientId;
		await db.items.put(item);
		lastUpdate = Date.now();
		return true;
	}
	false;
}

async function deleteItem(itemId: string) {
	if (db) {
		await db.items.delete(itemId);
		return true;
	}
	return false;
}

async function storeAllItems(items: Item[]) {
	if (db) {
		await db.items.bulkPut(
			Object.values(items).map((item) => {
				item.lastUpdatedBy = clientId;
				return item;
			}),
		);
		lastUpdate = Date.now();
		return true;
	}
	false;
}

async function storeCategory(category: Category) {
	if (db) {
		category.lastUpdatedBy = clientId;
		await db.categories.put(category);
		lastUpdate = Date.now();
		return true;
	}
	return false;
}

async function storeDump(dump: DataDump) {
	if (db) {
		await db.categories.clear();
		await db.categories.bulkAdd(
			dump.categories.map((cat) => {
				cat.lastUpdatedBy = clientId;
				return cat;
			}),
		);
		await db.items.clear();
		await db.items.bulkAdd(
			Object.values(dump.items).map((item) => {
				item.lastUpdatedBy = clientId;
				return item;
			}),
		);
		lastUpdate = Date.now();
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
TaskPort.register("storeCategory", storeCategory);
TaskPort.register("deleteItem", deleteItem);

export const flags = () => ({
	settings: {
		theme: "auto",
	},
});

export const onReady = ({ app }) => {
	liveQuery(async () => ({
		items: await db.items.toArray(),
		categories: await db.categories.toArray(),
	})).subscribe({
		next: (result) => {
			const hasUpdated = [...result.items, ...result.categories].some(
				(el: Item | Category) =>
					el.lastUpdatedBy !== clientId && el.updated >= lastUpdate,
			);
			if (hasUpdated) {
				app.ports.incoming.send({
					categories: result.categories,
					items: result.items.reduce(
						(acc: Record<string, Item>, item: Item) => {
							acc[item.id] = item;
							return acc;
						},
						{},
					),
				});
			}
		},
		error: (error) => console.error(error),
	});
};
