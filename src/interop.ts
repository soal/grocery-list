import Dexie, { type EntityTable } from "dexie";
import * as TaskPort from "elm-taskport";

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

let db: Dexie | null = null;

async function initDb({
	name,
	version,
}: {
	name: string;
	version: number;
}): Promise<boolean | Error> {
	try {
		console.log("INIT DB IN jS", name, version);
		db = new Dexie(name) as Dexie & {
			settings: EntityTable<AppSettings, "id">;
			items: EntityTable<Item, "id">;
			categories: EntityTable<Category, "id">;
		};

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
		console.log("Query!");
	}
	return {
		categories: [],
		items: {},
	};
}

TaskPort.register("initDb", initDb);
TaskPort.register("queryAllCatsAndItems", queryAllCatsAndItems);

export const flags = ({ env }) => ({
	settings: {
		theme: "auto",
	},
});

export const onReady = ({ app, env }) => {
	console.log("APP READY", app);
};
