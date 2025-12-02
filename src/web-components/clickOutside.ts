export class ClickOutside extends HTMLElement {
	connectedCallback() {
		this.onMouseDown = this.onMouseDown.bind(this);

		window.addEventListener("mousedown", this.onMouseDown);
	}

	onMouseDown(evt: MouseEvent) {
		const popups = document.querySelectorAll(".with-click-outside");

		const insiderIds = Array.from(popups)
			.filter((node: Node) => node.contains(evt.target as Node))
			.map((node: Element) => node?.id);

		if (insiderIds.length) {
			const event = new CustomEvent("clickInPopups", {
				detail: insiderIds,
			});
			this.dispatchEvent(event);
		} else {
			const event = new CustomEvent("clickoutside");
			this.dispatchEvent(event);
		}
	}

	disconnectedCallback() {
		window.removeEventListener("mousedown", this.onMouseDown);
	}
}
