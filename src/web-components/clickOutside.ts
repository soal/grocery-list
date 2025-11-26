export class ClickOutside extends HTMLElement {
	onMouseDown: (evt: MouseEvent) => void;
	connectedCallback() {
		this.onMouseDown = (evt) => {
			const popups = document.querySelectorAll(".with-click-outside");

			const insiderIds = Array.from(popups.values())
				.filter((node) => node.contains(evt.target as Node))
				.map((node) => node?.id);

			if (insiderIds.length) {
				const event = new CustomEvent("clickInPopups", {
					detail: insiderIds,
				});
				this.dispatchEvent(event);
			} else {
				const event = new CustomEvent("clickoutside");
				this.dispatchEvent(event);
			}
		};

		window.addEventListener("mousedown", this.onMouseDown);
	}

	disconnectedCallback() {
		window.removeEventListener("mousedown", this.onMouseDown);
	}
}

