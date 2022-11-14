import show_error from './show-error.mjs';

class ShuntingYardAppElement extends HTMLElement {
    constructor() {
        super();
        this.attachShadow({mode: 'open'});
        this.app_created = new Promise((resolve,reject) => {
            this.resolve_app_created = resolve;
        })
    }
    connectedCallback() {
        if(!this.app) {
            this.try_create();
        }
    }

    try_create() {
        const div = document.createElement('div');
        const link = document.createElement('link');
        link.setAttribute('rel','stylesheet');
        link.setAttribute('href','shunting-yard-animation/style.css');
        this.shadowRoot.appendChild(link);
        this.shadowRoot.appendChild(div);
        var app = this.app = Elm.ShuntingYardApp.init({ node: div, flags: {}})
    }
}

customElements.define('shunting-yard-app', ShuntingYardAppElement);
