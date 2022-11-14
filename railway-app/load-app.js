import show_error from './show-error.mjs';

class RailwayAppElement extends HTMLElement {
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
        const segments_slot = this.querySelector('slot[name="segments"]');
        const trains_slot = this.querySelector('slot[name="trains"]');
        const viewBox = this.getAttribute('viewBox');
        const speed = this.getAttribute('speed');
        const show_controls = this.getAttribute('controls') != 'false';
        const show_curvature = this.getAttribute('curvature') == 'true';
        const rail_style = this.getAttribute('rails');

        this.shadowRoot.innerHTML = '';

        const link = document.createElement('link');
        link.setAttribute('rel','stylesheet');
        link.setAttribute('href','railway-app/railway-style.css');
        this.shadowRoot.appendChild(link);

        const flags = {
            segments: segments_slot?.textContent,
            trains: trains_slot?.textContent,
            viewBox,
            speed,
            show_controls,
            show_curvature,
            rail_style
        };
        const div = document.createElement('div');
        this.shadowRoot.appendChild(div);
        var app = this.app = Elm.RailwayApp.init({ node: div, flags: flags })

        const intersection_observer = new IntersectionObserver((events) => {
            for(let e of events) {
                console.log(e.isIntersecting);
                app.ports.visibilityChange.send(e.isIntersecting);
            }
        }, document.body);
        intersection_observer.observe(this);

        this.resolve_app_created(app);
    }
}

customElements.define('railway-app', RailwayAppElement);
