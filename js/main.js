/* global Elm */

const root = document.createElement("div");
document.body.appendChild(root);

const app = Elm.Rio.Home.init({
    flags: {},
    node: root,
});

