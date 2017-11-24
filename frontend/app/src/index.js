// @flow
import _ from "lodash";
import "semantic-ui-css/semantic.min.css";

import React from "react";
import ReactDOM from "react-dom";
import { Provider } from "react-redux";
import store from "./Store";
import Layout from "./components/layout";
import backend from "./backend";
import "./index.css";

backend.connect();

const rootElement = document.getElementById("root");
const app = (
  <Provider store={store}>
    <Layout />
  </Provider>
);
if(rootElement) {
if (rootElement.hasChildNodes()) {
  // render the app on a fake root to fetch any required data.
  const fakeRoot = document.createElement("div");
  ReactDOM.render(app, fakeRoot);
  // after a quater second, hydrate the app using the fetched data.
  // $FlowFixMe
  _.delay(ReactDOM.hydrate, 250, app, rootElement);
} else {
  ReactDOM.render(app, rootElement);
}
} else {
  console.log('Root element not found!');
}
