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
if (!rootElement) throw new Error("Root element not found!");

const app = (
  <Provider store={store}>
    <Layout />
  </Provider>
);
if (rootElement.hasChildNodes()) {
  // $FlowFixMe
  ReactDOM.hydrate(app, rootElement);
} else {
  ReactDOM.render(app, rootElement);
}
