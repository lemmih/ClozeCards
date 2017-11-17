import _ from "lodash";
import 'semantic-ui-css/semantic.min.css'

import React from 'react'
import ReactDOM from 'react-dom'
import { Provider } from 'react-redux'
// import App from './App'
import store from './Store.js'
import Layout from './components/layout'
import backend from './backend'
import './index.css'

backend.connect();

const rootElement = document.getElementById('root');
const app = <Provider store={store}><Layout /></Provider>;
if(rootElement.hasChildNodes()) {
  // render the app on a fake root to fetch any required data.
  const fakeRoot = document.createElement("div");
  ReactDOM.render(app, fakeRoot);
  // after a quater second, hydrate the app using the fetched data.
  _.delay((a,b) => {
    console.log('Hydrating');
    ReactDOM.hydrate(a,b);
  }, 250, app, rootElement);
} else {
  ReactDOM.render(app, rootElement);
}
