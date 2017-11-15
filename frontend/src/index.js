import 'semantic-ui-css/semantic.min.css'

import React from 'react'
import ReactDOM from 'react-dom'
import { Provider } from 'react-redux'
// import App from './App'
import store from './Store.js'
import Layout from './components/layout'
import backend from './backend'
import './index.css'
import { setTimeout } from 'timers';

backend.connect();

const rootElement = document.getElementById('root');
if(rootElement.hasChildNodes()) {
  ReactDOM.hydrate(
    <Provider store={store}>
      <Layout />
    </Provider>,
    rootElement);
} else {
  ReactDOM.render(
    <Provider store={store}>
      <Layout />
    </Provider>,
    rootElement
  );
}
