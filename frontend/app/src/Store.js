import _ from "lodash";
import { applyMiddleware, createStore } from "redux";
import { createLogger } from "redux-logger";
import thunk from "redux-thunk";
import promise from "redux-promise-middleware";
import { Map } from "immutable";
import { SET_ACTIVE_USER } from "./actions/user.js";

// import uuid from 'uuid/v4'

import rootReducer from "./reducers";

const defaultState = {
  decks: Map(),
  decksBySlug: Map(),
  content: Map(),
  user: JSON.parse(localStorage.getItem("user")) || {}
};

const persist = store => next => action => {
  if (action.type === SET_ACTIVE_USER) {
    const user = Object.assign(
      { token: action.payload.token },
      action.payload.user
    );
    localStorage.setItem("user", JSON.stringify(user));
  }
  return next(action);
};

const isReactSnap = navigator.userAgent === "ReactSnap";
var actions = [];
const action_log = store => next => action => {
  if (isReactSnap) actions.push(action);
  return next(action);
};

const middleware = applyMiddleware(
  action_log,
  persist,
  promise(),
  thunk,
  createLogger()
);

const preloadedState = window.__PRELOADED_STATE__;
const store = createStore(rootReducer, defaultState, middleware);

_.forEach(preloadedState, action => {
  if (action.type !== SET_ACTIVE_USER) store.dispatch(action);
});

window.snapSaveState = () => ({
  __PRELOADED_STATE__: actions
});

export default store;
