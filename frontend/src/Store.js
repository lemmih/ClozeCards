import _ from 'lodash'
import { applyMiddleware, createStore } from "redux"
import {createLogger} from "redux-logger"
import thunk from "redux-thunk"
import promise from "redux-promise-middleware"
import { Map } from 'immutable'
import { SET_ACTIVE_USER } from './actions/user.js'


// import uuid from 'uuid/v4'

import rootReducer from './reducers'

const defaultState = {
  decks: Map(),
  decksBySlug: Map(),
  content: Map(),
  user: JSON.parse(localStorage.getItem('user')) || {},
};

const persist = store => next => action => {
  if( action.type === SET_ACTIVE_USER ) {
    console.log('Persisting login')
    const user = Object.assign({token: action.payload.token}, action.payload.user);
    localStorage.setItem('user', JSON.stringify(user));
  }
  return next(action);
}

const middleware = applyMiddleware(persist, promise(), thunk, createLogger());

export default createStore(rootReducer, defaultState, middleware)
