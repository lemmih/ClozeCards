import { RECEIVE_CONTENT, FETCH_CONTENT } from '../actions/content.js'
import { Map } from 'immutable'

export default function contentReducer(state=Map(), action) {
  switch (action.type) {
    case RECEIVE_CONTENT:
      return state.set(action.payload.id, action.payload.content);
    case FETCH_CONTENT:
      return state.set(action.payload, 'fetching');
    default:
      return state;
  }
}
