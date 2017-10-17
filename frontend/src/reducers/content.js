import { RECEIVE_CONTENT } from '../actions/contentActions.js'
import { Map } from 'immutable'

export default function contentReducer(state=Map(), action) {
  switch (action.type) {
    case RECEIVE_CONTENT:
      return state.set(action.payload.contentId, action.payload.content);
    default:
      return state;
  }
}
