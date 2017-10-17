import { RECEIVE_POST } from '../actions/postActions.js'
import { Map } from 'immutable'

export default function posts(state=Map(), action) {
  switch (action.type) {
    case RECEIVE_POST:
      return state.set(action.payload.id, action.payload);
    default:
      return state;
  }
}
