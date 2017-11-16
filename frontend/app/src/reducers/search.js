import _ from 'lodash'
import {FETCH_SEARCH_RESULTS, RECEIVE_SEARCH_RESULTS} from '../actions/search'

const defaultState = {
  query: '',
  order: 'ByLikes',
  status: 'partial',
  fetched: [],
};

export default (state=defaultState, action) => {
  switch (action.type) {
    case FETCH_SEARCH_RESULTS:
      return Object.assign({}, state, {
        status: 'fetching',
        order: action.payload.order,
        query: action.payload.query,
        fetched: _.take(state.fetched,action.payload.offset),
      });
    case RECEIVE_SEARCH_RESULTS:
      return Object.assign({}, state, {
        status: action.payload.length < 10 ? 'completed' : 'partial',
        fetched: state.fetched.concat(action.payload)
      });
    default:
      return state;
  }
}
