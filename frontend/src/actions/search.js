export const FETCH_SEARCH_RESULTS = "FETCH_SEARCH_RESULTS";
export const RECEIVE_SEARCH_RESULTS = "RECEIVE_SEARCH_RESULTS";

export function fetchSearchResults(query, order, offset) {
  return {
    type: FETCH_SEARCH_RESULTS,
    payload: {
      query,
      order,
      offset
    }
  };
}
