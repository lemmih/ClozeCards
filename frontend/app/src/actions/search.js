// @flow
export const FETCH_SEARCH_RESULTS: "FETCH_SEARCH_RESULTS" =
  "FETCH_SEARCH_RESULTS";
export const RECEIVE_SEARCH_RESULTS = "RECEIVE_SEARCH_RESULTS";

export type FetchSearchResultsAction = {|
  type: typeof FETCH_SEARCH_RESULTS,
  payload: {|
    query: string,
    order: string,
    offset: number
  |}
|};

export function fetchSearchResults(
  query: string,
  order: string,
  offset: number
): FetchSearchResultsAction {
  return {
    type: FETCH_SEARCH_RESULTS,
    payload: {
      query,
      order,
      offset
    }
  };
}
