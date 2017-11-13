export const RECEIVE_CONTENT = "RECEIVE_CONTENT";
export const FETCH_CONTENT = "FETCH_CONTENT";

//
// shouldFetchContent(contentId):bool
// fetchContentIfNeeded(contentId)
// fetchContent(contentId)
// requestContent(contentId)
//    Mark content as being fetched
// receiveContent(contentId, content)
//

export function receiveContent(id, content) {
  return {
    type: RECEIVE_CONTENT,
    payload: {
      id,
      content,
    }
  }
}
export function fetchContent(id) {
  return {
    type: FETCH_CONTENT,
    payload: id
  }
}
