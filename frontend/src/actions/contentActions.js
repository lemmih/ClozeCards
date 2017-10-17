
export const RECEIVE_CONTENT = "RECEIVE_CONTENT";

//
// shouldFetchContent(contentId):bool
// fetchContentIfNeeded(contentId)
// fetchContent(contentId)
// requestContent(contentId)
//    Mark content as being fetched
// receiveContent(contentId, content)
//

export function receiveContent(contentId, content) {
  return {
    type: RECEIVE_CONTENT,
    payload: {
      contentId,
      content,
    }
  }
}
