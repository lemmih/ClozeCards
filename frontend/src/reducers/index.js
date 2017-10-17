import { combineReducers } from 'redux'
import content from './content.js'
import posts from './posts.js'

export default combineReducers({
  posts,
  content
})
