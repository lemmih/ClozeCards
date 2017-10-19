import _ from 'lodash'
import React, { Component } from 'react'
import { withRouter, Redirect } from 'react-router-dom'
import { connect } from 'react-redux'
import {
  Container, Item,
} from 'semantic-ui-react'
import { Set } from 'immutable'
import uuid from 'uuid/v4'

import PostItem from './PostItem.js'
import PostBody from './PostBody.js'
import PostComments from './PostComments.js'

import {receivePost} from './actions/postActions'
import {receiveContent} from './actions/contentActions'

import mkSlug from './misc/slugs'

function toViewPostProps(store, ownProps) {
  const slug = ownProps.slug;
  const post = store.posts.find((post) => _.includes(post.slugs,slug));
  const content = post ? store.content.get(post.id) : null;
  return {
    takenSlugs: store.posts.map(p => post&&p.id===post.id ? Set() : Set(p.slugs)).toSet().flatten(),
    post: post,
    content: content
  };
}
export const ViewPost = connect(toViewPostProps)(class ViewPost extends Component {
  constructor(props) {
    super(props);
    const {study, notes} = props;
    this.state = { editable: false, study, notes };
    console.log('ViewPost Constructor', !!study, !!notes);
  }
  componentDidUpdate = (prevProps) => {
    // console.log('Did update', this.props.study, this.props.notes, this.state);
  }
  componentWillReceiveProps = (nextProps) => {
    console.log('Will receive', nextProps);
    if( nextProps.study === undefined && nextProps.notes === undefined)
      this.setState({study: false, notes: false});
    if(nextProps.study !== undefined)
      this.setState({study: nextProps.study});
    if(nextProps.notes !== undefined)
      this.setState({notes: nextProps.notes});
  }

  handleEdit = () => {
    this.setState({ editable: true });
  }
  handleSave = () => {
    const post = this.itemRef.getPost();
    const takenSlugs = this.props.takenSlugs;
    const slug = mkSlug(post.title, takenSlugs);
    const slugs = _.uniq([slug].concat(post.slugs));
    this.props.dispatch(receivePost(post.id, post.title, post.tags, post.types, slugs,0,0));
    this.props.dispatch(receiveContent(post.id, this.bodyRef.postEditorRaw()));
    this.setState({ editable: false });
    this.bodyRef.setState({editNotes: false});
    // this.props.history.push('/posts/'+slug);
  }
  handleDiscard = () => {
    this.itemRef.initialize();
    this.bodyRef.initialize();
    this.setState({ editable: false });
  }
  handleBodyRef = (ref) => this.bodyRef = ref;
  handlePostItemRef = (ref) => this.itemRef = ref;

  render = () => {
    if( !this.props.post )
      return <Redirect to="/"/>
    return(
      <div>
        <Container style={{paddingTop: "2em"}}>
          <Item.Group>
            <PostItem
              ref={this.handlePostItemRef}
              post={this.props.post}
              editable={this.state.editable}
              onEdit={this.handleEdit}
              onSave={this.handleSave}
              notesVisible={this.state.notes}
              mayEdit={true && !this.state.notes && !this.state.study}/>
          </Item.Group>

          <PostBody
            postContent={this.props.content}
            editable={this.state.editable}
            showAnnotations={this.state.notes}
            onSave={this.handleSave}
            onDiscard={this.handleDiscard}
            ref={this.handleBodyRef} />

          { false && <PostComments/>}
        </Container>
      </div>
    );
  }
})

function toNewPostProps(store) {
  return {
    posts: store.posts
  };
}

const NewPost = withRouter(connect(toNewPostProps)(class NewPost extends Component {
  onSave = (post) => {
    const takenSlugs = this.props.posts.map(post => Set(post.slugs)).toSet().flatten();
    const slug = mkSlug(post.title, takenSlugs);
    const postId = uuid();
    this.props.dispatch(receivePost(postId, post.title, post.tags, post.types, [slug],0,0));
    this.props.dispatch(receiveContent(postId, this.bodyRef.postEditorRaw()));
    this.props.history.push('/posts/'+slug);
  }
  handleBodyRef = (ref) => this.bodyRef = ref;

  render = () => {
    return(
      <div>
        <Container style={{paddingTop: "2em"}}>
          <Item.Group>
            <PostItem
              mayEdit={true}
              editable={true}
              onSave={this.onSave}
              />
          </Item.Group>

          <PostBody ref={this.handleBodyRef} editable={true} showAnnotations={false}/>
        </Container>
      </div>
    );
  }
}))

export { NewPost };
