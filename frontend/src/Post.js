import React, { Component } from 'react'
import {Container, Input, Sticky, Item, Label, Icon, Segment} from 'semantic-ui-react'
import { EditorState, convertFromRaw, RichUtils } from 'draft-js'
import Editor from 'draft-js-plugins-editor'
import createToolbarPlugin from 'draft-js-static-toolbar-plugin'
import {
  ItalicButton,
  BoldButton,
  UnderlineButton,
  CodeButton,
  HeadlineOneButton,
  HeadlineTwoButton,
  HeadlineThreeButton,
  UnorderedListButton,
  OrderedListButton,
  BlockquoteButton,
  CodeBlockButton,
} from 'draft-js-buttons'
import './App.css';
import {ResizeSensor} from 'css-element-queries'
import createChinesePlugin from './chinese-plugin.js'
import alignContent from './align.js'
import 'draft-js-static-toolbar-plugin/lib/plugin.css'

const inlineToolbarPlugin = createToolbarPlugin({
  structure: [
    ItalicButton,
    BoldButton,
    UnderlineButton,
    CodeButton,
    HeadlineOneButton,
    HeadlineTwoButton,
    HeadlineThreeButton,
    UnorderedListButton,
    OrderedListButton,
    BlockquoteButton,
    CodeBlockButton,
  ]
});
const { Toolbar } = inlineToolbarPlugin;

class Post extends Component {
  constructor(props) {
    super(props);
    this.state = {
      postEditor: EditorState.createEmpty(),
      annotationEditor: EditorState.createEmpty()
    };
    this.postEditorFocus = () => this.postEditor.focus();
    this.annotationEditorFocus = () => this.anotationEditor.focus();
    this.postEditorPlugins = [ createChinesePlugin(), inlineToolbarPlugin ];
    this.annotationEditorPlugins = [ createChinesePlugin() ];
  }
  editorStyles = {
    minHeight: "10em",
    backgroundColor: "white"
  }
  handleRef = (ref) => this.setState({ref})

  render = () => {
    return(
      <div>
        <Container style={{paddingTop: "2em"}}>
          <Item.Group>
            <Item>
              <Item.Image>
                <Segment style={{background: 'cornsilk'}}>
                  <Label style={{textAlign: 'center'}} attached='bottom'>Word List</Label>
                  <Icon name="file word outline" size="huge" style={{display: 'block', marginLeft: 'auto', marginRight: 'auto'}}/>
                </Segment>
              </Item.Image>

              <Item.Content verticalAlign='middle'>
                <Item.Header style={{display: "flex"}} as={Input} transparent={true} fluid={true} placeholder="Title...">
                </Item.Header>
                <Item.Meta>
                  <Label color="teal">tag</Label>
                </Item.Meta>
                <Item.Extra>
                  <span style={{float: "right"}}>
                    <Icon name="save" size="large"/>
                    <Icon name="write" size="big"/>
                    <Icon style={{marginLeft: "1em"}} name="remove" size="big"/>
                  </span>
                  <Icon name="student" size="big"/>
                  <Icon name="edit" size="big"/>
                  <span style={{position: "relative", paddingLeft: '1em'}}>
                    <Icon name="empty heart" size="big"/>
                    <Label circular color="grey" floating>34</Label>
                  </span>
                  <span style={{position: "relative", paddingLeft: '1em'}}>
                    <Icon name="comments" size="big"/>
                    <Label circular color="grey" floating>520</Label>
                  </span>
                </Item.Extra>
              </Item.Content>

            </Item>
          </Item.Group>
          <div ref={this.handleRef} className="editor">
            <Sticky context={this.state.ref}>
              <Toolbar/>
            </Sticky>
            <div onClick={this.postEditorFocus} >
              <Editor
                editorState={this.state.postEditor}
                handleKeyCommand={this.handleKeyCommandA}
                onChange={(postEditor) => this.setState({postEditor})}
                plugins={this.postEditorPlugins}
                placeholder="Post content..."
                ref={(ref) => this.postEditor = ref}
              />
            </div>
          </div>
          <div>Post Comments</div>
          <div>Annotation Comments</div>
          <div>Similar posts</div>
        </Container>
      </div>
    );
  }
}

export default Post;
