import _ from 'lodash'
import React, { Component } from 'react'
import {
  Sticky, Grid
} from 'semantic-ui-react'
import { EditorState, convertToRaw, convertFromRaw } from 'draft-js'
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


// editable
// !editable
// !editable && edit annotation
// !editable && view annotation
class PostBody extends Component {
  constructor(props) {
    super(props);
    if( props.postContent )
      this.state = {
        postEditor: EditorState.createWithContent(convertFromRaw(props.postContent)),
        annotationEditor: EditorState.createEmpty()
      };
    else
      this.state = {
        postEditor: EditorState.createEmpty(),
        annotationEditor: EditorState.createEmpty()
      };
    this.postEditorFocus = () => this.postEditor.focus();
    this.annotationEditorFocus = () => this.annotationEditor.focus();
    this.postEditorPlugins = [ createChinesePlugin(), inlineToolbarPlugin ];
    this.annotationEditorPlugins = [ createChinesePlugin() ];
  }
  editorStyles = {
    minHeight: "10em",
    backgroundColor: "white"
  }
  handleRef = (ref) => this.setState({ref})

  postEditorRaw = () => {
    return convertToRaw(this.state.postEditor.getCurrentContent())
  }


  alignText = _.throttle(() => alignContent(this.postEditor.editor, this.annotationEditor.editor), 250)

  componentDidMount = () => {
    if( this.annotationEditor ) {
      this.alignText();
      const self = this;
      this.rightSensor = new ResizeSensor(this.postEditor.editor.refs.editor, () => {
        self.alignText();
      });
      this.leftSensor = new ResizeSensor(this.annotationEditor.editor.refs.editor, () => {
        self.alignText();
      });
    }
  }
  componentWillUnmount = () => {
    if( this.annotationEditor ) {
      this.rightSensor.detach();
      this.leftSensor.detach();
    }
  }

  render = () => {
    const showAnnotations = !!this.props.showAnnotations;
    const editAnnotations = false;
    const editPost = this.props.editable;
    return(
      <div>
        { showAnnotations && <Grid>
          <Grid.Row>
            <Grid.Column width={10}>
            </Grid.Column>
            <Grid.Column width={6}>
              Viewing notes for: You
            </Grid.Column>
          </Grid.Row>
        </Grid>}
        <div ref={this.handleRef}>
          { editPost && <Sticky context={this.state.ref} offset={20} className="sticky">
            <Toolbar style={{position: 'absolute'}}/>
          </Sticky> }

          <div className="editor">
            <Grid celled="internally">
              <Grid.Row>
                <Grid.Column width={showAnnotations ? 10 : 16} onClick={() => this.postEditorFocus()}>
                  <Editor
                    editorState={this.state.postEditor}
                    onChange={(postEditor) => this.setState({postEditor})}
                    plugins={this.postEditorPlugins}
                    placeholder="Post content..."
                    readOnly={!editPost}
                    ref={(ref) => this.postEditor = ref}
                  />
                </Grid.Column>
                { showAnnotations &&
                <Grid.Column width={6} onClick={() => this.annotationEditorFocus()}>
                  <Editor
                    editorState={this.state.annotationEditor}
                    onChange={(annotationEditor) => this.setState({annotationEditor})}
                    plugins={this.annotationEditorPlugins}
                    placeholder="Annotation content..."
                    ref={(ref) => this.annotationEditor = ref}
                  />
                </Grid.Column>}
              </Grid.Row>
            </Grid>
          </div>
        </div>
      </div>
    );
  }
}

export default PostBody;
