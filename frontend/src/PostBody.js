import _ from 'lodash'
import React, { Component } from 'react'
import {
  Sticky, Grid, Icon, Button, Dropdown
} from 'semantic-ui-react'
import { EditorState, convertToRaw, convertFromRaw } from 'draft-js'
import Editor from 'draft-js-plugins-editor'
import {ResizeSensor} from 'css-element-queries'
import createChinesePlugin from './chinese-plugin.js'
import alignContent from './align.js'
import 'draft-js-static-toolbar-plugin/lib/plugin.css'
import mkToolbar from './components/Toolbar'

// const staticToolbarPlugin = createToolbarPlugin({
//   structure: [
//     ItalicButton,
//     BoldButton,
//     UnderlineButton,
//     CodeButton,
//     HeadlineOneButton,
//     HeadlineTwoButton,
//     HeadlineThreeButton,
//     UnorderedListButton,
//     OrderedListButton,
//     BlockquoteButton,
//     CodeBlockButton,
//   ]
// });
// const { Toolbar } = staticToolbarPlugin;


// editable
// !editable
// !editable && edit annotation
// !editable && view annotation
class PostBody extends Component {
  constructor(props) {
    super(props);
    if( props.postContent )
      this.state = {
        selection: 'self',
        editNotes: false,
        postEditor: EditorState.createWithContent(convertFromRaw(props.postContent)),
        annotationEditor: EditorState.createEmpty()
      };
    else
      this.state = {
        selection: 'self',
        postEditor: EditorState.createEmpty(),
        annotationEditor: EditorState.createEmpty()
      };
    this.postEditorFocus = () => this.postEditor.focus();
    this.annotationEditorFocus = () => this.annotationEditor.focus();
    this.postEditorPlugins = [ createChinesePlugin() ];
    this.annotationEditorPlugins = [ createChinesePlugin() ];
  }
  initialize = () => {
    if( this.props.postContent )
      this.setState({
        editNotes: false,
        postEditor: EditorState.createWithContent(convertFromRaw(this.props.postContent)),
        annotationEditor: EditorState.createEmpty()
      });
    else
      this.setState({
        editNotes: false,
        postEditor: EditorState.createEmpty(),
        annotationEditor: EditorState.createEmpty()
      });
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

  handlePostEditorChange = postEditor => this.setState({postEditor});
  handleAnnotationEditorChange = annotationEditor => this.setState({annotationEditor});

  handleSelectionChange = (e, {value}) => {
    this.setState({selection: value});
  }

  handleEditNotes = () => {
    this.setState({editNotes: true});
  }

  render = () => {
    const { editNotes } = this.state;
    const showAnnotations = !!this.props.showAnnotations;
    const mayEditNotes = (this.state.selection === 'self' || !this.state.selection);
    const editPost = this.props.editable;
    const anyEditable = editPost || editNotes;
    const Toolbar = editPost
                    ? mkToolbar(this.state.postEditor, this.handlePostEditorChange)
                    : mkToolbar(this.state.annotationEditor, this.handleAnnotationEditorChange);

    const options =
            [{ flag: 'dk', value: 'self', text: 'Personal Notes'}
            ,{ flag: 'tw', value: 'ching', text: 'Ching\'s Notes'}
            ,{ flag: 'dk', value: 'david', text: 'David\'s Notes'}];

    return(
      <div>
        { showAnnotations && !editNotes && <Grid>
          <Grid.Row>
            <Grid.Column width={10}>
            </Grid.Column>
            <Grid.Column width={6}>
              <Dropdown
                placeholder="Viewing notes for: you"
                search
                value={this.state.selection}
                selection
                selectOnNavigation={false}
                options={options}
                onChange={this.handleSelectionChange}/>
              { mayEditNotes
              ? <a onClick={this.handleEditNotes}><Icon name="write" size="big"/></a>
              : <Icon name="write" size="big"/> }
            </Grid.Column>
          </Grid.Row>
        </Grid>}
        <div ref={this.handleRef}>
          { anyEditable && <Sticky context={this.state.ref} offset={20} className="sticky">
            <div className="toolbar">
              <a onClick={this.props.onSave}><Icon size="big" name="save"/></a>
              <Button.Group compact basic size="big" onMouseDown={event => event.preventDefault()}>
                <Toolbar.Inline type="ITALIC" icon="italic"/>
                <Toolbar.Inline type="BOLD" icon="bold"/>
                <Toolbar.Inline type="UNDERLINE" icon="underline"/>
                <Toolbar.Inline type="CODE" icon="code"/>
              </Button.Group>
              {' - '}
              <Button.Group compact basic size="big" onMouseDown={event => event.preventDefault()}>
                <Toolbar.Block blockType='code-block' icon="terminal"/>
                <Toolbar.Block blockType='header-one' icon="header"/>
                <Toolbar.Block blockType='header-two'>
                  <Icon size="small" name="header"/>
                </Toolbar.Block>
                <Toolbar.Block blockType='header-three'>
                  <Icon size="tiny" name="header"/>
                </Toolbar.Block>
                <Toolbar.Block blockType='unordered-list-item' icon="unordered list"/>
                <Toolbar.Block blockType='ordered-list-item' icon="ordered list"/>
                <Toolbar.Block blockType='blockquote' icon="quote left"/>
              </Button.Group>
              <a onClick={this.props.onDiscard}><Icon size="big" name="trash"/></a>
            </div>
          </Sticky> }

          <div className="editor">
            <Grid padded={false} celled="internally">
              <Grid.Row>
                <Grid.Column width={showAnnotations ? 10 : 16} onClick={() => this.postEditorFocus()}>
                  <Editor
                    editorState={this.state.postEditor}
                    onChange={this.handlePostEditorChange}
                    plugins={this.postEditorPlugins}
                    placeholder="Post content..."
                    readOnly={!editPost}
                    ref={(ref) => this.postEditor = ref}
                  />
                </Grid.Column>
                <Grid.Column style={{display: showAnnotations ? 'initial' : 'none'}} width={6} onClick={() => this.annotationEditorFocus()}>
                  <Editor
                    editorState={this.state.annotationEditor}
                    onChange={this.handleAnnotationEditorChange}
                    plugins={this.annotationEditorPlugins}
                    placeholder={editNotes ? "Write personal notes here." : ""}
                    readOnly={!editNotes}
                    ref={(ref) => this.annotationEditor = ref}
                  />
                </Grid.Column>
              </Grid.Row>
            </Grid>
          </div>
        </div>
      </div>
    );
  }
}

export default PostBody;
