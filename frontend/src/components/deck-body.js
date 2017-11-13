import _ from 'lodash'
import React, { Component } from 'react'
import {
  Sticky, Grid, Icon, Button, Dropdown
} from 'semantic-ui-react'
import { EditorState, convertToRaw, convertFromRaw } from 'draft-js'
import Editor from 'draft-js-plugins-editor'
import 'draft-js-static-toolbar-plugin/lib/plugin.css'

import createChinesePlugin from '../plugins/chinese-plugin.js'
import createResizePlugin from '../plugins/resize'
import alignContent from '../align.js'
import mkToolbar from './toolbar'
import Loading from './loading'

// editable
// !editable
// !editable && edit annotation
// !editable && view annotation
class DeckBody extends Component {
  constructor(props) {
    super(props);
    if( props.deckContent )
      this.state = {
        selection: 'self',
        editNotes: false,
        deckEditor: EditorState.createWithContent(convertFromRaw(props.deckContent)),
        annotationEditor: EditorState.createEmpty()
      };
    else
      this.state = {
        selection: 'self',
        deckEditor: EditorState.createEmpty(),
        annotationEditor: EditorState.createEmpty()
      };
    this.deckEditorFocus = () => this.deckEditor.focus();
    this.annotationEditorFocus = () => this.annotationEditor.focus();
    this.deckEditorPlugins = [ createChinesePlugin(), createResizePlugin(this.alignText) ];
    this.annotationEditorPlugins = [ createChinesePlugin(), createResizePlugin(this.alignText) ];
  }
  initialize = () => {
    if( this.props.deckContent )
      this.setState({
        editNotes: false,
        deckEditor: EditorState.createWithContent(convertFromRaw(this.props.deckContent)),
        annotationEditor: EditorState.createEmpty()
      });
    else
      this.setState({
        editNotes: false,
        deckEditor: EditorState.createEmpty(),
        annotationEditor: EditorState.createEmpty()
      });
  }

  editorStyles = {
    minHeight: "10em",
    backgroundColor: "white"
  }
  handleRef = (ref) => this.setState({ref})

  deckEditorRaw = () => {
    return convertToRaw(this.state.deckEditor.getCurrentContent())
  }


  alignText = _.throttle(() => {
    if( this.props.showAnnotations )
      alignContent(this.deckEditor.editor, this.annotationEditor.editor);}
  , 250)

  handleDeckEditorChange = deckEditor => this.setState({deckEditor});
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
    const editDeck = this.props.editable;
    const anyEditable = editDeck || (showAnnotations && editNotes);
    const Toolbar = editDeck
                    ? mkToolbar(this.state.deckEditor, this.handleDeckEditorChange)
                    : mkToolbar(this.state.annotationEditor, this.handleAnnotationEditorChange);

    const options =
            [{ flag: 'dk', value: 'self', text: 'Personal Notes'}
            ,{ flag: 'tw', value: 'ching', text: 'Ching\'s Notes'}
            ,{ flag: 'dk', value: 'david', text: 'David\'s Notes'}];

    return(
      <Loading active>
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
                <Grid.Column width={showAnnotations ? 10 : 16} onClick={this.deckEditorFocus}>
                  <Editor
                    editorState={this.state.deckEditor}
                    onChange={this.handleDeckEditorChange}
                    plugins={this.deckEditorPlugins}
                    placeholder="Deck content..."
                    readOnly={!editDeck}
                    ref={(ref) => this.deckEditor = ref}
                  />
                </Grid.Column>
                { showAnnotations &&
                  <Grid.Column width={6} onClick={this.annotationEditorFocus}>
                    <Editor
                      editorState={this.state.annotationEditor}
                      onChange={this.handleAnnotationEditorChange}
                      plugins={this.annotationEditorPlugins}
                      placeholder={editNotes ? "Write personal notes here." : ""}
                      readOnly={!editNotes}
                      ref={(ref) => this.annotationEditor = ref}
                    />
                  </Grid.Column> }
              </Grid.Row>
            </Grid>
          </div>
        </div>
      </div>
      </Loading>
    );
  }
}

export default DeckBody;
