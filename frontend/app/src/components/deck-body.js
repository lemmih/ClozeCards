import _ from "lodash";
import React, { Component } from "react";
import {
  Sticky,
  Grid,
  Icon,
  Button,
  Dropdown,
  Loader
} from "semantic-ui-react";
import {
  EditorState,
  convertToRaw,
  convertFromRaw,
  ContentState
} from "draft-js";
import Editor from "draft-js-plugins-editor";
import { connect } from "react-redux";
import "draft-js-static-toolbar-plugin/lib/plugin.css";
import uuid from "uuid/v4";

import {
  fetchNotes,
  receiveNotes,
  fetchContent,
  receiveContent
} from "../actions";
import createChinesePlugin from "../plugins/chinese-plugin";
import createResizePlugin from "../plugins/resize";
import alignContent from "../align";
import mkToolbar from "./toolbar";
import Loading from "./loading";

import backend from "../backend";
// import { getUser } from "../common";

class DeckBody extends Component {
  constructor(props) {
    super(props);
    if (props.deckContent)
      this.state = {
        deckEditor: EditorState.createWithContent(
          convertFromRaw(props.deckContent)
        ),
        // deckEditor: EditorState.createEmpty(),
        annotationEditor: EditorState.createEmpty()
      };
    else
      this.state = {
        deckEditor: EditorState.createEmpty(),
        annotationEditor: EditorState.createEmpty()
      };
    this.deckEditorFocus = () => this.deckEditor.focus();
    this.annotationEditorFocus = () => {
      this.annotationEditor.focus();
    };
    this.deckEditorPlugins = [
      createChinesePlugin(),
      createResizePlugin(this.alignText)
    ];
    this.annotationEditorPlugins = [
      createChinesePlugin(),
      createResizePlugin(this.alignText)
    ];
  }
  componentDidUpdate() {
    this.alignText();
  }
  handleSave = () => {
    const { editDeck, onSave } = this.props;
    const editor = editDeck
      ? this.state.deckEditor
      : this.state.annotationEditor;
    onSave(convertToRaw(editor.getCurrentContent()));
  };
  initialize = () => {
    if (this.props.deckContent)
      this.setState({
        deckEditor: EditorState.createWithContent(
          convertFromRaw(this.props.deckContent)
        ),
        annotationEditor: EditorState.createEmpty()
      });
    else
      this.setState({
        deckEditor: EditorState.createEmpty(),
        annotationEditor: EditorState.createEmpty()
      });
  };

  editorStyles = {
    minHeight: "10em",
    backgroundColor: "white"
  };
  handleRef = ref => this.setState({ ref });

  alignText = _.throttle(() => {
    const { showAnnotations } = this.props;
    if (showAnnotations)
      if (this.deckEditor && this.annotationEditor)
        alignContent(this.deckEditor.editor, this.annotationEditor.editor);
  }, 250);

  handleDeckEditorChange = deckEditor => this.setState({ deckEditor });
  handleAnnotationEditorChange = annotationEditor =>
    this.setState({ annotationEditor });

  handleSelectionChange = (e, { value }) => {
    this.props.onSelectionChange(value);
  };

  getAvailableNotes = () => {
    const userId = JSON.parse(localStorage.getItem("user")).id;
    return [{ value: userId, text: "Personal Notes" }];
  };

  render = () => {
    const userId = JSON.parse(localStorage.getItem("user")).id;
    const { editNotes, selection } = this.props;
    const showAnnotations = !!this.props.showAnnotations;
    const mayEditNotes = selection === userId;
    const editDeck = this.props.editDeck;
    const anyEditable = editDeck || (showAnnotations && editNotes);
    const Toolbar = editDeck
      ? mkToolbar(this.state.deckEditor, this.handleDeckEditorChange)
      : mkToolbar(
          this.state.annotationEditor,
          this.handleAnnotationEditorChange
        );

    return (
      <Loading active>
        <div>
          {showAnnotations &&
            !editNotes && (
              <Grid>
                <Grid.Row>
                  <Grid.Column width={10} />
                  <Grid.Column width={6}>
                    <Dropdown
                      placeholder="Viewing notes for: you"
                      search
                      value={selection}
                      selection
                      selectOnNavigation={false}
                      options={this.getAvailableNotes()}
                      onChange={this.handleSelectionChange}
                    />
                    {mayEditNotes ? (
                      <a onClick={this.props.onEditNotes}>
                        <Icon name="write" size="big" />
                      </a>
                    ) : (
                      <Icon name="write" size="big" />
                    )}
                  </Grid.Column>
                </Grid.Row>
              </Grid>
            )}
          <div ref={this.handleRef}>
            {anyEditable && (
              <Sticky context={this.state.ref} offset={20} className="sticky">
                <div className="toolbar">
                  <a onClick={this.handleSave}>
                    <Icon size="big" name="save" />
                  </a>
                  <Button.Group
                    compact
                    basic
                    size="big"
                    onMouseDown={event => event.preventDefault()}
                  >
                    <Toolbar.Inline type="ITALIC" icon="italic" />
                    <Toolbar.Inline type="BOLD" icon="bold" />
                    <Toolbar.Inline type="UNDERLINE" icon="underline" />
                    <Toolbar.Inline type="CODE" icon="code" />
                    <Toolbar.Inline type="HIDDEN" icon="hide" />
                  </Button.Group>
                  {" - "}
                  <Button.Group
                    compact
                    basic
                    size="big"
                    onMouseDown={event => event.preventDefault()}
                  >
                    <Toolbar.Block blockType="code-block" icon="terminal" />
                    <Toolbar.Block blockType="header-one" icon="header" />
                    <Toolbar.Block blockType="header-two">
                      <Icon size="small" name="header" />
                    </Toolbar.Block>
                    <Toolbar.Block blockType="header-three">
                      <Icon size="tiny" name="header" />
                    </Toolbar.Block>
                    <Toolbar.Block
                      blockType="unordered-list-item"
                      icon="unordered list"
                    />
                    <Toolbar.Block
                      blockType="ordered-list-item"
                      icon="ordered list"
                    />
                    <Toolbar.Block blockType="blockquote" icon="quote left" />
                  </Button.Group>
                  <a onClick={this.props.onDiscard}>
                    <Icon size="big" name="trash" />
                  </a>
                </div>
              </Sticky>
            )}

            <div className="editor">
              <Grid padded={false} celled="internally">
                <Grid.Row>
                  <Grid.Column
                    width={showAnnotations ? 10 : 16}
                    onClick={this.deckEditorFocus}
                  >
                    <Editor
                      editorState={this.state.deckEditor}
                      onChange={this.handleDeckEditorChange}
                      plugins={this.deckEditorPlugins}
                      placeholder="Deck content..."
                      readOnly={!editDeck}
                      ref={ref => (this.deckEditor = ref)}
                    />
                  </Grid.Column>
                  {showAnnotations && (
                    <Grid.Column width={6} onClick={this.annotationEditorFocus}>
                      <Notes
                        userId={selection}
                        deckId={this.props.deck.id}
                        editorState={this.state.annotationEditor}
                        onChange={this.handleAnnotationEditorChange}
                        plugins={this.annotationEditorPlugins}
                        placeholder={
                          editNotes ? "Write personal notes here." : ""
                        }
                        readOnly={!editNotes}
                        setRef={ref => (this.annotationEditor = ref)}
                      />
                    </Grid.Column>
                  )}
                </Grid.Row>
              </Grid>
            </div>
          </div>
        </div>
      </Loading>
    );
  };
}

// userId
// deckId
// noteKey
// contentId
// content
function toNotesProps(store, ownProps) {
  const { userId, deckId } = ownProps;
  const key = userId + "-" + deckId;
  const contentId = store.notes.get(key);
  return {
    noteKey: key,
    contentId: contentId,
    content: store.content.get(contentId)
  };
}
const Notes = connect(toNotesProps)(
  class extends Component {
    constructor(props) {
      super(props);
      this.state = {
        contentId: undefined
      };
    }

    componentDidMount() {
      this.loadNotes();
      this.updState();
    }
    componentDidUpdate() {
      this.updState();
    }
    updState = () => {
      // If selection changes, fetch
      this.loadNotes();
      if (
        this.state.contentId !== this.props.contentId &&
        _.isPlainObject(this.props.content)
      ) {
        this.setState({ contentId: this.props.contentId });
        this.props.onChange(
          EditorState.createWithContent(convertFromRaw(this.props.content))
        );
      } else if (_.isNull(this.props.contentId)) {
        const { dispatch, userId, deckId } = this.props;
        const contentId = uuid();
        const emptyContent = convertToRaw(
          ContentState.createFromText("Notes go here")
        );
        dispatch(receiveContent(contentId, emptyContent));
        dispatch(receiveNotes(userId, deckId, contentId));
      }
    };
    loadNotes = () => {
      const { userId, deckId, contentId, content } = this.props;
      if (_.isUndefined(contentId)) {
        backend.relay(fetchNotes(userId, deckId));
      } else if (
        _.isString(contentId) &&
        contentId !== "fetching" &&
        _.isUndefined(content)
      ) {
        backend.relay(fetchContent(contentId));
      }
    };

    render = () => {
      const {
        editorState,
        onChange,
        plugins,
        placeholder,
        readOnly,
        setRef,
        content
      } = this.props;
      if (!_.isPlainObject(content))
        return (
          <div style={{ height: "5em", position: "relative" }}>
            <Loader active />
          </div>
        );
      return (
        <Editor
          editorState={editorState}
          onChange={onChange}
          plugins={plugins}
          placeholder={placeholder}
          readOnly={readOnly}
          ref={setRef}
        />
      );
    };
  }
);

export default DeckBody;
