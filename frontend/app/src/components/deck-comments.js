import _ from 'lodash'
import React, { Component } from 'react'
import {
  Grid, Comment
} from 'semantic-ui-react'
// import { EditorState, convertFromRaw, RichUtils } from 'draft-js'
//import Editor from 'draft-js-plugins-editor'
// import createToolbarPlugin from 'draft-js-static-toolbar-plugin'
// import {
//   ItalicButton,
//   BoldButton,
//   UnderlineButton,
//   CodeButton,
//   HeadlineOneButton,
//   HeadlineTwoButton,
//   HeadlineThreeButton,
//   UnorderedListButton,
//   OrderedListButton,
//   BlockquoteButton,
//   CodeBlockButton,
// } from 'draft-js-buttons'

// import createChinesePlugin from './chinese-plugin.js'
// import alignContent from './align.js'

class DeckComments extends Component {
  render = () => {
    return(
      <div>
        <Grid celled="internally">
          <Grid.Row>
            <Grid.Column width={10}>
              <Comment.Group>
                <Comment>
                  <Comment.Avatar>
                    Avatar
                  </Comment.Avatar>
                  <Comment.Content>
                    <Comment.Author>
                      Author
                    </Comment.Author>
                    <Comment.Metadata>
                      Meta
                    </Comment.Metadata>
                    <Comment.Text>

                    </Comment.Text>
                    <Comment.Actions>
                      <Comment.Action>Send</Comment.Action>
                    </Comment.Actions>
                  </Comment.Content>
                </Comment>
                <Comment>
                  <Comment.Avatar>
                    Avatar
                  </Comment.Avatar>
                  <Comment.Content>
                    <Comment.Author>
                      Author
                    </Comment.Author>
                    <Comment.Metadata>
                      Meta, Help wanted
                    </Comment.Metadata>
                    <Comment.Text>
                      Text
                    </Comment.Text>
                    <Comment.Actions>
                      <Comment.Action>Reply</Comment.Action>
                      <Comment.Action>Edit</Comment.Action>
                    </Comment.Actions>
                  </Comment.Content>
                </Comment>
              </Comment.Group>
            </Grid.Column>
            <Grid.Column width={6}>
              <Comment.Group>
                <Comment>
                  <Comment.Avatar>
                    Avatar
                  </Comment.Avatar>
                  <Comment.Content>
                    <Comment.Author>
                      Author
                    </Comment.Author>
                    <Comment.Metadata>
                      Meta
                    </Comment.Metadata>
                    <Comment.Text>
                      Text
                    </Comment.Text>
                    <Comment.Actions>
                      <Comment.Action>Reply</Comment.Action>
                      <Comment.Action>Edit</Comment.Action>
                    </Comment.Actions>
                  </Comment.Content>
                </Comment>
                <Comment>
                  <Comment.Avatar>
                    Avatar
                  </Comment.Avatar>
                  <Comment.Content>
                    <Comment.Author>
                      Author
                    </Comment.Author>
                    <Comment.Metadata>
                      Meta
                    </Comment.Metadata>
                    <Comment.Text>
                      Text
                    </Comment.Text>
                  </Comment.Content>
                </Comment>
              </Comment.Group>
            </Grid.Column>
          </Grid.Row>
        </Grid>
      </div>
    );
  }
}

export default DeckComments;
