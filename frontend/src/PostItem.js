import _ from 'lodash'
import React, { Component } from 'react'
import {
  Input, Item, Label, Icon, Segment, Popup,
  Dropdown, Grid
} from 'semantic-ui-react'
import { Link } from 'react-router-dom'

const opt = val => {return {key: val, text: val, value: val}};


// editable
// mayEdit
class PostItem extends Component {

  constructor(props) {
    super(props);
    this.state = { options: [
                opt('Audio'),
                opt('Comprehensive'),
                opt('Exam'),
                opt('Audio Book'),
                opt('Free'),opt('Non-Free'),
                opt('Mandarin Companion'),
                opt('Graded Reader'),
                opt('Slow Chinese')],
              title: (props.post && props.post.title) || '',
              currentValues: (props.post && props.post.tags) || [],
            }
  }

  handleAddition = (e, { value }) => {
    this.setState({
      options: [{ text: value, value}, ...this.state.options]
    });
  }
  handleChange = (e, { value }) => this.setState({ currentValues: value })
  handleChangeTitle = (e, { value}) => this.setState({ title: value })
  handleSave = () => {
    this.props.onSave({...this.props.post,
      title: this.state.title.trim() || 'Untitled',
      tags: this.state.currentValues,
      type: 'word-list',
    });
  }

  render = () => {
    const {mayEdit, editable, post} = this.props;

    const studyBase =
      <Popup content="Study" position="top center" trigger={<Icon name="student" size="big"/>}/>;
    const studyActive = <Link to="/">{studyBase}</Link>;

    const notesBase =
      <Popup content="Notes" position="top center" trigger={<Icon name="edit" size="big"/>}/>;
    const notesActive = <Link to="/">{notesBase}</Link>

    const saveBase =
      <Popup content="Save" position="top center" trigger={<Icon name="save" size="big"/>}/>;
    const saveActive =
      <a onClick={this.handleSave}>{saveBase}</a>

    const editBase = <Popup content="Edit post" position="top center" trigger={<Icon name="write" size="big"/>}/>;
    const editActive = <a onClick={this.props.onEdit}>{editBase}</a>;

    const removeBase = <Popup content="Remove post" position="top center" trigger={<Icon style={{marginLeft: "1em"}} name="remove" size="big"/>}/>;
    const removeActive = <a>{removeBase}</a>;

    return(
      <Item>
          <Item.Image className="ui">
            <Segment style={{background: 'cornsilk'}}>
              <Label style={{textAlign: 'center'}} attached='bottom'>Word List</Label>
              <Icon name="file word outline" size="huge" style={{display: 'block', marginLeft: 'auto', marginRight: 'auto'}}/>
            </Segment>
          </Item.Image>

          <Item.Content verticalAlign='middle'>
            { editable &&
              <Item.Header style={{display: "flex"}}
                           as={Input}
                           onChange={this.handleChangeTitle}
                           value={this.state.title}
                           transparent={true}
                           fluid={true}
                           placeholder="Title..."></Item.Header> }
            { !editable &&
              <Item.Header><Link to={'/posts/'+post.slugs[0]}>{this.state.title}</Link></Item.Header> }
            <Item.Meta>
              { editable && <Dropdown
                options={this.state.options}
                placeholder="Add tags"
                search selection fluid allowAdditions multiple
                value={this.state.currentValues}
                onAddItem={this.handleAddition}
                onChange={this.handleChange} /> }
              { !editable && <div>{post.tags.map(tag => <span key={tag}>{tag}</span>)}</div>}
            </Item.Meta>
            <Item.Extra>
              <Grid columns={3}>
                <Grid.Row>
                  <Grid.Column>
                    { editable
                        ? studyBase
                        : studyActive }
                    { editable
                        ? notesBase
                        : notesActive }
                  </Grid.Column>

                  { mayEdit
                    ? <Grid.Column textAlign="center">
                        { editable
                          ? saveActive
                          : saveBase }
                        { editable
                          ? editBase
                          : editActive }
                        { editable
                          ? removeBase
                          : removeActive }
                      </Grid.Column>
                    : <Grid.Column textAlign="center"/> }

                  <Grid.Column textAlign="right">
                    { !editable &&<a>
                      <span style={{position: "relative", paddingLeft: '1em'}}>
                        <Icon name="empty heart" size="big"/>
                        <Label circular color="grey" floating>{post.nLikes}</Label>
                      </span>
                    </a> }
                    { !editable && <a><span style={{position: "relative", paddingLeft: '1em'}}>
                      <Icon name="comments" size="big"/>
                      <Label circular color="grey" floating>{post.nComments}</Label>
                    </span></a> }

                    { editable &&
                      <span style={{position: "relative", paddingLeft: '1em'}}>
                        <Icon name="empty heart" size="big"/>
                      </span> }
                    { editable && <span style={{position: "relative", paddingLeft: '1em'}}>
                      <Icon name="comments" size="big"/>
                    </span>}
                  </Grid.Column>
                </Grid.Row>
              </Grid>
            </Item.Extra>
          </Item.Content>

        </Item>
    );
  }
}

export default PostItem;
