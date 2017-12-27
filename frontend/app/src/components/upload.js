// @flow
import React, { PureComponent, Component } from "react";
import { Modal, Input } from "semantic-ui-react";

type UploadState = {
  open: boolean,
  uploading: boolean,
  progress: number,
  xhr: ?XMLHttpRequest
};
type UploadProps = {
  trigger: ((void) => void) => Component<*>,
  onSuccess: string => void
};

export default class Upload extends PureComponent<UploadProps, UploadState> {
  constructor(props: UploadProps) {
    super(props);
    this.state = {
      open: false,
      uploading: false,
      progress: 0,
      xhr: null
    };
  }
  onChange = (evt: SyntheticInputEvent<*>) => {
    const file = evt.target.files[0];
    const request = new XMLHttpRequest();
    const { onSuccess } = this.props;
    const self = this;
    request.open("POST", "/api/blob/upload");
    this.setState({ xhr: request, uploading: true });
    request.addEventListener("load", function() {
      if (this.status >= 200 && this.status < 400) {
        console.log("Success", this.responseText);
        onSuccess(this.responseText);
        self.setState({
          open: false,
          uploading: false,
          progress: 0,
          xhr: null
        });
      } else {
        console.log("Failed", this.status, this.responseText);
      }
    });
    request.upload.addEventListener(
      "progress",
      (e: ProgressEvent) => {
        if (e.lengthComputable) {
          const percentage = Math.round(e.loaded * 100 / e.total);
          this.setState({ progress: percentage });
          console.log("Progress", percentage);
        }
      },
      false
    );
    const data = new FormData();
    data.append("file", file);
    request.send(data);
  };
  handleClose = () => {
    this.setState({ open: false });
  };
  handleOpen = () => {
    this.setState({ open: true });
  };
  render = () => {
    const { trigger } = this.props;
    const { open } = this.state;
    return (
      <Modal
        trigger={trigger(this.handleOpen)}
        open={open}
        onClose={this.handleClose}
      >
        <Modal.Header>Upload</Modal.Header>
        <Modal.Content>
          <Modal.Description>
            <Input
              type="file"
              onChange={this.onChange}
              input={<input accept="audio/*" />}
            />
          </Modal.Description>
        </Modal.Content>
      </Modal>
    );
  };
}
