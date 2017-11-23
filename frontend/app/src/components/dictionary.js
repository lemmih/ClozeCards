import React, { PureComponent } from "react";

// state contains a

const dictStyle = {
  position: "fixed",
  top: 0,
  left: "50%",
  marginLeft: "-15em",
  width: "30em",
  zIndex: 1001,
  boxShadow: "1px 1px 5px #0074D9",
  background: "white",
  padding: "1ex"
};
/*
mandarin::string
pinyin::string[]
definitions::string[][]
sentence_id
offset
text_id
*/
export default class extends PureComponent {
  render = () => {
    return <div style={dictStyle}>Dictionary</div>;
  };
}
