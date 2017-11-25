import _ from "lodash";
import { ResizeSensor } from "css-element-queries";

export default cb => {
  var sensor;
  return {
    initialize: plugin => {
      _.defer(() => {
        const editor = plugin.getEditorRef();
        if (editor) sensor = new ResizeSensor(editor.refs.editor, cb);
      });
    },
    willUnmount: plugin => {
      if (sensor) sensor.detach();
    }
  };
};
