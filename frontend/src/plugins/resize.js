import _ from 'lodash'
import {ResizeSensor} from 'css-element-queries'

export default (cb) => {
  var sensor;
  return {
    initialize: (plugin) => {
      _.defer(() => {
        sensor = new ResizeSensor(plugin.getEditorRef().refs.editor, cb);
      });
    },
    willUnmount: (plugin) => {
      sensor.detach();
    }
  }
}
