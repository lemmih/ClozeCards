export default function(diagnose = [], action) {
  switch (action.type) {
    case 'UPDATE_BRAIN': {
      const truncated = diagnose.slice(0,100);
      truncated.unshift(action.payload);
      return truncated;
    }
    default:
      return diagnose;
  }
}
