export const runImpl = ({ no, yes }, handlerLookup, state, msg) => {
  const stateType = state.type;
  const stateLookup = handlerLookup[stateType];
  if (stateLookup === undefined) return no;
  const handler = stateLookup[msg.type];
  if (handler === undefined) return no;
  return yes(handler(state.value, msg.value));
};
