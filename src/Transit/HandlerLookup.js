export const runImpl = ({ no, yes }, handlerLookup, state, msg) => {
  const stateLookupResult = handlerLookup[state.type];
  if (!stateLookupResult) return no;
  const handler = stateLookupResult[msg.type];
  if (!handler) return no;
  return yes(handler(state.value)(msg.value));
};
