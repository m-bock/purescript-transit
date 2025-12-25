export const runImpl =
  ({ fail, succeed }) =>
  (handlerLookup) =>
  (state) =>
  (msg) => {
    const stateLookupResult = handlerLookup[state.type];
    if (!stateLookupResult) return fail;
    const handler = stateLookupResult[msg.type];
    if (!handler) return fail;
    return succeed(handler(state.value)(msg.value));
  };
