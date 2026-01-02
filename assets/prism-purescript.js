Prism.languages.purescript = {
  comment: {
    pattern:
      /(^|[^-!#$%*+=?&@|~.:<>^\\\/])(?:--(?:(?=.)[^-!#$%*+=?&@|~.:<>^\\\/].*|$)|\{-[\s\S]*?-\})/m,
    lookbehind: true,
  },
  char: {
    pattern:
      /'(?:[^\\']|\\(?:[abfnrtv\\"'&]|\^[A-Z@[\]^_]|NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE|DC1|DC2|DC3|DC4|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS|US|SP|DEL|\d+|o[0-7]+|x[0-9a-fA-F]+))'/,
    alias: "string",
  },
  string: {
    pattern: /"(?:[^\\"]|\\(?:\S|\s+\\))*"/,
    greedy: true,
  },
  keyword:
    /\b(?:case|class|data|deriving|do|else|if|in|infixl|infixr|instance|let|module|newtype|of|primitive|then|type|where)\b/,
  "import-statement": {
    // The imported or hidden names are not included in this import
    // statement. This is because we want to highlight those exactly like
    // we do for the names in the program.
    pattern:
      /(^[\t ]*)import\s+(?:qualified\s+)?(?:[A-Z][\w']*)(?:\.[A-Z][\w']*)*(?:\s+as\s+(?:[A-Z][\w']*)(?:\.[A-Z][\w']*)*)?(?:\s+hiding\b)?/m,
    lookbehind: true,
    inside: {
      keyword: /\b(?:import|qualified|as|hiding)\b/,
      punctuation: /\./,
    },
  },
  // These are builtin variables only. Constructors are highlighted later as a constant.
  builtin: /\b(?:)\b/,
  // decimal integers and floating point numbers | octal integers | hexadecimal integers
  number: /\b(?:\d+(?:\.\d+)?(?:e[+-]?\d+)?|0o[0-7]+|0x[0-9a-f]+)\b/i,
  operator: [
    {
      // infix operator
      pattern: /`(?:[A-Z][\w']*\.)*[_a-z][\w']*`/,
      greedy: true,
    },
    {
      // function composition
      pattern: /(\s)\.(?=\s)/,
      lookbehind: true,
    },
    // Most of this is needed because of the meaning of a single '.'.
    // If it stands alone freely, it is the function composition.
    // It may also be a separator between a module name and an identifier => no
    // operator. If it comes together with other special characters it is an
    // operator too.
    //
    // This regex means: /[-!#$%*+=?&@|~.:<>^\\\/]+/ without /\./.
    /[-!#$%*+=?&@|~:<>^\\\/][-!#$%*+=?&@|~.:<>^\\\/]*|\.[-!#$%*+=?&@|~.:<>^\\\/]+/,
  ],
  // In Haskell, nearly everything is a variable, do not highlight these.
  hvariable: {
    pattern: /\b(?:[A-Z][\w']*\.)*[_a-z][\w']*/,
    inside: {
      punctuation: /\./,
    },
  },
  constant: {
    pattern: /\b(?:[A-Z][\w']*\.)*[A-Z][\w']*/,
    inside: {
      punctuation: /\./,
    },
  },
  punctuation: /[{}[\];(),.:]/,
};

Prism.languages.purs = Prism.languages.purescript;
