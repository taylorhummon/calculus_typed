/* -*- Mode: Javascript; indent-tabs-mode:nil; js-indent-level: 2 -*- */

MathJax.Hub.Config({
  jax: ["input/TeX", "output/SVG"],
  extensions: ["tex2jax.js", "TeX/AMSmath.js"],
  showProcessingMessages: true,
  messageStyle: "none",
  displayAlign: "left",
  displayIndent: "2.5em",
  delayStartupUntil: "none",
  positionToHash: true,
  showMathMenu: false,
  showMathMenuMSIE: false,
  errorSettings: {
    message: ["[",["MathProcessingError","Math Processing Error"],"]"],
    style: {color: "#CC0000", "font-style":"italic"}
  },
  tex2jax: {
    inlineMath: [['\\(','\\)']],
    displayMath: [['\\[','\\]']],
    balanceBraces: true,
    skipTags: ["script","noscript","style","textarea","pre","code","annotation","annotation-xml"],
    processEnvironments: false,
    processRefs: false,
    preview: "none"
  },
  TeX: {
    TagSide: "right",
    TagIndent: "0.8em",
    MultLineWidth: "85%",
    Macros: {
      Number: "{\\mathtt{Number}}",
      Point: "{\\mathtt{Point}}",
      Metric: "{\\mathtt{Metric}}",
      PolyMetric: "{\\mathtt{Polymetric}}",
      Vector: "{\\mathtt{Vector}}",
      PolyVector: "{\\mathtt{Polyvector}}",
      Ruler: "{\\mathtt{Ruler}}",
      PolyRuler: "{\\mathtt{Polyruler}}",
      Blip: "{\\mathtt{Blip}}",
      Interval: "{\\mathtt{Interval}}",
      BoxType: "{\\mathtt{Box}}",
      Path: "{\\mathtt{Path}}",
      slope: "\\operatorname{slope}",
      bend: "\\operatorname{bend}",
      warp: "\\operatorname{warp}",
      tangent: "\\operatorname{tangent}",
      hessian: "\\operatorname{hess}",
      discriminant: "\\operatorname{disc}",
      directional: "\\operatorname{direc}",
      gradient: "\\operatorname{grad}",
      magnitude: "\\operatorname{mag}",
      transpose: "\\operatorname{tran}",
      position: "\\operatorname{pos}",
      velocity: "\\operatorname{vel}",
      speed: "\\operatorname{speed}",
      naturalbase: "\\operatorname{nat}",
      revolution: "\\operatorname{rev}",
      sumcomponent: "\\operatorname{sum}",
      productcomponent: "\\operatorname{product}",
      diff: "\\partial",
      boundary: "\\ddot{\\partial}",
      standard: "\\varepsilon",
      depends: "\\mathrel{\\overset{\\cdot}{\\leftarrow}}",
      of: "{\\,}",
      mult: "\\mathbin{\\mkern -2mu \\cdot \\mkern -2mu}",
      change: "\\Delta",
      degrees: "\\circ",
      sliced: "{\\, \\cdot \\,}",
      andSpaced: "{\\quad \\text{and} \\quad}",
      orSpaced: "{\\quad \\text{or} \\quad}",
      rad: "\\mathrm{r}",
      ang: "\\mathrm{a}",
     },
    equationNumbers: {
      autoNumber: "none",
      useLabelIds: true
    },
    noErrors: {
      disabled: false,
      multiLine: true,
      inlineDelimiters: ["",""],
      style: {
        "font-size":   "90%",
        "text-align":  "left",
        "color":       "black",
        "padding":     "1px 3px",
        "border":      "1px solid"
      }
    },
    noUndefined: {
      disabled: false,
      attributes: {
        mathcolor: "red"
      }
    },
    unicode: {
      fonts: "STIXGeneral,'Arial Unicode MS'"
    }
  },
  SVG: {
    font: "TeX",
    undefinedFamily: "STIXGeneral, serif",
    EqnChunk: 50,
    EqnChunkFactor: 1.5,
    EqnChunkDelay: 100,
    matchFontHeight: true,
    styles: {
      ".MathJax_SVG_Display": {
        margin: "0px"
      }
    }
  },
  MathMenu: {
    delay: 150,
    helpURL: "http://www.mathjax.org/help-v2/user/",
    showRenderer: true,
    showMathPlayer: true,
    showFontMenu: false,
    showContext:  false,
    showDiscoverable: false,
    semanticsAnnotations: {
      "TeX": ["TeX", "LaTeX", "application/x-tex"],
      "StarMath": ["StarMath 5.0"],
      "Maple": ["Maple"],
      "ContentMathML": ["MathML-Content", "application/mathml-content+xml"],
      "OpenMath": ["OpenMath"]
    },
    windowSettings: {
      status: "no", toolbar: "no", locationbar: "no", menubar: "no",
      directories: "no", personalbar: "no", resizable: "yes", scrollbars: "yes",
      width: 100, height: 50
    }
  },
  MathEvents: {
    hover: 500
  }
});

MathJax.Ajax.loadComplete("{DOMAIN}jaxconfig.js");
