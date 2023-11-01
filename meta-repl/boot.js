async function load() {
  try {
    await Scheme.load_main("repl.wasm", {}, {
      document: {
        body() { return document.body; },
        getElementById: Document.prototype.getElementById.bind(document),
        createTextNode: Document.prototype.createTextNode.bind(document),
        createElement: Document.prototype.createElement.bind(document),
        createTreeWalker: Document.prototype.createTreeWalker.bind(document)
      },
      event: {
        preventDefault(event) {
          event.preventDefault();
        },
        keyboardCode(event) {
          return event.code;
        }
      },
      element: {
        value(elem) {
          return elem.value;
        },
        setValue(elem, value) {
          elem.value = value;
        },
        scrollHeight(elem) {
          return elem.scrollHeight;
        },
        setScrollTop(elem, value) {
          elem.scrollTop = value;
        },
        appendChild(parent, child) {
          return parent.appendChild(child);
        },
        setAttribute(elem, name, value) {
          elem.setAttribute(name, value);
        },
        removeAttribute(elem, name) {
          elem.removeAttribute(name);
        },
        remove(elem) {
          elem.remove();
        },
        replaceWith(oldElem, newElem) {
          oldElem.replaceWith(newElem);
        },
        addEventListener(elem, name, f) {
          elem.addEventListener(name, f);
        },
        removeEventListener(elem, name, f) {
          elem.removeEventListener(name, f);
        }
      },
      treeWalker: {
        currentNode(walker) {
          return walker.currentNode;
        },
        setCurrentNode(walker, node) {
          walker.currentNode = node;
        },
        nextNode(walker) {
          return walker.nextNode();
        },
        firstChild(walker) {
          return walker.firstChild();
        },
        nextSibling(walker) {
          return walker.nextSibling();
        }
      }
    });
  } catch(e) {
    if(e instanceof WebAssembly.CompileError) {
      document.getElementById("wasm-error").hidden = false;
    }
  }
}
window.addEventListener("load", load);
