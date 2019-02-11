import Map from './map_utils';
import { objectCompareByValue } from './obj_utils';
import statementIntoText from './stringify_statement';
import ruleNameToLabel from './stringify_rule';
import { DataSet, Network } from 'vis/index-network';
import './css/truth_tree.css';
import 'ionicons/dist/css/ionicons.min.css';

// TODO: To "move" the edges' origin points to under the text (rather than its center):
// Create an intermediary node with no text, add a hidden edge from the original node
// to this intermediary, and then make an edge from this one to the one the original
// one pointed to
// I wish there was simply a way to control an edge's origin point :(
// The solution above either requires me to modify _computeLevels to take into account
// those hidden edges, or _offsetLevels for every child branch. The first is messy,
// and the second unnecessarily slow.

export default class TruthTree {
  constructor() {
    this.network = null;

    this.container = document.getElementById('truth-tree');

    this.css = {
      'statement-text-colour': this._cssValue('--statement-text-colour'),
      'statement-text-size': parseInt(this._cssValue('--statement-text-size')),
      'statement-text-face': this._cssValue('--statement-text-face'),
      'branch-close-icon-face': this._cssValue('--branch-close-icon-face'),
      'branch-close-icon-code': String.fromCharCode(parseInt(this._cssValue('--branch-close-icon-code'), 16)),
      'branch-close-icon-size': parseInt(this._cssValue('--branch-close-icon-size')),
      'branch-close-icon-colour': this._cssValue('--branch-close-icon-colour'),
      'edge-colour': this._cssValue('--edge-colour'),
      'linenumber-text-face': this._cssValue('--linenumber-text-face'),
      'linenumber-text-size': parseInt(this._cssValue('--linenumber-text-size')),
      'linenumber-text-colour': this._cssValue('--linenumber-text-colour'),
      'derivedfrom-text-face': this._cssValue('--derivedfrom-text-face'),
      'derivedfrom-text-size': parseInt(this._cssValue('--derivedfrom-text-size')),
      'derivedfrom-text-colour': this._cssValue('--derivedfrom-text-colour'),
      'level-separation': parseInt(this._cssValue('--level-separation')),
      'node-spacing': parseInt(this._cssValue('--node-spacing')),
      'checkmark-size': parseInt(this._cssValue('--checkmark-size')),
      'checkmark-face': this._cssValue('--checkmark-face'),
      'checkmark-icon-code': String.fromCharCode(parseInt(this._cssValue('--checkmark-icon-code'), 16))
    }

    this.options = {
      autoResize: true,
      configure: {
        enabled: false,
      },
      physics: {
        enabled: false,
      },
      manipulation: {
        enabled: false,
      },
      interaction: {
        dragNodes: false,
        dragView: true,
        hover: false,
        hoverConnectedEdges: false,
        keyboard: {
          enabled: true,
          bindToWindow: false,
        },
        selectable: false,
        selectConnectedEdges: false,
      },
      layout: {
        hierarchical: {
          enabled: true,
          levelSeparation: this.css['level-separation'],
          nodeSpacing: this.css['node-spacing'],
          treeSpacing: 0,
          blockShifting: false,
          edgeMinimization: false,
          parentCentralization: false,
          direction: 'UD',
          sortMethod: 'directed',
        },
      },
    };

    this.idCounter = 0;
  }

  _parseBranchData(mapIds, branchData, parent) {
    var branch = {};

    branch.parent = parent;

    mapIds.set(branchData.id, branch);

    branch.nodes = branchData.nodes.reduce((nodes, nodeData) => {
      nodes.push(this._parseNodeData(mapIds, nodeData, branch));
      return nodes;
    }, []);

    branch.children = branchData.children.reduce(
      (children, childData) => {
        children.push(this._parseBranchData(mapIds, childData, branch));
        return children;
      },
      []
    );

    branch.closed = branchData.closed;

    return branch;
  }

  _parseNodeData(mapIds, nodeData, ownerBranch) {
    var node = {};

    mapIds.set(nodeData.id, node);

    node.text = statementIntoText(nodeData.statement);

    if (nodeData.derived_from != null) {
      var derivedFromNodeId = nodeData.derived_from.node_id;
      var derivedFromBranchId = nodeData.derived_from.branch_id;
      var derivedWithRule = nodeData.derived_from.rule;
      var derivationId = nodeData.derived_from.derivation_id;

      node.derivedFrom = {
        node: mapIds.findByObjectIndex(derivedFromNodeId),
        branch: mapIds.findByObjectIndex(derivedFromBranchId),
        rule: ruleNameToLabel(derivedWithRule),
        derivationId
      }

      // Mark the statement that this was derived from as done
      // (unless it's an UQ rule, which can be applied
      // infinitely many times)
      if (derivedWithRule != "UniversalQuantifier") {
        node.derivedFrom.node.done = true;
      }
    }
    else {
      node.derivedFrom = null;
    }

    node.done = false;
    node.ownerBranch = ownerBranch;

    node.treeLevel = null;
    node.treeId = null;

    return node;
  }

  _findAllNodesForEachDerivation(mainTrunk) {
    // A derivation can only appear in either one
    // single branch (one or more statements)
    // or spanning sibling branches, but NEVER 
    // more than once in the same tree path but different branches
    var mapDerivations = new Map();

    var stack = [mainTrunk];

    while (stack.length != 0) {
      var branch = stack.pop();

      branch.nodes.forEach(node => {
        if (node.derivedFrom != null) {
          var derivs = mapDerivations.findByObjectIndex(node.derivedFrom.derivationId);
          if (derivs != null) {
            if (!derivs.find(x => x.ownerBranch == node.ownerBranch)) {
              derivs.push(node);
            } else {
              derivs.push(node);
            }
          } else {
            mapDerivations.set(node.derivedFrom.derivationId, [node]);
          }
        }
      });

      stack.push(...branch.children);
    }

    return mapDerivations;
  }

  _offsetLevels(branch, start, offset) {
    branch.nodes.forEach(node => {
      if (node.treeLevel != null && node.treeLevel >= start) {
        node.treeLevel += offset;
      }
    });

    branch.children.forEach(child => {
      this._offsetLevels(child, child.nodes[0].treeLevel, offset);
    });
  }

  _computeLevels(mainTrunk) {
    // 1. Find and group all nodes from each different derivation
    // 2. Iterate over all nodes, assigning it a 'currLevel' (where currLevel is incremented
    // for every node, and resets to the level of the first node of one branch when moving
    // to a sibling branch), over each branch, 
    // and:
    //    3. If it is not a derived statement, assign a level of 'currLevel' and move on, else:
    //    4. Look up in the derivations map  
    //    5. Iterate over all nodes (where its level is 'y') from the same derivation (and same line)
    //    6. If there is some 'y > currLevel', set 'currLevel = y'
    //    7. If there is any 'y < currLevel', (for all) change 'y' to 'currLevel' and increment levels of all 
    //       following nodes in that branch of 'y'
    //    8. if 'y' is not defined yet, assign 'currLevel' to the node
    var mapDerivations = this._findAllNodesForEachDerivation(mainTrunk);

    var currLevel = 0;

    var stack = [[mainTrunk, currLevel]];

    while (stack.length != 0) {
      var [branch, currLevel] = stack.pop();

      branch.nodes.forEach(node => {
        currLevel += 1;

        if (node.derivedFrom == null) {
          node.treeLevel = currLevel;
        } else {
          var derivs = mapDerivations.findByObjectIndex(node.derivedFrom.derivationId);

          derivs.filter(x => objectCompareByValue(x.derivedFrom.derivationId, node.derivedFrom.derivationId)).forEach(x => {
            if (x.treeLevel == null) {
              node.treeLevel = currLevel;
            }
            else if (x.treeLevel > currLevel) {
              currLevel = x.treeLevel;
              node.treeLevel = currLevel;
            }
            else if (x.treeLevel < currLevel) {
              this._offsetLevels(
                x.ownerBranch,
                x.treeLevel,
                currLevel - x.treeLevel
              );
            }
          });
        }
      });

      stack.push(...branch.children.map(x => [x, currLevel]));
    }
  }

  _compute(data) {
    var mainTrunk = this._parseBranchData(new Map(), data.main_trunk, null);
    this._computeLevels(mainTrunk);

    var dataSet = {
      nodes: [],
      edges: [],
    };

    this._populateDataSet(dataSet, mainTrunk);
    this._computeLineNumbers(dataSet);

    return [{
      nodes: new DataSet(dataSet.nodes),
      edges: new DataSet(dataSet.edges),
    }, mainTrunk];
  }


  _cssValue(property) {
    return window.getComputedStyle(this.container).getPropertyValue(property);
  }

  _insertStatementNodeOnDataSet(dataSet, text, level, node) {
    this.idCounter += 1;
    dataSet.nodes.push({
      id: this.idCounter,
      level,
      shape: 'text',
      label: text,
      font: {
        color: this.css['statement-text-colour'],
        size: this.css['statement-text-size'],
        face: this.css['statement-text-face']
      },
      // own properties
      nodeRef: node, // useful to know which nodes refer to actual statements
      // (there can be hidden nodes & icon nodes as well)
      // and for post-processing of the dataset
      lineNumber: null, // line we'll associate to this node
    });
    return this.idCounter;
  }

  _insertBranchClosedNodeOnDataSet(dataSet, level) {
    this.idCounter += 1;
    dataSet.nodes.push({
      id: this.idCounter,
      level,
      shape: 'icon',
      icon: {
        face: this.css['branch-close-icon-face'],
        code: this.css['branch-close-icon-code'],
        size: this.css['branch-close-icon-size'],
        color: this.css['branch-close-icon-colour']
      }
    });
    return this.idCounter;
  }

  _insertHiddenEdgeOnDataSet(dataSet, from, to) {
    dataSet.edges.push({
      from,
      to,
      color: { opacity: 0 }
    });
  }

  _insertEdgeOnDataSet(dataSet, from, to) {
    dataSet.edges.push({
      from,
      to,
      color: { color: this.css['edge-colour'] }
    });
  }

  _populateDataSet(dataSet, branch) {
    var last = branch.nodes.reduce((prev, node) => {
      node.treeId = this._insertStatementNodeOnDataSet(dataSet, node.text, node.treeLevel, node);

      if (prev != null) {
        this._insertHiddenEdgeOnDataSet(dataSet, prev.treeId, node.treeId);
      }

      return node;
    }, null);

    if (branch.closed) {
      var closedIconNodeId = this._insertBranchClosedNodeOnDataSet(dataSet, last.treeLevel + 1);
      this._insertHiddenEdgeOnDataSet(dataSet, last.treeId, closedIconNodeId);
    }
    else {
      branch.children.forEach(child => {
        this._populateDataSet(dataSet, child);
        this._insertEdgeOnDataSet(dataSet, last.treeId, child.nodes[0].treeId);
      });
    }
  }

  _computeLineNumbers(dataSet) {
    // Associate a line number with each level that contains statement nodes,
    // in order from lowest level to highest level
    var visitedLevels = [];

    var lineNumberCounter = 0;

    dataSet.nodes.sort((a, b) => a.level - b.level).forEach(node => {
      if (node.nodeRef != null) {
        var existing = visitedLevels.find(x => x.level == node.level);
        if (existing) {
          node.lineNumber = existing.lineNumber;
        }
        else {
          lineNumberCounter += 1;
          node.lineNumber = lineNumberCounter;
          visitedLevels.push({
            level: node.level,
            lineNumber: lineNumberCounter
          });
        }
      }
    });
  }

  _renderLineNumber(ctx, lineNumber, xRightBound, y) { // x is the rightmost bound for where the text must be rendered
    ctx.font = this.css['linenumber-text-size'].toString().concat('px ', this.css['linenumber-text-face']);
    ctx.fillStyle = this.css['linenumber-text-colour'];
    ctx.textBaseline = 'middle'; // Absolutely critical

    var text = lineNumber.toString().concat('.');
    var textWidth = ctx.measureText(text).width;

    ctx.fillText(text, xRightBound - textWidth, y);

    return textWidth;
  }

  _renderDoneCheckMark(ctx, xRightBound, y) { // see _renderLineNumber for xRightBound
    ctx.font = this.css['checkmark-size'].toString().concat('px ', this.css['checkmark-face']);
    ctx.fillStyle = this.css['checkmark-colour'];

    var text = this.css['checkmark-icon-code'];
    var textWidth = ctx.measureText(text).width;

    ctx.fillText(text, xRightBound - textWidth, y);
  }

  _renderLeftGutter(ctx, dataSet) {
    // Find the leftmost boundary to the tree in canvas space
    var leftmostBoundX = window.innerWidth; // Some large value
    dataSet.nodes.forEach(node => {
      var leftBound = this.network.getBoundingBox(node.id).left;
      leftmostBoundX = Math.min(leftmostBoundX, leftBound);
    });

    // TODO: Should I bother ignoring already handled nodes from same level?

    // Draw line numbers from ]-infinity, leftmostBoundX] and check marks
    // for nodes that have had a rule applied to them already (except
    // for UQ)
    dataSet.nodes.forEach(node => {
      if (node.nodeRef != null) {
        var nodeOriginY = this._calculateNodeOriginY(node.id);

        var lineNumberTextWidth = this._renderLineNumber(
          ctx,
          node.lineNumber,
          leftmostBoundX,
          nodeOriginY
        );

        if (node.nodeRef.done) {
          this._renderDoneCheckMark(
            ctx,
            leftmostBoundX - lineNumberTextWidth - 5, // - 5 for spacing
            nodeOriginY
          );
        }
      }
    });
  }

  // Calculates the position on the Y axis of the origin of any given node
  // (canvas space)
  _calculateNodeOriginY(nodeId) {
    var boundingBox = this.network.getBoundingBox(nodeId);
    return (boundingBox.top + boundingBox.bottom) / 2;
  }

  // Renders '<derived from ...>' data on every line
  _renderRightGutter(ctx, dataSet) {
    // Find the rightmost boundary to the tree in canvas space
    var rightmostBoundX = -window.innerWidth; // Some large negative value
    dataSet.nodes.forEach(node => {
      var rightBound = this.network.getBoundingBox(node.id).right;
      rightmostBoundX = Math.max(rightmostBoundX, rightBound);
    });

    // At any one level, all statements are derived from the same one
    // statement, hence derivation data will be the same
    var data = {};

    dataSet.nodes.get({
      filter: x => x.nodeRef != null && x.nodeRef.derivedFrom != null
    }).forEach(node => {
      if (!Object.keys(data).find(x => x == node.nodeRef.treeLevel)) {
        data[node.level] = { nodeId: node.id, derivedFrom: node.nodeRef.derivedFrom };
      }
    });

    // Render
    ctx.font = this.css['derivedfrom-text-size'].toString().concat('px ', this.css['derivedfrom-text-face']);
    ctx.fillStyle = this.css['derivedfrom-text-colour'];
    ctx.textBaseline = 'middle'; // Absolutely critical (in order to really center the text)

    Object.values(data).forEach(levelData => {
      // Get the center Y position for this level
      var y = this._calculateNodeOriginY(levelData.nodeId);

      var lineNumber = dataSet.nodes.get(levelData.derivedFrom.node.treeId).lineNumber;
      var rule = levelData.derivedFrom.rule;

      var text = ['(', lineNumber, ', ', rule, ')'].join('');

      ctx.fillText(text, rightmostBoundX, y);
    });
  }

  render(data) {
    this.idCounter = 0;

    var [dataSet, mainTrunk] = this._compute(data);

    if (this.network != null) {
      this.network.destroy();
    }

    this.network = new Network(this.container, dataSet, this.options);

    this.network.on('afterDrawing', ctx => {
      this._renderLeftGutter(ctx, dataSet);

      this._renderRightGutter(
        ctx,
        dataSet
      );
    });
  }
}

// new TruthTree().render({
//   main_trunk: {
//     id: 0,
//     closed: false,
//     nodes: [
//       {
//         id: 20,
//         statement: {
//           LogicalDisjunction: [
//             {
//               Simple: ['A', null],
//             },
//             {
//               Simple: ['B', null]
//             }
//           ]
//         },
//         derived_from: null,
//       },
//       {
//         id: 21,
//         statement: {
//           Simple: ['D', null]
//         },
//         derived_from: null,
//       },
//       {
//         id: 24,
//         statement: {
//           LogicalConjunction: [
//             {
//               Simple: ['D', null]
//             },
//             {
//               Simple: ['E', null]
//             }
//           ],
//           derived_from: null,
//         },
//       }
//     ],
//     children: [
//       {
//         id: 1,
//         closed: true,
//         nodes: [
//           {
//             id: 22,
//             statement: {
//               Simple: ['A', null]
//             },
//             derived_from: {
//               node_id: 20,
//               branch_id: 0,
//               rule: 'Disjunction',
//               derivation_id: {
//                 id: 1,
//                 index: 1
//               },
//             },
//           },
//           {
//             id: 25,
//             statement: {
//               Simple: ['D', null]
//             },
//             derived_from: {
//               node_id: 24,
//               branch_id: 0,
//               rule: 'Conjunction',
//               derivation_id: {
//                 id: 2,
//                 index: 1,
//               },
//             }
//           },
//           {
//             id: 26,
//             statement: {
//               Simple: ['E', null]
//             },
//             derived_from: {
//               node_id: 24,
//               branch_id: 0,
//               rule: 'Conjunction',
//               derivation_id: {
//                 id: 2,
//                 index: 2
//               },
//             }
//           }
//         ],
//         children: [

//         ]
//       },
//       {
//         id: 2,
//         closed: true,
//         nodes: [
//           {
//             id: 23,
//             statement: {
//               Simple: ['B', null]
//             },
//             derived_from: {
//               node_id: 20,
//               branch_id: 0,
//               rule: 'Disjunction',
//               derivation_id: {
//                 id: 1,
//                 index: 1,
//               },
//             },
//           },
//           {
//             id: 27,
//             statement: {
//               Simple: ['G', null]
//             },
//             derived_from: null,
//           },
//           {
//             id: 25,
//             statement: {
//               Simple: ['D', null]
//             },
//             derived_from: {
//               node_id: 24,
//               branch_id: 0,
//               rule: 'Conjunction',
//               derivation_id: {
//                 id: 2,
//                 index: 1,
//               },
//             }
//           },
//           {
//             id: 26,
//             statement: {
//               Simple: ['E', null]
//             },
//             derived_from: {
//               node_id: 24,
//               branch_id: 0,
//               rule: 'Conjunction',
//               derivation_id: {
//                 id: 2,
//                 index: 2,
//               },
//             }
//           }
//         ],
//         children: [

//         ]
//       },
//     ]
//   }
// });